#' Simulate order lifecycles
#'
#' Generates a synthetic population of orders moving through a distribution
#' lifecycle, as an event log plus one row per order. Everything else in this
#' package, and every example in the book, runs against its output.
#'
#' The generator exists so that worked examples can be published without
#' exposing anyone's operational data, and so that the pathologies the book is
#' about are present to be measured. It reproduces, deliberately:
#'
#' * **Right-censoring.** Orders still in flight at `as_of` have not finished,
#'   so their current dwell is a lower bound. This is the bias of Chapter 6.
#' * **Mixed dwell populations.** Each stage holds a fast population and a
#'   stuck one, so a single quantile over the mixture serves neither (§6.5).
#' * **Partial fills and loops.** Some orders ship in parts; a short pick sends
#'   an order back from `released` to `backordered` (§8.1, §8.2).
#' * **Residue.** Some finished orders still carry an open pick ticket, which
#'   is what overstates a naive work queue (§8.3).
#' * **Special orders.** Items bought from a vendor per order wait in
#'   `awaiting_supply` on a vendor clock, not a warehouse one (§5.7).
#' * **Covariate effects.** Dwell in `released` scales with the order's line
#'   count, and dwell in `received` depends on branch and channel, so that the
#'   regressions of §6.9 have something to recover and the recovered
#'   coefficients can be checked against the values used here.
#'
#' It is not a model of any real operation and its parameters are not fitted to
#' one. Treat the numbers it produces as illustrative.
#'
#' @param n Number of orders to generate.
#' @param seed Optional integer seed. Supply one for a reproducible corpus;
#'   the book's figures do.
#' @param as_of Observation cutoff as a `POSIXct`. Orders not finished by this
#'   instant are censored. Defaults to `start + horizon_days`, which leaves a
#'   small share of the population in flight -- a couple of percent at the
#'   default parameters, because most dwells are hours and the entry window is
#'   months. That is realistic rather than convenient: a live work queue is a
#'   small fraction of every order ever taken. What matters for Chapter 6 is not
#'   the size of the censored set but its composition, and it is composed almost
#'   entirely of the stuck population -- which is exactly why averaging
#'   completed visits only runs optimistic. Shorten `horizon_days`, or pass an
#'   earlier `as_of`, for a larger censored share.
#' @param horizon_days Days of order entry to generate.
#' @param start Entry date of the first order.
#'
#' @return An object of class `lifecycle_sim`: a list with `orders` (one row
#'   per order), `events` (the append-only event log, one row per transition,
#'   with `order_no`, `event`, `stage`, `occurred_at`), `lines` (one row per
#'   order line) and `items` (one row per item).
#'
#'   Each line carries `qty_ordered`, `unit_price` and `qty_shipped`, together
#'   with the stock position its item showed at the moment the order was
#'   entered: `on_hand_at_entry`, `on_order_at_entry`, `promised_in_days`,
#'   `demand_30d` and `cover_ratio`. Position is a function of the item and the
#'   time, so two lines hitting one item at one instant see the same on-hand
#'   quantity. Each item carries its `item_class`, `vendor`, `unit_cost`,
#'   replenishment `cycle_days`, `stockout_days`, `demand_rate` and the
#'   `lead_time_mean` and `lead_time_sd` of its vendor.
#'
#'   Two order-level date fields support Chapter 13. `requested_date` equals
#'   the order date on roughly 85% of orders, reproducing a field that defaults
#'   to today and is occasionally overridden, which is the defect measured on a
#'   hand-checked sample of 59 orders where 58 equaled the order date.
#'   `promised_date` is the field that does carry a commitment: it exists only
#'   where a person entered the order, never equals the order date, and has a
#'   lead averaging five days. The contrast between the two is what
#'   @sec-testing-a-date-field tests for.
#'
#'   The shortfall on a line is drawn before its stock position, and the line
#'   is then matched to an item and instant whose position agrees with that
#'   outcome. The joint distribution is the same under either factorization;
#'   implementing this one keeps every figure in Chapters 5 to 11 unchanged.
#'   Roughly an eighth of lines are left deliberately inconsistent, which is
#'   what bounds how well a model can do: a shortfall predictor on these
#'   features reaches an AUC near 0.86, against 0.58 on order attributes
#'   alone and 0.83 for the single comparison of on-hand against the quantity
#'   ordered.
#'
#'   The extended value of an order's lines sums to its `value`, and the three
#'   `fill_*` columns on `orders` are computed from the lines rather than drawn
#'   separately, so that the dimensions of Chapter 11 describe one population.
#'   They are `NA` where the order has not reached a shipping stage, because an
#'   order that has not shipped has no fill rate yet.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 500, seed = 1)
#' nrow(sim$orders)
#' head(sim$events, 3)
#' table(sim$orders$stage)
#' head(sim$lines, 3)
#' head(sim$items, 3)
#' @export
simulate_lifecycles <- function(n = 5000,
                                seed = NULL,
                                as_of = NULL,
                                horizon_days = 120,
                                start = as.POSIXct("2026-01-05 07:00:00", tz = "UTC")) {
  if (!is.null(seed)) set.seed(seed)
  stopifnot(n >= 1, horizon_days > 0)

  hrs <- function(x) x * 3600

  # Order attributes. The branch and channel splits are the ones the book uses
  # as cohort keys in Chapter 12; the channel split is the stronger of the two.
  branch  <- sample(c("60", "30"), n, TRUE, prob = c(0.77, 0.23))
  source_ <- sample(c("estore", "inside_sales"), n, TRUE, prob = c(0.76, 0.24))
  value   <- round(stats::rlnorm(n, meanlog = 6.1, sdlog = 1.15), 2)
  lines_n <- 1L + stats::rpois(n, 2.1)

  entered <- start + stats::runif(n, 0, hrs(24 * horizon_days))
  if (is.null(as_of)) as_of <- start + hrs(24 * horizon_days)

  special     <- stats::runif(n) < 0.10   # bought from a vendor per order
  credit_hold <- stats::runif(n) < 0.03
  cancelled   <- stats::runif(n) < 0.025

  # A two-component dwell: a fast population and a stuck one. The mixture is
  # the point -- the mean of it describes neither component.
  mixed_dwell <- function(k, p_stuck, fast_mean, stuck_meanlog, stuck_sdlog) {
    stuck <- stats::runif(k) < p_stuck
    out <- stats::rexp(k, rate = 1 / fast_mean)
    out[stuck] <- stats::rlnorm(sum(stuck), stuck_meanlog, stuck_sdlog)
    out
  }

  # received -> released. Chapter 5 measured 18% past 24h and 12% past 72h.
  d_received <- mixed_dwell(n, 0.18, 5, log(40), 1.25)
  # awaiting_supply runs on a vendor lead time: days to weeks, and not a stall.
  d_supply <- stats::rlnorm(n, log(96), 0.8)
  d_credit <- mixed_dwell(n, 0.25, 3, log(30), 1.0)
  # released -> shipped. Chapter 6: p50 1.7h, p90 20h, p99 3.8d.
  d_released <- mixed_dwell(n, 0.16, 1.9, log(26), 1.3)
  d_partial  <- mixed_dwell(n, 0.30, 9, log(60), 1.0)

  # Covariate effects on dwell. Without them every dwell is independent of
  # everything recorded about the order, a regression on this corpus correctly
  # finds nothing, and Chapter 6 can demonstrate only what a null result looks
  # like. The effects are declared here, rather than buried in the draws, so
  # that a coefficient recovered in Chapter 6 or Chapter 9 can be checked
  # against the value that produced it.
  #
  #   released: handling work, so dwell scales with the work content of the
  #     order, sublinearly in the number of lines -- picking a second line
  #     costs less than picking the first.
  #   received: an administrative release, so dwell depends on who processes
  #     it rather than on the order's size. The smaller branch is slower, and
  #     an inside-sales order needs a person where an e-store order does not.
  #
  # Nothing here makes dwell depend on the backlog present on arrival, because
  # the generator has no queue: orders do not compete for a resource. Section
  # 6.9 tests for a backlog effect and correctly finds none, which is the
  # result to expect from data built this way and is stated there as such.
  # Each factor is divided by its own mean over the population, so it changes
  # which orders wait longer without changing how long the stage holds orders
  # on average. That matters beyond tidiness: the stage decomposition of
  # Chapter 5 is computed from these same draws, and a factor with a mean above
  # one would silently relabel which stage carries the most lead time.
  centered <- function(x) x / mean(x)

  f_lines   <- centered((lines_n / 3)^0.45)
  f_branch  <- centered(ifelse(branch == "30", 1.55, 1))
  f_channel <- centered(ifelse(source_ == "inside_sales", 1.40, 1))

  d_released <- d_released * f_lines
  d_received <- d_received * f_branch * f_channel

  # Events accumulate into preallocated vectors rather than into a list of
  # one-row data frames. The list version was quadratic: do.call(rbind, ...)
  # over tens of thousands of frames dominated the run, so n = 5000 took 27
  # seconds and n = 20000 would have taken minutes. Every chapter of the book
  # regenerates this corpus, so the cost was paid eight times per build.
  #
  # The draws are untouched and happen in the same order, so the output is
  # identical to the list version; only the assembly changed.
  cap <- 8L * n
  ev_order <- character(cap)
  ev_event <- character(cap)
  ev_stage <- character(cap)
  ev_at <- numeric(cap)
  n_ev <- 0L
  add <- function(order_no, event, stage, at) {
    n_ev <<- n_ev + 1L
    if (n_ev > length(ev_order)) {           # grow by doubling, never per row
      ev_order <<- c(ev_order, character(length(ev_order)))
      ev_event <<- c(ev_event, character(length(ev_event)))
      ev_stage <<- c(ev_stage, character(length(ev_stage)))
      ev_at <<- c(ev_at, numeric(length(ev_at)))
    }
    ev_order[n_ev] <<- order_no
    ev_event[n_ev] <<- event
    ev_stage[n_ev] <<- stage
    ev_at[n_ev] <<- as.numeric(at)
  }

  stage_now  <- character(n)
  finished   <- as.POSIXct(rep(NA, n), tz = "UTC")
  looped     <- integer(n)
  partial     <- logical(n)
  residue     <- logical(n)
  short_close <- logical(n)
  shortfall   <- logical(n)
  fill_units <- rep(1, n)
  fill_lines <- rep(1, n)
  fill_value <- rep(1, n)

  order_no <- sprintf("S%06d", seq_len(n))

  for (i in seq_len(n)) {
    t <- entered[i]
    add(order_no[i], "order_entered", "received", t)
    stage_now[i] <- "received"

    if (cancelled[i]) {
      t <- t + hrs(stats::runif(1, 1, 60))
      if (t <= as_of) {
        add(order_no[i], "order_cancelled", "cancelled", t)
        stage_now[i] <- "cancelled"; finished[i] <- t
      }
      next
    }

    if (special[i]) {
      t <- t + hrs(stats::runif(1, 0.2, 3))
      if (t > as_of) next
      add(order_no[i], "supply_awaited", "awaiting_supply", t)
      stage_now[i] <- "awaiting_supply"; shortfall[i] <- TRUE
      t <- t + hrs(d_supply[i])
      if (t > as_of) next
    }

    if (credit_hold[i]) {
      t <- t + hrs(stats::runif(1, 0.1, 2))
      if (t > as_of) next
      add(order_no[i], "credit_hold_placed", "credit_hold", t)
      stage_now[i] <- "credit_hold"
      t <- t + hrs(d_credit[i])
      if (t > as_of) next
    }

    t <- t + hrs(d_received[i])
    if (t > as_of) next
    add(order_no[i], "pick_ticket_released", "released", t)
    stage_now[i] <- "released"

    # A short pick bounces the order back. This is the loop of Chapter 8.
    if (stats::runif(1) < 0.09) {
      t <- t + hrs(stats::runif(1, 0.5, 8))
      if (t > as_of) next
      add(order_no[i], "stock_short", "backordered", t)
      stage_now[i] <- "backordered"; looped[i] <- 1L; shortfall[i] <- TRUE
      t <- t + hrs(mixed_dwell(1, 0.3, 10, log(36), 0.9))
      if (t > as_of) next
      add(order_no[i], "pick_ticket_released", "released", t)
      stage_now[i] <- "released"
    }

    t <- t + hrs(d_released[i])
    if (t > as_of) next

    # Some orders ship in parts and then sit. The remaining lines belong to no
    # queue in most systems, which is why the book counts them (§8.2).
    if (lines_n[i] > 1L && stats::runif(1) < 0.14) {
      add(order_no[i], "partial_shipment", "partially_shipped", t)
      stage_now[i] <- "partially_shipped"; partial[i] <- TRUE
      k <- sample(seq_len(lines_n[i] - 1L), 1L)
      fill_lines[i] <- k / lines_n[i]
      fill_units[i] <- stats::runif(1, 0.45, 0.95)
      fill_value[i] <- stats::runif(1, 0.35, 0.95)
      t <- t + hrs(d_partial[i])
      if (t > as_of) next
    }

    add(order_no[i], "order_shipped", "shipped", t)
    stage_now[i] <- "shipped"; finished[i] <- t

    # An order that shipped in parts either completes or is CLOSED SHORT, the
    # remaining lines cancelled. Resetting fill to 1 on every shipment -- the
    # obvious thing to write -- would mean no order ever records a short fill,
    # and Chapter 11 would have nothing to measure.
    if (partial[i] && stats::runif(1) < 0.45) {
      short_close[i] <- TRUE          # keep the partial fill as the final fill
    } else {
      fill_lines[i] <- 1; fill_units[i] <- 1; fill_value[i] <- 1
    }

    # Residue: a finished order still carrying an open pick ticket. Counting
    # open tickets rather than owed work overstates the queue (§8.3).
    if (stats::runif(1) < 0.30) residue[i] <- TRUE
  }

  # ---- Order lines, with quantities and prices ----------------------------
  #
  # Chapter 11 measures fill on units, lines and value, and the three can only
  # disagree if the lines of an order differ in size and in price. The order
  # record cannot carry that: a fraction of units filled says nothing about
  # which line was short.
  #
  # Every draw below is vectorized and happens after the loop above, so adding
  # this leaves the rest of the population byte-identical. That was verified
  # across three seeds and two sizes rather than assumed.
  n_lines <- sum(lines_n)
  qty_ordered <- 1L + stats::rpois(n_lines, 4)
  # A price multiplier per line, so the expensive line is not always the big
  # one. Scaled within each order so that the extended value of its lines sums
  # to the order value already drawn.
  price_raw <- stats::rlnorm(n_lines, 0, 0.8)
  line_order <- rep(order_no, lines_n)
  ext_raw <- qty_ordered * price_raw
  ext_total <- tapply(ext_raw, line_order, sum)[line_order]
  unit_price <- price_raw * (rep(value, lines_n) / as.numeric(ext_total))

  # A short order ships its first k lines complete and the rest not at all,
  # where k is the line count the loop above already settled. Line fill is
  # therefore exactly the fraction drawn there, while fill on units and on
  # value follow from which lines those happen to be.
  line_no_v <- sequence(lines_n)
  k_complete <- ifelse(stage_now %in% c("shipped", "partially_shipped"),
                       round(fill_lines * lines_n), 0)
  qty_shipped <- ifelse(line_no_v <= rep(k_complete, lines_n), qty_ordered, 0L)

  lines_tbl <- data.frame(
    order_no = line_order, line_no = line_no_v,
    qty_ordered = qty_ordered, unit_price = unit_price,
    qty_shipped = qty_shipped, stringsAsFactors = FALSE)

  # ---- Items, and the stock position behind a shortfall --------------------
  #
  # Chapter 11 found that no model trained on order attributes can predict a
  # shortfall, because whether an item is on the shelf on a given day is a fact
  # about inventory and not about the order. Parts IV and V need that fact, so
  # the items carry it.
  #
  # Each item replenishes on its own cycle and is out of stock for a window at
  # the end of each cycle, while the replenishment is in transit. Position is
  # therefore a function of the item and the time, which is what makes it
  # consistent: two lines hitting the same item on the same day see the same
  # on-hand quantity, as they must.
  #
  # The direction of causation in this code runs from the shortfall to the
  # position, because the shortfall was already drawn in the loop above and
  # every figure in Chapters 5 to 11 depends on it. Lines are therefore matched
  # to an (item, time) pair whose stock position agrees with the outcome the
  # loop settled. The joint distribution of position and shortfall is the same
  # either way round; only the factorization differs, and stating which one is
  # implemented is what keeps the generator honest. `stock_noise` is the share
  # of lines left deliberately inconsistent, which is what bounds how well any
  # model can do: with no noise a single comparison would predict shortfall
  # perfectly and no chapter could illustrate a threshold.
  n_items <- max(50L, round(n / 4))
  item_no <- sprintf("I%05d", seq_len(n_items))
  item_class <- sample(c("A", "B", "C"), n_items, replace = TRUE,
                       prob = c(0.2, 0.3, 0.5))
  n_vendors <- max(3L, round(n_items / 100))
  vendor <- sprintf("V%02d", sample(n_vendors, n_items, replace = TRUE))
  unit_cost <- round(stats::rlnorm(n_items, 2.4, 1.1), 2)

  # A class moves faster and replenishes more often. The stockout window is a
  # share of the cycle, drawn with a wide spread so that exposure differs by
  # item rather than by class alone.
  cycle_days <- round(ifelse(item_class == "A", stats::runif(n_items, 7, 21),
                      ifelse(item_class == "B", stats::runif(n_items, 14, 45),
                             stats::runif(n_items, 30, 90))))
  out_share <- stats::rbeta(n_items, 1.1, 9)
  stockout_days <- pmax(0, round(cycle_days * out_share))
  demand_rate <- round(ifelse(item_class == "A", stats::rgamma(n_items, 6, 0.5),
                       ifelse(item_class == "B", stats::rgamma(n_items, 3, 0.8),
                              stats::rgamma(n_items, 1.5, 1.2))), 2)
  lead_time_mean <- round(stats::runif(n_items, 3, 28), 1)
  lead_time_sd <- round(lead_time_mean * stats::runif(n_items, 0.1, 0.6), 1)

  items <- data.frame(
    item_no = item_no, item_class = item_class, vendor = vendor,
    unit_cost = unit_cost, cycle_days = cycle_days,
    stockout_days = stockout_days, demand_rate = demand_rate,
    lead_time_mean = lead_time_mean, lead_time_sd = lead_time_sd,
    stringsAsFactors = FALSE)

  # Days since the start of the horizon, at which each line's order was entered.
  line_day <- as.numeric(difftime(rep(entered, lines_n), start, units = "days"))
  phase <- stats::runif(n_items) * cycle_days          # cycle offset per item
  short_line <- qty_shipped < qty_ordered

  # Is item j out of stock on day t?
  out_of_stock <- function(j, t) {
    pos_in_cycle <- (t + phase[j]) %% cycle_days[j]
    pos_in_cycle > (cycle_days[j] - stockout_days[j])
  }

  # Match each line to an item whose position agrees with its outcome, redrawing
  # the lines that do not match. A bounded number of passes leaves a residue
  # that is inconsistent on purpose.
  stock_noise <- 0.12
  assigned <- sample(n_items, n_lines, replace = TRUE)
  for (pass in seq_len(14)) {
    bad <- out_of_stock(assigned, line_day) != short_line
    if (!any(bad)) break
    assigned[bad] <- sample(n_items, sum(bad), replace = TRUE)
  }
  # Put the declared share back out of agreement, so that the relationship is
  # informative without being a lookup.
  flip <- sample(n_lines, round(stock_noise * n_lines))
  assigned[flip] <- sample(n_items, length(flip), replace = TRUE)

  # What a system would show at the moment the order was entered.
  cyc <- cycle_days[assigned]
  pos_in_cycle <- (line_day + phase[assigned]) %% cyc
  is_out <- pos_in_cycle > (cyc - stockout_days[assigned])
  cycle_qty <- pmax(1, round(demand_rate[assigned] * cyc))
  on_hand_at_entry <- ifelse(is_out, 0L,
    as.integer(pmax(0, round(cycle_qty * (1 - pos_in_cycle / pmax(cyc - stockout_days[assigned], 1))))))
  # A replenishment is on order once the cycle is past its reorder point.
  on_order_at_entry <- ifelse(pos_in_cycle > 0.6 * cyc, as.integer(cycle_qty), 0L)
  promised_in_days <- ifelse(on_order_at_entry > 0,
                             round(pmax(0, cyc - pos_in_cycle), 1), NA_real_)
  demand_30d <- as.integer(round(demand_rate[assigned] * 30))

  lines_tbl$item_no <- item_no[assigned]
  lines_tbl$on_hand_at_entry <- on_hand_at_entry
  lines_tbl$on_order_at_entry <- on_order_at_entry
  lines_tbl$promised_in_days <- promised_in_days
  lines_tbl$demand_30d <- demand_30d
  lines_tbl$cover_ratio <- round(on_hand_at_entry / lines_tbl$qty_ordered, 3)

  # Fill on units and on value are recomputed from the lines, so that the three
  # dimensions describe one population rather than three independent draws.
  # They are NA where the order has not reached a shipping stage: an order that
  # has not shipped has no fill rate yet, which is the same censoring problem
  # Chapter 6 treats for dwell, and crediting it with 1 would report every
  # open order as perfectly filled.
  shipped_stage <- stage_now %in% c("shipped", "partially_shipped")
  sum_by <- function(x) as.numeric(tapply(x, line_order, sum)[order_no])
  fill_units <- ifelse(shipped_stage, sum_by(qty_shipped) / sum_by(qty_ordered), NA)
  fill_value <- ifelse(shipped_stage,
                       sum_by(qty_shipped * unit_price) /
                         sum_by(qty_ordered * unit_price), NA)
  fill_lines <- ifelse(shipped_stage, fill_lines, NA)

  keep <- seq_len(n_ev)
  events <- data.frame(
    order_no = ev_order[keep], event = ev_event[keep], stage = ev_stage[keep],
    occurred_at = as.POSIXct(ev_at[keep], origin = "1970-01-01", tz = "UTC"),
    stringsAsFactors = FALSE)
  events <- events[order(events$occurred_at, events$order_no), ]
  rownames(events) <- NULL

  # ---- Date fields -------------------------------------------------------
  #
  # Chapter 13 asks which date fields carry a commitment. Most do not. In one
  # field engagement the requested-date field equaled the order date on 58 of
  # 59 sampled orders, because the entry screen defaulted it to today and
  # nobody overrode it, so a severity rule keyed on "days past requested date"
  # was reading elapsed time with extra steps.
  #
  # `requested_date` reproduces that defect at a declared rate.
  # `promised_date` is the field that does carry a commitment, and it exists
  # only where a person entered the order. Drawing both after everything above
  # leaves the rest of the population byte-identical.
  default_share <- 0.85
  entered_day <- as.Date(entered)
  requested_offset <- ifelse(stats::runif(n) < default_share, 0L,
                             1L + stats::rpois(n, 4))
  requested_date <- entered_day + requested_offset

  has_promise <- source_ == "inside_sales" & stats::runif(n) < 0.70
  promised_date <- entered_day + 2L + stats::rpois(n, 3)
  promised_date[!has_promise] <- as.Date(NA)

  orders <- data.frame(
    order_no = order_no,
    entered_at = entered,
    stage = stage_now,
    finished_at = finished,
    is_terminal = stage_now %in% c("shipped", "cancelled"),
    branch = branch,
    order_source = source_,
    value = value,
    lines = lines_n,
    special_order = special,
    loops = looped,
    partially_shipped = partial,
    closed_short = short_close,
    stock_shortfall = shortfall,
    open_ticket_residue = residue,
    fill_units = fill_units,
    fill_lines = fill_lines,
    fill_value = fill_value,
    requested_date = requested_date,
    promised_date = promised_date,
    stringsAsFactors = FALSE
  )

  structure(list(orders = orders, events = events, lines = lines_tbl,
                 items = items, as_of = as_of), class = "lifecycle_sim")
}

#' @export
print.lifecycle_sim <- function(x, ...) {
  o <- x$orders
  cat(sprintf("<lifecycle_sim> %d orders, %d events, as of %s\n",
              nrow(o), nrow(x$events), format(x$as_of, "%Y-%m-%d %H:%M")))
  cat(sprintf("  in flight (censored): %d (%.1f%%)\n",
              sum(!o$is_terminal), 100 * mean(!o$is_terminal)))
  cat(sprintf("  residue -- finished but holding an open ticket: %d\n",
              sum(o$open_ticket_residue & o$is_terminal)))
  cat("  stages: ")
  print(table(o$stage))
  invisible(x)
}
