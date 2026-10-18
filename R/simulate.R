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
#' @return An object of class `lifecycle_sim`: a list with
#'   `orders` (one row per order) and `events` (the append-only event log,
#'   one row per transition, with `order_no`, `event`, `stage`, `occurred_at`).
#'
#' @examples
#' sim <- simulate_lifecycles(n = 500, seed = 1)
#' nrow(sim$orders)
#' head(sim$events, 3)
#' table(sim$orders$stage)
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

  ev <- list()
  add <- function(order_no, event, stage, at) {
    ev[[length(ev) + 1L]] <<- data.frame(
      order_no = order_no, event = event, stage = stage,
      occurred_at = at, stringsAsFactors = FALSE)
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

  events <- do.call(rbind, ev)
  events <- events[order(events$occurred_at, events$order_no), ]
  rownames(events) <- NULL

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
    stringsAsFactors = FALSE
  )

  structure(list(orders = orders, events = events, as_of = as_of),
            class = "lifecycle_sim")
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
