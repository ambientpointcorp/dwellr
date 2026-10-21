# Chapter 20 models. A serial network of stages, each holding stock and
# supplying the one below it, under echelon base-stock control.
#
# The cost convention is stated once here because every figure depends on it:
# installation holding `holding[j]` is charged on end-of-period on-hand stock
# at stage j, `penalty` is charged per unit of backorder per period at stage 1,
# and units in transit are charged to nobody until they arrive. Stage 1 faces
# demand; stage N orders from an outside source assumed never to be short.

#' Simulate a serial multi-echelon system under echelon base-stock control
#'
#' Stage 1 faces demand and stage `N` orders from an outside source. Each
#' stage raises its *echelon* inventory position -- the stock at that stage and
#' everything below it, in transit included, net of backorders -- to its base
#' stock level, shipping downstream only what it has on hand (§20.1).
#'
#' Echelon positions are what make the policy work on a network: a stage
#' ordering against its own local stock alone is the installation policy of
#' §20.1, which is also available here and which carries more stock for the
#' same service.
#'
#' Two identities are returned for checking, because a simulator that gets
#' them wrong gets everything else wrong too. Mean echelon-j stock, units in
#' transit included, equals `levels[j]` less one period of mean demand; mean
#' net inventory at stage 1 equals `levels[1]` less mean demand over
#' `lead_times[1] + 1` periods. Both hold exactly while no stage is starved by
#' the one above it, and the gap measures the starvation when one is.
#'
#' @param levels Echelon base-stock levels, stage 1 first. Nondecreasing.
#' @param demand_mean,demand_sd Per-period demand at stage 1. Normal,
#'   truncated at zero.
#' @param lead_times Transit periods into each stage, stage 1 first. The last
#'   entry is the lead time from the outside source.
#' @param holding Installation holding cost per unit per period, stage 1
#'   first. Nonincreasing upstream.
#' @param penalty Backorder penalty per unit per period at stage 1.
#' @param periods,warmup Periods simulated, and the leading periods excluded
#'   from the averages.
#' @param stall Optional list with `stage`, `probability` and `periods`: with
#'   that probability a shipment into that stage is delayed by that many extra
#'   periods. This is the administrative stall of §20.6, which is a lead time
#'   from downstream and nothing else.
#' @param review Periods between orders. One is continuous review; a larger
#'   value is the `(R,S)` batching of §17.4, which is the administrative cause
#'   of the amplification measured in §20.7.
#' @param policy `"echelon"` or `"installation"`.
#' @param seed Random seed. Runs compared against each other should share one,
#'   so that a difference in cost is a difference in policy.
#'
#' @return A list with `cost` per period, `holding_cost`, `penalty_cost`,
#'   `on_hand` and `backorders` per stage, the stage-1 `fill_rate`, the share
#'   of periods stage 1 was short (`stockout_periods`), the `orders` placed by
#'   the most upstream stage, the realized `demand`, and two identity checks,
#'   `identity` on echelon stock and `net_inventory_identity` on stage 1.
#'
#' @examples
#' echelon_simulate(c(27, 59), demand_mean = 10, demand_sd = 3,
#'                  lead_times = c(1, 2), holding = c(1, 0.6),
#'                  penalty = 9, periods = 2000)$cost
#' @export
echelon_simulate <- function(levels, demand_mean, demand_sd, lead_times,
                             holding, penalty, periods = 20000,
                             warmup = 500, stall = NULL, review = 1,
                             policy = c("echelon", "installation"),
                             seed = 1) {
  policy <- match.arg(policy)
  review <- as.integer(review)
  stopifnot(review >= 1)
  n <- length(levels)
  stopifnot(n >= 1, length(lead_times) == n, length(holding) == n,
            demand_sd >= 0, penalty > 0, periods > warmup,
            all(lead_times >= 0), all(diff(holding) <= 1e-12))
  if (n > 1 && any(diff(levels) < 0))
    stop("echelon base-stock levels must be nondecreasing upstream.",
         call. = FALSE)
  if (any(abs(lead_times - round(lead_times)) > 1e-9))
    stop("lead times are whole periods here; a fractional mean lead time ",
         "is expressed as a `stall` with a probability (see Section 20.6).",
         call. = FALSE)
  L <- as.integer(round(lead_times))
  extra <- if (is.null(stall)) 0L else as.integer(stall$periods)
  if (!is.null(stall))
    stopifnot(stall$stage >= 1, stall$stage <= n,
              stall$probability >= 0, stall$probability <= 1)

  set.seed(seed)
  d <- pmax(0, stats::rnorm(periods, demand_mean, demand_sd))
  u <- if (is.null(stall)) NULL else stats::runif(periods)

  # Circular arrival buffer: pipe[j, slot(head + k - 1)] arrives at stage j in
  # k periods. A pointer avoids reallocating a vector every period.
  width <- max(L) + extra + 1L
  pipe <- matrix(0, n, width)
  slot <- function(k) ((k - 1L) %% width) + 1L
  head <- 1L
  for (j in seq_len(n)) if (L[j] > 0)
    pipe[j, slot(head + seq_len(L[j]))] <- demand_mean

  on_hand <- numeric(n)
  on_hand[1] <- levels[1] - demand_mean * L[1]
  if (n > 1) on_hand[-1] <- pmax(0, diff(levels))

  acc_h <- numeric(n); acc_b <- 0; acc_oh <- numeric(n); acc_bo <- 0
  acc_ech <- numeric(n); kept <- 0L
  filled <- 0; demanded <- 0; short_periods <- 0L
  top_orders <- numeric(periods)
  in_pipe <- numeric(n)

  for (t in seq_len(periods)) {
    h <- slot(head)
    on_hand <- on_hand + pipe[, h]
    pipe[, h] <- 0
    for (j in seq_len(n)) in_pipe[j] <- sum(pipe[j, ])

    pos <- cumsum(on_hand + in_pipe)          # echelon positions
    local <- on_hand + in_pipe                # installation positions

    ordering <- (t - 1L) %% review == 0L
    o <- if (ordering) max(0, levels[n] -
                           if (policy == "echelon") pos[n] else local[n]) else 0
    top_orders[t] <- o
    src_delay <- L[n]
    if (!is.null(stall) && stall$stage == n && u[t] < stall$probability)
      src_delay <- src_delay + extra
    if (src_delay == 0) on_hand[n] <- on_hand[n] + o else {
      k <- slot(head + src_delay)
      pipe[n, k] <- pipe[n, k] + o
    }

    if (n > 1) for (j in seq(n, 2)) {
      want <- if (!ordering) 0 else
        max(0, levels[j - 1] -
            if (policy == "echelon") pos[j - 1] else local[j - 1])
      ship <- min(want, max(0, on_hand[j]))
      on_hand[j] <- on_hand[j] - ship
      delay <- L[j - 1]
      if (!is.null(stall) && stall$stage == j - 1 && u[t] < stall$probability)
        delay <- delay + extra
      if (delay == 0) on_hand[j - 1] <- on_hand[j - 1] + ship else {
        kk <- slot(head + delay)
        pipe[j - 1, kk] <- pipe[j - 1, kk] + ship
      }
      pos[j - 1] <- pos[j - 1] + ship
      local[j - 1] <- local[j - 1] + ship
    }

    served <- min(max(on_hand[1], 0), d[t])
    on_hand[1] <- on_hand[1] - d[t]

    if (t > warmup) {
      kept <- kept + 1L
      oh <- pmax(on_hand, 0)
      bo <- max(-on_hand[1], 0)
      acc_oh <- acc_oh + oh; acc_bo <- acc_bo + bo
      acc_h <- acc_h + holding * oh; acc_b <- acc_b + penalty * bo
      for (j in seq_len(n)) in_pipe[j] <- sum(pipe[j, ])
      acc_ech <- acc_ech + cumsum(on_hand + in_pipe)
      filled <- filled + served; demanded <- demanded + d[t]
      if (bo > 0) short_periods <- short_periods + 1L
    }
    head <- head + 1L
  }

  # Two identities hold exactly when no stage is starved by the one above it.
  # Echelon stock counts units in transit, so one period of demand has left
  # it; stage 1's net inventory has lost the whole of its lead time's demand.
  ident <- data.frame(
    stage = seq_len(n),
    echelon_stock = round(acc_ech / kept, 3),
    implied = round(levels - demand_mean, 3))
  ident$gap <- round(ident$echelon_stock - ident$implied, 3)
  net1 <- c(observed = round((acc_oh[1] - acc_bo) / kept, 3),
            implied = round(levels[1] - demand_mean * (L[1] + 1), 3))

  list(cost = (sum(acc_h) + acc_b) / kept,
       holding_cost = acc_h / kept,
       penalty_cost = acc_b / kept,
       on_hand = acc_oh / kept,
       backorders = acc_bo / kept,
       fill_rate = filled / demanded,
       stockout_periods = short_periods / kept,
       orders = top_orders[(warmup + 1):periods],
       demand = d[(warmup + 1):periods],
       identity = ident,
       net_inventory_identity = net1)
}

#' Search for echelon base-stock levels
#'
#' Coordinate descent on [echelon_simulate()], one stage at a time, over a
#' grid. Every evaluation shares a seed, so a difference in cost between two
#' candidate levels is a difference in the policy and not in the demand drawn.
#'
#' The search is here because the exact decomposition of §20.2 fixes the
#' downstream level by a critical fractile and leaves the upstream levels to
#' numerical work, and because a simulator is the only arbiter available once
#' lead times are stochastic or a stall is present.
#'
#' @inheritParams echelon_simulate
#' @param start Starting levels. Defaults to mean demand over each stage's
#'   lead time plus one period.
#' @param steps Grid steps, applied in order. A wide step finds the
#'   neighborhood and a narrow one the level.
#' @param span How many steps either side of the current level each sweep
#'   considers.
#' @param max_sweeps Cap on passes per step size. A pass that changes nothing
#'   ends that step size early.
#'
#' @return A list with the chosen `levels`, the `cost` at them, `path`, the
#'   cost after each sweep, and the number of `evaluations`.
#'
#' @examples
#' echelon_optimize(demand_mean = 10, demand_sd = 3, lead_times = c(1, 2),
#'                  holding = c(1, 0.6), penalty = 9, periods = 1500,
#'                  steps = c(4, 1))$levels
#' @export
echelon_optimize <- function(demand_mean, demand_sd, lead_times, holding,
                             penalty, start = NULL, steps = c(8, 2, 1),
                             span = 3, max_sweeps = 8, periods = 5000,
                             warmup = 500, stall = NULL, review = 1,
                             policy = c("echelon", "installation"),
                             seed = 1) {
  policy <- match.arg(policy)
  n <- length(lead_times)
  stopifnot(all(steps > 0), span >= 1, max_sweeps >= 1)
  if (is.null(start))
    start <- cumsum(demand_mean * (lead_times + review))
  cur <- sort(start)

  evaluate <- function(lv) {
    if (any(lv < 0)) return(Inf)
    echelon_simulate(sort(lv), demand_mean, demand_sd, lead_times, holding,
                     penalty, periods = periods, warmup = warmup,
                     stall = stall, review = review, policy = policy,
                     seed = seed)$cost
  }
  best <- evaluate(cur)
  path <- best
  # Coarse to fine: a wide step finds the neighborhood, a narrow one the
  # level. Sweeping stops when a whole pass changes nothing, which is what
  # makes the result independent of the starting point rather than a
  # fixed number of steps away from it.
  for (step in steps) {
    for (s in seq_len(max_sweeps)) {
      moved <- FALSE
      for (j in seq_len(n)) {
        grid <- cur[j] + step * seq(-span, span)
        grid <- grid[grid >= 0 & grid != cur[j]]
        costs <- vapply(grid, function(v) {
          trial <- cur; trial[j] <- v; evaluate(trial)
        }, numeric(1))
        k <- which.min(costs)
        if (length(k) && costs[k] < best - 1e-9) {
          best <- costs[k]; cur[j] <- grid[k]; moved <- TRUE
        }
      }
      cur <- sort(cur)
      path <- c(path, best)
      if (!moved) break
    }
  }
  list(levels = cur, cost = best, path = path, evaluations = length(path))
}

#' Variance amplification between demand and replenishment orders
#'
#' The bullwhip ratio, \eqn{Var(O)/Var(D)}, measured on a realized pair of
#' series. A ratio above one says the orders a supplier sees are more variable
#' than the demand that caused them, and §20.7 measures how much of that an
#' unwatched upstream stall contributes.
#'
#' @param demand Per-period demand.
#' @param orders Per-period replenishment orders placed upstream.
#'
#' @return A list with the variance `ratio`, the standard-deviation ratio, the
#'   two coefficients of variation, and the lag-1 autocorrelation of orders,
#'   which is what a batching or stalling stage leaves behind.
#'
#' @examples
#' set.seed(1)
#' d <- rnorm(500, 10, 3)
#' bullwhip(d, d + rnorm(500, 0, 2))
#' @export
bullwhip <- function(demand, orders) {
  stopifnot(length(demand) == length(orders), length(demand) > 2)
  vd <- stats::var(demand); vo <- stats::var(orders)
  list(ratio = vo / vd,
       sd_ratio = sqrt(vo / vd),
       cv_demand = stats::sd(demand) / mean(demand),
       cv_orders = stats::sd(orders) / mean(orders),
       order_autocorrelation = stats::acf(orders, lag.max = 1,
                                          plot = FALSE)$acf[2])
}
