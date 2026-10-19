# Part I models, promoted from the chapters as they are written.
#
# Argument order follows eoq(): demand rate first, then costs, then anything
# specific to the model. Every function here is exercised by a test that
# asserts the figure printed in the corresponding section of the book, so a
# change that alters a published result fails the build.

#' Relevant cost at a given order interval
#'
#' The EOQ objective expressed in terms of the replenishment interval
#' \eqn{T = Q/d} rather than the quantity, which is the form
#' @section Chapter 2: Section 2.6 uses for powers-of-two policies.
#'
#' @param interval Replenishment interval \eqn{T}, in periods.
#' @inheritParams eoq
#' @return Relevant cost per unit time.
#' @examples
#' eoq_cost_interval(4.4721, demand_rate = 10, order_cost = 100,
#'                   holding_cost = 1)
#' @export
eoq_cost_interval <- function(interval, demand_rate, order_cost,
                              holding_cost) {
  stopifnot(all(interval > 0), demand_rate > 0, order_cost > 0,
            holding_cost > 0)
  order_cost / interval + holding_cost * demand_rate * interval / 2
}

#' Cost penalty for departing from the economic order quantity
#'
#' The ratio of the cost at \eqn{Q} to the cost at \eqn{Q^*} depends only on
#' \eqn{Q/Q^*}:
#'
#' \deqn{\frac{C_r(Q)}{C_r(Q^*)} = \frac{1}{2}\left(\frac{Q}{Q^*} +
#'       \frac{Q^*}{Q}\right).}
#'
#' Every instance-specific quantity cancels, so one table serves all instances.
#' The curve is flat near the optimum, which is what makes rounding to a case
#' or pallet quantity essentially free (Section 2.3).
#'
#' @param ratio \eqn{Q/Q^*}, the chosen quantity as a multiple of the optimum.
#' @return Fractional cost penalty; `0` at `ratio = 1`.
#' @examples
#' eoq_penalty(1.2)   # ordering 20% above the optimum costs 1.67%
#' eoq_penalty(2)     # doubling it costs 25%
#' @export
eoq_penalty <- function(ratio) {
  stopifnot(all(ratio > 0))
  0.5 * (ratio + 1 / ratio) - 1
}

#' Cost penalty from a misestimated ordering or holding cost
#'
#' Using \eqn{fK} in place of \eqn{K} gives \eqn{\sqrt{f}\,Q^*}, so the cost
#' penalty is `eoq_penalty(sqrt(f))`. Because \eqn{K} and \eqn{h} enter
#' \eqn{Q^*} symmetrically, an error of factor \eqn{f} in \eqn{h} carries the
#' same penalty as an error of factor \eqn{1/f} in \eqn{K} (Section 2.4).
#'
#' @param factor The estimate as a multiple of the true value.
#' @return Fractional cost penalty.
#' @examples
#' eoq_misestimate_penalty(0.6)  # a 40% underestimate costs 3.28%
#' eoq_misestimate_penalty(2)    # a factor of two costs 6.07%
#' @export
eoq_misestimate_penalty <- function(factor) {
  stopifnot(all(factor > 0))
  eoq_penalty(sqrt(factor))
}

#' Reorder point under a known, constant lead time
#'
#' The inventory **position** at which a replenishment must be placed so that
#' it arrives as stock reaches zero. Numerically identical to the pipeline
#' inventory of Section 1.6: one is the stock that must be in transit, the
#' other the position at which transit must begin.
#'
#' @inheritParams eoq
#' @param lead_time Lead time \eqn{L}, in periods.
#' @return The reorder point \eqn{r = dL}.
#' @examples
#' reorder_point(demand_rate = 10, lead_time = 0.5)
#' @export
reorder_point <- function(demand_rate, lead_time) {
  stopifnot(demand_rate > 0, lead_time >= 0)
  demand_rate * lead_time
}

#' Powers-of-two replenishment interval
#'
#' Constrains the order interval to \eqn{T_b 2^k} for integer \eqn{k}, so that
#' items on different cycles share delivery days instead of drifting in and out
#' of alignment. Because the cost function is convex in \eqn{T}, the best
#' admissible interval is the smallest \eqn{k} with
#' \eqn{T_b 2^k \ge T^*/\sqrt{2}}.
#'
#' The resulting policy is never worse than
#' \eqn{\frac{1}{2}(\sqrt{2} + 1/\sqrt{2}) = 1.0607} times the unconstrained
#' optimum, a bound independent of the instance (Section 2.6).
#'
#' @param optimal_interval The unconstrained optimum \eqn{T^* = Q^*/d}.
#' @param base_interval The base period \eqn{T_b}.
#' @return The chosen interval.
#' @examples
#' pow2_interval(optimal_interval = 4.4721, base_interval = 0.7)
#' @export
pow2_interval <- function(optimal_interval, base_interval) {
  stopifnot(optimal_interval > 0, base_interval > 0)
  k <- max(0, ceiling(log2(optimal_interval / (base_interval * sqrt(2)))))
  base_interval * 2^k
}

#' Worst-case penalty of a powers-of-two policy
#'
#' @return The bound 0.0607, as a fraction.
#' @examples
#' pow2_bound()
#' @export
pow2_bound <- function() eoq_penalty(sqrt(2))

#' Order quantity under an all-units quantity discount
#'
#' Restores the purchase cost to the objective, which the EOQ drops because it
#' does not depend on \eqn{Q}. Under an all-units schedule, reaching a
#' breakpoint lowers the price on the whole order, so the total cost function
#' is discontinuous at each breakpoint and is **not** convex. The optimum is
#' frequently at a breakpoint rather than at any band's EOQ, so the bands are
#' enumerated rather than searched by derivative (Section 2.7).
#'
#' @inheritParams eoq
#' @param breakpoints Ascending lower bounds of each price band, starting at 0.
#' @param prices Unit price in each band, the same length as `breakpoints`.
#' @return A data frame of the candidate per band, with the chosen row
#'   identifiable by the minimum `cost`. Bands whose EOQ lies above them are
#'   omitted, which is sound only because prices fall as quantity rises.
#' @examples
#' eoq_quantity_discount(demand_rate = 10, order_cost = 100,
#'                       holding_cost = 1,
#'                       breakpoints = c(0, 60, 120),
#'                       prices = c(5, 4.5, 4))
#' @export
eoq_quantity_discount <- function(demand_rate, order_cost, holding_cost,
                                  breakpoints, prices) {
  stopifnot(length(breakpoints) == length(prices),
            !is.unsorted(breakpoints), breakpoints[1] == 0,
            !is.unsorted(rev(prices)))
  q <- eoq(demand_rate, order_cost, holding_cost)
  rows <- lapply(seq_along(prices), function(j) {
    lo <- breakpoints[j]
    hi <- if (j < length(breakpoints)) breakpoints[j + 1] else Inf
    cand <- if (q < lo) lo else if (q >= hi) return(NULL) else q
    data.frame(band = j, lower = lo, price = prices[j], Q = cand,
               cost = order_cost * demand_rate / cand +
                      holding_cost * cand / 2 + prices[j] * demand_rate)
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Economic order quantity with planned backorders
#'
#' Treats a shortage as a cost to be traded off rather than avoided. Valid only
#' under the backorder regime of Section 1.4, where unmet demand waits rather
#' than departing.
#'
#' The order quantity is the ordinary EOQ inflated by \eqn{\sqrt{(h+b)/b}}, and
#' the optimal backordered fraction depends only on the ratio of \eqn{h} to
#' \eqn{b} (Section 2.8).
#'
#' @inheritParams eoq
#' @param shortage_cost Shortage cost \eqn{b} per unit per period. The book
#'   reserves \eqn{p} for the selling price and uses \eqn{b} throughout.
#' @return A list with the order quantity `Q`, the planned backordered
#'   `fraction`, and the resulting `cost` per unit time.
#' @examples
#' backorder_policy(demand_rate = 10, order_cost = 100,
#'                  holding_cost = 1, shortage_cost = 5)
#' @export
backorder_policy <- function(demand_rate, order_cost, holding_cost,
                             shortage_cost) {
  stopifnot(demand_rate > 0, order_cost > 0, holding_cost > 0,
            shortage_cost > 0)
  h <- holding_cost; b <- shortage_cost
  list(Q = sqrt(2 * order_cost * demand_rate * (h + b) / (h * b)),
       fraction = h / (b + h),
       cost = sqrt(2 * order_cost * demand_rate * h * b / (h + b)))
}

#' Economic production quantity
#'
#' Replenishment at a finite rate rather than as a single delivery. Stock
#' accumulates at \eqn{P - d} while it is being consumed, so the average
#' inventory over a cycle is \eqn{Q(1-\rho)/2} with \eqn{\rho = d/P}.
#'
#' The result is the EOQ with \eqn{h} replaced by \eqn{h(1-\rho)}: finite
#' production acts exactly as a reduction in the holding cost. As
#' \eqn{P \to \infty} it recovers the EOQ (Section 2.9).
#'
#' @inheritParams eoq
#' @param production_rate Production rate \eqn{P}, which must exceed
#'   `demand_rate`.
#' @return A list with the run quantity `Q`, the `cost` per unit time, and the
#'   utilization `rho`.
#' @examples
#' epq(demand_rate = 10, order_cost = 100, holding_cost = 1,
#'     production_rate = 20)
#' @export
epq <- function(demand_rate, order_cost, holding_cost, production_rate) {
  stopifnot(production_rate > demand_rate, demand_rate > 0,
            order_cost > 0, holding_cost > 0)
  rho <- demand_rate / production_rate
  list(Q = sqrt(2 * order_cost * demand_rate / (holding_cost * (1 - rho))),
       cost = sqrt(2 * order_cost * demand_rate * holding_cost * (1 - rho)),
       rho = rho)
}

#' Common-cycle solution to the economic lot scheduling problem
#'
#' Several items sharing one machine, restricted to schedules in which every
#' item is produced exactly once per cycle. The exact problem is NP-hard; this
#' restriction makes it tractable and is the standard practical response
#' (Section 2.10).
#'
#' @param items A data frame with columns `d`, `P`, `K` and `h`: demand rate,
#'   production rate, setup cost and holding cost for each item.
#' @return A list with the common cycle `interval`, a data frame of per-item
#'   `runs`, and `busy_fraction` — the share of the cycle spent producing.
#'   A `busy_fraction` above 1 means the schedule is infeasible.
#' @examples
#' elsp_common_cycle(data.frame(d = c(10, 5, 8), P = c(50, 40, 60),
#'                              K = c(100, 60, 80), h = c(1, 2, 1.5)))
#' @export
elsp_common_cycle <- function(items) {
  stopifnot(all(c("d", "P", "K", "h") %in% names(items)),
            all(items$P > items$d))
  interval <- sqrt(2 * sum(items$K) /
                     sum(items$h * items$d * (1 - items$d / items$P)))
  items$run_qty <- items$d * interval
  items$run_time <- items$run_qty / items$P
  list(interval = interval, runs = items,
       busy_fraction = sum(items$run_time) / interval)
}

#' Joint replenishment of items sharing a fixed ordering cost
#'
#' Splits the fixed cost into a major cost incurred whenever any order is
#' placed and a minor cost per item included in it. Each item joins every
#' `m`-th order, so items that are slow moving and cheap to hold ride along
#' with deliveries that would have been made regardless (Section 2.11).
#'
#' Solved by alternating between the base interval and the multiples, which
#' converges quickly. This is a heuristic, not an exact method.
#'
#' @param major_cost Cost incurred whenever any order is placed.
#' @param minor_costs Per-item cost of including an item in an order.
#' @param holding_costs Per-item holding cost per unit per period.
#' @param demand_rates Per-item demand rate.
#' @param max_iter Maximum alternations before giving up.
#' @return A list with the base `interval`, the integer `multiples`, and the
#'   total `cost` per unit time.
#' @examples
#' jrp(major_cost = 80, minor_costs = c(20, 15, 30),
#'     holding_costs = c(1, 2, 0.2), demand_rates = c(10, 5, 0.5))
#' @export
jrp <- function(major_cost, minor_costs, holding_costs, demand_rates,
                max_iter = 50) {
  stopifnot(length(minor_costs) == length(holding_costs),
            length(minor_costs) == length(demand_rates))
  m <- rep(1, length(minor_costs))
  interval <- NA_real_
  for (i in seq_len(max_iter)) {
    interval <- sqrt(2 * (major_cost + sum(minor_costs / m)) /
                       sum(holding_costs * demand_rates * m))
    m_new <- pmax(1, round(sqrt(2 * minor_costs /
                   (holding_costs * demand_rates * interval^2))))
    if (identical(m_new, m)) break
    m <- m_new
  }
  list(interval = interval, multiples = m,
       cost = major_cost / interval +
              sum(minor_costs / (m * interval) +
                  holding_costs * demand_rates * m * interval / 2))
}

#' Wagner-Whitin dynamic lot sizing
#'
#' Exact lot sizing for demand that is known but not constant. Two properties
#' make it tractable: it is never optimal to order while stock remains, and
#' each order therefore covers a contiguous block of periods. The problem
#' reduces to a shortest path on an acyclic graph and is solved by one forward
#' pass (Section 2.12).
#'
#' @param demand Demand in each period, in order.
#' @inheritParams eoq
#' @return A list with the total `cost`, the `periods` in which to order, and
#'   the `quantities` to order in each.
#' @examples
#' dls_wagner_whitin(c(20, 50, 10, 50, 30), order_cost = 100,
#'                   holding_cost = 1)
#' @export
dls_wagner_whitin <- function(demand, order_cost, holding_cost) {
  stopifnot(length(demand) >= 1, all(demand >= 0), order_cost > 0,
            holding_cost >= 0)
  n <- length(demand)
  best <- c(0, rep(Inf, n))     # best[j]: least cost covering periods 1..j-1
  from <- integer(n + 1)

  for (j in 2:(n + 1)) {
    for (i in 1:(j - 1)) {
      held <- if (j - 1 >= i + 1) {
        sum(holding_cost * (seq(i + 1, j - 1) - i) * demand[seq(i + 1, j - 1)])
      } else 0
      cost <- best[i] + order_cost + held
      if (cost < best[j]) { best[j] <- cost; from[j] <- i }
    }
  }

  periods <- integer(0); j <- n + 1
  while (j > 1) { periods <- c(from[j], periods); j <- from[j] }
  quantities <- vapply(seq_along(periods), function(i) {
    s <- periods[i]
    e <- if (i < length(periods)) periods[i + 1] - 1 else n
    sum(demand[s:e])
  }, numeric(1))

  list(cost = best[n + 1], periods = periods, quantities = quantities)
}
