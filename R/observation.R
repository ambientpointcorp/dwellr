# Chapter 23 models. What monitoring costs, in the terms Chapter 2 uses for
# ordering. The cost of a look and the cost of a wait trade against each other
# the way an ordering cost trades against a holding cost, and the review
# interval that balances them has the same square-root form.

#' The cost structure of a monitoring program
#'
#' Three components, and they scale differently (§23.1). *Instrumentation* is
#' fixed: the extract, the event log, the deployment. *Observation* is per
#' scan, so it scales with cadence and not with volume. *Decision* is per item
#' decided, so it scales with volume, and when inference is metered it is the
#' component that grows without anybody choosing to grow it.
#'
#' Reporting them separately is the point. A program whose cost is dominated by
#' instrumentation has a volume problem it does not yet have; one dominated by
#' per-decision cost has an allocation problem, which is §23.3.
#'
#' @param fixed_annual Annual instrumentation cost.
#' @param per_observation Cost of one scan.
#' @param observations_per_day Scans per day across all cadences.
#' @param per_decision Cost of deciding about one item, metered inference
#'   included.
#' @param decisions_per_day Items decided per day.
#' @param days Days in the accounting period.
#'
#' @return A data frame with one row per component carrying its annual cost and
#'   share, and a `total` row.
#'
#' @examples
#' observation_cost(fixed_annual = 40000, per_observation = 0.004,
#'                  observations_per_day = 180, per_decision = 0.012,
#'                  decisions_per_day = 14)
#' @export
observation_cost <- function(fixed_annual, per_observation,
                             observations_per_day, per_decision,
                             decisions_per_day, days = 365) {
  stopifnot(fixed_annual >= 0, per_observation >= 0, per_decision >= 0,
            observations_per_day >= 0, decisions_per_day >= 0, days > 0)
  obs <- per_observation * observations_per_day * days
  dec <- per_decision * decisions_per_day * days
  out <- data.frame(
    component = c("instrumentation", "observation", "decision"),
    scales_with = c("nothing", "cadence", "volume"),
    annual = c(fixed_annual, obs, dec),
    stringsAsFactors = FALSE)
  out$share <- round(out$annual / sum(out$annual), 4)
  rbind(out, data.frame(component = "total", scales_with = NA_character_,
                        annual = sum(out$annual), share = 1))
}

#' Cost of a review interval, and the interval that minimizes it
#'
#' A scan costs something and a condition that has arisen waits, on average,
#' half the interval before the next scan finds it. Those two costs move
#' oppositely in the interval, which makes the problem the one Chapter 2
#' solves for order quantity:
#'
#' \deqn{C(R) = \frac{c_s}{R} + \frac{\lambda c_\ell R}{2},
#'       \qquad R^{*} = \sqrt{\frac{2 c_s}{\lambda c_\ell}}.}
#'
#' The analogy carries the insensitivity with it. Half or double the optimal
#' interval costs 25% more, the same figure Chapter 2 measures for order
#' quantity, which is the reason a cadence chosen to the nearest convenient
#' round number is usually good enough (§23.2).
#'
#' Where it stops holding is worth stating. The derivation prices a wait
#' linearly, and a wait against a deadline is not priced linearly: it costs
#' nothing until the slack is gone and everything afterward. Pass
#' `lag_cost_fn` to price the wait any other way and the optimum is found
#' numerically instead.
#'
#' @param scan_cost Cost of one scan.
#' @param arrival_rate Conditions arising per period.
#' @param lag_cost Cost per period of delay in noticing one condition.
#' @param interval Interval to price, for [cadence_cost()].
#' @param lag_cost_fn Optional function of the mean delay returning its cost
#'   per condition, replacing the linear `lag_cost` term.
#' @param max_interval Upper bound for the numerical search.
#'
#' @return [cadence_cost()] returns the cost per period at `interval`.
#'   [cadence_optimal()] returns a list with `interval`, the `cost` at it,
#'   whether it came from the `closed_form`, and the `scan` and `lag` cost
#'   components at the optimum.
#'
#' @examples
#' cadence_optimal(scan_cost = 12, arrival_rate = 5, lag_cost = 0.8)
#' cadence_cost(interval = 2.449, scan_cost = 12, arrival_rate = 5,
#'              lag_cost = 0.8)
#' @name cadence
#' @export
cadence_cost <- function(interval, scan_cost, arrival_rate, lag_cost,
                         lag_cost_fn = NULL) {
  stopifnot(all(interval > 0), scan_cost >= 0, arrival_rate >= 0)
  wait <- interval / 2
  lag <- if (is.null(lag_cost_fn)) lag_cost * wait else
    vapply(wait, lag_cost_fn, numeric(1))
  scan_cost / interval + arrival_rate * lag
}

#' @rdname cadence
#' @export
cadence_optimal <- function(scan_cost, arrival_rate, lag_cost,
                            lag_cost_fn = NULL, max_interval = 1000) {
  stopifnot(scan_cost >= 0, arrival_rate > 0, max_interval > 0)
  if (is.null(lag_cost_fn)) {
    stopifnot(lag_cost > 0)
    r <- sqrt(2 * scan_cost / (arrival_rate * lag_cost))
    closed <- TRUE
  } else {
    g <- exp(seq(log(1e-4), log(max_interval), length.out = 4000))
    r <- g[which.min(cadence_cost(g, scan_cost, arrival_rate, lag_cost,
                                  lag_cost_fn))]
    closed <- FALSE
  }
  list(interval = r,
       cost = cadence_cost(r, scan_cost, arrival_rate, lag_cost, lag_cost_fn),
       closed_form = closed,
       scan = scan_cost / r,
       lag = cadence_cost(r, scan_cost, arrival_rate, lag_cost, lag_cost_fn) -
             scan_cost / r)
}

#' Three ways to price a stall that was missed
#'
#' The miss cost is the input the threshold of @sec-the-critical-fractile is
#' most sensitive to and the one nobody measures. Three approaches are in
#' common use, they rest on different assumptions, and they disagree by enough
#' to move the threshold (§23.7).
#'
#' *Expediting* prices the miss at what it costs to recover: the freight
#' upgrade, the overtime, the partial shipment. It is the only one of the three
#' that is observable in the ledger, and it understates, since it prices the
#' recoveries that happened and not the orders nobody recovered.
#'
#' *Lost margin* prices the miss at the margin on an order assumed lost. It
#' overstates whenever the customer waits, which most do.
#'
#' *Retention* prices the miss at its share of the lifetime value of a customer
#' who leaves, which is the largest of the three and the least observable.
#'
#' @param order_value Value of the order at risk.
#' @param expedite Cost of recovering a missed stall.
#' @param margin_rate Gross margin as a share of order value.
#' @param loss_probability Probability the order is lost when the stall is
#'   missed, used by the lost-margin estimate.
#' @param lifetime_value Customer lifetime value.
#' @param churn_probability Probability a missed stall ends the relationship.
#' @param cost_look Cost of one look, for the implied threshold.
#'
#' @return A data frame with one row per approach carrying the implied miss
#'   cost and the critical fractile [threshold_from_cost()] would use.
#'
#' @examples
#' miss_cost(order_value = 865, expedite = 75, margin_rate = 0.22,
#'           loss_probability = 0.15, lifetime_value = 18000,
#'           churn_probability = 0.01, cost_look = 40)
#' @export
miss_cost <- function(order_value, expedite, margin_rate, loss_probability,
                      lifetime_value, churn_probability, cost_look) {
  stopifnot(order_value >= 0, expedite >= 0, margin_rate >= 0,
            margin_rate <= 1, loss_probability >= 0, loss_probability <= 1,
            lifetime_value >= 0, churn_probability >= 0,
            churn_probability <= 1, cost_look > 0)
  est <- c(expediting = expedite,
           lost_margin = order_value * margin_rate * loss_probability,
           retention = lifetime_value * churn_probability)
  data.frame(approach = names(est),
             miss_cost = round(as.numeric(est), 2),
             observable = c("in the ledger", "partly", "no"),
             p_star = round(cost_look / (cost_look + as.numeric(est)), 4),
             stringsAsFactors = FALSE)
}
