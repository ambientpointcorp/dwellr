#' Economic order quantity
#'
#' The order quantity minimizing the sum of fixed ordering and holding cost
#' under known, constant demand (§2.2).
#'
#' @param demand_rate Demand per unit time.
#' @param order_cost Fixed cost `K` of placing an order.
#' @param holding_cost Holding cost `h` per unit per unit time.
#'
#' @return The optimal quantity `Q*`.
#'
#' @examples
#' eoq(demand_rate = 10, order_cost = 100, holding_cost = 2)
#' @export
eoq <- function(demand_rate, order_cost, holding_cost) {
  stopifnot(demand_rate > 0, order_cost > 0, holding_cost > 0)
  sqrt(2 * order_cost * demand_rate / holding_cost)
}

#' Total relevant cost of an order quantity
#'
#' Evaluates the EOQ objective at any `Q`, which is how §2.3 shows how flat the
#' cost curve is near the optimum -- and therefore how much rounding to a case
#' quantity actually costs.
#'
#' @inheritParams eoq
#' @param Q Order quantity to evaluate.
#'
#' @return Total relevant cost per unit time.
#'
#' @examples
#' q <- eoq(10, 100, 2)
#' eoq_cost(10, 100, 2, q)
#' # 20% over the optimum costs under 2% in total cost:
#' eoq_cost(10, 100, 2, 1.2 * q) / eoq_cost(10, 100, 2, q) - 1
#' @export
eoq_cost <- function(demand_rate, order_cost, holding_cost, Q) {
  stopifnot(all(Q > 0))
  order_cost * demand_rate / Q + holding_cost * Q / 2
}

#' Newsvendor critical fractile
#'
#' The service level implied by a pair of overage and underage costs (§3.1).
#' Chapter 14 reuses this expression for alerting; see [threshold_from_cost()].
#'
#' @param overage Cost per unit left unsold, `o`.
#' @param underage Cost per unit of unmet demand, `u`.
#'
#' @return The critical fractile `u / (o + u)`.
#'
#' @examples
#' nv_critical_fractile(overage = 1, underage = 5)
#' @export
nv_critical_fractile <- function(overage, underage) {
  stopifnot(overage > 0, underage > 0)
  underage / (overage + underage)
}

#' Newsvendor order quantity
#'
#' The quantity achieving the critical fractile under a given demand
#' distribution.
#'
#' @inheritParams nv_critical_fractile
#' @param quantile_fn A quantile function taking a probability, e.g.
#'   `function(p) qpois(p, 100)`.
#'
#' @return The optimal quantity.
#'
#' @examples
#' nv_quantity(1, 5, function(p) qpois(p, lambda = 100))
#' @export
nv_quantity <- function(overage, underage, quantile_fn) {
  quantile_fn(nv_critical_fractile(overage, underage))
}
