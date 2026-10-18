#' Alerting threshold from a cost ratio
#'
#' The dwell time at which an order should be surfaced to a person, derived
#' rather than chosen.
#'
#' The decision is a newsvendor problem over the dwell distribution. Let `c_u`
#' be the expected cost of a stall nobody caught and `c_o` the cost of a
#' reviewer's attention spent on an order that was fine. The optimal threshold
#' is the critical fractile
#'
#' \deqn{p^{*} = \frac{c_u}{c_u + c_o}}
#'
#' of the conditional dwell distribution -- the same expression as the
#' classical newsvendor of Chapter 3, over a different random variable
#' (Chapter 14).
#'
#' Two honest limits. The threshold is only as good as the two costs, and `c_u`
#' is the hard one: it has to be priced through the service consequence and the
#' safety-stock valuation of Chapter 7. And a threshold computed on a
#' distribution that has since moved is a stale threshold, which is why
#' Chapter 12 treats baselines as a thing to revalidate.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param cost_miss Cost of failing to surface a genuine stall, `c_u`.
#' @param cost_look Cost of one reviewer look that turns out unnecessary, `c_o`.
#'
#' @return A list with the `fractile` used and the `hours` threshold.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' d <- stage_dwell(sim, "released")
#' # A missed stall priced at 40x a wasted look puts the threshold in the tail.
#' threshold_from_cost(d, cost_miss = 400, cost_look = 10)
#' @export
threshold_from_cost <- function(dwell, cost_miss, cost_look) {
  stopifnot(cost_miss > 0, cost_look > 0)
  p <- cost_miss / (cost_miss + cost_look)
  list(fractile = p, hours = dwell_quantile(dwell, p))
}
