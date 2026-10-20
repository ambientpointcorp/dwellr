#' Alerting threshold from a cost ratio
#'
#' The point at which an order should be surfaced to a person, derived rather
#' than chosen.
#'
#' Let `c_miss` be the expected cost of a stall nobody caught and `c_look` the
#' cost of a reviewer's attention spent on an order that turned out to be fine.
#' An order carrying stall probability `q` is worth looking at when the
#' expected cost of leaving it alone exceeds the expected cost of the look,
#' `q * c_miss > (1 - q) * c_look`, which rearranges to a threshold on the
#' probability itself:
#'
#' \deqn{p^{*} = \frac{c_{look}}{c_{look} + c_{miss}}}
#'
#' The rule flags every order whose probability exceeds `p*`. Because the
#' comparison is made order by order, the rule is the pointwise minimizer of
#' expected cost, and no other assignment of the same orders to review can do
#' better under the same two costs. The returned `expected_cost` reports the
#' rule against the two policies it has to beat, reviewing nothing and
#' reviewing everything.
#'
#' The expression is the newsvendor critical fractile of Chapter 3
#' ([nv_critical_fractile()]) with the costs in the other order, and the
#' reversal is not cosmetic. The newsvendor chooses a stocking quantity, where
#' holding more is what protects against the expensive shortage, so its
#' fractile rises with the underage cost. Alerting chooses when to spend
#' attention, where looking sooner is what protects against the expensive
#' miss. The threshold therefore falls as a missed stall gets dearer and rises
#' as reviewer attention does.
#'
#' Two honest limits. The threshold is only as good as the two costs, and
#' `c_miss` is the hard one, since it has to be priced through the service
#' consequence and the safety-stock valuation of Chapter 7. And the
#' probabilities have to be calibrated, not merely ranked: a model that orders
#' orders correctly but reports 0.4 where the observed rate is 0.1 will cross
#' `p*` at the wrong place, which is why Chapter 9 calibrates before
#' thresholding. Compare the flagged count against review capacity with
#' [monitoring_budget()].
#'
#' A rule is also only as discriminating as the probabilities handed to it. A
#' `p*` that falls below the lowest probability in `prob` flags the whole
#' population, and a `p*` above the highest flags nothing. The rule is still
#' minimizing cost in both cases, reporting that under these costs and these
#' probabilities a blanket policy is the cheapest available. The
#' `expected_cost` element is the diagnostic: when the rule's cost equals the
#' cost of reviewing everything, or of reviewing nothing, the threshold has
#' fallen outside what the probabilities resolve, and the way forward is a
#' model that separates the population further rather than a different
#' threshold.
#'
#' Where no fitted model is available, [threshold_from_dwell_quantile()] reads
#' the threshold off the dwell distribution instead, under an assumption that
#' section documents.
#'
#' @param prob Calibrated probabilities that each open order is stalled, one
#'   per order. From a fitted model, or from the `posterior` element of
#'   [dwell_mixture()].
#' @param cost_miss Cost of failing to surface a genuine stall, `c_miss`.
#' @param cost_look Cost of one reviewer look that turns out unnecessary,
#'   `c_look`.
#' @param hours Optional dwell hours for the same orders, in the same order as
#'   `prob`. When supplied, the probability threshold is translated into the
#'   dwell time at which it is crossed, which is the form an operating rule
#'   usually has to take.
#'
#' @return A list with the `probability_threshold` `p*`, the logical `flag`
#'   vector, `flagged_n` and `flagged_share`, the `expected_cost` of the rule
#'   beside reviewing nothing and everything, and, when `hours` is supplied,
#'   the `hours` at which the threshold is crossed.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1200, seed = 3)
#' d <- stage_dwell(sim, "released")
#' mix <- dwell_mixture(d)
#' q <- mix$posterior(d$dwell_hours, censored = d$censored)
#' # Attention priced at 10x a missed stall flags the tail, and the crossing
#' # translates the probability threshold back into hours.
#' threshold_from_cost(q, cost_miss = 40, cost_look = 400,
#'                     hours = d$dwell_hours)[-2]
#' # Reversing the ratio puts p* below what this posterior resolves, and the
#' # rule reports as much by costing what reviewing everything costs.
#' threshold_from_cost(q, cost_miss = 400, cost_look = 40)$expected_cost
#' @export
threshold_from_cost <- function(prob, cost_miss, cost_look, hours = NULL) {
  stopifnot(cost_miss > 0, cost_look > 0)
  stopifnot(is.numeric(prob), length(prob) > 0, !anyNA(prob),
            all(prob >= 0), all(prob <= 1))
  p <- cost_look / (cost_miss + cost_look)
  flag <- prob > p

  # The pointwise cost of each order under the rule, and under the two
  # policies the rule has to beat. A look is charged only when it was
  # unnecessary, which is the accounting the threshold is derived from.
  cost_of <- function(f) sum(ifelse(f, (1 - prob) * cost_look, prob * cost_miss))
  out <- list(
    probability_threshold = p,
    flag = flag,
    flagged_n = sum(flag),
    flagged_share = mean(flag),
    expected_cost = c(rule = cost_of(flag),
                      none = cost_of(rep(FALSE, length(prob))),
                      all  = cost_of(rep(TRUE, length(prob)))))

  if (!is.null(hours)) {
    stopifnot(length(hours) == length(prob), !anyNA(hours))
    # The crossing is read off a monotone fit of probability on dwell rather
    # than off the raw probabilities, because a model that is only
    # approximately increasing in dwell would otherwise cross p* several
    # times and the first crossing would be noise.
    fit <- stats::isoreg(hours, prob)
    hit <- which(fit$yf > p)
    out$hours <- if (length(hit)) sort(hours)[hit[1]] else NA_real_
  }
  out
}

#' Alerting threshold read off the dwell distribution
#'
#' The fallback for [threshold_from_cost()] when no fitted stall probability is
#' available: the same cost ratio, applied as a fractile of the conditional
#' dwell distribution.
#'
#' The threshold is `dwell_quantile(dwell, p*)` with `p*` the probability
#' threshold of [threshold_from_cost()], so the comparative statics are the
#' same and the flagged share is `1 - p*`.
#'
#' What the fallback assumes is that an order's position in the dwell
#' distribution is its probability of being stalled, which is what lets a
#' fractile stand in for a posterior. The assumption ignores the base rate. It
#' leaves no room for an order to be both slow and healthy, so when a miss is
#' priced far above a look the rule will flag most of the population rather
#' than the tail that a fitted model would isolate. Treat the result as a
#' starting operating point to be replaced once a calibrated probability
#' exists, and check it against review capacity with [monitoring_budget()].
#'
#' @param dwell A data frame from [stage_dwell()].
#' @inheritParams threshold_from_cost
#'
#' @return A list with the `fractile` used, the `hours` threshold, and the
#'   `flagged_share` of the population it selects.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' d <- stage_dwell(sim, "released")
#' # Attention priced at 10x a missed stall keeps the threshold in the tail.
#' threshold_from_dwell_quantile(d, cost_miss = 40, cost_look = 400)
#' @export
threshold_from_dwell_quantile <- function(dwell, cost_miss, cost_look) {
  stopifnot(cost_miss > 0, cost_look > 0)
  p <- cost_look / (cost_miss + cost_look)
  list(fractile = p, hours = dwell_quantile(dwell, p), flagged_share = 1 - p)
}
