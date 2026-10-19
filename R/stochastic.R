# Chapter 3 models. Demand is uncertain; the policies below trade holding cost
# against shortage cost rather than against a fixed ordering cost.
#
# Notation follows Chapters 1 and 2: h is the holding cost and b the shortage
# cost per unit per period. The book reserves p for the selling price.

#' Complementary first order loss function
#'
#' \eqn{E[Q - \omega]^{+}}, the expected leftover stock when \eqn{Q} is
#' stocked against demand \eqn{\omega}. For normal demand it has a closed form
#' in terms of the standard normal density and distribution functions, with
#' \eqn{z = (Q - \mu)/\sigma}:
#'
#' \deqn{E[Q - \omega]^{+} = (Q - \mu)\Phi(z) + \sigma\phi(z).}
#'
#' @param Q Quantity stocked.
#' @param mean,sd Mean and standard deviation of demand.
#' @return Expected leftover stock.
#' @examples
#' nv_leftover(11.93, mean = 10, sd = 2)
#' @export
nv_leftover <- function(Q, mean, sd) {
  stopifnot(sd > 0)
  z <- (Q - mean) / sd
  (Q - mean) * stats::pnorm(z) + sd * stats::dnorm(z)
}

#' First order loss function
#'
#' \eqn{E[\omega - Q]^{+}}, the expected unmet demand. Obtained from
#' [nv_leftover()] by the identity
#' \eqn{E[\omega - Q]^{+} = E[Q - \omega]^{+} - (Q - E[\omega])}.
#'
#' @inheritParams nv_leftover
#' @return Expected unmet demand.
#' @examples
#' nv_shortfall(11.93, mean = 10, sd = 2)
#' @export
nv_shortfall <- function(Q, mean, sd) nv_leftover(Q, mean, sd) - (Q - mean)

#' Newsvendor cost
#'
#' Expected cost of stocking `Q`, charging `overage` per unit left over and
#' `underage` per unit of unmet demand. Normal demand uses the closed form of
#' [nv_leftover()]; a discrete demand is evaluated exactly over its support.
#'
#' @inheritParams nv_critical_fractile
#' @param Q Quantity stocked.
#' @param mean,sd Mean and standard deviation, for normal demand.
#' @param support,probs Demand values and their probabilities, for a discrete
#'   demand. Supply either these or `mean`/`sd`.
#' @return Expected cost.
#' @examples
#' nv_cost(11.93, overage = 1, underage = 5, mean = 10, sd = 2)
#'
#' # Exact Poisson, against the normal approximation to it
#' d <- 0:200
#' nv_cost(110, 1, 5, support = d, probs = dpois(d, 100))
#' nv_cost(110, 1, 5, mean = 100, sd = 10)
#' @export
nv_cost <- function(Q, overage, underage, mean = NULL, sd = NULL,
                    support = NULL, probs = NULL) {
  stopifnot(overage > 0, underage > 0)
  if (!is.null(support)) {
    stopifnot(length(support) == length(probs))
    probs <- probs / sum(probs)
    return(overage  * sum(pmax(Q - support, 0) * probs) +
           underage * sum(pmax(support - Q, 0) * probs))
  }
  (overage + underage) * nv_leftover(Q, mean, sd) - underage * (Q - mean)
}

#' Service levels attained by a stocking quantity
#'
#' The two targets practitioners impose in place of explicit costs
#' (Section 3.2).
#'
#' The **alpha** service level is the probability of no stockout in a cycle,
#' \eqn{F(Q)}. The **beta** service level, or fill rate, is the expected
#' fraction of demand served from stock,
#' \eqn{1 - E[\omega - Q]^{+}/E[\omega]}.
#'
#' The two are not interchangeable. Beta is typically far higher than alpha at
#' the same quantity, because a stockout that leaves one unit unserved counts
#' fully against alpha and barely against beta.
#'
#' @inheritParams nv_leftover
#' @return A list with `alpha` and `beta`.
#' @examples
#' nv_service_levels(11.93, mean = 10, sd = 2)
#' @export
nv_service_levels <- function(Q, mean, sd) {
  list(alpha = stats::pnorm(Q, mean, sd),
       beta  = 1 - nv_shortfall(Q, mean, sd) / mean)
}

#' Quantity achieving a target service level
#'
#' For `type = "alpha"` this inverts the demand distribution directly. For
#' `type = "beta"` it solves \eqn{E[\omega - Q]^{+}/E[\omega] = 1 - \beta}
#' numerically, which has no closed form.
#'
#' @inheritParams nv_leftover
#' @param target Service level in (0, 1).
#' @param type Either `"alpha"` or `"beta"`.
#' @return The quantity attaining `target`.
#' @examples
#' nv_quantity_for_service(0.95, mean = 10, sd = 2, type = "alpha")
#' nv_quantity_for_service(0.95, mean = 10, sd = 2, type = "beta")
#' @export
nv_quantity_for_service <- function(target, mean, sd,
                                    type = c("alpha", "beta")) {
  type <- match.arg(type)
  stopifnot(target > 0, target < 1)
  if (type == "alpha") return(stats::qnorm(target, mean, sd))
  stats::uniroot(function(q) nv_service_levels(q, mean, sd)$beta - target,
                 c(mean - 10 * sd, mean + 10 * sd))$root
}

# ---- internal: one-period expected cost, discrete demand ----------------
.period_cost <- function(y, support, probs, holding_cost, shortage_cost) {
  holding_cost * sum(pmax(y - support, 0) * probs) +
    shortage_cost * sum(pmax(support - y, 0) * probs)
}

#' Base-stock policy by stochastic dynamic programming
#'
#' With no fixed ordering cost, the optimal policy is order-up-to: in each
#' period, raise the inventory position to \eqn{S_t} if it is below, and
#' otherwise do nothing. This function computes \eqn{S_t} by backward
#' recursion over a finite horizon (Section 3.5).
#'
#' Under stationary demand the levels are constant and coincide with the
#' single-period Newsvendor solution at the critical fractile
#' \eqn{b/(h+b)}, which is the cheapest check on the result.
#'
#' @param periods Horizon length.
#' @param support,probs Demand values and probabilities, assumed the same in
#'   every period.
#' @param holding_cost,shortage_cost Costs \eqn{h} and \eqn{b}.
#' @param max_level Largest inventory level considered.
#' @return A list with the order-up-to levels `S` by period, and `cost`, the
#'   expected cost over the horizon starting from zero stock.
#' @examples
#' d <- 0:40
#' policy_base_stock(periods = 5, support = d, probs = dpois(d, 10),
#'                   holding_cost = 1, shortage_cost = 5)$S
#' @export
policy_base_stock <- function(periods, support, probs, holding_cost,
                              shortage_cost, max_level = NULL) {
  probs <- probs / sum(probs)
  if (is.null(max_level)) max_level <- max(support) * 2L
  states <- 0:max_level
  V <- numeric(max_level + 1L)
  S <- numeric(periods)

  for (t in periods:1) {
    G <- vapply(states, function(y) {
      nxt <- pmax(pmin(y - support, max_level), 0)
      .period_cost(y, support, probs, holding_cost, shortage_cost) +
        sum(probs * V[nxt + 1L])
    }, numeric(1))
    S[t] <- states[which.min(G)]
    V <- G[pmax(states, S[t]) + 1L]
  }
  list(S = S, cost = V[1])
}

#' Long-run average cost of an (s, S) policy
#'
#' Evaluates a given policy exactly, by solving for the stationary
#' distribution of the inventory position on the embedded Markov chain
#' \eqn{\{s+1, \ldots, S\}} and applying renewal reward (Section 3.7).
#'
#' @param s Reorder point: order when the position falls to `s` or below.
#' @param S Order-up-to level.
#' @inheritParams policy_base_stock
#' @param order_cost Fixed cost \eqn{K} per order.
#' @return Expected cost per period.
#' @examples
#' d <- 0:40
#' ss_cost(6, 40, support = d, probs = dpois(d, 10),
#'         order_cost = 64, holding_cost = 1, shortage_cost = 9)
#' @export
ss_cost <- function(s, S, support, probs, order_cost, holding_cost,
                    shortage_cost) {
  stopifnot(S > s)
  probs <- probs / sum(probs)
  ys <- (s + 1):S
  n <- length(ys)

  P <- matrix(0, n, n)
  for (i in seq_len(n)) {
    for (k in seq_along(support)) {
      nx <- ys[i] - support[k]
      j <- if (nx > s) match(nx, ys) else n     # below s: order up to S
      P[i, j] <- P[i, j] + probs[k]
    }
  }
  pi <- Re(eigen(t(P))$vectors[, 1])
  pi <- pi / sum(pi)

  Gy <- vapply(ys, .period_cost, numeric(1), support, probs,
               holding_cost, shortage_cost)
  p_order <- vapply(ys, function(y) sum(probs[(y - support) <= s]), numeric(1))
  sum(pi * Gy) + order_cost * sum(pi * p_order)
}

#' Optimal stationary (s, S) policy
#'
#' Searches the two parameters, evaluating each candidate exactly with
#' [ss_cost()]. With a fixed ordering cost the optimal policy is of
#' \eqn{(s, S)} form, a consequence of the K-convexity of the cost-to-go
#' (Section 3.7).
#'
#' The search is exhaustive over the supplied range rather than using the
#' Zheng-Federgruen recursion, which is faster but harder to read; on the
#' instances in the book the two agree.
#'
#' @inheritParams ss_cost
#' @param S_range Candidate order-up-to levels.
#' @return A list with `s`, `S` and `cost`.
#' @examples
#' d <- 0:40
#' policy_ss(support = d, probs = dpois(d, 10), order_cost = 64,
#'           holding_cost = 1, shortage_cost = 9, S_range = 20:50)
#' @export
policy_ss <- function(support, probs, order_cost, holding_cost,
                      shortage_cost, S_range = NULL) {
  if (is.null(S_range)) S_range <- seq_len(ceiling(max(support) * 4))
  best <- list(cost = Inf)
  for (S in S_range) {
    for (s in 0:(S - 1)) {
      cost <- ss_cost(s, S, support, probs, order_cost, holding_cost,
                      shortage_cost)
      if (cost < best$cost) best <- list(s = s, S = S, cost = cost)
    }
  }
  best
}

#' Long-run average cost of an (R, S) policy
#'
#' Periodic review: the position is inspected every `R` periods and raised to
#' `S`. Between reviews the system is uncovered, so the quantity must span
#' \eqn{R} periods of demand rather than one, and the cost is evaluated over
#' the review interval (Section 3.10).
#'
#' @param R Review interval, in periods.
#' @param S Order-up-to level.
#' @inheritParams policy_base_stock
#' @param order_cost Fixed cost \eqn{K} per review at which an order is placed.
#' @return Expected cost per period.
#' @examples
#' d <- 0:40
#' policy_rs(R = 3, S = 40, support = d, probs = dpois(d, 10),
#'           order_cost = 64, holding_cost = 1, shortage_cost = 9)
#' @export
policy_rs <- function(R, S, support, probs, order_cost, holding_cost,
                      shortage_cost) {
  stopifnot(R >= 1, R == round(R))
  probs <- probs / sum(probs)
  # Demand over j periods, by repeated convolution of the one-period law.
  conv <- probs
  total <- 0
  for (j in seq_len(R)) {
    sup_j <- (min(support) * j):(max(support) * j)
    total <- total + .period_cost(S, sup_j, conv, holding_cost, shortage_cost)
    if (j < R) conv <- as.numeric(stats::convolve(conv, rev(probs),
                                                  type = "open"))
  }
  (order_cost + total) / R
}
