#' Is a review queue stable?
#'
#' Alerts arrive and reviewers serve them, which makes an exception channel a
#' queue and gives it the property every queue has: if work arrives faster than
#' it is served, the backlog grows without bound and no amount of effort inside
#' the channel repairs it (§15.1).
#'
#' The ratio of the two rates is the utilization. Below one the queue is
#' stable and the backlog has a finite expectation; at or above one it is not,
#' and the oldest item in the queue gets older every day.
#'
#' The queue-length figure assumes Poisson arrivals and exponential service,
#' which an alert channel does not satisfy: alerts arrive in bursts when a
#' vendor misses a sailing, and reviews take a fairly predictable ten minutes.
#' Read it as the order of magnitude the design has to live with rather than a
#' forecast, and note that both departures from the assumptions push the real
#' queue the same way, which is longer.
#'
#' @param alerts_per_day Arrival rate of exceptions.
#' @param reviews_per_day Service rate the team can sustain.
#'
#' @return A list with the two rates, the `utilization`, whether the queue is
#'   `stable`, and under the M/M/1 reading the expected `queue_length` and
#'   `wait_days` for an arriving alert.
#'
#' @examples
#' queue_stability(alerts_per_day = 3.8, reviews_per_day = 12)
#' queue_stability(alerts_per_day = 37.9, reviews_per_day = 12)
#' @export
queue_stability <- function(alerts_per_day, reviews_per_day) {
  stopifnot(alerts_per_day >= 0, reviews_per_day > 0)
  rho <- alerts_per_day / reviews_per_day
  stable <- rho < 1
  list(alerts_per_day = alerts_per_day,
       reviews_per_day = reviews_per_day,
       utilization = rho,
       stable = stable,
       queue_length = if (stable) rho / (1 - rho) else Inf,
       wait_days = if (stable) rho / (reviews_per_day * (1 - rho)) else Inf)
}

#' Admit a queue against a review capacity
#'
#' A threshold answers whether an instance is worth reviewing. It does not ask
#' whether anybody is free to review it, and Section 14.3 showed
#' that a cost ratio with a dear miss admits almost the whole population. Where
#' capacity binds, the operating question is not which instances qualify but
#' which `capacity` of them to work first (§15.5).
#'
#' Admission by capacity turns the threshold problem into a ranking problem.
#' The instances admitted are the top `capacity` by score, and what matters
#' about the score stops being its calibration and becomes its ordering, which
#' is the reverse of the emphasis in Section 14.5 and worth
#' noticing when the same model serves both.
#'
#' The `marginal` table is the part to read. Average precision over an admitted
#' queue says what the queue was worth; precision in the last band says what
#' the next reviewer hour would be worth, and that is the quantity a decision
#' to add or remove capacity turns on.
#'
#' @param score Ranking score, one per instance. Higher is more urgent.
#' @param capacity Number of instances that can be reviewed in the cycle.
#' @param outcome Optional logical, `TRUE` where review would have found a
#'   genuine problem. Supply it to get precision and recall.
#' @param value Optional exposure per instance, to report what the admitted
#'   queue is worth.
#' @param p_star Optional threshold probability from
#'   [threshold_from_cost()]. Supply it to get the capacity at which the
#'   marginal review stops paying.
#' @param band Width of the trailing band used for marginal precision.
#'
#' @return A list with the `admitted` logical vector, the `cutoff` score, the
#'   `precision` and `recall` at this capacity, a `marginal` data frame, the
#'   `paying_capacity` where marginal precision last exceeds `p_star`, and the
#'   `value_admitted` where exposure was supplied.
#'
#' @examples
#' set.seed(1)
#' s <- runif(500)
#' y <- runif(500) < s^2
#' capacity_admit(s, capacity = 50, outcome = y, p_star = 0.25)
#' @export
capacity_admit <- function(score, capacity, outcome = NULL, value = NULL,
                           p_star = NULL, band = 50) {
  stopifnot(length(score) > 0, capacity >= 1, band >= 1)
  capacity <- min(as.integer(capacity), length(score))
  ord <- order(score, decreasing = TRUE)
  take <- ord[seq_len(capacity)]

  admitted <- rep(FALSE, length(score))
  admitted[take] <- TRUE
  out <- list(admitted = admitted, capacity = capacity,
              cutoff = score[ord[capacity]])

  if (!is.null(outcome)) {
    stopifnot(length(outcome) == length(score), is.logical(outcome))
    out$precision <- mean(outcome[take])
    out$recall <- sum(outcome[take]) / max(sum(outcome), 1)

    # Precision inside each trailing band, which is what the next block of
    # reviewer time would be spent on.
    ends <- seq(band, length(ord), by = band)
    out$marginal <- data.frame(
      up_to = ends,
      band_precision = vapply(ends, function(k)
        mean(outcome[ord[(k - band + 1):k]]), numeric(1)),
      cumulative_precision = vapply(ends, function(k)
        mean(outcome[ord[seq_len(k)]]), numeric(1)))

    if (!is.null(p_star)) {
      below <- which(out$marginal$band_precision < p_star)
      out$paying_capacity <- if (length(below)) {
        if (below[1] == 1) 0L else out$marginal$up_to[below[1] - 1]
      } else length(ord)
    }
  }

  if (!is.null(value)) {
    stopifnot(length(value) == length(score))
    out$value_admitted <- sum(value[take])
    out$value_share <- sum(value[take]) / sum(value)
  }
  out
}
