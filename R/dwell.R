#' Stage dwell times, with censoring
#'
#' Reduces an event log to one row per (order, stage visit) with the time spent
#' in that stage and whether the visit had ended by `as_of`.
#'
#' The `censored` column is the point. An order sitting in a stage right now has
#' not finished waiting, so its dwell is a lower bound, not an observation.
#' Dropping those rows -- which is what a naive average over completed visits
#' does -- removes exactly the orders that are stuck, and biases the result
#' optimistic by most when the operation is performing worst (§6.2).
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()], or a list with
#'   `events` and `as_of`.
#' @param stage Optional stage name to restrict to.
#'
#' @return A data frame with `order_no`, `stage`, `entered_at`, `left_at`,
#'   `dwell_hours` and `censored` (`TRUE` when the visit was still open).
#'
#' @examples
#' sim <- simulate_lifecycles(n = 400, seed = 1)
#' d <- stage_dwell(sim, stage = "released")
#' mean(d$censored)
#' @export
stage_dwell <- function(sim, stage = NULL) {
  ev <- sim$events
  as_of <- sim$as_of
  ev <- ev[order(ev$order_no, ev$occurred_at), ]

  nxt <- c(ev$occurred_at[-1], NA)
  same <- c(ev$order_no[-1] == ev$order_no[-nrow(ev)], FALSE)
  left <- as.POSIXct(ifelse(same, nxt, NA), origin = "1970-01-01", tz = "UTC")

  censored <- is.na(left)
  left[censored] <- as_of

  out <- data.frame(
    order_no = ev$order_no,
    stage = ev$stage,
    entered_at = ev$occurred_at,
    left_at = left,
    dwell_hours = as.numeric(difftime(left, ev$occurred_at, units = "hours")),
    censored = censored,
    stringsAsFactors = FALSE
  )
  # A terminal stage is not a wait; it is where the order stopped.
  out <- out[!out$stage %in% c("shipped", "cancelled"), ]
  if (!is.null(stage)) out <- out[out$stage %in% stage, ]
  rownames(out) <- NULL
  out
}

#' Survival curve for a stage dwell
#'
#' Kaplan-Meier estimate of the time an order spends in a stage, treating
#' still-open visits as right-censored.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param by Optional column name to stratify by, e.g. `"branch"` -- requires
#'   that column to have been joined on first.
#'
#' @return A `survfit` object.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' fit <- dwell_survival(stage_dwell(sim, "released"))
#' summary(fit, times = c(4, 8, 12, 24, 48, 72))$surv
#' @export
dwell_survival <- function(dwell, by = NULL) {
  stopifnot(all(c("dwell_hours", "censored") %in% names(dwell)))
  # survival's event indicator is 1 = event observed, 0 = censored.
  s <- survival::Surv(pmax(dwell$dwell_hours, 1e-6), as.integer(!dwell$censored))
  if (is.null(by)) {
    survival::survfit(s ~ 1)
  } else {
    survival::survfit(s ~ dwell[[by]])
  }
}

#' Operating quantile of a stage dwell
#'
#' The dwell time by which a given share of orders has left the stage,
#' estimated from the censored distribution rather than from completed visits
#' only. This is the quantity a threshold should be set from (§6.7).
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param p Share left, between 0 and 1. `p = 0.9` returns the time by which
#'   90% of orders have moved on.
#'
#' @return Numeric hours, or `NA` if the curve does not reach `p` within the
#'   observation window -- which is itself a finding, not a failure.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' dwell_quantile(stage_dwell(sim, "released"), c(0.5, 0.9))
#' @export
dwell_quantile <- function(dwell, p = 0.9) {
  fit <- dwell_survival(dwell)
  vapply(p, function(pi) {
    hit <- which(fit$surv <= 1 - pi)
    if (!length(hit)) NA_real_ else fit$time[hit[1]]
  }, numeric(1))
}
