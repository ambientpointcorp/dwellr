# Chapter 13 models. A commitment is when the customer expects something; the
# last useful moment to act precedes it by however long the work takes. The
# gap between those two is what an alert should be keyed on (§13.4).

#' Derive the last useful moment to act from a commitment
#'
#' A commitment is rarely the moment the operation has to act, because acting
#' takes time. The *actionable horizon* is the commitment less the handling
#' time less any external cutoff, and both offsets are measurable: the handling
#' time from the stage dwells of [dwell_quantile()], the cutoff from a
#' carrier's schedule (§13.4).
#'
#' Keying an alert on the commitment rather than the horizon surfaces orders
#' that are already too late to save, which is the practical reason to derive
#' this at all.
#'
#' The function will not invent a commitment. A row whose commitment is missing
#' comes back with `defined = FALSE` and a missing horizon, because
#' §13.8 establishes that declaring no deadline is a smaller claim than
#' inventing one and the only claim the data supports. Anything downstream has
#' to handle the missing case explicitly, which is the point.
#'
#' @param commitment Promised date or time, one per instance. `NA` where none
#'   was given.
#' @param handling_hours Hours of work between the last useful moment and the
#'   commitment, scalar or one per instance. A cohort-specific figure is the
#'   usual case, since handling time differs by what is being handled.
#' @param cutoff_hours Hours of external cutoff, such as a carrier departing
#'   before the promise.
#' @param as_of Optional observation time. Supplying it adds the remaining
#'   hours and the two breach flags.
#' @param entered_at Optional entry time per instance. Supplying it flags
#'   commitments that were never achievable, where the derived horizon falls
#'   before the instance existed.
#'
#' @return A data frame with one row per instance: `commitment`, `defined`,
#'   `offset_hours`, `horizon`, and, where `as_of` is given,
#'   `hours_to_horizon`, `past_horizon` and `past_commitment`. Where
#'   `entered_at` is given it also carries `infeasible`.
#'
#' @examples
#' promised <- as.POSIXct(c("2026-03-10 17:00", NA, "2026-03-09 12:00"),
#'                        tz = "UTC")
#' deadline_derive(promised, handling_hours = 15.5, cutoff_hours = 4,
#'                 as_of = as.POSIXct("2026-03-09 08:00", tz = "UTC"))
#' @export
deadline_derive <- function(commitment, handling_hours = 0, cutoff_hours = 0,
                            as_of = NULL, entered_at = NULL) {
  n <- length(commitment)
  stopifnot(n > 0)
  if (!inherits(commitment, "POSIXt") && !inherits(commitment, "Date"))
    stop("`commitment` must be a date or a time, not ",
         class(commitment)[1], ".", call. = FALSE)
  rec <- function(x, nm) {
    if (length(x) == 1) x <- rep(x, n)
    if (length(x) != n)
      stop("`", nm, "` must be length 1 or ", n, ".", call. = FALSE)
    if (any(x < 0, na.rm = TRUE))
      stop("`", nm, "` cannot be negative; an offset moves the horizon ",
           "earlier than the commitment, never later.", call. = FALSE)
    x
  }
  handling_hours <- rec(handling_hours, "handling_hours")
  cutoff_hours   <- rec(cutoff_hours, "cutoff_hours")

  commitment <- as.POSIXct(commitment)
  offset <- handling_hours + cutoff_hours
  horizon <- commitment - offset * 3600

  out <- data.frame(commitment = commitment,
                    defined = !is.na(commitment),
                    offset_hours = offset,
                    horizon = horizon)
  out$horizon[!out$defined] <- as.POSIXct(NA)

  if (!is.null(as_of)) {
    stopifnot(length(as_of) == 1)
    as_of <- as.POSIXct(as_of)
    out$hours_to_horizon <- round(
      as.numeric(difftime(out$horizon, as_of, units = "hours")), 4)
    out$past_horizon <- !is.na(out$horizon) & out$horizon < as_of
    out$past_commitment <- out$defined & commitment < as_of
  }

  if (!is.null(entered_at)) {
    if (length(entered_at) == 1) entered_at <- rep(entered_at, n)
    stopifnot(length(entered_at) == n)
    # A horizon before the instance arrived means the promise was never
    # achievable at the handling time assumed, which is a finding about the
    # promise rather than about the order.
    out$infeasible <- !is.na(out$horizon) &
      out$horizon < as.POSIXct(entered_at)
  }
  out
}
