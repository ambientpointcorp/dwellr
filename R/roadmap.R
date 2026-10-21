# Declared, not yet implemented.
#
# These are the function families the book's remaining chapters need. They are
# declared here, with the chapter each belongs to, for one reason: a reader
# who finds a name in the function reference and calls it should be told it is
# not written yet, not handed something plausible. An unimplemented method that
# returns a number is worse than one that errors.
#
# Each gains a real implementation as its chapter is drafted. Remove from this
# file when that happens.

not_yet <- function(what, chapter) {
  stop(sprintf("%s is not implemented yet (%s).\n  See OUTLINE.md in ambientpointcorp/lifecycle-analytics-book.",
               what, chapter), call. = FALSE)
}


#' Modeling helpers
#'
#' @description Not yet implemented. See Chapter 9.
#' @param ... Arguments, once there are any.
#' @name roadmap-modeling
#' @keywords internal
NULL

#' @rdname roadmap-modeling
#' @export
fit_dwell_model <- function(...) not_yet("fit_dwell_model()", "Chapter 9, machine learning for lifecycle data")

#' @rdname roadmap-modeling
#' @export
calibrate_probability <- function(...) not_yet("calibrate_probability()", "Chapter 9, calibration")

#' Deadlines, capacity, routing and evaluation
#'
#' @description Not yet implemented. See Parts III, IV and V.
#' @param ... Arguments, once there are any.
#' @name roadmap-operations
#' @keywords internal
NULL

#' @rdname roadmap-operations
#' @export
deadline_derive <- function(...) not_yet("deadline_derive()", "Chapter 13, deadlines and date fields")

#' @rdname roadmap-operations
#' @export
evaluate_policy <- function(...) not_yet("evaluate_policy()", "Chapter 21, evaluating a monitoring policy")
