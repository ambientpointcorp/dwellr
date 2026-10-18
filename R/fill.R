#' Fill rate on three dimensions
#'
#' Order fill measured on units, lines and value, plus the binding dimension.
#'
#' The three disagree, and the disagreement is informative rather than noise.
#' Six of eight units is 0.75 by units; if the missing unit is the expensive one
#' it is worse by value; a one-of-three-lines order is worse by lines. The
#' binding dimension -- the minimum -- is the one the customer experiences
#' (Chapter 11).
#'
#' @param orders A data frame with `fill_units`, `fill_lines`, `fill_value`,
#'   as produced by [simulate_lifecycles()].
#'
#' @return The input with `fill_binding` and `fill_binding_dimension` added.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 300, seed = 1)
#' f <- fill_rate(sim$orders)
#' table(f$fill_binding_dimension[f$fill_binding < 1])
#' @export
fill_rate <- function(orders) {
  dims <- c("fill_units", "fill_lines", "fill_value")
  stopifnot(all(dims %in% names(orders)))
  m <- as.matrix(orders[, dims])
  orders$fill_binding <- apply(m, 1, min)
  orders$fill_binding_dimension <- sub("^fill_", "", dims[apply(m, 1, which.min)])
  orders
}
