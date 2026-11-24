#' Fill rate on three dimensions
#'
#' Order fill measured on units, lines and value, plus the binding dimension.
#'
#' The three dimensions can disagree. Six of eight units is 0.75 by units; if
#' the missing unit is the expensive one, the value figure is lower; a
#' one-of-three-lines order is lower by lines. The binding dimension is the
#' minimum of the three, which is the one the customer experiences.
#'
#' Orders that have not reached a shipping stage carry `NA` on all three
#' dimensions, since no fill rate is defined before shipment. The
#' binding dimension is `NA` for those rows rather than 1, so that averaging
#' over the column cannot credit an open order with a complete shipment. The
#' returned `fill_defined` column marks the rows the three dimensions are
#' available for, and any rate quoted from this frame should report that
#' denominator beside it.
#'
#' @param orders A data frame with `fill_units`, `fill_lines`, `fill_value`,
#'   as produced by [simulate_lifecycles()].
#'
#' @param cols Optional named character vector renaming the columns read,
#'   for a population that calls the fractions something else. Understands
#'   `units`, `lines` and `value`; the defaults are the generator's own
#'   names. The reported binding dimension is the quantity's name rather
#'   than the column's, so results stay comparable across populations.
#' @return The input with `fill_binding`, `fill_binding_dimension` and
#'   `fill_defined` added.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 300, seed = 1)
#' f <- fill_rate(sim$orders)
#' table(f$fill_binding_dimension[f$fill_binding < 1])
#' # The denominator a fill rate is quoted against.
#' c(defined = sum(f$fill_defined), open = sum(!f$fill_defined))
#' @export
fill_rate <- function(orders, cols = NULL) {
  cn <- resolve_cols(
    orders,
    default = c(units = "fill_units", lines = "fill_lines",
                value = "fill_value"),
    map = cols, what = "`orders`")
  dims <- unname(cn[c("units", "lines", "value")])
  m <- as.matrix(orders[, dims])
  defined <- stats::complete.cases(m)

  binding <- rep(NA_real_, nrow(m))
  dimension <- rep(NA_character_, nrow(m))
  if (any(defined)) {
    md <- m[defined, , drop = FALSE]
    binding[defined] <- apply(md, 1, min)
    # The reported dimension is the quantity's name rather than the column's,
    # so a population whose columns are named differently still reports
    # `units`, `lines` or `value` and stays comparable with the rest.
    dimension[defined] <- names(cn)[apply(md, 1, which.min)]
  }

  orders$fill_binding <- binding
  orders$fill_binding_dimension <- dimension
  orders$fill_defined <- defined
  orders
}

#' Fill on three dimensions, computed from the lines
#'
#' The order-level fractions give how much of an order was filled. They do not
#' identify which line was short, and a fill rate aggregated over orders
#' weights a one-line order the same as a twelve-line one. This function
#' computes each dimension over a set of lines, the grain the quantities were
#' recorded at.
#'
#' Three denominators are available and they answer different questions. The
#' unit rate is the share of units shipped, the line rate is the share of lines
#' shipped complete, and the value rate is the share of extended value shipped.
#' An order-weighted figure averages each order's own rate and so gives every
#' order equal weight; a line-weighted figure pools the lines and so gives
#' every line equal weight. Which one applies depends on whether the question
#' concerns customers or items. Both are reported together, since quoting one
#' under the other's name is a common error.
#'
#' @param lines A line table with `order_no`, `qty_ordered`, `qty_shipped` and
#'   `unit_price`, as returned in the `lines` element of
#'   [simulate_lifecycles()] or by [order_lines()].
#'
#' @param cols Optional named character vector renaming the columns read.
#'   Understands `order`, `ordered`, `shipped` and `price`; the defaults
#'   are the generator's own names.
#' @return A list with the pooled `units`, `lines` and `value` rates, the
#'   `order_weighted` mean of each order's own rates, and the counts behind
#'   them.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' fill_dimensions(sim$lines[sim$lines$order_no %in%
#'   sim$orders$order_no[sim$orders$stage == "shipped"], ])
#' @export
fill_dimensions <- function(lines, cols = NULL) {
  cn <- resolve_cols(
    lines,
    default = c(order = "order_no", ordered = "qty_ordered",
                shipped = "qty_shipped", price = "unit_price"),
    map = cols, what = "`lines`")
  stopifnot(nrow(lines) > 0)
  order_no <- lines[[cn[["order"]]]]
  qty_ordered <- lines[[cn[["ordered"]]]]
  qty_shipped <- lines[[cn[["shipped"]]]]
  unit_price <- lines[[cn[["price"]]]]

  complete <- qty_shipped >= qty_ordered
  ext_ord <- qty_ordered * unit_price
  ext_shp <- qty_shipped * unit_price

  by_order <- function(x) as.numeric(tapply(x, order_no, sum))
  n_ord <- length(unique(order_no))
  ow <- c(units = mean(by_order(qty_shipped) / by_order(qty_ordered)),
          lines = mean(as.numeric(tapply(complete, order_no, mean))),
          value = mean(by_order(ext_shp) / by_order(ext_ord)))

  list(units = sum(qty_shipped) / sum(qty_ordered),
       lines = mean(complete),
       value = sum(ext_shp) / sum(ext_ord),
       order_weighted = ow,
       n_orders = n_ord,
       n_lines = nrow(lines),
       units_ordered = sum(qty_ordered))
}
