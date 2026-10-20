test_that("fill_rate takes the minimum across dimensions", {
  o <- data.frame(fill_units = c(1, 0.75, 0.9),
                  fill_lines = c(1, 0.90, 0.5),
                  fill_value = c(1, 0.60, 0.8))
  f <- fill_rate(o)
  expect_equal(f$fill_binding, c(1, 0.60, 0.5))
  expect_equal(f$fill_binding_dimension, c("units", "value", "lines"))
  expect_true(all(f$fill_defined))
})

test_that("an order that has not shipped has no fill rate", {
  # NA rather than 1. Crediting an open order with a complete shipment is the
  # censoring error of Section 6.2 pointed the other way, and averaging over
  # the column would then report every backlog as perfectly filled (§11.2).
  o <- data.frame(fill_units = c(1, NA, 0.5),
                  fill_lines = c(1, NA, 0.8),
                  fill_value = c(1, NA, 0.9))
  f <- fill_rate(o)
  expect_equal(f$fill_defined, c(TRUE, FALSE, TRUE))
  expect_true(is.na(f$fill_binding[2]))
  expect_true(is.na(f$fill_binding_dimension[2]))
  expect_equal(f$fill_binding[c(1, 3)], c(1, 0.5))

  # A frame with nothing determined must still return, not error on which.min.
  none <- fill_rate(data.frame(fill_units = NA_real_, fill_lines = NA_real_,
                               fill_value = NA_real_))
  expect_false(none$fill_defined)
  expect_true(is.na(none$fill_binding))
})

test_that("the simulated population is short on more than one dimension", {
  sim <- simulate_lifecycles(n = 1200, seed = 1)
  f <- fill_rate(sim$orders)
  short <- f[which(f$fill_binding < 1), ]
  expect_gt(nrow(short), 0)
  expect_gt(length(unique(short$fill_binding_dimension)), 1)
  # Fill is determined exactly for the orders that reached a shipping stage.
  expect_equal(f$fill_defined,
               sim$orders$stage %in% c("shipped", "partially_shipped"))
})

test_that("the three dimensions are computed from the same lines", {
  sim <- simulate_lifecycles(n = 1500, seed = 11)
  o <- fill_rate(sim$orders)
  sl <- sim$lines[sim$lines$order_no %in% o$order_no[o$fill_defined], ]
  d <- fill_dimensions(sl)

  expect_equal(d$n_lines, nrow(sl))
  expect_equal(d$units, sum(sl$qty_shipped) / sum(sl$qty_ordered))
  expect_true(all(c(d$units, d$lines, d$value) >= 0))
  expect_true(all(c(d$units, d$lines, d$value) <= 1))

  # The line dimension computed from the lines must agree with the order-level
  # fraction the generator recorded, since both count complete lines.
  per_order <- tapply(sl$qty_shipped >= sl$qty_ordered, sl$order_no, mean)
  recorded <- o$fill_lines[match(names(per_order), o$order_no)]
  expect_equal(as.numeric(per_order), recorded)

  # Likewise for units and value, which the generator derives from these lines
  # rather than drawing separately.
  su <- tapply(sl$qty_shipped, sl$order_no, sum) /
        tapply(sl$qty_ordered, sl$order_no, sum)
  expect_equal(as.numeric(su), o$fill_units[match(names(su), o$order_no)])
})

test_that("line quantities and prices reconcile with the order record", {
  sim <- simulate_lifecycles(n = 900, seed = 5)
  L <- sim$lines
  o <- sim$orders

  expect_equal(nrow(L), sum(o$lines))
  expect_true(all(L$qty_ordered >= 1))
  expect_true(all(L$unit_price > 0))
  expect_true(all(L$qty_shipped >= 0 & L$qty_shipped <= L$qty_ordered))

  # Extended value of an order's lines sums to the order value.
  ext <- tapply(L$qty_ordered * L$unit_price, L$order_no, sum)
  expect_equal(as.numeric(ext[o$order_no]), o$value, tolerance = 1e-8)

  # Nothing ships on an order that has not reached a shipping stage.
  unshipped <- o$order_no[!o$stage %in% c("shipped", "partially_shipped")]
  expect_true(all(L$qty_shipped[L$order_no %in% unshipped] == 0))
})
