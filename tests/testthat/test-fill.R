test_that("fill_rate takes the minimum across dimensions", {
  o <- data.frame(fill_units = c(1, 0.75, 0.9),
                  fill_lines = c(1, 0.90, 0.5),
                  fill_value = c(1, 0.60, 0.8))
  f <- fill_rate(o)
  expect_equal(f$fill_binding, c(1, 0.60, 0.5))
  expect_equal(f$fill_binding_dimension, c("units", "value", "lines"))
})

test_that("the three dimensions genuinely disagree on real-shaped data", {
  sim <- simulate_lifecycles(n = 2000, seed = 42)
  f <- fill_rate(sim$orders)
  short <- f[f$fill_binding < 1, ]
  expect_gt(nrow(short), 0)
  # More than one dimension binds across the population. If only one ever did,
  # Chapter 11's argument would be unnecessary.
  expect_gt(length(unique(short$fill_binding_dimension)), 1)
})
