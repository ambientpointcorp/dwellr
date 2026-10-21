test_that("both steady-state identities hold when no stage is starved", {
  r <- echelon_simulate(c(27, 59), demand_mean = 10, demand_sd = 3,
                        lead_times = c(1, 2), holding = c(1, 0.6),
                        penalty = 9, periods = 6000)
  expect_lt(max(abs(r$identity$gap)), 0.3)
  expect_lt(abs(diff(r$net_inventory_identity)), 0.3)
  expect_gt(r$fill_rate, 0.97)
})

test_that("a single stage reproduces the newsvendor level", {
  f <- function(y) echelon_simulate(y, 10, 3, 1, 1, 9, periods = 6000)$cost
  g <- seq(21, 30)
  star <- 10 * 2 + qnorm(9 / (9 + 1)) * 3 * sqrt(2)
  expect_lt(abs(g[which.min(vapply(g, f, 0))] - star), 1.5)
})

test_that("echelon control carries less upstream stock than installation", {
  a <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                        periods = 6000)
  b <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                        periods = 6000, policy = "installation")
  expect_equal(a$fill_rate, b$fill_rate, tolerance = 1e-3)
  expect_lt(a$on_hand[2], b$on_hand[2])
  expect_lt(a$cost, b$cost)
})

test_that("a stall costs more at the downstream echelon than upstream", {
  base <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                           periods = 6000)
  down <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                           periods = 6000,
                           stall = list(stage = 1, probability = 0.1,
                                        periods = 4))
  up <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                         periods = 6000,
                         stall = list(stage = 2, probability = 0.1,
                                      periods = 4))
  expect_gt(down$cost - base$cost, up$cost - base$cost)
  expect_lt(down$fill_rate, up$fill_rate)
})

test_that("unit review passes demand through without amplification", {
  r <- echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(1, 0.6), 9,
                        periods = 6000)
  expect_lt(abs(bullwhip(r$demand, r$orders)$ratio - 1), 0.05)
})

test_that("batching the review amplifies the orders a supplier sees", {
  r <- echelon_simulate(c(70, 110), 10, 3, c(1, 2), c(1, 0.6), 9,
                        periods = 6000, review = 5)
  expect_gt(bullwhip(r$demand, r$orders)$ratio, 10)
})

test_that("levels must be nondecreasing upstream", {
  expect_error(echelon_simulate(c(59, 27), 10, 3, c(1, 2), c(1, 0.6), 9,
                                periods = 600, warmup = 100),
               "nondecreasing")
})

test_that("holding costs must not rise upstream", {
  expect_error(echelon_simulate(c(27, 59), 10, 3, c(1, 2), c(0.6, 1), 9,
                                periods = 600, warmup = 100),
               "diff\\(holding\\)")
})

test_that("the search improves on its starting point", {
  o <- echelon_optimize(10, 3, c(1, 2), c(1, 0.6), 9, periods = 2500,
                        steps = c(4, 1), span = 3)
  start <- echelon_simulate(cumsum(10 * (c(1, 2) + 1)), 10, 3, c(1, 2),
                            c(1, 0.6), 9, periods = 2500)$cost
  expect_lte(o$cost, start)
  expect_true(all(diff(o$levels) >= 0))
})

test_that("bullwhip reports the ratio and what produced it", {
  set.seed(2)
  d <- rnorm(400, 10, 2)
  b <- bullwhip(d, d)
  expect_equal(b$ratio, 1)
  expect_equal(b$cv_demand, b$cv_orders)
})
