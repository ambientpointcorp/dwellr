test_that("the three cost components are reported separately and sum", {
  k <- observation_cost(fixed_annual = 40000, per_observation = 0.004,
                        observations_per_day = 180, per_decision = 0.012,
                        decisions_per_day = 14)
  expect_equal(nrow(k), 4)
  expect_equal(k$annual[k$component == "total"], sum(k$annual[1:3]))
  expect_equal(sum(k$share[1:3]), 1, tolerance = 1e-3)
  expect_equal(k$scales_with[1:3], c("nothing", "cadence", "volume"))
})

test_that("per-decision cost scales with volume and observation with cadence", {
  base <- observation_cost(1000, 0.01, 100, 0.02, 100)
  more_volume <- observation_cost(1000, 0.01, 100, 0.02, 1000)
  more_scans <- observation_cost(1000, 0.01, 1000, 0.02, 100)
  expect_equal(more_volume$annual[2], base$annual[2])
  expect_equal(more_volume$annual[3], base$annual[3] * 10)
  expect_equal(more_scans$annual[2], base$annual[2] * 10)
  expect_equal(more_scans$annual[3], base$annual[3])
})

test_that("the closed-form interval is the numerical minimum", {
  for (p in list(c(12, 5, 0.8), c(2, 40, 2), c(50, 1, 10))) {
    o <- cadence_optimal(p[1], p[2], p[3])
    g <- seq(0.01, 60, by = 0.005)
    num <- g[which.min(cadence_cost(g, p[1], p[2], p[3]))]
    expect_lt(abs(o$interval - num), 0.01)
    expect_true(o$closed_form)
  }
})

test_that("the square-root rule is as insensitive here as for order quantity", {
  o <- cadence_optimal(12, 5, 0.8)
  for (f in c(0.5, 2))
    expect_lt(abs(cadence_cost(f * o$interval, 12, 5, 0.8) / o$cost - 1.25),
              1e-9)
  expect_lt(cadence_cost(0.75 * o$interval, 12, 5, 0.8) / o$cost - 1, 0.05)
})

test_that("scan and lag cost are equal at the optimum", {
  o <- cadence_optimal(12, 5, 0.8)
  expect_equal(o$scan, o$lag, tolerance = 1e-8)
})

test_that("a nonlinear wait cost is optimized numerically", {
  # Nothing is lost until the slack is gone, then the whole miss cost lands.
  deadline <- function(wait) if (wait < 6) 0 else 400
  o <- cadence_optimal(12, 5, lag_cost = NA, lag_cost_fn = deadline)
  expect_false(o$closed_form)
  expect_lt(o$interval, 12)          # must scan before the slack runs out
  expect_lt(o$cost, cadence_cost(24, 12, 5, NA, deadline))
})

test_that("a zero arrival rate is refused rather than dividing by it", {
  expect_error(cadence_optimal(12, 0, 0.8), "arrival_rate > 0")
})

test_that("the three miss-cost approaches imply different thresholds", {
  m <- miss_cost(order_value = 865, expedite = 75, margin_rate = 0.22,
                 loss_probability = 0.15, lifetime_value = 18000,
                 churn_probability = 0.01, cost_look = 40)
  expect_equal(nrow(m), 3)
  expect_equal(m$miss_cost[m$approach == "expediting"], 75)
  expect_equal(m$miss_cost[m$approach == "retention"], 180)
  expect_true(all(diff(m$p_star) <= 0 | diff(m$p_star) >= 0))
  expect_true(all(m$p_star > 0 & m$p_star < 1))
  expect_equal(m$p_star[1], 40 / (40 + 75), tolerance = 1e-4)
})

test_that("a margin rate outside zero and one is refused", {
  expect_error(miss_cost(865, 75, 1.4, 0.15, 18000, 0.01, 40),
               "margin_rate <= 1")
})
