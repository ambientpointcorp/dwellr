test_that("stage_dwell excludes terminal stages and flags open visits", {
  sim <- simulate_lifecycles(n = 400, seed = 3)
  d <- stage_dwell(sim)
  expect_false(any(d$stage %in% c("shipped", "cancelled")))
  expect_type(d$censored, "logical")
  expect_true(all(d$dwell_hours >= 0))
  # A censored visit ends at the observation cutoff, by construction.
  expect_true(all(abs(as.numeric(difftime(d$left_at[d$censored], sim$as_of,
                                          units = "secs"))) < 1))
})

test_that("dwell_quantile is monotone and uses the censored estimate", {
  sim <- simulate_lifecycles(n = 1500, seed = 5)
  d <- stage_dwell(sim, "released")
  q <- dwell_quantile(d, c(0.5, 0.75, 0.9))
  expect_true(all(diff(q) >= 0))
  # p90 should land well past the median on a heavy-tailed dwell -- the reason
  # Section 6.7 prefers a quantile to mean-plus-sigma.
  expect_gt(q[3], q[1])
})

test_that("a threshold follows the cost ratio", {
  sim <- simulate_lifecycles(n = 1500, seed = 5)
  d <- stage_dwell(sim, "released")
  cheap <- threshold_from_cost(d, cost_miss = 20, cost_look = 10)
  dear  <- threshold_from_cost(d, cost_miss = 400, cost_look = 10)
  expect_equal(cheap$fractile, 20 / 30)
  expect_equal(dear$fractile, 400 / 410)
  # A more expensive miss buys a later threshold -- you tolerate more waiting
  # before spending attention only when attention is relatively dear.
  expect_gte(dear$hours, cheap$hours)
})
