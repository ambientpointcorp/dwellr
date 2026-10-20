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
  cheap <- threshold_from_dwell_quantile(d, cost_miss = 20, cost_look = 10)
  dear  <- threshold_from_dwell_quantile(d, cost_miss = 400, cost_look = 10)
  expect_equal(cheap$fractile, 10 / 30)
  expect_equal(dear$fractile, 10 / 410)
  # A more expensive miss buys an earlier threshold and a larger flagged
  # share, because looking sooner is what protects against the miss. The
  # newsvendor fractile runs the other way, since its decision variable is a
  # stocking quantity and this one is a waiting time.
  expect_lte(dear$hours, cheap$hours)
  expect_gt(dear$flagged_share, cheap$flagged_share)
  expect_equal(dear$flagged_share, 1 - dear$fractile)
})

test_that("a cost threshold on fitted probabilities minimizes expected cost", {
  sim <- simulate_lifecycles(n = 1500, seed = 5)
  d <- stage_dwell(sim, "released")
  mix <- dwell_mixture(d)
  q <- mix$posterior(d$dwell_hours, censored = d$censored)

  # The mixture posterior is a probability and rises with time waited, which
  # is what lets a threshold on it be read back as an hours threshold.
  expect_true(all(q >= 0 & q <= 1))
  expect_true(all(diff(mix$posterior(sort(d$dwell_hours), censored = TRUE)) >= -1e-9))

  r <- threshold_from_cost(q, cost_miss = 40, cost_look = 400, hours = d$dwell_hours)
  expect_equal(r$probability_threshold, 400 / 440)
  expect_equal(r$flagged_share, mean(q > 400 / 440))

  # The rule flags order by order against the same comparison, so it is the
  # pointwise minimizer: no fixed policy can cost less under these costs.
  expect_lte(r$expected_cost[["rule"]], r$expected_cost[["none"]])
  expect_lte(r$expected_cost[["rule"]], r$expected_cost[["all"]])
  set.seed(11)
  for (i in 1:20) {
    f <- stats::runif(length(q)) < 0.5
    cost <- sum(ifelse(f, (1 - q) * 400, q * 40))
    expect_lte(r$expected_cost[["rule"]], cost)
  }

  # A dearer miss lowers the threshold, flags more and surfaces sooner.
  loose <- threshold_from_cost(q, cost_miss = 400, cost_look = 40, hours = d$dwell_hours)
  expect_lt(loose$probability_threshold, r$probability_threshold)
  expect_gt(loose$flagged_share, r$flagged_share)
  expect_lte(loose$hours, r$hours)

  expect_error(threshold_from_cost(c(0.5, 1.5), 40, 400), "prob")
  expect_error(threshold_from_cost(q, 40, 400, hours = d$dwell_hours[-1]), "length")
})
