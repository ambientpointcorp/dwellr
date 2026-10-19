test_that("the two terms of the variance expression are reported separately", {
  sim <- simulate_lifecycles(n = 2000, seed = 11)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  vc <- dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3)
  tot <- attr(vc, "total")

  expect_equal(tot[["demand_variance"]] + tot[["lead_variance"]],
               tot[["ltd_variance"]], tolerance = 1e-10)
  # Section 7.1: the lead-time term is multiplied by the square of mean demand
  # and dominates once the lead time is dispersed.
  expect_gt(tot[["lead_variance"]] / tot[["ltd_variance"]], 0.9)
  expect_equal(unname(tot[["safety_stock"]]),
               qnorm(0.95) * unname(tot[["ltd_sd"]]), tolerance = 1e-10)
})

test_that("stage variance shares sum to the lead-time term's share", {
  sim <- simulate_lifecycles(n = 2000, seed = 11)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  vc <- dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3)
  tot <- attr(vc, "total")
  expect_equal(sum(vc$variance), unname(tot[["lead_variance"]]),
               tolerance = 1e-8)
  expect_equal(sum(vc$share_of_ltd_variance),
               unname(tot[["lead_variance"]] / tot[["ltd_variance"]]),
               tolerance = 1e-8)
})

test_that("marginal attribution sums to less than the total, proportional to it", {
  sim <- simulate_lifecycles(n = 2000, seed = 11)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  vc <- dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3)
  ss <- attr(vc, "total")[["safety_stock"]]

  # Safety stock is proportional to a standard deviation, so removing a stage
  # releases less than its share of the variance implies. Reporting only the
  # proportional figures would overstate what any one improvement achieves.
  expect_lt(sum(vc$marginal_units), ss)
  expect_equal(sum(vc$proportional_units), unname(ss) *
                 sum(vc$share_of_ltd_variance), tolerance = 1e-8)
  expect_true(all(vc$marginal_units <= vc$proportional_units + 1e-9))
})

test_that("units are a stated convention, not an assumption", {
  sim <- simulate_lifecycles(n = 1500, seed = 11)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  daily <- dwell_variance_contribution(dec, 10, 3, hours_per_period = 24)
  hourly <- dwell_variance_contribution(dec, 10 / 24, 3 / sqrt(24),
                                        hours_per_period = 1)
  # The same physical situation, in two period lengths: the lead time in
  # periods differs by 24 and the attribution shares do not.
  expect_equal(attr(hourly, "total")[["lead_time_mean"]],
               attr(daily, "total")[["lead_time_mean"]] * 24,
               tolerance = 1e-8)
  expect_equal(hourly$share_of_ltd_variance, daily$share_of_ltd_variance,
               tolerance = 0.02)
})

test_that("a reduction is worth less than a proportional reading suggests", {
  sim <- simulate_lifecycles(n = 2000, seed = 11)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  vc <- dwell_variance_contribution(dec, 10, 3)
  full <- variance_reduction_value(vc, "received", 1.0, unit_cost = 120)
  half <- variance_reduction_value(vc, "received", 0.5, unit_cost = 120)

  expect_equal(unname(half$safety_stock_before),
               unname(attr(vc, "total")[["safety_stock"]]))
  expect_lt(half$units, full$units)

  # Halving a stage's variance releases LESS than half of what eliminating it
  # would, because safety stock is the square root of a sum and the return to
  # removing variance rises as more of it goes. The economics therefore favor
  # eliminating a stage's variance over trimming it, which is the opposite of
  # the intuition that a partial fix buys a proportional share of the benefit.
  expect_lt(half$units, full$units / 2)
  expect_gt(half$units, 0.85 * full$units / 2)
  expect_equal(half$annual_value, half$units * 120 * 0.22, tolerance = 1e-10)
  expect_error(variance_reduction_value(vc, "no_such_stage", 0.5, 120))
})

test_that("a monitoring budget is a break-even and says when it is exceeded", {
  b <- monitoring_budget(186, cost_per_review = 4, reviews_per_year = 30)
  expect_equal(b$break_even_reviews, 46.5)
  expect_true(b$justified)
  expect_false(monitoring_budget(186, 4, reviews_per_year = 60)$justified)
  expect_null(monitoring_budget(186, 4)$justified)
})

test_that("driver attribution ranks the drivers and reports what it cannot explain", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  rel <- merge(stage_backlog(sim, "released"),
               sim$orders[, c("order_no", "branch", "order_source", "lines")],
               by = "order_no")
  dv <- driver_variance(rel, c("branch", "order_source", "lines", "backlog"))

  # `lines` is the only covariate the generator attaches to this stage.
  expect_equal(dv$ranking$term[1], "lines")
  expect_equal(dv$ranking$share_of_log_variance,
               sort(dv$ranking$share_of_log_variance, decreasing = TRUE))
  # Section 7.3: almost all of the variance is the fast-and-stuck mixture of
  # Section 6.5, which no order attribute predicts. The ranking is a ranking.
  expect_gt(dv$unexplained, 0.9)
  expect_lt(dv$max_correlation, 0.1)
})

test_that("the service-level gap is attributed to the lead time", {
  g <- service_level_gap(demand_mean = 10, demand_sd = 3,
                         assumed_mean = 2, assumed_sd = 0.5,
                         actual_mean = 2.02, actual_sd = 4.37)
  expect_lt(g$alpha_attained, g$alpha_target)
  expect_lt(g$beta_attained, g$beta_target)
  expect_gt(g$shortfall_multiple, 5)
  expect_gt(g$safety_stock_required, g$safety_stock_carried)

  # With the lead time correctly measured there is no gap to attribute.
  ok <- service_level_gap(10, 3, 2, 0.5, 2, 0.5)
  expect_equal(ok$alpha_attained, ok$alpha_target, tolerance = 1e-8)
  expect_equal(ok$shortfall_multiple, 1, tolerance = 1e-8)
})

test_that("the empirical convolution checks the normal approximation", {
  set.seed(4)
  sim <- simulate_lifecycles(n = 3000, seed = 11)
  d <- stage_dwell(sim)
  done <- sim$orders$order_no[sim$orders$stage == "shipped"]
  keep <- d$order_no %in% done
  L <- tapply(d$dwell_hours[keep], d$order_no[keep], sum) / 24

  e <- lead_time_demand_empirical(L, 10, 3, n = 1e5)
  # The normal matching these moments puts real probability on negative
  # demand, so it is the wrong shape even where its upper quantile is close.
  expect_gt(e$normal_mass_below_zero, 0.2)
  expect_lt(abs(e$normal_safety_stock / e$safety_stock - 1), 0.25)
  expect_gt(e$safety_stock, 0)
})
