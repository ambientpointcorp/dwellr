test_that("observe_at reconstructs the population as it stood earlier", {
  sim <- simulate_lifecycles(n = 600, seed = 3)
  earlier <- observe_at(sim, sim$as_of - 60 * 24 * 3600)

  expect_true(all(earlier$events$occurred_at <= earlier$as_of))
  expect_lt(nrow(earlier$events), nrow(sim$events))
  # Observing earlier can only leave more work in flight, never less.
  expect_gte(mean(!earlier$orders$is_terminal), mean(!sim$orders$is_terminal))
  # An order that had not been entered yet is absent rather than empty.
  expect_true(all(earlier$orders$order_no %in% sim$orders$order_no))
  expect_true(all(is.na(earlier$orders$finished_at[!earlier$orders$is_terminal])))
})

test_that("the naive mean is biased low, and worse the more work is in flight", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  bias <- vapply(c(20, 40, 60, 120), function(days) {
    s <- observe_at(sim, sim$as_of - (120 - days) * 24 * 3600)
    dwell_mean(stage_dwell(s, "awaiting_supply"), restrict_to = 400)$bias
  }, numeric(1))

  # Section 6.2: on a short window the completed-visits-only average
  # understates badly; the bias shrinks as the window lengthens.
  expect_lt(bias[1], -0.2)
  expect_gt(bias[4], -0.02)
  expect_lt(bias[1], bias[3])
})

test_that("dwell_mean reports a restricted mean and names its horizon", {
  sim <- simulate_lifecycles(n = 800, seed = 1)
  m <- dwell_mean(stage_dwell(sim, "released"), restrict_to = 100)
  expect_equal(m$horizon, 100)
  expect_gt(m$restricted, 0)
  expect_lt(m$restricted, 100)
  # A longer horizon can only add area under the curve.
  m2 <- dwell_mean(stage_dwell(sim, "released"), restrict_to = 400)
  expect_gte(m2$restricted, m$restricted)
})

test_that("the hazard of leaving released falls with elapsed time", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  h <- dwell_hazard(stage_dwell(sim, "released"), c(0, 2, 8, 24, 72, Inf))
  expect_true(all(diff(h$per_hour) < 0))
  # A falling hazard is the signature of the mixed population of Section 6.5.
  expect_gt(h$per_hour[1] / h$per_hour[nrow(h) - 1], 10)
})

test_that("exit types separate forward, backward and cancelled moves", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  spec <- distribution_stage_spec()

  rel <- stage_exit_type(sim, "released", spec)
  # A short pick sends an order back from released to backordered, which is a
  # backward move and must not be counted as progress.
  expect_true("backward" %in% rel$exit_type)
  expect_true(all(rel$exit_to[rel$exit_type == "backward"] == "backordered"))

  rec <- stage_exit_type(sim, "received", spec)
  expect_true("cancelled" %in% rec$exit_type)
  expect_equal(sum(rec$exit_type == "censored"), sum(rec$censored))
})

test_that("competing incidence is monotone and sums with the waiting share", {
  sim <- simulate_lifecycles(n = 2000, seed = 5)
  ci <- dwell_competing(stage_exit_type(sim, "received",
                                        distribution_stage_spec()))
  num <- ci[, setdiff(names(ci), "hours"), drop = FALSE]
  expect_true(all(abs(rowSums(num) - 1) < 1e-8))
  expect_true(all(diff(ci$forward) >= -1e-12))
  expect_true(all(diff(ci$still_waiting) <= 1e-12))
})

test_that("parametric families rank by AIC and report the hazard they imply", {
  sim <- simulate_lifecycles(n = 2000, seed = 5)
  p <- dwell_parametric(stage_dwell(sim, "released"))
  expect_equal(p$AIC, sort(p$AIC))
  # A heavy-tailed dwell beats the constant hazard of the exponential.
  expect_true(p$dist[1] != "exponential")
  expect_equal(p$hazard[p$dist == "weibull"], "falling")
  expect_lt(p$shape[p$dist == "weibull"], 1)
})

test_that("the mixture recovers the two populations it was generated from", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  fit <- dwell_mixture(stage_dwell(sim, "released"))

  # simulate_lifecycles() builds released dwell as 16% stuck, a fast
  # exponential with mean 1.9 h, and a stuck log-normal with median 26 h.
  expect_lt(abs(unname(fit$weight[2]) - 0.16), 0.05)
  expect_lt(abs(fit$params[[1]][1] - 1.9), 0.6)
  expect_lt(abs(exp(fit$params[[2]][1]) - 26), 8)

  # The pooled quantile sits between the two components, which is why it
  # serves neither (Section 6.5).
  comp <- fit$component_quantile(0.9)
  pooled <- fit$quantile(0.9)
  expect_gt(pooled, comp[1, 1])
  expect_lt(pooled, comp[2, 1])
})

test_that("the mixture uses censored visits rather than discarding them", {
  sim <- simulate_lifecycles(n = 3000, seed = 7)
  s <- observe_at(sim, sim$as_of - 100 * 24 * 3600)
  d <- stage_dwell(s, "awaiting_supply")
  expect_gt(mean(d$censored), 0.1)

  with_cens <- dwell_mixture(d, families = c("lnorm", "lnorm"))
  dropped <- dwell_mixture(d[!d$censored, ], families = c("lnorm", "lnorm"))
  # Dropping the open visits removes the longest waits, so the slow component
  # comes back shorter -- the bias of Section 6.2, inside the mixture fit.
  expect_lt(exp(dropped$params[[2]][1]), exp(with_cens$params[[2]][1]))
})

test_that("regression recovers the generator's covariate effects", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)

  rec <- merge(stage_dwell(sim, "received"),
               sim$orders[, c("order_no", "branch", "order_source")],
               by = "order_no")
  f <- dwell_regression(rec, c("branch", "order_source"))
  tr <- f$table$time_ratio[f$table$term == "branch60"]
  # Branch 30 is built to wait 1.55x as long as branch 60 in received.
  expect_lt(abs(1 / tr - 1.55), 0.25)
  expect_lt(f$table$cox_p[f$table$term == "branch60"], 1e-6)
})

test_that("the proportional-hazards test catches a time-multiplier effect", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  rel <- merge(stage_dwell(sim, "released"),
               sim$orders[, c("order_no", "lines")], by = "order_no")
  f <- dwell_regression(rel, "lines")

  # The generator multiplies released dwell by a function of `lines`, which is
  # an accelerated-failure-time mechanism and not a proportional-hazards one.
  # The test should therefore reject, and the AFT fit is the specified model.
  expect_lt(f$ph_test$table["lines", "p"], 0.01)
  expect_gt(f$table$time_ratio[f$table$term == "lines"], 1)
})

test_that("a backlog effect is absent, because the generator has no queue", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  rel <- stage_backlog(sim, "released")
  f <- dwell_regression(rel, "backlog")
  # Stated rather than hidden: Section 6.9 reports this null result as a null
  # result, and a test that silently expected an effect would be wrong.
  expect_gt(f$table$cox_p[f$table$term == "backlog"], 0.05)
  expect_true(all(rel$backlog >= 0))
})
