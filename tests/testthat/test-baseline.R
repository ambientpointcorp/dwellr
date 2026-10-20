test_that("cohort cells fall back rather than publishing a thin estimate", {
  sim <- simulate_lifecycles(n = 2500, seed = 11)
  d <- stage_dwell(sim, "released")
  o <- sim$orders[match(d$order_no, sim$orders$order_no), ]

  b <- baseline_cohort(d, o[, c("branch", "order_source")], p = 0.9,
                       min_n = 50)
  expect_equal(nrow(b$cells), 4)
  expect_equal(sum(b$cells$n), nrow(d))
  expect_true(all(b$cells$n[!b$cells$fell_back] >= 50))
  expect_true(all(b$cells$threshold[b$cells$fell_back] == b$pooled))

  # A minimum above every cell size collapses the scheme to the pooled one.
  all_back <- baseline_cohort(d, o[, "branch", drop = FALSE], p = 0.9,
                              min_n = nrow(d) + 1)
  expect_true(all(all_back$cells$fell_back))
  expect_true(all(all_back$assign(o[, "branch", drop = FALSE]) ==
                  all_back$pooled))

  # A cohort never seen at fit time gets the pooled threshold, not NA.
  novel <- data.frame(branch = "99", order_source = "fax",
                      stringsAsFactors = FALSE)
  expect_equal(b$assign(novel), b$pooled)
})

test_that("exceedance is reported per cohort, not only overall", {
  sim <- simulate_lifecycles(n = 4000, seed = 11)
  d <- stage_dwell(sim, "released")
  o <- sim$orders[match(d$order_no, sim$orders$order_no), ]
  set.seed(2)
  fit <- sample(c(TRUE, FALSE), nrow(d), replace = TRUE)

  b <- baseline_cohort(d[fit, ], o[fit, "branch", drop = FALSE], p = 0.9)
  e <- baseline_exceedance(d[!fit, ], o[!fit, "branch", drop = FALSE], b)

  expect_equal(e$target, 0.1)
  expect_equal(nrow(e$by_cohort), 2)
  expect_true(e$overall > 0 && e$overall < 1)
  expect_equal(e$worst_cohort_error, max(e$by_cohort$error))
  # An overall figure can hide a cohort; the worst cohort is at least as far
  # from target as the population is (§12.2).
  expect_gte(e$worst_cohort_error, e$error - 1e-9)

  # An open visit that has not yet passed its threshold is undecided and is
  # excluded rather than counted as compliant.
  expect_gte(e$excluded_undecided, 0)
})

test_that("a longer window is steadier", {
  sim <- simulate_lifecycles(n = 4000, seed = 11)
  d <- stage_dwell(sim, "released")
  at <- sim$orders$entered_at[match(d$order_no, sim$orders$order_no)]

  s <- baseline_stability(d, at, widths = c(7, 60), p = 0.9, step = 7)
  expect_equal(nrow(s), 2)
  expect_true(all(s$estimates > 1))
  expect_lt(s$cv[s$width_days == 60], s$cv[s$width_days == 7])
})

test_that("drift decomposition separates mix from process", {
  v <- c(stock = 40, specialty = 160)
  w <- c(stock = 0.9, specialty = 0.1)

  # Mix alone: every cohort unchanged, so the process component is zero.
  mix <- baseline_drift_decompose(v, v, w, c(stock = 0.8, specialty = 0.2))
  expect_equal(mix$process, 0)
  expect_equal(mix$mix, mix$change)
  expect_gt(mix$change, 0)

  # Process alone: the mix unchanged, so the mix component is zero.
  pr <- baseline_drift_decompose(v, v * 1.25, w, w)
  expect_equal(pr$mix, 0)
  expect_equal(pr$process, pr$change)

  # The two components always reconstruct the total.
  both <- baseline_drift_decompose(v, v * 1.25, w,
                                   c(stock = 0.8, specialty = 0.2))
  expect_equal(both$process + both$mix, both$change)
})

test_that("drift decomposition aligns by name", {
  # tapply returns its groups in sorted order, so a caller writing the weights
  # in the order the cohorts occur to them would otherwise pair each weight
  # with the wrong cohort. The first draft of Section 12.10 reported a mix
  # effect of +85 hours that way against a true +12.
  v <- c(specialty = 160, stock = 40)
  w <- c(specialty = 0.1, stock = 0.9)

  sorted <- baseline_drift_decompose(v, v, w, c(specialty = 0.2, stock = 0.8))
  jumbled <- baseline_drift_decompose(v, v, w, c(stock = 0.8, specialty = 0.2))
  expect_equal(sorted$mix, jumbled$mix)
  expect_equal(sorted$now, jumbled$now)

  # Names that do not match are refused rather than silently recycled.
  expect_error(baseline_drift_decompose(v, v, w, c(a = 0.2, b = 0.8)),
               "do not match")
})
