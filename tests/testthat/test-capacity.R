test_that("queue stability turns on the utilization", {
  ok <- queue_stability(alerts_per_day = 3.8, reviews_per_day = 12)
  expect_true(ok$stable)
  expect_equal(ok$utilization, 3.8 / 12)
  expect_true(is.finite(ok$queue_length))

  # At or above one the backlog has no finite expectation, and reporting a
  # number there would be worse than reporting none (§15.1).
  bad <- queue_stability(alerts_per_day = 37.9, reviews_per_day = 12)
  expect_false(bad$stable)
  expect_true(is.infinite(bad$queue_length))
  expect_true(is.infinite(bad$wait_days))

  expect_equal(queue_stability(0, 10)$utilization, 0)
  expect_error(queue_stability(1, 0), "reviews_per_day")
})

test_that("admission takes the top of the ranking and reports what it cost", {
  set.seed(2)
  s <- runif(400)
  y <- runif(400) < s^2

  a <- capacity_admit(s, capacity = 40, outcome = y, value = rep(10, 400))
  expect_equal(sum(a$admitted), 40)
  expect_equal(a$cutoff, sort(s, decreasing = TRUE)[40])
  expect_true(all(s[a$admitted] >= a$cutoff))
  expect_equal(a$precision, mean(y[a$admitted]))
  expect_equal(a$recall, sum(y[a$admitted]) / sum(y))
  expect_equal(a$value_admitted, 400)

  # A capacity beyond the queue admits the queue and nothing more.
  all_in <- capacity_admit(s, capacity = 10000, outcome = y)
  expect_equal(sum(all_in$admitted), 400)
  expect_equal(all_in$recall, 1)
})

test_that("the marginal band is what a capacity decision turns on", {
  # A score that genuinely ranks puts the true cases first, so precision in a
  # trailing band falls as capacity grows while the cumulative figure lags.
  set.seed(3)
  s <- runif(1000)
  y <- runif(1000) < s
  a <- capacity_admit(s, capacity = 100, outcome = y, p_star = 0.3, band = 100)

  expect_equal(nrow(a$marginal), 10)
  expect_gt(a$marginal$band_precision[1], a$marginal$band_precision[10])
  expect_true(all(a$marginal$cumulative_precision <= 1))
  # The paying capacity is the last band whose own precision clears p*.
  expect_true(a$paying_capacity %in% c(0L, a$marginal$up_to))
  below <- a$marginal$band_precision < 0.3
  if (any(below) && which(below)[1] > 1)
    expect_equal(a$paying_capacity, a$marginal$up_to[which(below)[1] - 1])
})

test_that("a score that ranks nothing pays nothing beyond the base rate", {
  set.seed(4)
  s <- runif(600)
  y <- runif(600) < 0.1          # outcome independent of the score
  a <- capacity_admit(s, capacity = 60, outcome = y, p_star = 0.5, band = 100)
  expect_lt(abs(a$precision - 0.1), 0.12)
  # With p* above the base rate no band pays, so the paying capacity is zero.
  expect_equal(a$paying_capacity, 0L)
})
