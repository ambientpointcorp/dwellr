test_that("reliability reports the gap a ranking metric cannot see", {
  set.seed(1)
  truth <- runif(2000) < 0.25
  # a monotone transform: same ranking, wrong scale
  p <- pmin(1, pmax(0, 0.25 + 0.5 * truth + rnorm(2000, 0, 0.15)))^0.4
  k <- calibrate_reliability(p, truth, bins = 5)
  expect_equal(nrow(k$reliability), 5)
  expect_equal(sum(k$reliability$n), 2000)
  expect_gt(k$mean_predicted, k$observed)      # inflated
  expect_gt(k$ece, 0.1)
})

test_that("Platt scaling cannot reorder, and repairs the level", {
  set.seed(4)
  n <- 5000
  truth <- runif(n) < 0.25
  raw <- plogis(-1.2 + 1.8 * truth + rnorm(n, 0, 1.1))
  inflated <- raw^0.35

  auc <- function(s, y) {
    r <- rank(s); n1 <- sum(y); n0 <- length(y) - n1
    (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  cal <- calibrate_probability(inflated, truth, method = "platt")
  rp <- predict(cal, inflated)

  expect_lt(abs(mean(rp) - mean(truth)), 0.01)
  # strictly increasing, so the ordering is untouched
  expect_equal(auc(rp, as.integer(truth)), auc(inflated, as.integer(truth)),
               tolerance = 1e-9)
  expect_equal(length(unique(rp)), length(unique(inflated)))
  expect_lt(cal$after$ece, cal$before$ece / 10)
})

test_that("isotonic repairs the level and ties predictions as it goes", {
  set.seed(4)
  n <- 5000
  truth <- runif(n) < 0.25
  inflated <- plogis(-1.2 + 1.8 * truth + rnorm(n, 0, 1.1))^0.35

  cal <- calibrate_probability(inflated, truth, method = "isotonic")
  rp <- predict(cal, inflated)

  expect_lt(abs(mean(rp) - mean(truth)), 0.01)
  # a step function is only weakly increasing: §9.14's qualification
  expect_lt(length(unique(rp)), length(unique(inflated)))
})

test_that("the map applies to predictions it was not fitted on", {
  set.seed(7)
  truth <- runif(3000) < 0.3
  p <- plogis(-1 + 1.5 * truth + rnorm(3000, 0, 1))^0.5
  fit_on <- seq_len(1500)
  cal <- calibrate_probability(p[fit_on], truth[fit_on], method = "platt")

  held <- predict(cal, p[-fit_on])
  expect_equal(length(held), 1500)
  expect_true(all(held >= 0 & held <= 1))
  expect_lt(abs(mean(held) - mean(truth[-fit_on])), 0.03)
})

test_that("a calibration set with one outcome is refused", {
  expect_error(calibrate_probability(c(0.2, 0.3, 0.4), c(0, 0, 0)),
               "needs both outcomes")
})

test_that("probabilities outside zero and one are refused", {
  expect_error(calibrate_probability(c(-0.1, 0.5), c(0, 1)),
               "p >= 0")
  cal <- calibrate_probability(c(0.1, 0.9, 0.4, 0.6), c(0, 1, 0, 1))
  expect_error(predict(cal, c(0.5, 1.4)), "newdata >= 0")
})

test_that("a prediction of exactly zero or one does not break Platt", {
  set.seed(2)
  truth <- runif(400) < 0.4
  p <- ifelse(truth, 1, 0)                    # separable and degenerate
  p[1:20] <- 0.5
  cal <- suppressWarnings(calibrate_probability(p, truth, method = "platt"))
  out <- predict(cal, c(0, 0.5, 1))
  expect_true(all(is.finite(out)))
  expect_true(all(out >= 0 & out <= 1))
})
