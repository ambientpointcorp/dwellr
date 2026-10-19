# Figures printed in Chapter 4.

demand_series <- function() {
  set.seed(42)
  t <- 1:156
  round(pmax(0, 120 + 0.35 * t + 18 * sin(2 * pi * t / 52) +
               stats::rnorm(156, 0, 12)))
}

test_that("the benchmark forecasts are what they claim to be", {
  y <- c(10, 12, 11, 14, 13, 16)
  expect_equal(fc_mean(y, 3), rep(mean(y), 3))
  expect_equal(fc_naive(y, 3), rep(16, 3))
  # Drift draws a line through the first and last observations.
  slope <- (16 - 10) / 5
  expect_equal(fc_drift(y, 3), 16 + (1:3) * slope)
  # Seasonal naive repeats the previous season.
  expect_equal(fc_seasonal_naive(y, 4, period = 3), c(14, 13, 16, 14))
})

test_that("MASE is 1 for the naive forecast on its own training data", {
  y <- demand_series()
  # By construction: the scale is the in-sample naive MAE.
  e <- abs(diff(y, lag = 1))
  acc <- fc_accuracy(y[-length(y)], y[-1], y, scale_lag = 1)
  expect_equal(unname(acc["MASE"]), 1, tolerance = 1e-12)
  expect_equal(unname(acc["MAE"]), mean(e), tolerance = 1e-12)
})

test_that("the benchmark comparison reproduces Section 4.2", {
  y <- demand_series()
  train <- y[1:130]; test <- y[131:156]; h <- length(test)
  tab <- rbind(
    mean   = fc_accuracy(fc_mean(train, h),   test, train),
    naive  = fc_accuracy(fc_naive(train, h),  test, train),
    drift  = fc_accuracy(fc_drift(train, h),  test, train),
    snaive = fc_accuracy(fc_seasonal_naive(train, h, 52), test, train))

  expect_equal(round(tab[, "RMSE"], 3),
               c(mean = 16.666, naive = 14.831, drift = 17.128,
                 snaive = 21.465))
  # Naive wins here. Seasonal naive is worst, because the seasonal amplitude
  # is small relative to the noise; it is not universally the better method.
  expect_equal(names(which.min(tab[, "RMSE"])), "naive")
  expect_equal(names(which.max(tab[, "RMSE"])), "snaive")
  # The mean forecast is biased low on a trending series.
  expect_gt(tab["mean", "ME"], 0)
})

test_that("rolling-origin evaluation agrees with the single split here", {
  y <- demand_series()
  r_naive <- fc_rolling_origin(y, fc_naive, h = 1, n_origins = 40)
  r_mean  <- fc_rolling_origin(y, fc_mean,  h = 1, n_origins = 40)
  expect_lt(r_naive[["RMSE"]], r_mean[["RMSE"]])
  expect_equal(round(r_naive[["RMSE"]], 3), 17.022)
})

test_that("lead-time demand matches Section 4.11", {
  ltd <- lead_time_demand(demand_mean = 10, demand_sd = 3,
                          lead_time_mean = 10, lead_time_sd = 2)
  expect_equal(ltd$mean, 100)
  expect_equal(ltd$variance, 490)          # 10*9 + 100*4
  expect_equal(round(ltd$sd, 3), 22.136)
  expect_equal(round(ltd$lead_time_share, 3), 0.816)
  expect_equal(ltd$demand_share + ltd$lead_time_share, 1)

  # With a constant lead time the second term vanishes and the first is all
  # that remains, which is the Chapter 2 case.
  const <- lead_time_demand(10, 3, 10, 0)
  expect_equal(const$variance, 90)
  expect_equal(const$lead_time_share, 0)
})

test_that("safety stock responds far more to lead-time variability", {
  base <- lead_time_demand(10, 3, 10, 2)
  ss0 <- safety_stock(base, service_level = 0.95)
  expect_equal(round(ss0, 2), 36.41)

  half_d <- safety_stock(lead_time_demand(10, 1.5, 10, 2), service_level = 0.95)
  half_L <- safety_stock(lead_time_demand(10, 3, 10, 1),   service_level = 0.95)

  # Halving demand variability cuts safety stock by about 7%; halving
  # lead-time variability cuts it by about 38%.
  expect_equal(round(100 * (1 - half_d / ss0), 1), 7.1)
  expect_equal(round(100 * (1 - half_L / ss0), 1), 37.7)
  expect_gt(1 - half_L / ss0, 5 * (1 - half_d / ss0))

  # Costs and a service level are two routes to the same number.
  expect_equal(safety_stock(base, holding_cost = 1, shortage_cost = 19),
               safety_stock(base, service_level = 0.95), tolerance = 1e-9)
})
