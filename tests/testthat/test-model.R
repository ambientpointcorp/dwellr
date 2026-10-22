make_frame <- function(n = 2000, seed = 11) {
  sim <- simulate_lifecycles(n = n, seed = seed)
  d <- stage_dwell(sim)
  total <- tapply(d$dwell_hours, d$order_no, sum)
  o <- sim$orders
  o$lead_hours <- as.numeric(total[o$order_no])
  o$log_value <- log(o$value)
  o
}

test_that("the protocol reports the metrics the decision consumes", {
  o <- make_frame()
  m <- fit_dwell_model(o, "lead_hours",
                       c("special_order", "log_value", "lines"),
                       at = "entered_at", horizon = 120, capacity = 100)
  expect_equal(m$split$kind, "chronological")
  expect_equal(m$metrics$capacity, 100)
  expect_true(all(c("auc", "precision_at_capacity", "ece", "brier") %in%
                  names(m$metrics)))
  expect_gt(m$metrics$auc, 0.5)
  # the point of precision at capacity: better than the base rate
  expect_gt(m$metrics$precision_at_capacity, m$metrics$base_rate)
})

test_that("a chronological split is used when a timestamp is given", {
  o <- make_frame()
  chrono <- fit_dwell_model(o, "lead_hours", c("special_order", "log_value"),
                            at = "entered_at", horizon = 120)
  random <- fit_dwell_model(o, "lead_hours", c("special_order", "log_value"),
                            horizon = 120)
  expect_equal(chrono$split$kind, "chronological")
  expect_equal(random$split$kind, "random")
  expect_false(is.na(chrono$split$boundary))
  expect_true(is.na(random$split$boundary))
})

test_that("the forest is available and needs ranger", {
  skip_if_not_installed("ranger")
  o <- make_frame()
  m <- fit_dwell_model(o, "lead_hours",
                       c("special_order", "log_value", "lines"),
                       at = "entered_at", horizon = 120, method = "forest")
  expect_equal(m$method, "forest")
  expect_equal(length(m$predictions), m$metrics$n_test)
  expect_true(all(m$predictions >= 0 & m$predictions <= 1))
})

test_that("an outcome taking one value is refused rather than fitted", {
  o <- make_frame()
  expect_error(fit_dwell_model(o, "lead_hours", "log_value",
                               horizon = 1e6), "takes one value")
})

test_that("a missing outcome is refused rather than dropped", {
  o <- make_frame()
  o$lead_hours[1:5] <- NA
  expect_error(fit_dwell_model(o, "lead_hours", "log_value", horizon = 120),
               "missing values")
})

test_that("a holdout too small to evaluate is refused", {
  o <- make_frame(n = 40)
  expect_error(fit_dwell_model(o, "lead_hours", "log_value", horizon = 120,
                               prop = 0.99), "lower `prop`")
})

test_that("the calibration summary travels with the fit", {
  o <- make_frame()
  m <- fit_dwell_model(o, "lead_hours", c("special_order", "log_value"),
                       at = "entered_at", horizon = 120)
  expect_equal(m$calibration$observed, m$metrics$base_rate, tolerance = 1e-3)
  expect_equal(nrow(m$calibration$reliability), 10)
})
