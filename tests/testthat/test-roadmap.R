# Nothing in the package is declared-but-unimplemented any more. Earlier
# versions carried stubs that errored with the chapter they belonged to, on the
# principle that a reader who calls a function named in the reference should be
# told it is not written rather than handed a plausible number.
#
# The check is kept, inverted: the invariant now is that no stub exists. It is
# DERIVED rather than hand-kept, which is the same discipline Section 20.10
# describes -- a hand-maintained list falls silently out of step with what it
# is supposed to describe.

stub_names <- function() {
  Filter(function(nm) {
    f <- get(nm, envir = asNamespace("dwellr"))
    is.function(f) && any(grepl("not_yet", deparse(body(f)), fixed = TRUE))
  }, ls(asNamespace("dwellr")))
}

test_that("no exported function is a stub", {
  expect_length(stub_names(), 0)
})

test_that("the families that were stubbed longest are callable", {
  # These were the last three to be implemented, and each is reachable with
  # arguments rather than erroring on principle.
  # Four rows with a clean separation make glm warn about fitted
  # probabilities at zero and one, which is true of the data rather than of
  # the function.
  expect_s3_class(
    suppressWarnings(calibrate_probability(c(0.1, 0.4, 0.6, 0.9),
                                           c(0, 0, 1, 1), method = "platt")),
    "dwellr_calibration")

  d <- deadline_derive(as.POSIXct("2026-03-10 17:00", tz = "UTC"),
                       handling_hours = 10)
  expect_true(d$defined)

  sim <- simulate_lifecycles(n = 400, seed = 11)
  dw <- stage_dwell(sim)
  o <- sim$orders
  o$lead_hours <- as.numeric(tapply(dw$dwell_hours, dw$order_no,
                                    sum)[o$order_no])
  m <- fit_dwell_model(o, "lead_hours", c("special_order", "lines"),
                       horizon = 120)
  expect_true(is.finite(m$metrics$auc))
})
