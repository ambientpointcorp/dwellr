test_that("the horizon precedes the commitment by the offsets", {
  p <- as.POSIXct("2026-03-10 17:00", tz = "UTC")
  d <- deadline_derive(p, handling_hours = 15.5, cutoff_hours = 4)
  expect_equal(d$offset_hours, 19.5)
  expect_equal(as.numeric(difftime(p, d$horizon, units = "hours")), 19.5)
  expect_true(d$defined)
})

test_that("a missing commitment yields no deadline rather than a guess", {
  p <- as.POSIXct(c("2026-03-10 17:00", NA), tz = "UTC")
  d <- deadline_derive(p, handling_hours = 10)
  expect_equal(d$defined, c(TRUE, FALSE))
  expect_true(is.na(d$horizon[2]))
})

test_that("handling time may differ per instance", {
  p <- as.POSIXct(rep("2026-03-10 17:00", 3), tz = "UTC")
  d <- deadline_derive(p, handling_hours = c(4, 10, 40))
  expect_equal(d$offset_hours, c(4, 10, 40))
  expect_true(all(diff(as.numeric(d$horizon)) < 0))
})

test_that("the two breach flags differ, which is the point of deriving one", {
  p <- as.POSIXct("2026-03-09 12:00", tz = "UTC")
  d <- deadline_derive(p, handling_hours = 15.5, cutoff_hours = 4,
                       as_of = as.POSIXct("2026-03-09 08:00", tz = "UTC"))
  expect_true(d$past_horizon)          # too late to act
  expect_false(d$past_commitment)      # the promise has not yet been missed
  expect_lt(d$hours_to_horizon, 0)
})

test_that("a promise that was never achievable is flagged", {
  d <- deadline_derive(as.POSIXct("2026-03-08 19:00", tz = "UTC"),
                       handling_hours = 15.5, cutoff_hours = 4,
                       as_of = as.POSIXct("2026-03-08 12:00", tz = "UTC"),
                       entered_at = as.POSIXct("2026-03-08 09:00", tz = "UTC"))
  expect_true(d$infeasible)
})

test_that("a negative offset is refused", {
  p <- as.POSIXct("2026-03-10 17:00", tz = "UTC")
  expect_error(deadline_derive(p, handling_hours = -1), "cannot be negative")
  expect_error(deadline_derive(p, cutoff_hours = -4), "cannot be negative")
})

test_that("a commitment that is not a time is refused", {
  expect_error(deadline_derive("2026-03-10"), "must be a date or a time")
})

test_that("an offset of the wrong length is refused", {
  p <- as.POSIXct(rep("2026-03-10 17:00", 3), tz = "UTC")
  expect_error(deadline_derive(p, handling_hours = c(1, 2)),
               "must be length 1 or 3")
})
