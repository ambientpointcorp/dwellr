# A lifecycle whose vocabulary is nothing like the book's, which is the whole
# point: every assertion here fails if a function reads a stage name out of a
# literal instead of out of the specification.
freight_spec <- function() {
  stage_spec(
    stages      = c("open", "accumulating", "tendered", "departed", "cancelled"),
    transitions = c("open -> accumulating", "accumulating -> tendered",
                    "tendered -> accumulating", "tendered -> departed",
                    "open -> cancelled", "accumulating -> cancelled"),
    terminal    = c("departed", "cancelled"),
    fulfilling  = "departed")
}

freight_log <- function(t0 = as.POSIXct("2026-01-01 00:00", tz = "UTC")) {
  data.frame(
    order_no    = c("L1", "L1", "L1", "L1", "L2", "L2", "L2", "L3", "L3"),
    stage       = c("open", "accumulating", "tendered", "departed",
                    "open", "accumulating", "cancelled",
                    "open", "accumulating"),
    occurred_at = t0 + 3600 * c(0, 2, 5, 9, 0, 3, 10, 1, 4),
    stringsAsFactors = FALSE)
}

test_that("as_lifecycle refuses what it cannot measure", {
  log <- freight_log()
  spec <- freight_spec()

  expect_error(as_lifecycle(log, spec = "departed"), "must be a stage_spec")
  expect_error(as_lifecycle(log[, c("order_no", "stage")], spec),
               "missing occurred_at")
  expect_error(as_lifecycle(log[0, ], spec), "no rows")

  chr <- log
  chr$occurred_at <- format(chr$occurred_at)
  expect_error(as_lifecycle(chr, spec), "must be POSIXct")

  na <- log
  na$occurred_at[2] <- NA
  expect_error(as_lifecycle(na, spec), "missing values")

  # An undeclared stage is the failure that is otherwise silent: it is neither
  # an ending nor a declared wait, so it measures as a wait that never ends.
  bad <- log
  bad$stage[3] <- "staged_loaded"
  expect_error(as_lifecycle(bad, spec), "does not declare")

  # An as_of before the log it claims to cut.
  expect_error(as_lifecycle(log, spec, as_of = min(log$occurred_at)),
               "precedes the last event")
})

test_that("as_lifecycle derives the population from the log, not from a status", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  pop <- as_lifecycle(freight_log(t0), freight_spec(), as_of = t0 + 3600 * 12)

  expect_s3_class(pop, "lifecycle_sim")
  expect_equal(nrow(pop$orders), 3)
  expect_equal(pop$orders$stage, c("accumulating", "departed", "cancelled")[c(2, 3, 1)])
  expect_equal(pop$orders$is_terminal, c(TRUE, TRUE, FALSE))
  expect_true(is.na(pop$orders$finished_at[3]))
  expect_identical(pop$spec, freight_spec())

  # A stale status column loses to the log, every time.
  claimed <- data.frame(order_no = c("L1", "L2", "L3"),
                        stage = "departed", region = c("NE", "SE", "NE"),
                        stringsAsFactors = FALSE)
  pop2 <- as_lifecycle(freight_log(t0), freight_spec(), orders = claimed,
                       as_of = t0 + 3600 * 12)
  expect_equal(pop2$orders$stage, c("departed", "cancelled", "accumulating"))
  expect_equal(pop2$orders$region, c("NE", "SE", "NE"))

  expect_warning(
    as_lifecycle(freight_log(t0), freight_spec(),
                 orders = rbind(claimed,
                                data.frame(order_no = "L9", stage = "open",
                                           region = "W",
                                           stringsAsFactors = FALSE)),
                 as_of = t0 + 3600 * 12),
    "no events")
})

test_that("a terminal stage is not measured as a wait, whatever it is called", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  pop <- as_lifecycle(freight_log(t0), freight_spec(), as_of = t0 + 3600 * 12)

  d <- stage_dwell(pop)
  expect_false("departed" %in% d$stage)
  expect_false("cancelled" %in% d$stage)
  expect_equal(nrow(d), 7)
  expect_equal(sum(d$censored), 1)

  # The regression this guards: measured against the book's own stage set,
  # `departed` is not terminal and is counted as a three-hour wait.
  wrong <- stage_dwell(pop, spec = distribution_stage_spec())
  expect_true("departed" %in% wrong$stage)
  expect_equal(nrow(wrong), 8)
})

test_that("exit type reads abandonment from the specification", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  pop <- as_lifecycle(freight_log(t0), freight_spec(), as_of = t0 + 3600 * 12)

  x <- stage_exit_type(pop, "accumulating")
  expect_equal(x$exit_type[x$order_no == "L1"], "forward")
  expect_equal(x$exit_type[x$order_no == "L2"], "cancelled")
  expect_equal(x$exit_type[x$order_no == "L3"], "censored")

  # Backwardness is read from the declared order of progress.
  back <- stage_exit_type(pop, "tendered")
  expect_equal(back$exit_type, "forward")
})

test_that("lead_time_decompose infers `through` from the specification", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  pop <- as_lifecycle(freight_log(t0), freight_spec(), as_of = t0 + 3600 * 12)

  ltd <- lead_time_decompose(pop)
  expect_equal(attr(ltd, "through"), "departed")
  expect_equal(attr(ltd, "total")[["n"]], 1)
  expect_equal(sum(ltd$mean_hours), 9)

  # A specification with no fulfilling ending cannot have one inferred, and
  # says so rather than picking one.
  nf <- stage_spec(c("a", "b"), "a -> b", terminal = "b")
  pop$spec <- nf
  expect_error(lead_time_decompose(pop), "cannot be inferred")
})

test_that("observe_at carries the lines, items and spec with the window", {
  sim <- simulate_lifecycles(n = 200, seed = 4)
  earlier <- observe_at(sim, sim$as_of - 30 * 24 * 3600)

  expect_s3_class(earlier$spec, "stage_spec")
  expect_identical(earlier$spec, sim$spec)
  expect_false(is.null(earlier$lines))
  expect_false(is.null(earlier$items))
  # A re-cut population still answers the questions the full one answers.
  expect_s3_class(fill_rate(earlier$orders), "data.frame")
})

test_that("the specification resolves in a stated order of precedence", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  pop <- as_lifecycle(freight_log(t0), freight_spec(), as_of = t0 + 3600 * 12)

  # Explicit argument beats the one carried on the object.
  expect_equal(nrow(stage_dwell(pop, spec = distribution_stage_spec())), 8)
  # Carried spec beats the package default.
  expect_equal(nrow(stage_dwell(pop)), 7)
  # A bare list with neither falls back, which is the documented back-compat
  # path and the reason as_lifecycle() requires a spec.
  bare <- list(events = pop$events, as_of = pop$as_of)
  expect_equal(nrow(stage_dwell(bare)), 8)

  expect_error(stage_dwell(pop, spec = "departed"), "must be a stage_spec")
})

test_that("stage_spec separates delivery from ending", {
  spec <- distribution_stage_spec()
  expect_equal(spec$fulfilling, c("partially_shipped", "shipped"))
  expect_equal(spec$abandoned, "cancelled")
  # partially_shipped delivers and does not end; cancelled ends and does not
  # deliver. Neither property implies the other.
  expect_false("partially_shipped" %in% spec$terminal)
  expect_false("cancelled" %in% spec$fulfilling)

  expect_error(stage_spec(c("a", "b"), "a -> b", fulfilling = "z"),
               "not declared")
})
