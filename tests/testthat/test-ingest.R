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
  # A bare list carries no specification, and there is no third branch to
  # fall back to. Refusing is the point: the fallback that used to live here
  # answered with the book's stage set and no warning.
  bare <- list(events = pop$events, as_of = pop$as_of)
  expect_error(stage_dwell(bare), "no stage specification")
  expect_equal(nrow(stage_dwell(bare, spec = freight_spec())), 7)

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

# Every exported function that takes a population is run against a vocabulary
# that is not the book's. The table is the test: a new `sim`-taking export
# with no row here fails the last expectation, so the coverage cannot quietly
# lapse the way it did when the generator was the only way in.
test_that("every population-taking export runs on a foreign vocabulary", {
  t0 <- as.POSIXct("2026-01-01 00:00", tz = "UTC")
  spec <- freight_spec()
  pop <- as_lifecycle(freight_log(t0), spec, as_of = t0 + 3600 * 12)

  # order_lines() reads quantities the log alone does not carry, so the
  # population is given the two it requires under names that are NOT the
  # generator's -- which exercises the column map at the same time.
  pop$orders$truck_lines <- c(3L, 2L, 4L)
  pop$orders$line_fill <- c(1, 0, 0.5)

  calls <- list(
    stage_dwell         = function() stage_dwell(pop),
    stage_validate      = function() stage_validate(pop, spec),
    stage_exit_type     = function() stage_exit_type(pop, "accumulating"),
    stage_backlog       = function() stage_backlog(pop, "accumulating"),
    observe_at          = function() observe_at(pop, t0 + 3600 * 8),
    lead_time_decompose = function() lead_time_decompose(pop),
    time_in_stage       = function() time_in_stage(pop, "accumulating"),
    revisit_count       = function() revisit_count(pop),
    lifecycle_variants  = function() lifecycle_variants(pop),
    stage_separation    = function() stage_separation(pop, spec),
    degenerate_stages   = function() degenerate_stages(pop),
    queue_reconcile     = function() {
      p <- pop; p$orders$ticket_open <- c(TRUE, FALSE, FALSE)
      queue_reconcile(p, open_flag = "ticket_open")
    },
    order_lines         = function()
      order_lines(pop, cols = c(lines = "truck_lines", fill = "line_fill"))
  )

  for (nm in names(calls)) {
    expect_no_error(calls[[nm]](), message = paste(nm, "on a foreign vocabulary"))
  }

  # The guard: if a new export starts taking a population, it needs a row.
  takes <- Filter(function(n) {
    f <- get(n, envir = asNamespace("dwellr"))
    is.function(f) && "sim" %in% names(formals(f))
  }, getNamespaceExports("dwellr"))
  expect_setequal(takes, names(calls))
})

test_that("a column map is how a population names things differently", {
  o <- data.frame(pct_units = c(1, 0.5), pct_lines = c(1, 0.5),
                  pct_value = c(1, 0.4))
  expect_error(fill_rate(o), "looked for column `fill_units`")

  f <- fill_rate(o, cols = c(units = "pct_units", lines = "pct_lines",
                             value = "pct_value"))
  # The binding dimension reports the quantity, not the column, so two
  # populations with different column names stay comparable.
  expect_equal(f$fill_binding, c(1, 0.4))
  expect_equal(f$fill_binding_dimension, c("units", "value"))

  expect_error(fill_rate(o, cols = c(unit = "pct_units")),
               "does not use: unit")
  expect_error(fill_rate(o, cols = "pct_units"), "must be a named")

  # The generator's own names remain the defaults, so nothing that worked
  # before needs a map.
  expect_equal(fill_rate(orders$orders)$fill_binding,
               fill_rate(orders$orders, cols = c(units = "fill_units"))$fill_binding)
  expect_equal(fill_dimensions(orders$lines)$units,
               fill_dimensions(orders$lines,
                               cols = c(ordered = "qty_ordered"))$units)
})

test_that("the shipped dataset is what its builder produces", {
  # data-raw/make_orders.R is one seeded call to the generator. When the
  # generator changes and the dataset is not rebuilt, the two drift and the
  # package ships data no longer reproducible from the script that documents
  # it -- which is exactly what had happened before Chapter 13's date fields
  # were added.
  #
  # expect_equal rather than expect_identical, because this is the one
  # comparison in the suite that spans two R installations: the .rda was
  # written by whichever R built it and the right-hand side is computed by
  # whichever R is running the check. Bit-equality is not guaranteed across
  # versions, and `fill_value` differed by one ulp -- 2.2e-16, against a
  # tolerance of 1.5e-8 -- which failed the check on every CI platform while
  # passing on the machine the dataset was built on. A real drift of the kind
  # this test exists to catch is orders of magnitude larger than the
  # tolerance. The same-session reproducibility check in test-simulate.R
  # stays identical(): two calls on one build must agree exactly.
  expect_equal(orders, simulate_lifecycles(n = 2000, seed = 42))
  expect_s3_class(orders$spec, "stage_spec")
})
