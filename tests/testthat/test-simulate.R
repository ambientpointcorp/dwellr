test_that("the generator is reproducible from its seed", {
  a <- simulate_lifecycles(n = 200, seed = 7)
  b <- simulate_lifecycles(n = 200, seed = 7)
  expect_identical(a$orders, b$orders)
  expect_identical(a$events, b$events)
})

test_that("the event log is well formed", {
  sim <- simulate_lifecycles(n = 300, seed = 1)
  expect_s3_class(sim, "lifecycle_sim")
  expect_true(all(c("order_no", "event", "stage", "occurred_at") %in% names(sim$events)))
  # Every order starts in `received` and nothing precedes its entry.
  first <- do.call(rbind, lapply(split(sim$events, sim$events$order_no),
                                 function(d) d[order(d$occurred_at), ][1, ]))
  expect_true(all(first$stage == "received"))
  expect_true(all(sim$events$occurred_at <= sim$as_of))
})

test_that("the pathologies the book measures are present", {
  sim <- simulate_lifecycles(n = 2000, seed = 42)
  o <- sim$orders
  expect_gt(sum(!o$is_terminal), 0)            # censoring
  expect_gt(sum(o$loops > 0), 0)               # released -> backordered loops
  expect_gt(sum(o$partially_shipped), 0)       # partial fills
  expect_gt(sum(o$closed_short), 0)            # orders that closed short
  expect_gt(sum(o$special_order), 0)           # vendor-sourced orders
  expect_gt(sum(o$open_ticket_residue), 0)     # residue on finished orders
})

test_that("censored visits are the slow ones, which is the Chapter 6 bias", {
  sim <- simulate_lifecycles(n = 2000, seed = 42)
  d <- stage_dwell(sim)
  open_med <- stats::median(d$dwell_hours[d$censored])
  done_med <- stats::median(d$dwell_hours[!d$censored])
  # Dropping still-open visits -- what a naive average over completed ones does
  # -- removes exactly the stuck population and runs optimistic.
  expect_gt(open_med, done_med)
})

test_that("the event log assembles the same way at every size", {
  # The assembly was rewritten from a list of one-row data frames to
  # preallocated vectors, because do.call(rbind, ...) over tens of thousands
  # of frames made the generator quadratic: n = 5000 took 27 seconds and every
  # chapter of the book regenerates that corpus. The draws are unchanged, so
  # this guards the property that made the rewrite safe -- the log is still
  # assembled in one order, sorted by instant then identifier, with no gaps.
  sim <- simulate_lifecycles(n = 1500, seed = 11)
  ev <- sim$events

  expect_false(any(is.na(ev$occurred_at)))
  expect_false(any(ev$order_no == ""))
  expect_equal(ev$occurred_at, sort(ev$occurred_at))
  expect_s3_class(ev$occurred_at, "POSIXct")
  expect_equal(attr(ev$occurred_at, "tzone"), "UTC")
  expect_equal(rownames(ev), as.character(seq_len(nrow(ev))))

  # Every order contributes at least its entry event, and no order contributes
  # more events than the longest declared path allows.
  per <- table(ev$order_no)
  expect_equal(length(per), nrow(sim$orders))
  expect_true(all(per >= 1))
  expect_lte(max(per), 8)
})

test_that("generation stays roughly linear in the number of orders", {
  skip_on_cran()
  t1 <- system.time(simulate_lifecycles(n = 1000, seed = 3))[["elapsed"]]
  t4 <- system.time(simulate_lifecycles(n = 4000, seed = 3))[["elapsed"]]
  # Quadratic assembly made this ratio about 16; linear-ish assembly keeps it
  # near 4. The bound is loose because timing on a shared machine is noisy.
  expect_lt(t4 / max(t1, 0.05), 9)
})
