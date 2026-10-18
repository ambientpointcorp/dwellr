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
