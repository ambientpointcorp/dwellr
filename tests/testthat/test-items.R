test_that("items reconcile with the lines that use them", {
  sim <- simulate_lifecycles(n = 1500, seed = 11)
  it <- sim$items
  L <- sim$lines

  expect_gt(nrow(it), 0)
  expect_equal(anyDuplicated(it$item_no), 0L)
  expect_true(all(L$item_no %in% it$item_no))
  expect_true(all(it$item_class %in% c("A", "B", "C")))
  expect_true(all(it$stockout_days <= it$cycle_days))
  expect_true(all(it$unit_cost > 0, it$demand_rate > 0))
  expect_true(all(it$lead_time_sd < it$lead_time_mean))
})

test_that("stock position is a function of the item and the instant", {
  # Two lines hitting one item at one instant must show the same on-hand
  # quantity. Drawing the position per line would break that, and a reader
  # comparing two orders for the same item on the same morning would see two
  # different warehouses.
  sim <- simulate_lifecycles(n = 2000, seed = 7)
  L <- sim$lines
  entered <- sim$orders$entered_at[match(L$order_no, sim$orders$order_no)]
  key <- paste(L$item_no, as.numeric(entered))
  distinct <- tapply(L$on_hand_at_entry, key, function(x) length(unique(x)))
  expect_true(all(distinct == 1))

  # And on_order must be consistent at the same instant for the same reason.
  distinct_oo <- tapply(L$on_order_at_entry, key, function(x) length(unique(x)))
  expect_true(all(distinct_oo == 1))
})

test_that("the recorded position is informative about the shortfall", {
  sim <- simulate_lifecycles(n = 3000, seed = 11)
  L <- sim$lines
  o <- fill_rate(sim$orders)
  term <- L[L$order_no %in% o$order_no[o$fill_defined], ]
  short <- term$qty_shipped < term$qty_ordered

  expect_gt(sum(short), 20)
  # A line that shorted is far more likely to have been out of stock at entry.
  expect_gt(mean(term$on_hand_at_entry[short] == 0),
            3 * mean(term$on_hand_at_entry[!short] == 0))
  expect_lt(median(term$cover_ratio[short]), median(term$cover_ratio[!short]))

  # And informative without being a lookup: the share left inconsistent means
  # the obvious comparison cannot be perfect, which is what lets later
  # chapters illustrate a threshold rather than a rule.
  rule <- term$on_hand_at_entry < term$qty_ordered
  expect_gt(mean(rule == short), 0.8)
  expect_lt(mean(rule == short), 0.99)
})

test_that("position fields are internally coherent", {
  sim <- simulate_lifecycles(n = 1200, seed = 3)
  L <- sim$lines
  expect_true(all(L$on_hand_at_entry >= 0))
  expect_true(all(L$on_order_at_entry >= 0))
  expect_true(all(L$demand_30d >= 0))
  # A promised date exists exactly where a replenishment is on order.
  expect_equal(is.na(L$promised_in_days), L$on_order_at_entry == 0)
  expect_true(all(L$promised_in_days >= 0, na.rm = TRUE))
  expect_equal(L$cover_ratio, round(L$on_hand_at_entry / L$qty_ordered, 3))
})
