# These reproduce worked numerical results printed in the book. A change that
# breaks one fails the build rather than going unnoticed -- which is the whole
# reason the test suite is tied to the text.

test_that("EOQ matches the book's Example 3", {
  # demand 10/period, K = 100, h = 2  ->  Q* = sqrt(2*100*10/2) = 31.6228
  q <- eoq(demand_rate = 10, order_cost = 100, holding_cost = 2)
  expect_equal(q, sqrt(1000), tolerance = 1e-9)
  # At the optimum, ordering and holding cost are equal -- the property the
  # derivation turns on.
  expect_equal(100 * 10 / q, 2 * q / 2, tolerance = 1e-9)
  expect_equal(eoq_cost(10, 100, 2, q), 2 * sqrt(1000), tolerance = 1e-9)
})

test_that("the EOQ cost curve is flat near the optimum (Section 2.3)", {
  q <- eoq(10, 100, 2)
  # 20% above the optimum costs under 2% in total cost. This is the result that
  # licenses rounding to a case or pallet quantity.
  penalty <- eoq_cost(10, 100, 2, 1.2 * q) / eoq_cost(10, 100, 2, q) - 1
  expect_lt(penalty, 0.02)
  expect_equal(penalty, (1 / 1.2 + 1.2) / 2 - 1, tolerance = 1e-9)
})

test_that("the newsvendor matches the book's lambda = 100 example", {
  expect_equal(nv_critical_fractile(overage = 1, underage = 5), 5 / 6)
  expect_equal(nv_quantity(1, 5, function(p) stats::qpois(p, lambda = 100)), 110)
})

test_that("costs must be positive", {
  expect_error(eoq(10, 0, 2))
  expect_error(nv_critical_fractile(0, 5))
})
