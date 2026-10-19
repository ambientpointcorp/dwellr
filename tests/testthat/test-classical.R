# These reproduce worked numerical results printed in the book. A change that
# breaks one fails the build rather than going unnoticed -- which is the whole
# reason the test suite is tied to the text.

test_that("EOQ reproduces the book's instance (Chapter 2, Example 3)", {
  # The instance carried through Chapter 2: K = 100, h = 1, d = 10.
  # Q* = sqrt(2 * 100 * 10 / 1) = sqrt(2000) = 44.7214.
  #
  # An earlier version of this test cited Example 3 while using h = 2. The
  # arithmetic was correct and the citation was not, so the suite was not in
  # fact reproducing the figure printed in the book.
  q <- eoq(demand_rate = 10, order_cost = 100, holding_cost = 1)
  expect_equal(q, sqrt(2000), tolerance = 1e-9)
  expect_equal(round(q, 4), 44.7214)

  # Cr(Q*) = sqrt(2 K d h), which equals Q* here only because h = 1.
  expect_equal(eoq_cost(10, 100, 1, q), sqrt(2 * 100 * 10 * 1),
               tolerance = 1e-9)
  expect_equal(round(eoq_cost(10, 100, 1, q), 4), 44.7214)

  # The cycle length Q*/d, quoted in the text as 4.4721 periods.
  expect_equal(round(q / 10, 4), 4.4721)

  # At the optimum, ordering and holding cost are equal. This is the property
  # the derivation turns on, and it is the cheapest check on an implementation.
  expect_equal(100 * 10 / q, 1 * q / 2, tolerance = 1e-9)
  expect_equal(round(100 * 10 / q, 4), 22.3607)
})

test_that("the EOQ cost curve is flat near the optimum (Section 2.3)", {
  K <- 100; d <- 10; h <- 1
  q <- eoq(d, K, h)

  # The penalty depends only on Q/Q*, not on the instance:
  #   Cr(Q) / Cr(Q*) = (Q/Q* + Q*/Q) / 2
  penalty <- function(ratio) 0.5 * (ratio + 1 / ratio) - 1
  for (r in c(0.5, 0.8, 1.25, 2)) {
    expect_equal(eoq_cost(d, K, h, r * q) / eoq_cost(d, K, h, q) - 1,
                 penalty(r), tolerance = 1e-9)
  }

  # Figures printed in Section 2.3.
  expect_equal(round(100 * penalty(1.2), 2), 1.67)
  expect_equal(round(100 * penalty(0.8), 2), 2.50)
  expect_equal(round(100 * penalty(2.0), 2), 25.00)

  # The result that licenses rounding to a case or pallet quantity.
  expect_lt(eoq_cost(d, K, h, 50) / eoq_cost(d, K, h, q) - 1, 0.01)
})

test_that("a misestimated K costs little (Section 2.4)", {
  K <- 100; d <- 10; h <- 1
  q_true <- eoq(d, K, h)

  # Using f*K in place of K gives sqrt(f) * Q*, so the cost penalty is
  # (sqrt(f) + 1/sqrt(f)) / 2 - 1.
  mis <- function(f) 0.5 * (sqrt(f) + 1 / sqrt(f)) - 1
  for (f in c(0.6, 2, 4)) {
    q_hat <- eoq(d, f * K, h)
    expect_equal(q_hat / q_true, sqrt(f), tolerance = 1e-9)
    expect_equal(eoq_cost(d, K, h, q_hat) / eoq_cost(d, K, h, q_true) - 1,
                 mis(f), tolerance = 1e-9)
  }

  # Figures printed in Section 2.4: a 40% underestimate costs 3.28%, and a
  # factor-of-two error in either direction costs 6.07%.
  expect_equal(round(100 * mis(0.6), 2), 3.28)
  expect_equal(round(100 * mis(0.5), 2), 6.07)
  expect_equal(round(100 * mis(2.0), 2), 6.07)

  # h enters Q* symmetrically with K, so an error of factor f in h behaves as
  # an error of factor 1/f in K, and the penalty is unchanged.
  expect_equal(mis(2), mis(0.5), tolerance = 1e-12)
})

test_that("the newsvendor matches the book's lambda = 100 example", {
  expect_equal(nv_critical_fractile(overage = 1, underage = 5), 5 / 6)
  expect_equal(nv_quantity(1, 5, function(p) stats::qpois(p, lambda = 100)), 110)
})

test_that("costs must be positive", {
  expect_error(eoq(10, 0, 1))
  expect_error(nv_critical_fractile(0, 5))
})
