# Each test asserts a figure printed in the book, so a change that alters a
# published result fails the build rather than going unnoticed.

K <- 100; d <- 10; h <- 1    # the instance carried through Chapter 2

test_that("powers-of-two policy matches Section 2.6", {
  T_star <- eoq(d, K, h) / d
  expect_equal(round(T_star, 4), 4.4721)

  T_pow <- pow2_interval(T_star, base_interval = 0.7)
  expect_equal(T_pow, 5.6, tolerance = 1e-9)        # 0.7 * 2^3

  penalty <- eoq_cost_interval(T_pow, d, K, h) /
             eoq_cost_interval(T_star, d, K, h) - 1
  expect_equal(round(100 * penalty, 2), 2.54)

  # The worst-case bound, and its agreement with a factor-of-two cost error.
  expect_equal(round(100 * pow2_bound(), 2), 6.07)
  expect_equal(pow2_bound(), eoq_misestimate_penalty(2), tolerance = 1e-12)
})

test_that("all-units discounts put the optimum at a breakpoint (2.7)", {
  cand <- eoq_quantity_discount(d, K, h,
                                breakpoints = c(0, 60, 120),
                                prices = c(5, 4.5, 4))
  expect_equal(nrow(cand), 3)
  expect_equal(round(cand$cost, 2), c(94.72, 91.67, 108.33))

  best <- cand[which.min(cand$cost), ]
  expect_equal(best$Q, 60)              # a breakpoint, not any band's EOQ
  expect_equal(best$price, 4.5)
  expect_true(best$Q > eoq(d, K, h))    # ordering above the EOQ, deliberately
})

test_that("planned backorders match Section 2.8", {
  bp <- backorder_policy(d, K, h, shortage_cost = 5)
  expect_equal(round(bp$Q, 2), 48.99)
  expect_equal(bp$fraction, 1/6, tolerance = 1e-9)
  expect_equal(round(bp$cost, 2), 40.82)

  # Permitting backorders can only reduce cost.
  expect_lt(bp$cost, eoq_cost(d, K, h, eoq(d, K, h)))
  expect_equal(round(100 * (1 - bp$cost / eoq_cost(d, K, h, eoq(d, K, h))), 1),
               8.7)

  # As shortages become ruinous the policy approaches the no-shortage EOQ.
  far <- backorder_policy(d, K, h, shortage_cost = 1e6)
  expect_equal(far$Q, eoq(d, K, h), tolerance = 1e-3)
})

test_that("EPQ matches Section 2.9 and reduces to the EOQ", {
  ep <- epq(d, K, h, production_rate = 20)
  expect_equal(ep$rho, 0.5)
  expect_equal(round(ep$Q, 4), 63.2456)
  expect_equal(round(ep$cost, 4), 31.6228)

  # Finite production acts as a reduction in the holding cost.
  expect_equal(ep$Q, eoq(d, K, h * (1 - ep$rho)), tolerance = 1e-9)

  # As P -> Inf the EPQ recovers the EOQ exactly.
  fast <- epq(d, K, h, production_rate = 1e9)
  expect_equal(fast$Q, eoq(d, K, h), tolerance = 1e-3)
})

test_that("the ELSP common cycle is feasible on the Chapter 2 instance", {
  items <- data.frame(d = c(10, 5, 8), P = c(50, 40, 60),
                      K = c(100, 60, 80), h = c(1, 2, 1.5))
  r <- elsp_common_cycle(items)
  expect_equal(round(r$interval, 4), 4.2047)
  expect_lt(r$busy_fraction, 1)             # the feasibility check
  expect_equal(round(r$busy_fraction, 4), 0.4583)
  expect_equal(round(r$runs$run_qty, 2), c(42.05, 21.02, 33.64))
})

test_that("joint replenishment lets a slow mover ride along (2.11)", {
  r <- jrp(major_cost = 80, minor_costs = c(20, 15, 30),
           holding_costs = c(1, 2, 0.2), demand_rates = c(10, 5, 0.5))
  expect_equal(round(r$interval, 4), 3.3949)
  expect_equal(r$multiples, c(1, 1, 7))
  expect_equal(round(r$cost, 3), 70.274)

  # Cheaper than ordering each item independently, each bearing the major cost.
  independent <- sum(sqrt(2 * (80 + c(20, 15, 30)) *
                          c(1, 2, 0.2) * c(10, 5, 0.5)))
  expect_lt(r$cost, independent)
  expect_equal(round(100 * (1 - r$cost / independent), 1), 24.4)
})

test_that("Wagner-Whitin matches Section 2.12 and beats both naive policies", {
  demand <- c(20, 50, 10, 50, 30)
  ww <- dls_wagner_whitin(demand, order_cost = 100, holding_cost = 1)
  expect_equal(ww$cost, 300)
  expect_equal(ww$periods, c(1, 4))
  expect_equal(ww$quantities, c(80, 80))

  lot_for_lot  <- length(demand) * 100
  single_order <- 100 + sum((seq_along(demand) - 1) * demand)
  expect_equal(c(lot_for_lot, single_order), c(500, 440))
  expect_lt(ww$cost, min(lot_for_lot, single_order))

  # With no holding cost, ordering once is optimal; with none fixed, so is
  # ordering every period.
  expect_equal(dls_wagner_whitin(demand, 100, 0)$periods, 1)
  expect_equal(length(dls_wagner_whitin(demand, 1e-9, 1)$periods),
               length(demand))
})

test_that("the reorder point is the pipeline inventory (1.6, 2.5)", {
  expect_equal(reorder_point(demand_rate = 10, lead_time = 0.5), 5)
  # Section 1.6 derived L * d as pipeline inventory; 2.5 derives it as the
  # position at which to order. They are the same quantity.
  L <- 2
  expect_equal(reorder_point(10, L), L * 10)
})
