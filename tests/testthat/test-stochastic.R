# Figures printed in Chapter 3.

d10 <- 0:40
p10 <- stats::dpois(d10, 10)

test_that("the Newsvendor matches Section 3.1 for normal demand", {
  Q <- nv_quantity(1, 5, function(p) stats::qnorm(p, 10, 2))
  expect_equal(round(Q, 4), 11.9348)
  expect_equal(round(nv_cost(Q, 1, 5, mean = 10, sd = 2), 4), 2.9982)

  # The loss-function identity the derivation rests on:
  #   E[d - Q]+ = E[Q - d]+ - (Q - E[d])
  for (q in c(6, 10, 14)) {
    expect_equal(nv_shortfall(q, 10, 2),
                 nv_leftover(q, 10, 2) - (q - 10), tolerance = 1e-12)
  }
})

test_that("the normal approximation and exact Poisson agree on Q, not cost", {
  dd <- 0:300
  pp <- stats::dpois(dd, 100)
  # Both give 110.
  expect_equal(stats::qpois(5 / 6, 100), 110)
  expect_equal(round(stats::qnorm(5 / 6, 100, 10)), 110)
  expect_equal(dd[which.min(vapply(dd, nv_cost, numeric(1), 1, 5,
                                   support = dd, probs = pp))], 110)
  # The costs differ by about 1.5%, which is the price of the approximation.
  exact  <- nv_cost(110, 1, 5, support = dd, probs = pp)
  approx <- nv_cost(110, 1, 5, mean = 100, sd = 10)
  expect_equal(round(exact, 4), 15.2253)
  expect_equal(round(approx, 4), 14.9989)
  expect_lt(abs(approx / exact - 1), 0.02)
})

test_that("alpha and beta are different targets (Section 3.2)", {
  Q <- stats::qnorm(5 / 6, 10, 2)
  sl <- nv_service_levels(Q, 10, 2)
  # The critical fractile IS the alpha attained.
  expect_equal(sl$alpha, 5 / 6, tolerance = 1e-9)
  # Beta is far higher at the same quantity.
  expect_equal(round(sl$beta, 4), 0.9823)
  expect_gt(sl$beta, sl$alpha)

  # And a beta target needs much less stock than the same alpha target.
  q_beta  <- nv_quantity_for_service(0.95, 10, 2, "beta")
  q_alpha <- nv_quantity_for_service(0.95, 10, 2, "alpha")
  expect_equal(round(q_beta, 4), 10.6897)
  expect_equal(round(q_alpha, 4), 13.2897)
  expect_lt(q_beta, q_alpha)
})

test_that("base-stock levels match the Newsvendor solution (Section 3.5)", {
  bs <- policy_base_stock(periods = 5, support = d10, probs = p10,
                          holding_cost = 1, shortage_cost = 5)
  expect_equal(bs$S, rep(13, 5))
  # Under stationary demand the SDP must agree with the single-period
  # critical fractile b/(h+b) = 5/6.
  expect_equal(bs$S[1], stats::qpois(5 / 6, 10))
})

test_that("the optimal (s, S) policy matches Section 3.7", {
  expect_equal(round(ss_cost(6, 40, d10, p10, 64, 1, 9), 4), 35.0216)
  best <- policy_ss(d10, p10, order_cost = 64, holding_cost = 1,
                    shortage_cost = 9, S_range = 20:50)
  expect_equal(c(best$s, best$S), c(6, 40))
  expect_equal(round(best$cost, 2), 35.02)
  # Any neighbouring policy is worse, which is what "optimal" means here.
  expect_gt(ss_cost(5, 40, d10, p10, 64, 1, 9), best$cost)
  expect_gt(ss_cost(7, 40, d10, p10, 64, 1, 9), best$cost)
  expect_gt(ss_cost(6, 39, d10, p10, 64, 1, 9), best$cost)
  expect_gt(ss_cost(6, 41, d10, p10, 64, 1, 9), best$cost)
})

test_that("(R, S) reduces to a one-period review when R = 1", {
  G <- sum(p10 * (1 * pmax(40 - d10, 0) + 9 * pmax(d10 - 40, 0)))
  expect_equal(policy_rs(1, 40, d10, p10, 64, 1, 9), 64 + G,
               tolerance = 1e-9)
  # Reviewing less often spreads K over more periods but leaves the system
  # uncovered for longer.
  costs <- vapply(1:4, policy_rs, numeric(1), S = 40, support = d10,
                  probs = p10, order_cost = 64, holding_cost = 1,
                  shortage_cost = 9)
  expect_true(all(is.finite(costs)))
  expect_lt(costs[2], costs[1])
})
