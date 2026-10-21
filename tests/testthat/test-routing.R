rules <- data.frame(
  key = c("received", "credit_hold", "awaiting_supply"),
  action = c("release the order", "clear the credit hold", "chase the vendor"),
  recipient = c("warehouse", "finance", "buyer"),
  urgency = c("notify", "escalate", "escalate"),
  stringsAsFactors = FALSE)

test_that("routing keys on the action and reports what it could not match", {
  r <- route_action(c("received", "credit_hold", "backordered"), rules)
  expect_equal(r$recipient[1:2], c("warehouse", "finance"))
  expect_equal(r$matched, c(TRUE, TRUE, FALSE))
  # An unmatched item carries no recipient, which is what makes the gap
  # visible instead of silent (§18.8).
  expect_true(is.na(r$recipient[3]))

  # A duplicate key is a table defect and is refused rather than resolved by
  # whichever row happened to come first.
  dup <- rbind(rules, rules[1, ])
  expect_error(route_action("received", dup), "duplicate")
})

test_that("a default routes the unmatched without concealing them", {
  d <- data.frame(action = "review manually", recipient = "supervisor",
                  urgency = "notify", stringsAsFactors = FALSE)
  r <- route_action(c("received", "backordered"), rules, default = d)
  expect_equal(r$recipient, c("warehouse", "supervisor"))
  # Still FALSE: the row was routed, and it was routed because nothing matched.
  expect_equal(r$matched, c(TRUE, FALSE))
})

test_that("completeness is tested against the live population", {
  keys <- c("received", "received", "backordered", "partially_shipped")
  val  <- c(100, 200, 9000, 50)
  g <- routing_gaps(keys, rules, value = val)

  expect_equal(g$covered, 0.5)
  expect_equal(nrow(g$gaps), 2)
  # Ordered by exposure, so the costly gap is the first thing read.
  expect_equal(g$gaps$key[1], "backordered")
  expect_equal(g$gaps$exposure[1], 9000)
  expect_equal(g$gaps$max_exposure[1], 9000)
  # A rule nobody's population exercises is also worth knowing about.
  expect_setequal(g$unused, c("credit_hold", "awaiting_supply"))
})

test_that("a complete table reports no gaps", {
  g <- routing_gaps(c("received", "credit_hold"), rules)
  expect_equal(g$covered, 1)
  expect_equal(nrow(g$gaps), 0)
  expect_equal(g$unused, "awaiting_supply")
  expect_true(all(route_action(c("received", "credit_hold"), rules)$matched))
})

test_that("routing covers the simulated open queue and finds its gap", {
  sim <- simulate_lifecycles(n = 3000, seed = 11)
  open <- sim$orders[!sim$orders$is_terminal, ]
  full <- data.frame(
    key = c("received", "released", "awaiting_supply", "backordered",
            "credit_hold"),
    action = c("release", "chase pick", "chase vendor", "chase stock",
               "clear credit"),
    recipient = c("warehouse", "warehouse", "buyer", "buyer", "finance"),
    urgency = "notify", stringsAsFactors = FALSE)

  g <- routing_gaps(open$stage, full, value = open$value)
  expect_lt(g$covered, 1)              # partially_shipped has no rule
  expect_true("partially_shipped" %in% g$gaps$key)
  expect_equal(sum(g$gaps$items), sum(!open$stage %in% full$key))
})
