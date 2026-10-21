test_that("the three lists partition what changed", {
  cur <- c("none", "notify", "escalate", "notify", "none")
  pro <- c("notify", "notify", "notify", "none", "none")
  e <- evaluate_policy(cur, pro, value = c(100, 200, 9000, 50, 10))
  expect_equal(nrow(e$newly_surfacing), 1)
  expect_equal(nrow(e$ceasing), 1)
  expect_equal(nrow(e$urgency_changed), 1)
  expect_equal(e$volume[["current"]], 3)
  expect_equal(e$volume[["proposed"]], 3)
  expect_equal(e$volume[["change"]], 0)
  expect_equal(e$exposure[["ceasing"]], 50)
})

test_that("items ceasing to surface are ordered by exposure", {
  e <- evaluate_policy(current = rep("notify", 3), proposed = rep("none", 3),
                       id = c("a", "b", "c"), value = c(10, 9000, 500))
  expect_equal(e$ceasing$id, c("b", "c", "a"))
})

test_that("quality is reported for both policies when outcomes are known", {
  cur <- c("notify", "notify", "none", "none")
  pro <- c("notify", "none", "notify", "none")
  e <- evaluate_policy(cur, pro, outcome = c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(e$quality$policy, c("current", "proposed"))
  expect_equal(e$quality$precision, c(0.5, 1))
  expect_equal(e$quality$recall, c(0.5, 1))
})

test_that("a severity outside the scale is refused", {
  expect_error(evaluate_policy("none", "critical"), "proposed %in% levels")
})

test_that("a rule reading an absent field is unreachable", {
  rules <- data.frame(name = "high value", field = "order_value", op = ">",
                      value = "1000", stringsAsFactors = FALSE)
  r <- rule_reachable(rules, data.frame(value = c(2000, 50)))
  expect_false(r$reachable)
  expect_match(r$reason, "field absent")
})

test_that("a rule reading an unpopulated field is unreachable", {
  rules <- data.frame(name = "promised", field = "promised_date", op = "<",
                      value = "100", stringsAsFactors = FALSE)
  r <- rule_reachable(rules, data.frame(promised_date = rep(NA_real_, 23)))
  expect_false(r$reachable)
  expect_match(r$reason, "never populated")
  expect_equal(r$populated, 0L)
})

test_that("a rule keyed on a stage the list never had is unreachable", {
  rules <- data.frame(name = "aged hold", field = "stage", op = "in",
                      value = "credit_hold", stringsAsFactors = FALSE)
  r <- rule_reachable(rules, data.frame(stage = c("received", "released")))
  expect_false(r$reachable)
  expect_match(r$reason, "no row matches")
  expect_equal(r$match, 0L)
})

test_that("a rule that can fire is reported reachable with its match count", {
  rules <- data.frame(name = "aged hold", field = "stage", op = "in",
                      value = "credit_hold, backordered",
                      stringsAsFactors = FALSE)
  r <- rule_reachable(rules, data.frame(
    stage = c("credit_hold", "received", "backordered")))
  expect_true(r$reachable)
  expect_equal(r$match, 2L)
  expect_true(is.na(r$reason))
})

test_that("an unsupported operator is refused rather than guessed", {
  rules <- data.frame(name = "x", field = "v", op = "~=", value = "1",
                      stringsAsFactors = FALSE)
  expect_error(rule_reachable(rules, data.frame(v = 1)), "unsupported operator")
})

test_that("inverse propensity weighting recovers a known policy value", {
  set.seed(3)
  n <- 20000
  a <- ifelse(runif(n) < 0.5, "look", "pass")
  mu <- c(look = 1.2, pass = 0.4)
  r <- rnorm(n, mu[a], 0.5)
  v <- offpolicy_value(a, r, rep(0.5, n), rep("look", n),
                       target_propensity = rep(0.5, n))
  expect_lt(abs(v$ips - mu[["look"]]), 0.05)
  expect_equal(v$positivity, 1)
  expect_lt(abs(v$evidence - 0.5), 0.02)
})

test_that("the doubly robust estimate is no worse when the model is right", {
  set.seed(4)
  n <- 4000
  a <- ifelse(runif(n) < 0.3, "look", "pass")
  mu <- c(look = 1.2, pass = 0.4)
  r <- rnorm(n, mu[a], 1)
  qm <- cbind(look = rep(mu[["look"]], n), pass = rep(mu[["pass"]], n))
  v <- offpolicy_value(a, r, ifelse(a == "look", 0.3, 0.7),
                       rep("look", n), reward_model = qm)
  expect_lt(abs(v$dr - mu[["look"]]), 0.05)
  expect_lt(v$dr_se, v$ips_se)
})

test_that("a deterministic log reports the positivity failure it hides", {
  # The logged policy took what it took with certainty, so nothing in
  # `propensity` looks wrong. The items where the proposal differs carry no
  # weight, and `evidence` is what says so.
  logged <- c("look", "look", "pass", "pass")
  target <- c("look", "pass", "pass", "look")
  v <- offpolicy_value(logged, c(1, 1, 0, 0), propensity = rep(1, 4),
                       target = target,
                       target_propensity = as.numeric(logged == target))
  expect_equal(v$evidence, 0.5)
  expect_equal(v$positivity, 0.5)
})

test_that("evidence counts only the items the estimate rests on", {
  v <- offpolicy_value(c("look", "pass", "pass"), c(1, 0, 0),
                       propensity = c(1, 0, 0.5),
                       target = c("look", "pass", "pass"))
  expect_equal(v$evidence, 2 / 3)
  expect_true(is.na(v$positivity))
})
