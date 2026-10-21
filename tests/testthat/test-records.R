test_that("a decision with no policy version is refused", {
  expect_error(decision_record("notify", "A", Sys.time(), NA_character_,
                               "rule"),
               "needs a policy version")
  expect_error(decision_record("notify", "A", Sys.time(), "", "rule"),
               "needs a policy version")
})

test_that("a model decision with no model version is refused", {
  expect_error(decision_record(c("notify", "escalate"), c("A", "B"),
                               Sys.time(), "v4", c("rule", "model")),
               "no model version recorded")
})

test_that("a score with no model version is refused", {
  expect_error(decision_record("notify", "A", Sys.time(), "v4", "rule",
                               score = 0.8),
               "score with no model version")
})

test_that("a reason outside the vocabulary is refused", {
  expect_error(decision_record("notify", "A", Sys.time(), "v4", "rule",
                               reason = "model was busy"),
               "fallback_reasons")
})

test_that("a complete record carries every provenance column", {
  r <- decision_record(c("notify", "escalate"), c("A", "B"),
                       rep(Sys.time(), 2), "v4", c("rule", "model"),
                       model_version = c(NA, "forest-1"),
                       score = c(NA, 0.81),
                       reason = c("model_disabled", NA))
  expect_equal(nrow(r), 2)
  expect_true(all(c("policy_version", "source", "model_version", "score",
                    "reason", "basis") %in% names(r)))
  expect_true(record_conformance(r)$ok)
})

test_that("conformance names the absent column rather than failing late", {
  r <- decision_record("notify", "A", Sys.time(), "v4", "rule")
  r$policy_version <- NULL
  k <- record_conformance(r)
  expect_false(k$ok)
  expect_true(any(grepl("policy_version", k$problems$detail)))
})

test_that("conformance catches a value outside the declared scale", {
  r <- decision_record("notify", "A", Sys.time(), "v4", "rule")
  r$decided <- "CRITICAL"
  k <- record_conformance(r)
  expect_false(k$ok)
  expect_true(any(k$problems$check == "value outside the declared scale"))
})

test_that("replay divergence is reported per item", {
  r <- decision_record(c("notify", "none", "escalate"), c("A", "B", "C"),
                       rep(Sys.time(), 3), "v4", "rule")
  k <- replay_check(r, c("notify", "escalate", "escalate"))
  expect_equal(k$agreement, 2 / 3)
  expect_equal(k$diverged$id, "B")
  expect_equal(k$diverged$replayed, "escalate")
})

test_that("a replay missing an item reports it rather than scoring it", {
  r <- decision_record(c("notify", "none"), c("A", "B"), rep(Sys.time(), 2),
                       "v4", "rule")
  k <- replay_check(r, data.frame(id = "A", decided = "notify",
                                  stringsAsFactors = FALSE))
  expect_equal(k$missing, "B")
  expect_equal(k$agreement, 0.5)
  expect_equal(nrow(k$diverged), 0)
})

test_that("the grammar states its own limits", {
  g <- rule_grammar()
  expect_true(all(c("in", ">=") %in% g$operators))
  expect_equal(g$connectives, c("and", "or"))
  expect_true("negation" %in% g$cannot_express$construct)
  expect_true(all(nzchar(g$cannot_express$workaround)))
})

test_that("a rule in the grammar compiles and applies", {
  r <- rule_compile("stage in credit_hold,backordered and value > 1000",
                    fields = c(stage = "character", value = "numeric"))
  expect_true(r$ok)
  expect_equal(nrow(r$terms), 2)
  expect_equal(r$connective, "and")
  d <- data.frame(stage = c("credit_hold", "credit_hold", "received"),
                  value = c(5000, 10, 9000), stringsAsFactors = FALSE)
  expect_equal(rule_apply(r, d), c(TRUE, FALSE, FALSE))
})

test_that("an unknown field is refused and the vocabulary is shown", {
  r <- rule_compile("order_value > 1000", fields = c(value = "numeric"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "unknown field: order_value")
  expect_match(paste(r$errors, collapse = " "), "available: value")
  expect_error(rule_apply(r, data.frame(value = 1)), "did not compile")
})

test_that("a construct the grammar lacks is named rather than ignored", {
  r <- rule_compile("not stage in shipped",
                    fields = c(stage = "character"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "no not")

  a <- rule_compile("due_date - 2 < 0", fields = c(due_date = "numeric"))
  expect_false(a$ok)
})

test_that("a numeric field compared to text is refused", {
  r <- rule_compile("value > soon", fields = c(value = "numeric"))
  expect_false(r$ok)
  expect_match(paste(r$errors, collapse = " "), "is numeric; value is not")
})

test_that("or is applied as a union", {
  r <- rule_compile("stage in credit_hold or value > 1000",
                    fields = c(stage = "character", value = "numeric"))
  expect_true(r$ok)
  expect_equal(r$connective, "or")
  d <- data.frame(stage = c("credit_hold", "received", "received"),
                  value = c(10, 5000, 10), stringsAsFactors = FALSE)
  expect_equal(rule_apply(r, d), c(TRUE, TRUE, FALSE))
})
