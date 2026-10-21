test_that("raise_only never lowers the rule's severity", {
  r <- c("none", "notify", "escalate")
  p <- decision_path(r, model = c("escalate", "none", "none"))
  expect_equal(p$decided, c("escalate", "notify", "escalate"))
  expect_false(any(p$lowered))
  expect_equal(p$source, c("model", "rule", "rule"))
})

test_that("replace takes the model's proposal, lowering included", {
  p <- decision_path(c("escalate", "none"), model = c("none", "escalate"),
                     policy = "replace")
  expect_equal(p$decided, c("none", "escalate"))
  expect_equal(p$lowered, c(TRUE, FALSE))
})

test_that("advise records the model without acting on it", {
  p <- decision_path(c("none", "notify"), model = c("escalate", "escalate"),
                     policy = "advise")
  expect_equal(p$decided, c("none", "notify"))
  expect_false(any(p$raised))
  expect_equal(p$model, c("escalate", "escalate"))
})

test_that("a missing model proposal needs a named reason", {
  expect_error(decision_path(c("none", "notify"), model = c("escalate", NA)),
               "no `reason`")
  expect_error(decision_path(c("none", "notify"), model = c("escalate", NA),
                             reason = "model was busy"),
               "fallback_reasons")
  p <- decision_path(c("none", "notify"), model = c("escalate", NA),
                     reason = "model_timeout")
  expect_equal(p$reason, c(NA, "model_timeout"))
  expect_equal(p$decided, c("escalate", "notify"))
  expect_equal(p$source, c("model", "rule"))
})

test_that("the rule decides every item when no model runs at all", {
  r <- c("none", "notify", "escalate")
  p <- decision_path(r, reason = "model_disabled")
  expect_equal(p$decided, r)
  expect_true(all(p$source == "rule"))
  expect_equal(unique(p$reason), "model_disabled")
})

test_that("a severity outside the scale is refused", {
  expect_error(decision_path("none", model = "critical"),
               "outside `levels`")
  expect_error(decision_path("critical"), "all\\(rule %in% levels\\)")
})

test_that("the audit reports reasons separately, not summed", {
  p <- decision_path(c("none", "none", "notify", "escalate"),
                     model = c("escalate", NA, NA, "none"),
                     reason = c(NA, "model_disabled", "model_unreachable", NA))
  a <- decision_audit(p)
  expect_equal(a$without_model, 0.5)
  expect_equal(nrow(a$reasons), 2)
  expect_equal(sort(a$reasons$reason),
               c("model_disabled", "model_unreachable"))
  expect_equal(a$raised, 0.25)
  expect_equal(a$lowered, 0)
})

test_that("the budget keeps the essential calls and sheds the rest", {
  b <- inference_budget(calls = 400, cost_per_call = 0.02, cap = 5,
                        essential = 120)
  expect_equal(b$affordable, 250)
  expect_equal(b$exhausted_after, 250)
  expect_equal(b$essential_covered, 1)
  expect_equal(b$share_metered, 0.625)

  tight <- inference_budget(calls = 400, cost_per_call = 0.02, cap = 1,
                            essential = 120)
  expect_equal(tight$affordable, 50)
  expect_lt(tight$essential_covered, 1)
})

test_that("a cap that covers everything binds nowhere", {
  b <- inference_budget(calls = 10, cost_per_call = 0.01, cap = 100)
  expect_equal(b$affordable, 10)
  expect_true(is.na(b$exhausted_after))
  expect_equal(b$share_metered, 1)
})
