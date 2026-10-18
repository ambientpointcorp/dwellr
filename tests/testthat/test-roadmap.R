test_that("unimplemented methods error rather than returning something plausible", {
  # The failure mode this guards against: a reader calls a declared function,
  # gets a number, and believes it.
  for (f in list(safety_stock, fit_dwell_model, baseline_cohort,
                 route_action, evaluate_policy, policy_ss)) {
    expect_error(f(), "not implemented yet")
  }
})
