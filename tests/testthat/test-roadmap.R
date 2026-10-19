# Declared-but-unimplemented methods must error rather than return something
# plausible. The failure mode this guards against: a reader calls a function
# named in the reference, gets a number, and believes it.
#
# The list is DERIVED, not hand-kept. An earlier version named the stubs
# explicitly and broke the moment policy_ss() was implemented, which is the
# same defect Section 20.10 describes: a hand-maintained list silently falls
# out of step with what it is supposed to describe.

stub_names <- function() {
  exported <- ls(asNamespace("dwellr"))
  Filter(function(nm) {
    f <- get(nm, envir = asNamespace("dwellr"))
    is.function(f) && any(grepl("not_yet", deparse(body(f)), fixed = TRUE))
  }, exported)
}

test_that("every declared stub errors with the chapter it belongs to", {
  stubs <- stub_names()
  expect_gt(length(stubs), 0)   # if this fails, Part V is written

  for (nm in stubs) {
    f <- get(nm, envir = asNamespace("dwellr"))
    expect_error(f(), "not implemented yet", info = nm)
    expect_error(f(), "Chapter", info = nm)
  }
})

test_that("implemented methods are not stubs", {
  stubs <- stub_names()
  implemented <- c("eoq", "epq", "policy_ss", "policy_base_stock",
                   "policy_rs", "dls_wagner_whitin", "eoq_quantity_discount",
                   "simulate_lifecycles", "stage_dwell", "fill_rate")
  expect_length(intersect(stubs, implemented), 0)
})
