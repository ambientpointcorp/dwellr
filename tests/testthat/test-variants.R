test_that("variants enumerate the paths that occur and concentrate in a few", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  v <- lifecycle_variants(sim, min_share = 0)

  expect_equal(sum(v$instances), nrow(sim$orders))
  expect_equal(sum(v$share), 1, tolerance = 1e-10)
  expect_equal(v$instances, sort(v$instances, decreasing = TRUE))
  # Section 8.7: a handful of paths carry nearly all of the population, which
  # is the argument for enumerating them before writing a state machine.
  expect_gt(sum(v$share[1:5]), 0.85)
  expect_equal(v$path[1], "received > released > shipped")
  # A truncated path belongs to work in flight, and is reported as censored.
  expect_equal(v$censored[v$path == "received"], 1)
})

test_that("pooling small variants preserves the totals", {
  sim <- simulate_lifecycles(n = 2000, seed = 5)
  full <- lifecycle_variants(sim, min_share = 0)
  pooled <- lifecycle_variants(sim, min_share = 0.01)
  expect_lt(nrow(pooled), nrow(full))
  expect_equal(sum(pooled$instances), sum(full$instances))
  expect_match(pooled$path[nrow(pooled)], "^\\(other: \\d+ variants\\)$")
})

test_that("revisits are found only where the generator loops", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  r <- revisit_count(sim)
  # The only loop built into the corpus is a short pick sending an order back
  # from released to backordered and then to released again.
  expect_equal(unique(r$stage), "released")
  expect_true(all(r$visits == 2))
  expect_equal(nrow(r), sum(sim$orders$loops > 0 & sim$orders$stage != "backordered"))
})

test_that("time since entry understates the wait on a looped instance", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  t <- time_in_stage(sim, "released")

  single <- t[t$visits == 1, ]
  expect_equal(single$since_entry_hours, single$total_hours)
  expect_true(all(single$understated == 0))

  looped <- t[t$visits > 1, ]
  expect_gt(nrow(looped), 300)
  expect_true(all(looped$total_hours > looped$since_entry_hours))
  # Section 8.1: the figure an operational report shows loses most of the wait.
  expect_gt(median(looped$understated), 0.5)
})

test_that("a queue of open items is reconciled in both directions", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  q <- queue_reconcile(sim)

  expect_equal(q$owed, sum(!sim$orders$is_terminal))
  expect_gt(q$overstatement, 1)
  expect_gt(q$residue_share, 0.5)
  # The reverse error is reported too: an instance genuinely in flight that
  # carries no open item is missed by a queue built from items.
  expect_lte(q$missed, q$owed)
  expect_error(queue_reconcile(sim, "value"))
})

test_that("every declared stage separates from its neighbors", {
  sim <- simulate_lifecycles(n = 5000, seed = 11)
  s <- stage_separation(sim, distribution_stage_spec())
  expect_true(all(s$verdict == "separate"))
  expect_true(all(s$p < 0.01))
})

test_that("a split that holds the same wait is told to collapse", {
  set.seed(3)
  sim <- simulate_lifecycles(n = 4000, seed = 11)
  ev <- sim$events
  rel <- which(ev$stage == "released")
  half <- sample(rel, length(rel) %/% 2)
  ev$stage[half] <- "released_a"
  ev$stage[setdiff(rel, half)] <- "released_b"
  split_sim <- sim
  split_sim$events <- ev

  spec <- stage_spec(
    stages = c("released_a", "released_b", "shipped"),
    transitions = c("released_a -> released_b", "released_b -> released_a",
                    "released_a -> shipped", "released_b -> shipped"),
    terminal = "shipped")
  s <- stage_separation(split_sim, spec)
  # Splitting one stage at random produces two stages holding one wait, and
  # the verdict says so instead of accepting the split (Section 8.8).
  #
  # The assertion is on the verdict and not on p > 0.05 deliberately. With
  # thousands of visits on each side a log-rank test has the power to reject a
  # random split now and then -- this split lands at p = 0.019 -- which is why
  # stage_separation() calls it at 0.01 and why Section 8.8 reads the test with
  # the effect size beside it.
  expect_equal(s$verdict[1], "collapse")
  expect_gt(s$p[1], 0.01)
  expect_lt(abs(s$median_a[1] / s$median_b[1] - 1), 0.1)
})
