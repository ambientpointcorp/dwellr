test_that("stage_spec rejects a specification that is not self-consistent", {
  expect_error(stage_spec(stages = c("a", "b"), transitions = "a > b"),
               "from -> to")
  expect_error(stage_spec(stages = "a", transitions = "a -> b"),
               "not declared")
  expect_error(stage_spec(stages = c("a", "b"), transitions = "a -> b",
                          class = "queueing"),
               "must be named")
  spec <- stage_spec(c("a", "b"), "a -> b", terminal = "b",
                     class = c(a = "queueing"))
  expect_s3_class(spec, "stage_spec")
  expect_equal(unname(spec$class[["b"]]), "unclassified")
})

test_that("the book's specification validates against the book's data", {
  sim <- simulate_lifecycles(n = 800, seed = 2)
  v <- stage_validate(sim, distribution_stage_spec())
  expect_equal(sum(v$violations), 0)
})

test_that("stage_validate catches each of its three failures", {
  sim <- simulate_lifecycles(n = 400, seed = 2)
  spec <- distribution_stage_spec()

  # An undeclared stage.
  extra <- sim
  extra$events$stage[1] <- "awaiting_paperwork"
  v <- stage_validate(extra, spec)
  expect_equal(v$violations[v$check == "undeclared stage"], 1)

  # An illegal transition: dropping a release leaves received -> shipped.
  id <- sim$orders$order_no[sim$orders$stage == "shipped"][1]
  broken <- sim
  drop <- which(broken$events$order_no == id &
                broken$events$event == "pick_ticket_released")
  broken$events <- broken$events[-drop, ]
  v <- stage_validate(broken, spec)
  expect_gte(v$violations[v$check == "illegal transition"], 1)
  expect_match(v$detail[v$check == "illegal transition"], id)

  # An event recorded after the order had already shipped.
  reopened <- sim
  last <- sim$events[sim$events$order_no == id, ]
  add <- last[nrow(last), ]
  add$event <- "pick_ticket_released"; add$stage <- "released"
  add$occurred_at <- add$occurred_at + 3600
  reopened$events <- rbind(reopened$events, add)
  v <- stage_validate(reopened, spec)
  expect_gte(v$violations[v$check == "event after terminal"], 1)
})

test_that("the mean of lead time decomposes exactly", {
  sim <- simulate_lifecycles(n = 1200, seed = 7)
  dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
  total <- attr(dec, "total")
  # Section 5.4: expectation is linear, so the column is a decomposition.
  expect_equal(sum(dec$mean_hours), total[["mean"]], tolerance = 1e-8)
})

test_that("variance is attributed by covariance with the total, so shares sum to one", {
  sim <- simulate_lifecycles(n = 1200, seed = 7)
  dec <- lead_time_decompose(sim)
  expect_equal(sum(dec$var_share), 1, tolerance = 1e-8)
  expect_equal(sum(dec$cov_with_total), attr(dec, "total")[["var"]],
               tolerance = 1e-6)
  # Summing own variances instead -- the expression that assumes independence --
  # does not reproduce it, which is the point of using the covariance form.
  expect_false(isTRUE(all.equal(sum(dec$own_var),
                                attr(dec, "total")[["var"]],
                                tolerance = 1e-6)))
})

test_that("only completed lifecycles enter the lead-time total", {
  sim <- simulate_lifecycles(n = 1200, seed = 7)
  dec <- lead_time_decompose(sim, through = "shipped")
  expect_equal(attr(dec, "total")[["n"]],
               sum(sim$orders$stage == "shipped"))
  expect_error(lead_time_decompose(sim, through = "no_such_stage"),
               "no instance reached")
})

test_that("a repeat visit contributes its total wait, not its last one", {
  sim <- simulate_lifecycles(n = 2000, seed = 13)
  dec <- lead_time_decompose(sim)
  rel <- dec[dec$stage == "released", ]
  # Looped orders visit `released` twice, so visits exceed orders visiting.
  expect_gt(rel$visits, rel$visit_share * attr(dec, "total")[["n"]])
})

test_that("degenerate_stages distinguishes an empty stage from a terminal one", {
  sim <- simulate_lifecycles(n = 600, seed = 4)
  scan <- degenerate_stages(sim)

  # Terminal stages have no successor, so no dwell can be computed for them.
  term <- scan[scan$stage %in% c("shipped", "cancelled"), ]
  expect_true(all(term$n_measurable == 0))
  expect_true(all(term$verdict == "terminal: no successor"))
  expect_true(all(scan$verdict[scan$stage == "awaiting_supply"] == "ok"))

  # A print event logged one second before the release it accompanies is a
  # second name for that release, not a stage (Section 5.6).
  printed <- sim$events[sim$events$event == "pick_ticket_released", ]
  printed$event <- "pick_ticket_printed"
  printed$stage <- "printed"
  printed$occurred_at <- printed$occurred_at - 1
  cfg <- sim
  cfg$events <- rbind(sim$events, printed)
  cfg$events <- cfg$events[order(cfg$events$order_no, cfg$events$occurred_at), ]

  scan2 <- degenerate_stages(cfg)
  row <- scan2[scan2$stage == "printed", ]
  expect_equal(row$zero_share, 1)
  expect_equal(row$collapses_onto, "released")
  expect_equal(row$verdict, "degenerate: remove")

  # A tolerance finer than the source clock misses it, which is why the
  # argument exists and why it should be set to the timestamp resolution.
  fine <- degenerate_stages(cfg, tol_hours = 1 / 7200)
  expect_equal(fine$verdict[fine$stage == "printed"], "ok")
})
