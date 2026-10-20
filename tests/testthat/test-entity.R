test_that("line expansion reconciles with the order record", {
  sim <- simulate_lifecycles(n = 1200, seed = 11)
  o <- sim$orders
  ln <- order_lines(sim)

  expect_equal(nrow(ln), sum(o$lines))
  expect_equal(as.integer(table(ln$order_no)[o$order_no]), as.integer(o$lines))
  # Line numbers run 1..lines within every order, with no gaps.
  expect_true(all(tapply(ln$line_no, ln$order_no, function(x)
    identical(sort(x), seq_along(x)))))
  # Value is split without loss.
  expect_equal(sum(ln$value_share), sum(o$value))

  # A line counts as shipped only where the order reached a shipping stage.
  # Reading fill_lines without the stage would credit an order sitting in
  # `received` with a complete shipment, which is the error the helper exists
  # to avoid.
  unshipped <- !o$stage %in% c("shipped", "partially_shipped")
  expect_false(any(ln$shipped[ln$order_no %in% o$order_no[unshipped]]))
  done <- o$order_no[o$stage == "shipped" & !o$closed_short]
  expect_true(all(ln$shipped[ln$order_no %in% done]))

  # The derived flag is a superset of what the disposition code marks, since
  # a recovered shortfall leaves no trace in a status (§10.8).
  expect_gt(sum(ln$shortfall), sum(ln$disposition == "cancelled_at_close"))
  expect_true(all(ln$disposition %in%
    c("shipped", "cancelled_at_close", "cancelled", "open")))
})

test_that("the entity test separates attachable from unattachable candidates", {
  sim <- simulate_lifecycles(n = 1200, seed = 11)
  o <- sim$orders
  e <- sim$events
  ln <- order_lines(sim)

  # The order header is the grain the log was written at, so every event
  # attaches to exactly one instance.
  hdr <- entity_check(e, o, "order_no")
  expect_equal(hdr$attached, nrow(e))
  expect_equal(hdr$ambiguous, 0L)
  expect_true(hdr$unique)

  # At line grain an order's event has to be assigned to each of its lines,
  # and the ambiguous count is exactly the events of multi-line orders.
  lg <- entity_check(e, ln, "order_no")
  expect_false(lg$unique)
  expect_equal(lg$ambiguous,
               sum(e$order_no %in% o$order_no[o$lines > 1]))
  expect_equal(lg$attached + lg$ambiguous, nrow(e))

  # The ticket fails both ways: orders that released twice make events
  # ambiguous, and orders that never reached release leave events unattached.
  tk <- e[e$event == "pick_ticket_released", ]
  tc <- entity_check(e, tk, "order_no")
  expect_false(tc$unique)
  expect_gt(tc$unattached, 0L)
  expect_gt(tc$ambiguous, 0L)

  # The obligation condition is separate from attachment. A flag true for
  # every instance answers the question for none of them, which is the state
  # a durable entity is in (§10.6).
  o$owes <- !o$is_terminal
  expect_true(entity_check(e, o, "order_no", owed = "owes")$knows_owed)
  o$always <- rep(TRUE, nrow(o))
  expect_false(entity_check(e, o, "order_no", owed = "always")$knows_owed)
})

test_that("an identifier audit recovers collisions and fragmentations", {
  # Two ids standing for two entities each, and one entity carrying two ids.
  a <- identifier_audit(id = c("A", "A", "B", "B", "C", "D"),
                        entity = c("1", "2", "3", "4", "5", "5"))
  expect_equal(a$collisions, 2)
  expect_equal(a$collision_records, 4)
  expect_equal(a$fragmentations, 1)
  expect_equal(a$fragmented_records, 2)

  # A clean one-to-one mapping reports neither failure.
  b <- identifier_audit(id = letters[1:5], entity = as.character(1:5))
  expect_equal(b$collisions, 0)
  expect_equal(b$fragmentations, 0)
  expect_equal(b$n_ids, b$n_entities)

  expect_equal(identifier_audit(c("A", NA), c("1", "2"))$missing, 1)
  expect_error(identifier_audit(c("A", "B"), "1"), "length")
})

test_that("a segmentation key is judged by reclassification", {
  sim <- simulate_lifecycles(n = 1500, seed = 11)
  d <- stage_dwell(sim, "released")
  seg <- sim$orders$order_source[match(d$order_no, sim$orders$order_no)]

  r <- segment_decision_shift(d, seg, cost_miss = 40, cost_look = 400)
  expect_equal(r$pooled,
               threshold_from_dwell_quantile(d, 40, 400)$hours)
  expect_equal(nrow(r$per_level), length(unique(seg)))
  expect_equal(sum(r$per_level$n), nrow(d))
  expect_true(r$share_changed >= 0 && r$share_changed <= 1)

  # A key with one level cannot reclassify anything, since its own threshold
  # is the pooled one.
  one <- segment_decision_shift(d, rep("all", nrow(d)), 40, 400)
  expect_equal(one$n_changed, 0L)
  expect_equal(one$per_level$hours, one$pooled)

  # Levels too small for a threshold of their own keep the pooled one and are
  # reported rather than silently dropped.
  thin <- ifelse(seq_len(nrow(d)) <= 5, "rare", "common")
  t2 <- segment_decision_shift(d, thin, 40, 400, min_n = 50)
  expect_equal(t2$pooled_levels, "rare")
  expect_true(is.na(t2$per_level$hours[t2$per_level$level == "rare"]))
})
