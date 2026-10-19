# The eight results Chapter 1 prints. The chapter displays its own copy of this
# class for teaching; these tests are what make a divergence between the two
# visible.

N <- 20; d <- 5

test_that("Chapter 1's runs reproduce", {
  w <- Warehouse$new(100)
  for (t in 0:(N - 1)) w$ship(d, t)
  expect_equal(tail(w$history()$level, 1), 0)

  w <- Warehouse$new(50)
  for (t in 0:(N - 1)) { if (t == 10) w$receive(50, t); w$ship(d, t) }
  expect_equal(tail(w$history()$level, 1), 0)

  w <- Warehouse$new(50)
  for (t in 0:(N - 1)) { if (t == 10) w$receive(40, t); w$ship(d, t) }
  h <- w$history()
  issues <- h[h$event == "issue", ]
  expect_equal(issues$level[issues$period == 19], -5)
  expect_equal(issues$level[issues$period == 20], -10)
  expect_equal(issues$owed[issues$period == 20], 10)
  expect_equal(min(h$pickable), 0)
})

test_that("the lean policy holds nothing between periods", {
  w <- Warehouse$new(0)
  for (t in 0:(N - 1)) { w$receive(d, t); w$ship(d, t) }
  expect_equal(range(w$history()$level), c(0, 5))
})

test_that("a primed pipeline never goes negative and never exceeds L*d", {
  L <- 2
  w <- Warehouse$new(L * d)
  arrivals <- numeric(N + L + 1)
  for (t in 0:(N - 1)) {
    if (arrivals[t + 1] > 0) w$receive(arrivals[t + 1], t)
    if (t + L <= N - 1) { w$place(d, t); arrivals[t + L + 1] <- d }
    w$ship(d, t)
  }
  h <- w$history()
  expect_equal(range(h$level), c(0, L * d))

  # Position is constant at the same point in each cycle, while the level
  # cycles beneath it. This is the Section 1.2 argument, in the output.
  placed <- h[h$event == "order placed" & h$period %in% 5:9, ]
  expect_equal(length(unique(placed$position)), 1)
  expect_gt(length(unique(h$level[h$event == "issue"])), 1)
})

test_that("an unprimed pipeline sits in a stable deficit", {
  L <- 2
  w <- Warehouse$new(0)
  arrivals <- numeric(N + L + 1)
  for (t in 0:(N - 1)) {
    if (arrivals[t + 1] > 0) w$receive(arrivals[t + 1], t)
    if (t + L <= N - 1) arrivals[t + L + 1] <- d
    w$ship(d, t)
  }
  expect_equal(range(w$history()$level), c(-L * d, 0))
})

test_that("the Section 1.2 identities hold throughout a run", {
  w <- Warehouse$new(50)
  for (t in 0:(N - 1)) { if (t == 10) w$receive(40, t); w$ship(d, t) }
  h <- w$history()
  expect_equal(h$level, h$pickable - h$owed)          # I = I+ - I-
  expect_equal(h$position, h$level + h$on_order)      # IP = I + O
  expect_true(all(h$pickable == 0 | h$owed == 0))     # at most one non-zero
})
