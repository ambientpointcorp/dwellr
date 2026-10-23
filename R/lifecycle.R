#' Declare a stage set as a state machine
#'
#' A stage set is not a list of names. It is a directed graph: which stages
#' exist, which moves between them are legitimate, which stages an instance can
#' stop in, and what kind of wait each stage represents. Writing it down is what
#' makes an event log checkable (§5.3).
#'
#' The `class` argument carries the taxonomy of §5.5 -- whether a stage holds
#' physical work, a queue, a batch, an approval, or nothing at all. It is
#' optional for the mechanics and essential for the diagnosis, because the
#' remedy for a long approval wait has nothing in common with the remedy for a
#' long transport leg.
#'
#' @param stages Character vector of stage names, in nominal order.
#' @param transitions Character vector of legitimate moves, each written
#'   `"from -> to"`. Any move not listed is a violation.
#' @param terminal Stages an instance may stop in.
#' @param fulfilling Stages in which the instance's work has been delivered,
#'   whole or in part. This is not the same property as `terminal` and neither
#'   implies the other: a partial shipment has delivered something and the
#'   instance is still running, while a cancellation is an ending in which
#'   nothing was delivered. Functions that ask whether an instance was
#'   satisfied read this; functions that ask whether it stopped read
#'   `terminal`. Stages that are terminal and not fulfilling are reported as
#'   `abandoned`.
#' @param class Optional named character vector mapping stage to dwell class.
#'   Unnamed stages are classed `"unclassified"`.
#'
#' @return An object of class `stage_spec`, carrying the declared `stages`,
#'   `transitions`, `terminal`, `fulfilling` and `class`, plus the derived
#'   `abandoned` set.
#'
#' @examples
#' spec <- stage_spec(
#'   stages = c("received", "released", "shipped"),
#'   transitions = c("received -> released", "released -> shipped"),
#'   terminal = "shipped",
#'   fulfilling = "shipped",
#'   class = c(received = "queueing", released = "handling"))
#' spec
#' @export
stage_spec <- function(stages, transitions, terminal = character(0),
                       fulfilling = character(0), class = NULL) {
  stopifnot(is.character(stages), length(stages) >= 1)
  parts <- strsplit(transitions, "\\s*->\\s*")
  if (any(lengths(parts) != 2))
    stop("each transition must be written \"from -> to\"", call. = FALSE)
  tr <- data.frame(from = vapply(parts, `[`, "", 1),
                   to   = vapply(parts, `[`, "", 2),
                   stringsAsFactors = FALSE)

  unlisted <- setdiff(unique(c(tr$from, tr$to, terminal, fulfilling)), stages)
  if (length(unlisted))
    stop("transitions, terminal or fulfilling states name stages not declared: ",
         paste(unlisted, collapse = ", "), call. = FALSE)

  cls <- stats::setNames(rep("unclassified", length(stages)), stages)
  if (!is.null(class)) {
    if (is.null(names(class))) stop("`class` must be named", call. = FALSE)
    cls[names(class)] <- unname(class)
  }

  structure(list(stages = stages, transitions = tr,
                 terminal = terminal, fulfilling = fulfilling,
                 abandoned = setdiff(terminal, fulfilling), class = cls),
            class = "stage_spec")
}

#' @export
print.stage_spec <- function(x, ...) {
  cat(sprintf(
    "<stage_spec> %d stages, %d legitimate transitions, %d terminal, %d fulfilling\n",
    length(x$stages), nrow(x$transitions), length(x$terminal),
    length(x$fulfilling)))
  for (s in x$stages) {
    out <- x$transitions$to[x$transitions$from == s]
    mark <- paste(c(if (s %in% x$terminal) "terminal",
                    if (s %in% x$fulfilling) "fulfilling"), collapse = ", ")
    cat(sprintf("  %-18s %-14s %s\n", s, paste0("[", x$class[[s]], "]"),
                if (nzchar(mark)) paste0("(", mark, ")")
                else paste("->", paste(out, collapse = ", "))))
  }
  invisible(x)
}

#' Check an event log against a stage specification
#'
#' Three failures matter and none of them is visible by reading the log. A
#' stage appears that the specification does not declare; a move is recorded
#' that the specification forbids; or an event is recorded for an instance that
#' had already reached a terminal state.
#'
#' All three are configuration errors rather than operational ones, and all
#' three corrupt dwell measurement silently: an undeclared stage is dropped
#' from summaries, an illegal move produces a dwell for a wait that did not
#' happen, and an event after a terminal state extends a lifecycle that was
#' over.
#'
#' Instances still in flight are not a violation. They end in a non-terminal
#' stage because they have not finished, which is the subject of Chapter 6.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()] or
#'   [as_lifecycle()], or a list with an `events` data frame.
#' @param spec A [stage_spec()].
#'
#' @return A data frame with one row per check: `check`, `violations`, and a
#'   `detail` string naming up to three instances.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 300, seed = 1)
#' spec <- distribution_stage_spec()
#' stage_validate(sim, spec)
#' @export
stage_validate <- function(sim, spec) {
  stopifnot(inherits(spec, "stage_spec"))
  ev <- sim$events
  ev <- ev[order(ev$order_no, ev$occurred_at), ]

  naming <- function(ids) {
    ids <- unique(ids)
    if (!length(ids)) return("")
    paste0(paste(utils::head(ids, 3), collapse = ", "),
           if (length(ids) > 3) sprintf(", and %d more", length(ids) - 3) else "")
  }

  bad_stage <- !ev$stage %in% spec$stages

  same <- c(ev$order_no[-1] == ev$order_no[-nrow(ev)], FALSE)
  from <- ev$stage[same]
  to   <- ev$stage[c(FALSE, same[-length(same)])]
  who  <- ev$order_no[same]
  legit <- paste(spec$transitions$from, spec$transitions$to)
  bad_move <- !paste(from, to) %in% legit

  after_terminal <- from %in% spec$terminal

  data.frame(
    check = c("undeclared stage", "illegal transition", "event after terminal"),
    violations = c(sum(bad_stage), sum(bad_move), sum(after_terminal)),
    detail = c(naming(ev$order_no[bad_stage]),
               naming(who[bad_move]),
               naming(who[after_terminal])),
    stringsAsFactors = FALSE
  )
}

#' The stage set used throughout the book
#'
#' The specification derived in §5.3 from the event log of
#' [simulate_lifecycles()], supplied as a function so that every chapter
#' validates against one definition rather than retyping it.
#'
#' @return A [stage_spec()].
#'
#' @examples
#' distribution_stage_spec()
#' @export
distribution_stage_spec <- function() {
  stage_spec(
    # Declared in order of PROGRESS, not in the order the stages happen to be
    # listed anywhere else. `stage_exit_type()` reads backwardness from this
    # order, so `backordered` has to sit behind `released`: a short pick is a
    # move back, and classing it as forward would confound the hazard that
    # §6.8 separates.
    stages = c("received", "awaiting_supply", "credit_hold", "backordered",
               "released", "partially_shipped", "shipped", "cancelled"),
    transitions = c(
      "received -> awaiting_supply",
      "received -> credit_hold",
      "received -> released",
      "received -> cancelled",
      "awaiting_supply -> credit_hold",
      "awaiting_supply -> released",
      "credit_hold -> released",
      "released -> backordered",
      "released -> partially_shipped",
      "released -> shipped",
      "backordered -> released",
      "partially_shipped -> shipped"),
    terminal = c("shipped", "cancelled"),
    # Delivery, not ending. `partially_shipped` has put goods on a truck and
    # is still running; `cancelled` is an ending that delivered nothing. The
    # two properties cross, which is why they are declared separately.
    fulfilling = c("partially_shipped", "shipped"),
    class = c(received          = "queueing",
              awaiting_supply   = "external supply",
              credit_hold       = "approval",
              released          = "handling",
              backordered       = "queueing",
              partially_shipped = "batching",
              shipped           = "terminal",
              cancelled         = "terminal"))
}

# Which stage specification applies to this call.
#
# An explicit `spec` argument wins; otherwise the one the population carries,
# which [simulate_lifecycles()] and [as_lifecycle()] both attach. There is no
# third branch, and that is the design rather than an omission.
#
# An earlier version fell back to [distribution_stage_spec()] when a bare list
# carried none. That default is right for exactly one corpus and silently
# wrong for every other: an operation whose work ends in `departed` would be
# measured against a stage set that has never heard of it, so every completed
# instance gains a long open visit and the stage where the work finishes reads
# as the worst in the operation. A wrong number with no warning is worse than
# a refusal, so this refuses.
spec_of <- function(sim, spec = NULL) {
  if (!is.null(spec)) {
    if (!inherits(spec, "stage_spec"))
      stop("`spec` must be a stage_spec(), not ", class(spec)[1], call. = FALSE)
    return(spec)
  }
  if (inherits(sim$spec, "stage_spec")) return(sim$spec)
  stop("no stage specification: the population carries none and none was ",
       "given. Build the population with as_lifecycle(), which requires one, ",
       "or pass spec = stage_spec(...) to this call. A stage set cannot be ",
       "inferred from an event log.", call. = FALSE)
}

#' Decompose lead time into stage dwells
#'
#' Lead time is the sum of the time an instance spends in each stage of its own
#' lifecycle. This function performs that sum per instance and then reports what
#' each stage contributes to the mean and to the variance of the total (§5.4).
#'
#' Two details of the arithmetic are worth stating, because the obvious version
#' of each is wrong.
#'
#' Only instances that reached `through` are included. A lifecycle still in
#' flight has an incomplete total, and averaging incomplete totals with complete
#' ones understates lead time by an amount that grows as the operation degrades.
#'
#' Variance is attributed by covariance with the total, not by a stage's own
#' variance. Stage dwells are not independent -- an order held for credit is
#' also an order that waited to be released -- so \eqn{\sum_k
#' \mathrm{Var}(D_k)} does not equal \eqn{\mathrm{Var}(L)}. The identity that
#' does hold is
#'
#' \deqn{\mathrm{Var}(L) = \sum_k \mathrm{Cov}(D_k, L),}
#'
#' which is exact whatever the dependence, and whose terms therefore sum to one
#' when expressed as shares. The `own_var` column is reported alongside so the
#' difference between the two is visible rather than assumed away.
#'
#' A stage not visited by an instance contributes a dwell of zero to that
#' instance, so `mean_hours` is an unconditional contribution to
#' \eqn{E[L]} and the column sums to the mean lead time. `mean_when_visited`
#' is the conditional figure, and the two differ by the visit share.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param through Stage an instance must have reached to be counted. Defaults
#'   to the stages the specification declares both `terminal` and `fulfilling`
#'   -- the endings in which the work was actually delivered, which is what a
#'   lead time is the length of.
#' @param spec Optional [stage_spec()]. Used to label each stage with its class
#'   and to supply `through`. Defaults to the one carried on `sim`.
#'
#' @return A data frame of class `lead_time_decomposition`, one row per stage,
#'   with the mean lead time and its standard deviation carried in the `total`
#'   attribute.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 1)
#' lead_time_decompose(sim, spec = distribution_stage_spec())
#' @export
lead_time_decompose <- function(sim, through = NULL, spec = NULL) {
  spec <- spec_of(sim, spec)
  if (is.null(through)) {
    through <- intersect(spec$terminal, spec$fulfilling)
    if (!length(through))
      stop("the specification declares no stage that is both terminal and ",
           "fulfilling, so `through` cannot be inferred -- name it",
           call. = FALSE)
  }
  d <- stage_dwell(sim, spec = spec)
  done <- sim$orders$order_no[sim$orders$stage %in% through]
  d <- d[d$order_no %in% done, ]
  if (!nrow(d))
    stop("no instance reached '", paste(through, collapse = "' or '"), "'",
         call. = FALSE)

  # One row per instance, one column per stage, summed over repeat visits so a
  # loop contributes its total wait rather than its last one.
  wide <- tapply(d$dwell_hours, list(d$order_no, d$stage), sum)
  wide[is.na(wide)] <- 0
  L <- rowSums(wide)
  n <- length(L)

  stages <- colnames(wide)
  res <- data.frame(
    stage             = stages,
    class             = unname(spec$class[stages]),
    visits            = as.integer(table(factor(d$stage, levels = stages))),
    visit_share       = vapply(stages, function(s) mean(wide[, s] > 0), 0),
    mean_hours        = vapply(stages, function(s) mean(wide[, s]), 0),
    mean_when_visited = vapply(stages, function(s) {
                          v <- wide[, s]; mean(v[v > 0]) }, 0),
    own_var           = vapply(stages, function(s) stats::var(wide[, s]), 0),
    cov_with_total    = vapply(stages, function(s)
                          stats::cov(wide[, s], L), 0),
    stringsAsFactors = FALSE)
  res$var_share <- res$cov_with_total / stats::var(L)
  res <- res[order(-res$mean_hours), ]
  rownames(res) <- NULL

  structure(res,
            class = c("lead_time_decomposition", "data.frame"),
            total = c(n = n, mean = mean(L), sd = stats::sd(L),
                      var = stats::var(L)),
            through = through)
}

#' @export
print.lead_time_decomposition <- function(x, digits = 2, ...) {
  tot <- attr(x, "total")
  if (!is.null(tot)) {
    cat(sprintf("<lead_time_decomposition> %d instances reaching '%s'\n",
                tot[["n"]], paste(attr(x, "through"), collapse = "' or '")))
    cat(sprintf("  lead time: mean %.2f h, sd %.2f h\n",
                tot[["mean"]], tot[["sd"]]))
  }
  y <- as.data.frame(x)
  num <- vapply(y, is.numeric, TRUE)
  y[num] <- lapply(y[num], round, digits)
  print(y, ...)
  invisible(x)
}

#' Find stages nothing dwells in
#'
#' A stage that every instance leaves at the instant it arrives is not a stage.
#' It is a flag, or a system event, or a configuration row that was never a wait
#' in the first place, and it will never produce a measurement worth acting on
#' (§5.6).
#'
#' The scan is a dwell distribution per stage with the share of visits ending
#' within `tol_hours`, plus the stage those instantaneous visits collapse onto.
#' The second column is what identifies the duplicate: a stage recorded at the
#' same instant as the one after it is a second name for that event.
#'
#' Visits with no following event are not measurable -- the instance is either
#' finished or still in flight -- so they are counted and excluded. A stage
#' whose visits are all of that kind is terminal rather than degenerate, which
#' the verdict distinguishes.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()], or a list with
#'   an `events` data frame.
#' @param tol_hours Dwell at or below which a visit counts as instantaneous.
#'   Defaults to one minute. Set it to the resolution of the source timestamps:
#'   a system recording whole minutes cannot distinguish a true zero from a
#'   30-second wait.
#'
#' @return A data frame, one row per stage, with `n`, `n_measurable`,
#'   `zero_share`, `median_hours`, `p90_hours`, `collapses_onto` and a
#'   `verdict`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 600, seed = 1)
#' degenerate_stages(sim)
#' @export
degenerate_stages <- function(sim, tol_hours = 1 / 60) {
  ev <- sim$events
  ev <- ev[order(ev$order_no, ev$occurred_at), ]
  nr <- nrow(ev)

  same <- c(ev$order_no[-1] == ev$order_no[-nr], FALSE)
  gap <- rep(NA_real_, nr)
  gap[same] <- as.numeric(difftime(ev$occurred_at[c(FALSE, same[-nr])],
                                   ev$occurred_at[same], units = "hours"))
  nextst <- rep(NA_character_, nr)
  nextst[same] <- ev$stage[c(FALSE, same[-nr])]

  stages <- sort(unique(ev$stage))
  modal <- function(z) {
    z <- z[!is.na(z)]
    if (!length(z)) NA_character_ else names(sort(table(z), decreasing = TRUE))[1]
  }

  out <- do.call(rbind, lapply(stages, function(s) {
    i <- ev$stage == s
    g <- gap[i]
    m <- !is.na(g)
    zero <- if (any(m)) mean(g[m] <= tol_hours) else NA_real_
    data.frame(
      stage = s,
      n = sum(i),
      n_measurable = sum(m),
      zero_share = zero,
      median_hours = if (any(m)) stats::median(g[m]) else NA_real_,
      p90_hours = if (any(m)) unname(stats::quantile(g[m], 0.9)) else NA_real_,
      collapses_onto = modal(nextst[i & !is.na(gap) & gap <= tol_hours]),
      stringsAsFactors = FALSE)
  }))

  out$verdict <- ifelse(
    out$n_measurable == 0, "terminal: no successor",
    ifelse(out$zero_share >= 0.995, "degenerate: remove",
           ifelse(out$zero_share >= 0.9, "near-degenerate: investigate", "ok")))
  rownames(out) <- NULL
  out
}
