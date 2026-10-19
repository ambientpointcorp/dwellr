#' Distinct paths through the lifecycle, with their frequencies
#'
#' A stage specification declares which paths are legitimate. It does not say
#' which ones occur, or how often, and those are the facts that decide where
#' measurement effort belongs. Enumerating the paths before writing a state
#' machine is what keeps the machine descriptive of the operation it is
#' supposed to describe (§8.7).
#'
#' The enumeration is the simplest form of process mining: group the event log
#' by instance, reduce each to its ordered sequence of stages, and count. No
#' model is fitted and nothing is assumed, which is the point at this stage of
#' an engagement.
#'
#' Instances still in flight produce truncated paths, and they are reported
#' with a `censored` share so that a short path can be recognized as unfinished
#' work instead of as a variant of its own.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param min_share Variants below this share of instances are pooled into a
#'   single `"(other)"` row. Set to 0 to list every one.
#'
#' @return A data frame with one row per variant: the `path`, how many
#'   `instances` took it, its `share`, the number of `stages` in it, the
#'   `median_hours` those instances took end to end, and the `censored` share.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' head(lifecycle_variants(sim), 5)
#' @export
lifecycle_variants <- function(sim, min_share = 0.005) {
  ev <- sim$events[order(sim$events$order_no, sim$events$occurred_at), ]
  path <- tapply(ev$stage, ev$order_no, function(s) paste(s, collapse = " > "))
  n_stages <- lengths(tapply(ev$stage, ev$order_no, identity))

  d <- stage_dwell(sim)
  total <- tapply(d$dwell_hours, d$order_no, sum)
  term <- stats::setNames(sim$orders$is_terminal, sim$orders$order_no)

  ids <- names(path)
  df <- data.frame(id = ids, path = unname(path),
                   stages = unname(n_stages[ids]),
                   hours = unname(total[ids]),
                   terminal = unname(term[ids]),
                   stringsAsFactors = FALSE)

  agg <- do.call(rbind, lapply(split(df, df$path), function(g) data.frame(
    path = g$path[1], instances = nrow(g),
    stages = g$stages[1],
    median_hours = stats::median(g$hours, na.rm = TRUE),
    censored = mean(!g$terminal),
    stringsAsFactors = FALSE)))
  agg$share <- agg$instances / nrow(df)
  agg <- agg[order(-agg$instances), c("path", "instances", "share", "stages",
                                      "median_hours", "censored")]

  if (min_share > 0 && any(agg$share < min_share)) {
    small <- agg$share < min_share
    pooled <- data.frame(
      path = sprintf("(other: %d variants)", sum(small)),
      instances = sum(agg$instances[small]),
      share = sum(agg$share[small]),
      stages = NA_integer_,
      median_hours = stats::median(df$hours[df$path %in% agg$path[small]],
                                   na.rm = TRUE),
      censored = NA_real_, stringsAsFactors = FALSE)
    agg <- rbind(agg[!small, ], pooled)
  }
  rownames(agg) <- NULL
  agg
}

#' Count revisits to a stage
#'
#' A lifecycle that returns to a stage it has already left is a loop, and a
#' loop is the case most reporting is not built for. Counting revisits is the
#' first step, and §8.6 uses the count as a severity signal in its
#' own right.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param stage Optional stage to restrict to.
#'
#' @return A data frame with one row per instance and stage that was visited
#'   more than once: `order_no`, `stage`, and `visits`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' table(revisit_count(sim)$stage)
#' @export
revisit_count <- function(sim, stage = NULL) {
  d <- stage_dwell(sim, stage)
  tab <- as.data.frame(table(order_no = d$order_no, stage = d$stage),
                       stringsAsFactors = FALSE)
  tab <- tab[tab$Freq > 1, ]
  names(tab)[names(tab) == "Freq"] <- "visits"
  tab <- tab[order(-tab$visits, tab$order_no), ]
  rownames(tab) <- NULL
  tab
}

#' Time in a stage, counted two ways
#'
#' An operational report almost always shows time since the instance entered
#' its current stage, because that is the quantity a status field supports.
#' Where an instance has looped, that figure restarts on re-entry and the wait
#' already endured disappears from it (§8.1).
#'
#' Both quantities are returned so the gap between them can be measured. The
#' one to alert on is `total_hours`, since an order on its second pass has
#' already consumed the patience the first pass was given.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param stage Stage to measure.
#'
#' @return A data frame with one row per instance that visited the stage:
#'   `order_no`, `visits`, `since_entry_hours` for the most recent visit,
#'   `total_hours` across all visits, and the `understated` share.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' t <- time_in_stage(sim, "released")
#' summary(t$understated[t$visits > 1])
#' @export
time_in_stage <- function(sim, stage) {
  d <- stage_dwell(sim, stage)
  d <- d[order(d$order_no, d$entered_at), ]
  g <- split(d$dwell_hours, d$order_no)
  out <- data.frame(
    order_no = names(g),
    visits = lengths(g),
    since_entry_hours = vapply(g, function(x) x[length(x)], 0),
    total_hours = vapply(g, sum, 0),
    stringsAsFactors = FALSE)
  out$understated <- 1 - out$since_entry_hours / out$total_hours
  rownames(out) <- NULL
  out
}

#' Reconcile a work queue against the work actually owed
#'
#' A queue built by counting open work items counts two things that are not
#' work: items belonging to instances that have already finished, and items
#' recorded against instances that were cancelled. Both are residue, and
#' neither can be acted on (§8.3).
#'
#' The reconciliation is a two-way comparison, because the reverse error exists
#' as well. An instance can be genuinely in flight and carry no open item,
#' which a queue built from items will miss entirely.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param open_flag Name of a logical column on `sim$orders` that marks an
#'   instance as holding an open work item in the source system.
#'
#' @return A list with the `flagged` count, the `residue` among them, the
#'   `owed` work, the `missed` instances in flight with no open item, and the
#'   `overstatement` factor of the naive count.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' unlist(queue_reconcile(sim))
#' @export
queue_reconcile <- function(sim, open_flag = "open_ticket_residue") {
  o <- sim$orders
  stopifnot(open_flag %in% names(o), is.logical(o[[open_flag]]))
  flag <- o[[open_flag]]
  in_flight <- !o$is_terminal

  flagged <- sum(flag | in_flight)      # what counting open items would return
  residue <- sum(flag & o$is_terminal)
  owed <- sum(in_flight)
  missed <- sum(in_flight & !flag)

  list(flagged = flagged, residue = residue, owed = owed, missed = missed,
       residue_share = residue / flagged,
       overstatement = flagged / max(owed, 1))
}

#' Do two stages hold distinguishable dwells?
#'
#' Section 5.8 argued that a stage earns its place in a specification
#' only if it has a dwell distribution of its own, and that criterion is
#' testable. Two adjacent stages whose dwells cannot be told apart carry
#' parameters and no information, and collapsing them loses nothing (§8.8).
#'
#' The test is a log-rank comparison, which uses the censored visits and
#' assumes nothing about shape. Read it with the sample sizes beside it: a
#' stage with few visits will fail to separate from its neighbor for want of
#' power, and that is a different finding from a stage that genuinely holds the
#' same wait.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param spec A [stage_spec()]. Adjacent pairs are taken from its declared
#'   transitions, excluding terminal stages, which hold no dwell.
#'
#' @return A data frame with one row per adjacent pair: the two stages, their
#'   visit counts and median dwells, the log-rank `p`, and a `verdict`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' stage_separation(sim, distribution_stage_spec())
#' @export
stage_separation <- function(sim, spec) {
  stopifnot(inherits(spec, "stage_spec"))
  d <- stage_dwell(sim)
  tr <- spec$transitions
  tr <- tr[!tr$from %in% spec$terminal & !tr$to %in% spec$terminal, ]
  # One row per unordered pair, since the comparison is symmetric.
  key <- apply(tr[c("from", "to")], 1, function(r) paste(sort(r), collapse = "|"))
  tr <- tr[!duplicated(key), ]

  rows <- lapply(seq_len(nrow(tr)), function(i) {
    a <- tr$from[i]; b <- tr$to[i]
    x <- d[d$stage %in% c(a, b), ]
    if (length(unique(x$stage)) < 2) return(NULL)
    s <- survival::survdiff(
      survival::Surv(pmax(x$dwell_hours, 1e-6), !x$censored) ~ x$stage)
    p <- stats::pchisq(s$chisq, length(s$n) - 1, lower.tail = FALSE)
    data.frame(stage_a = a, stage_b = b,
               n_a = sum(x$stage == a), n_b = sum(x$stage == b),
               median_a = stats::median(x$dwell_hours[x$stage == a]),
               median_b = stats::median(x$dwell_hours[x$stage == b]),
               p = p, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  small <- pmin(out$n_a, out$n_b) < 30
  out$verdict <- ifelse(out$p < 0.01, "separate",
                 ifelse(small, "too few visits to tell", "collapse"))
  out <- out[order(out$p, decreasing = TRUE), ]
  rownames(out) <- NULL
  out
}
