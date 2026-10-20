#' Expand an order population to line grain
#'
#' The order table carries a line count and a line fill fraction. This expands
#' it into one row per line, which is the grain Chapter 10 needs in order to
#' ask what changes when the entity is finer than the record the events were
#' written against.
#'
#' The expansion is deterministic and it is an expansion, not an independent
#' draw. A line inherits its order's events, its branch and its channel, and
#' the lines that shipped are taken to be the first `fill_lines * lines` of
#' them. That supports questions about grain, aggregation and identity, since
#' those depend on how many lines an order has and on how many were satisfied.
#' It does not support questions about lines moving independently of their
#' order, because in this table they cannot: a line has no events of its own.
#' Section 8.5 measures the independent case from the split records, which is
#' where that question belongs.
#'
#' A line counts as shipped only when the order reached a stage in which
#' shipping has happened. The `fill_lines` fraction is 1 for every order that
#' has not shipped short, including orders that have not shipped at all, so
#' reading it without the stage would credit an order sitting in `received`
#' with a complete shipment.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#'
#' @return A data frame with one row per line: `order_no`, `line_no`, the
#'   order's `stage`, `branch`, `order_source` and `lines`, a `value_share` of
#'   the order value, whether the line `shipped`, the source `disposition`
#'   code, and the derived `shortfall` flag.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 500, seed = 11)
#' ln <- order_lines(sim)
#' nrow(ln)
#' table(ln$disposition)
#' @export
order_lines <- function(sim) {
  o <- sim$orders
  stopifnot(all(c("lines", "fill_lines", "stage", "closed_short") %in% names(o)))

  shipped_n <- ifelse(o$stage %in% c("shipped", "partially_shipped"),
                      round(o$fill_lines * o$lines), 0)
  idx <- rep(seq_len(nrow(o)), o$lines)
  line_no <- sequence(o$lines)

  out <- data.frame(
    order_no = o$order_no[idx],
    line_no = line_no,
    stage = o$stage[idx],
    branch = o$branch[idx],
    order_source = o$order_source[idx],
    lines = o$lines[idx],
    value_share = o$value[idx] / o$lines[idx],
    shipped = line_no <= shipped_n[idx],
    stringsAsFactors = FALSE)

  # The source disposition is what a line-level status column in an order
  # management system would hold: shipped, cancelled at close, or open. It is
  # a statement about where the line stands and not about what happened to it,
  # which is the distinction Section 10.8 measures.
  out$disposition <- ifelse(
    out$shipped, "shipped",
    ifelse(o$closed_short[idx], "cancelled_at_close",
           ifelse(o$stage[idx] == "cancelled", "cancelled", "open")))

  # The derived flag asks whether the line's order ever went short, which
  # includes the shortfalls that were recovered and that no status code
  # retains.
  out$shortfall <- o$stock_shortfall[idx] | o$closed_short[idx]
  out
}

#' Test a candidate entity against an event log
#'
#' Section 8.7 states the test an entity choice has to pass: every event in the
#' log attaches to exactly one instance of the chosen entity. This applies it
#' and reports the two ways the test fails, since an event that attaches to no
#' instance and an event that attaches to several are different problems with
#' different repairs.
#'
#' The second condition is separate and it is the one that disqualifies most
#' convenient entities. An instance record has to be able to say whether
#' somebody still owes something, because that is the question a monitoring
#' system exists to answer. A record that is closed when its work is finished
#' can answer it; a record that is closed when a document is printed cannot,
#' and Section 10.2 measures what the difference costs.
#'
#' @param events An event log with one row per transition.
#' @param instances A candidate instance table, one row per instance.
#' @param key Name of the identifying column, present in both frames.
#' @param owed Optional name of a logical column on `instances` that is `TRUE`
#'   while the instance still owes work. Supply it to test the second
#'   condition.
#'
#' @return A list with the event and instance counts, the events `attached` to
#'   exactly one instance, those `unattached` and those `ambiguous`, the
#'   `orphan` instances carrying no event, the `attach_rate`, whether the
#'   attachment is `unique`, and, when `owed` is given, the count still owing
#'   and whether the record `knows_owed`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 500, seed = 11)
#' entity_check(sim$events, sim$orders, key = "order_no",
#'              owed = "is_terminal")
#' @export
entity_check <- function(events, instances, key, owed = NULL) {
  stopifnot(key %in% names(events), key %in% names(instances))
  ek <- events[[key]]
  ik <- instances[[key]]
  per_key <- table(ik[!is.na(ik)])
  matched <- per_key[match(ek, names(per_key))]
  matched[is.na(matched)] <- 0L

  out <- list(
    n_events = length(ek),
    n_instances = nrow(instances),
    attached = sum(matched == 1L),
    unattached = sum(matched == 0L | is.na(ek)),
    ambiguous = sum(matched > 1L),
    orphan = sum(!ik %in% ek))
  out$attach_rate <- out$attached / max(out$n_events, 1)
  out$unique <- out$ambiguous == 0L && out$unattached == 0L

  if (!is.null(owed)) {
    stopifnot(owed %in% names(instances), is.logical(instances[[owed]]))
    out$owing <- sum(instances[[owed]])
    # A record knows whether work is owed when the flag separates instances
    # at all. A column that is constant carries the answer for no instance,
    # which is the state a printed-document entity is in.
    out$knows_owed <- out$owing > 0 && out$owing < nrow(instances)
  }
  out
}

#' Audit an identifier against the entity it is meant to identify
#'
#' Two failures occur in every integration that joins systems, and they pull in
#' opposite directions. A collision is one identifier standing for several
#' entities, which merges distinct work and understates the population. A
#' fragmentation is one entity carrying several identifiers, which splits work
#' and overstates it. Both are measurable before anything is built on the join
#' (§10.5).
#'
#' @param id Identifier values, one per record.
#' @param entity The entity each record belongs to, same length as `id`.
#'
#' @return A list with the record, identifier and entity counts, the number of
#'   `collisions` and `fragmentations` with the records they affect, and each
#'   as a share of records.
#'
#' @examples
#' identifier_audit(id = c("A", "A", "B", "C", "C"),
#'                  entity = c("1", "2", "3", "4", "4"))
#' @export
identifier_audit <- function(id, entity) {
  stopifnot(length(id) == length(entity))
  keep <- !is.na(id) & !is.na(entity)
  id <- id[keep]; entity <- entity[keep]

  per_id <- tapply(entity, id, function(x) length(unique(x)))
  per_entity <- tapply(id, entity, function(x) length(unique(x)))
  coll <- names(per_id)[per_id > 1]
  frag <- names(per_entity)[per_entity > 1]

  list(n_records = length(id),
       n_ids = length(per_id),
       n_entities = length(per_entity),
       collisions = length(coll),
       collision_records = sum(id %in% coll),
       collision_share = sum(id %in% coll) / max(length(id), 1),
       fragmentations = length(frag),
       fragmented_records = sum(entity %in% frag),
       fragmentation_share = sum(entity %in% frag) / max(length(id), 1),
       missing = sum(!keep))
}

#' Does conditioning on a key change the decision?
#'
#' A segmentation key earns its place by changing what the operation does, and
#' not by being significant or by partitioning the population into groups that
#' differ. This fits the alerting threshold of
#' [threshold_from_dwell_quantile()] once on the pooled population and once
#' within each level of the key, and reports how many visits are classified
#' differently under the two (§10.7).
#'
#' The quantity to read is `share_changed`. A key that moves no visit across
#' the threshold adds a dimension to every report and changes nothing, whatever
#' its p-value. A key that moves a material share is worth conditioning on,
#' and the per-level thresholds are the operating numbers it produces.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param segment A vector of segment labels, one per row of `dwell`.
#' @inheritParams threshold_from_cost
#' @param min_n Levels with fewer visits than this are not given their own
#'   threshold, since a threshold fitted to a handful of visits is noise. They
#'   are reported as `pooled_levels`.
#'
#' @return A list with the `pooled` threshold in hours, a data frame of
#'   per-level thresholds and counts, the number and `share_changed` of visits
#'   whose classification differs, and the levels left pooled for want of data.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1500, seed = 11)
#' d <- stage_dwell(sim, "released")
#' seg <- sim$orders$branch[match(d$order_no, sim$orders$order_no)]
#' segment_decision_shift(d, seg, cost_miss = 40, cost_look = 400)
#' @export
segment_decision_shift <- function(dwell, segment, cost_miss, cost_look,
                                   min_n = 50) {
  stopifnot(nrow(dwell) == length(segment), min_n >= 1)
  pooled <- threshold_from_dwell_quantile(dwell, cost_miss, cost_look)$hours

  lv <- unique(stats::na.omit(segment))
  rows <- lapply(lv, function(l) {
    sub <- dwell[!is.na(segment) & segment == l, , drop = FALSE]
    h <- if (nrow(sub) >= min_n) {
      threshold_from_dwell_quantile(sub, cost_miss, cost_look)$hours
    } else NA_real_
    data.frame(level = as.character(l), n = nrow(sub), hours = h,
               stringsAsFactors = FALSE)
  })
  per_level <- do.call(rbind, rows)
  per_level <- per_level[order(per_level$level), ]
  rownames(per_level) <- NULL

  # A visit is classified differently when the pooled threshold and its own
  # segment's threshold put it on opposite sides. Levels without a threshold
  # of their own keep the pooled one and so cannot change.
  own <- per_level$hours[match(as.character(segment), per_level$level)]
  own[is.na(own)] <- pooled
  changed <- (dwell$dwell_hours > pooled) != (dwell$dwell_hours > own)

  list(pooled = pooled,
       per_level = per_level,
       n_changed = sum(changed, na.rm = TRUE),
       share_changed = mean(changed, na.rm = TRUE),
       pooled_levels = per_level$level[is.na(per_level$hours)])
}
