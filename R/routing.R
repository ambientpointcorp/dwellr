#' Route exceptions to the action each one needs
#'
#' An exception is useful to the person who receives it only if they can do
#' something about it. Routing on what is wrong sends a credit hold to a
#' warehouse that cannot release it; routing on what needs doing sends it to
#' whoever can clear credit (§18.4).
#'
#' The table is therefore keyed on the required action and not on the symptom.
#' Several conditions often need the same action, and one condition sometimes
#' needs several, so the mapping is many-to-many and has to be written down
#' rather than inferred from a stage name.
#'
#' `matched` is the column to test. An item with no rule is not routed to
#' nobody by accident: it is reported, and Section 18.8 measures what a
#' silent default costs.
#'
#' @param keys Routing key for each item, one per item.
#' @param table A data frame of rules with columns `key`, `action`,
#'   `recipient` and `urgency`.
#' @param default Optional single-row data frame used for keys with no rule.
#'   Supplying it marks those rows `matched = FALSE` even though they are
#'   routed, so a default can never hide a gap.
#'
#' @return A data frame with one row per item: `key`, `action`, `recipient`,
#'   `urgency` and `matched`.
#'
#' @examples
#' rules <- data.frame(
#'   key = c("received", "credit_hold"),
#'   action = c("release the order", "clear the credit hold"),
#'   recipient = c("warehouse", "finance"),
#'   urgency = c("notify", "escalate"), stringsAsFactors = FALSE)
#' route_action(c("received", "credit_hold", "backordered"), rules)
#' @export
route_action <- function(keys, table, default = NULL) {
  need <- c("key", "action", "recipient", "urgency")
  stopifnot(all(need %in% names(table)), length(keys) > 0)
  if (anyDuplicated(table$key))
    stop("routing table has duplicate keys: ",
         paste(unique(table$key[duplicated(table$key)]), collapse = ", "),
         call. = FALSE)

  i <- match(keys, table$key)
  out <- data.frame(key = keys,
                    action = table$action[i],
                    recipient = table$recipient[i],
                    urgency = table$urgency[i],
                    matched = !is.na(i),
                    stringsAsFactors = FALSE)
  if (!is.null(default)) {
    stopifnot(all(need[-1] %in% names(default)), nrow(default) == 1)
    out$action[!out$matched]    <- default$action
    out$recipient[!out$matched] <- default$recipient
    out$urgency[!out$matched]   <- default$urgency
  }
  out
}

#' Which routing keys the table does not cover
#'
#' Routing-table completeness is an invariant, which means it can be tested
#' rather than hoped for. In one engagement four items fell through to a default
#' map carrying no action type, and the highest-value order in the queue
#' escalated to no recipient at all (§18.8).
#'
#' The test to run is this one, on the live population and not on the keys
#' somebody remembered when the table was written. A key that appears once a
#' quarter is exactly the key nobody thought of.
#'
#' @param keys Routing keys observed in the population.
#' @param table A routing table as used by [route_action()].
#' @param value Optional exposure per item, to report what is unrouted.
#'
#' @return A list with the `covered` share of items, a `gaps` data frame of
#'   uncovered keys with their counts and exposure, and the `unused` rules that
#'   matched nothing.
#'
#' @examples
#' rules <- data.frame(key = "received", action = "release",
#'                     recipient = "warehouse", urgency = "notify",
#'                     stringsAsFactors = FALSE)
#' routing_gaps(c("received", "received", "credit_hold"), rules,
#'              value = c(100, 200, 9000))
#' @export
routing_gaps <- function(keys, table, value = NULL) {
  stopifnot("key" %in% names(table), length(keys) > 0)
  if (!is.null(value)) stopifnot(length(value) == length(keys))

  missing_keys <- setdiff(unique(keys), table$key)
  rows <- lapply(missing_keys, function(k) {
    i <- keys == k
    data.frame(key = k, items = sum(i),
               exposure = if (is.null(value)) NA_real_ else sum(value[i]),
               max_exposure = if (is.null(value)) NA_real_ else max(value[i]),
               stringsAsFactors = FALSE)
  })
  gaps <- if (length(rows)) do.call(rbind, rows) else
    data.frame(key = character(0), items = integer(0),
               exposure = numeric(0), max_exposure = numeric(0))
  if (nrow(gaps)) gaps <- gaps[order(-gaps$exposure, -gaps$items), ]
  rownames(gaps) <- NULL

  list(covered = mean(keys %in% table$key),
       gaps = gaps,
       unused = setdiff(table$key, unique(keys)))
}
