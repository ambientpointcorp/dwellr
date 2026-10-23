#' Build a lifecycle population from a recorded event log
#'
#' Every analysis function in this package reads one object: a list with an
#' event log, the population those events belong to, a stage specification and
#' the instant the data was cut. [simulate_lifecycles()] produces one.
#' This produces the same thing from records a real operation already keeps,
#' which is what makes the methods in the book usable on anything other than
#' the synthetic corpus.
#'
#' @section The shape this expects:
#'
#' `events` is the whole requirement. One row per stage entry, in any order,
#' with three columns:
#'
#' \describe{
#'   \item{`order_no`}{the instance identifier. Whatever the instance is --
#'     an order, a load, a claim, a renewal -- it is the thing whose lifecycle
#'     is being measured, and the column keeps this name because every other
#'     function in the package reads it.}
#'   \item{`stage`}{the stage entered, named in `spec`.}
#'   \item{`occurred_at`}{a `POSIXct` of when the move happened, not when it
#'     was recorded. Dwell is elapsed time in the world; a processing
#'     timestamp measures the reporting pipeline instead.}
#' }
#'
#' An `event` column naming the transition is kept when present and invented
#' from the stage when it is not.
#'
#' @section Why `as_of` is not optional in spirit:
#'
#' A population is only meaningful as of an instant. Every visit with no later
#' event is open at `as_of` and is a right-censored observation, which is the
#' whole subject of Chapter 6. Defaulting it to the last event in the log is a
#' convenience with a trap in it: if the extract was cut on Tuesday and the
#' last event happens to be Friday-week-before, the default silently declares
#' nine days of in-flight work finished. Pass the instant the data was cut.
#'
#' @section Exporting from a source system:
#'
#' This function is the contract, and it is deliberately a small one so that
#' the export side can be written against a published shape rather than
#' against this package's internals. An exporter needs to produce the three
#' event columns, a stage specification matching the operation's own
#' lifecycle, and the extract instant. Everything else is optional and
#' enriches what can be asked: `orders` carries per-instance attributes to
#' group and segment by, `lines` the line grain of Chapter 10, and `items`
#' the demand and supply figures Chapter 7 converts into safety stock.
#'
#' @param events A data frame of stage entries. See the shape above.
#' @param spec A [stage_spec()] for the operation being measured. Required:
#'   the stages an instance can stop in, and the stages in which its work was
#'   delivered, cannot be guessed from a log and are wrong by default.
#' @param orders Optional data frame, one row per instance. Derived from the
#'   log when absent, carrying `order_no`, `stage`, `is_terminal` and
#'   `finished_at`. When supplied, those four columns are recomputed from the
#'   log rather than trusted, since they are a function of it.
#' @param lines Optional data frame, one row per line of an instance.
#' @param items Optional data frame, one row per item.
#' @param as_of Observation instant. Defaults to the last event in the log,
#'   with the caveat above.
#'
#' @return An object of class `lifecycle_sim`.
#'
#' @examples
#' spec <- stage_spec(
#'   stages      = c("open", "accumulating", "departed", "cancelled"),
#'   transitions = c("open -> accumulating", "accumulating -> departed",
#'                   "open -> cancelled", "accumulating -> cancelled"),
#'   terminal    = c("departed", "cancelled"),
#'   fulfilling  = "departed")
#'
#' log <- data.frame(
#'   order_no    = c("L1", "L1", "L1", "L2", "L2"),
#'   stage       = c("open", "accumulating", "departed", "open", "accumulating"),
#'   occurred_at = as.POSIXct("2026-01-01 08:00", tz = "UTC") +
#'                 c(0, 3600, 9000, 1800, 7200))
#'
#' pop <- as_lifecycle(log, spec,
#'                     as_of = as.POSIXct("2026-01-01 12:00", tz = "UTC"))
#' pop
#' stage_dwell(pop)
#' @export
as_lifecycle <- function(events, spec, orders = NULL, lines = NULL,
                         items = NULL, as_of = NULL) {
  if (!inherits(spec, "stage_spec"))
    stop("`spec` must be a stage_spec(); the stages an instance can stop in ",
         "cannot be inferred from a log", call. = FALSE)
  if (!is.data.frame(events))
    stop("`events` must be a data frame of stage entries", call. = FALSE)

  need <- c("order_no", "stage", "occurred_at")
  missing <- setdiff(need, names(events))
  if (length(missing))
    stop("`events` is missing ", paste(missing, collapse = ", "),
         call. = FALSE)
  if (!nrow(events)) stop("`events` has no rows", call. = FALSE)
  if (!inherits(events$occurred_at, "POSIXct"))
    stop("`events$occurred_at` must be POSIXct, not ",
         class(events$occurred_at)[1], call. = FALSE)
  if (anyNA(events$occurred_at))
    stop("`events$occurred_at` has missing values; a visit with no start ",
         "time has no dwell", call. = FALSE)

  # An undeclared stage is a configuration error and it is silent: the stage
  # is neither an ending nor a declared wait, so it is measured as a wait that
  # never completes. Refusing here is the same check stage_validate() reports
  # on, applied at the door instead.
  undeclared <- setdiff(unique(events$stage), spec$stages)
  if (length(undeclared))
    stop("`events` names stages the specification does not declare: ",
         paste(utils::head(undeclared, 5), collapse = ", "),
         if (length(undeclared) > 5)
           sprintf(" (and %d more)", length(undeclared) - 5),
         call. = FALSE)

  ev <- data.frame(
    order_no    = as.character(events$order_no),
    event       = if (!is.null(events$event)) as.character(events$event)
                  else paste0("entered_", events$stage),
    stage       = as.character(events$stage),
    occurred_at = events$occurred_at,
    stringsAsFactors = FALSE)
  ev <- ev[order(ev$order_no, ev$occurred_at), ]
  rownames(ev) <- NULL

  if (is.null(as_of)) as_of <- max(ev$occurred_at)
  if (!inherits(as_of, "POSIXct"))
    stop("`as_of` must be POSIXct", call. = FALSE)
  if (as_of < max(ev$occurred_at))
    stop("`as_of` precedes the last event; cut the log first, or use ",
         "observe_at() to re-cut a population", call. = FALSE)

  last <- !duplicated(ev$order_no, fromLast = TRUE)
  now <- data.frame(order_no = ev$order_no[last], stage = ev$stage[last],
                    occurred_at = ev$occurred_at[last],
                    stringsAsFactors = FALSE)

  if (is.null(orders)) {
    o <- data.frame(order_no = now$order_no, stringsAsFactors = FALSE)
  } else {
    if (!"order_no" %in% names(orders))
      stop("`orders` must carry `order_no`", call. = FALSE)
    o <- orders[match(now$order_no, as.character(orders$order_no)), ,
                drop = FALSE]
    o$order_no <- now$order_no
    unknown <- setdiff(as.character(orders$order_no), now$order_no)
    if (length(unknown))
      warning(length(unknown), " instance(s) in `orders` have no events and ",
              "were dropped", call. = FALSE)
  }

  # Recomputed, never trusted. A status column in a source system is a
  # statement somebody maintained; the log is what happened, and where the two
  # disagree the log is the one that can be checked.
  o$stage       <- now$stage
  o$is_terminal <- o$stage %in% spec$terminal
  o$finished_at <- as.POSIXct(ifelse(o$is_terminal, now$occurred_at, NA),
                              origin = "1970-01-01", tz = "UTC")
  rownames(o) <- NULL

  structure(list(orders = o, events = ev, lines = lines, items = items,
                 as_of = as_of, spec = spec), class = "lifecycle_sim")
}

# Resolve a column map against a data frame.
#
# `default` names the columns the generator produces; `map` overrides any of
# them for a population that calls the same quantity something else. The
# refusal names the quantity AND the column looked for, because "fill_lines
# not found" is useless to somebody whose extract calls it `line_fill_pct`.
resolve_cols <- function(data, default, map = NULL, required = names(default),
                         what = "data") {
  cols <- default
  if (!is.null(map)) {
    if (is.null(names(map)) || any(!nzchar(names(map))))
      stop("`cols` must be a named character vector, e.g. ",
           "c(fill = \"line_fill_pct\")", call. = FALSE)
    unknown <- setdiff(names(map), names(default))
    if (length(unknown))
      stop("`cols` names quantities this function does not use: ",
           paste(unknown, collapse = ", "), ". It understands: ",
           paste(names(default), collapse = ", "), call. = FALSE)
    cols[names(map)] <- unname(map)
  }
  missing <- required[!cols[required] %in% names(data)]
  if (length(missing))
    stop(what, " is missing ",
         paste(sprintf("%s (looked for column `%s`)", missing, cols[missing]),
               collapse = ", "),
         ". Pass `cols` to name the columns this population uses.",
         call. = FALSE)
  cols
}
