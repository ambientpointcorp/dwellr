#' dwellr: Lifecycle Monitoring Analytics for Supply Chain Operations
#'
#' Companion package to the book *Lifecycle Monitoring Analytics: A toolkit for
#' continuous supply chain operations*.
#'
#' Functions are named by the chapter family they belong to, which is what
#' keeps a package spanning EOQ through routing navigable:
#'
#' * `eoq_*`, `dls_*`, `nv_*`, `policy_*` — classical inventory control (Part I)
#' * `simulate_*`, `stage_*`, `dwell_*`, `safety_*` — stage process (Part II)
#' * `fit_*`, `calibrate_*` — modeling (Chapter 9)
#' * `fill_*`, `baseline_*`, `deadline_*` — measurement (Part III)
#' * `threshold_*`, `capacity_*`, `route_*` — attention (Part IV)
#' * `evaluate_*` — evaluation (Part V)
#'
#' Start with [simulate_lifecycles()]: everything else runs against its output,
#' and the built-in [orders] dataset is one call to it.
#'
#' @keywords internal
"_PACKAGE"

#' A small synthetic order population
#'
#' Two thousand synthetic orders and their event log, produced by
#' `simulate_lifecycles(n = 2000, seed = 42)`. Ships with the package so that
#' every example and test runs on install, with no download.
#'
#' The full corpus the book's figures are built from is larger; it is generated
#' by the same function in `dwellr-examples`, at a bigger `n`. One generator,
#' two scales, so they cannot drift apart.
#'
#' @format A `lifecycle_sim`: a list of `orders` (2000 rows), `events`, and
#'   `as_of`.
#' @source `data-raw/make_orders.R`
#' @examples
#' orders
#' mean(!orders$orders$is_terminal)   # share still in flight, hence censored
"orders"
