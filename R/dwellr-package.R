#' dwellr: Lifecycle Monitoring Analytics for Supply Chain Operations
#'
#' Measures the time orders spend in each stage of their lifecycle, and
#' supports deciding what to do about it.
#'
#' Functions are named by the family they belong to. The package spans EOQ
#' through routing, and the prefixes are:
#'
#' * `eoq_*`, `dls_*`, `nv_*`, `policy_*` — classical inventory control
#' * `simulate_*`, `stage_*`, `dwell_*`, `safety_*` — the stage process
#' * `fit_*`, `calibrate_*` — modeling
#' * `fill_*`, `baseline_*`, `deadline_*` — measurement
#' * `threshold_*`, `capacity_*`, `route_*` — allocating attention
#' * `evaluate_*` — policy evaluation
#'
#' Every other function runs against the output of [simulate_lifecycles()],
#' and the built-in [orders] dataset is one call to it.
#'
#' @keywords internal
"_PACKAGE"
