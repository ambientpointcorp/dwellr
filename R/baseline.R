#' Cohort-relative baselines
#'
#' A threshold is a claim about a distribution, so it has to be estimated over a
#' population that the instance being judged belongs to. One global threshold is
#' simultaneously too tight for the fast cohorts and too loose for the slow
#' ones, and conditioning on a segment is what repairs that (§12.2).
#'
#' Granularity is a statistical choice with a cost on each side. Coarse cohorts
#' carry bias, since the threshold describes a population the instance is only
#' partly a member of. Fine cohorts carry variance, since a quantile estimated
#' from a handful of visits moves with the handful. `min_n` sets where that
#' trade-off is resolved: a cell with fewer visits keeps the pooled threshold
#' and is reported as having done so, which is preferable to publishing a
#' number estimated from four observations.
#'
#' The estimator is the Kaplan-Meier quantile of [dwell_quantile()], so
#' still-open visits contribute rather than being dropped. A baseline fitted to
#' completed visits only is optimistic by exactly the mechanism of §6.2.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param cohort A data frame of cohort keys, one row per row of `dwell`. Each
#'   column is a key, and cells are the observed combinations.
#' @param p Quantile to estimate, for example 0.9.
#' @param min_n Minimum visits for a cell to carry its own threshold.
#'
#' @return A list with the `pooled` threshold, a `cells` data frame giving each
#'   cell's keys, count, threshold and whether it `fell_back`, and an
#'   `assign()` function mapping a cohort frame to thresholds.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 2000, seed = 11)
#' d <- stage_dwell(sim, "released")
#' o <- sim$orders[match(d$order_no, sim$orders$order_no), ]
#' b <- baseline_cohort(d, o[, c("branch", "order_source")], p = 0.9)
#' b$cells
#' @export
baseline_cohort <- function(dwell, cohort, p = 0.9, min_n = 50) {
  cohort <- as.data.frame(cohort, stringsAsFactors = FALSE)
  stopifnot(nrow(dwell) == nrow(cohort), ncol(cohort) >= 1,
            p > 0, p < 1, min_n >= 1)

  key <- do.call(paste, c(cohort, sep = "\r"))
  pooled <- dwell_quantile(dwell, p)

  cells <- lapply(unique(key), function(k) {
    sub <- dwell[key == k, , drop = FALSE]
    own <- if (nrow(sub) >= min_n) dwell_quantile(sub, p) else NA_real_
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    out <- as.data.frame(as.list(parts), stringsAsFactors = FALSE)
    names(out) <- names(cohort)
    out$n <- nrow(sub)
    out$threshold <- if (is.na(own)) pooled else own
    out$fell_back <- is.na(own)
    out
  })
  cells <- do.call(rbind, cells)
  cells <- cells[do.call(order, cells[names(cohort)]), ]
  rownames(cells) <- NULL

  assign_fn <- function(new_cohort) {
    new_cohort <- as.data.frame(new_cohort, stringsAsFactors = FALSE)
    stopifnot(identical(names(new_cohort), names(cohort)))
    k <- do.call(paste, c(new_cohort, sep = "\r"))
    ck <- do.call(paste, c(cells[names(cohort)], sep = "\r"))
    out <- cells$threshold[match(k, ck)]
    out[is.na(out)] <- pooled          # a cell never seen keeps the pooled one
    out
  }

  list(pooled = pooled, cells = cells, p = p, min_n = min_n,
       keys = names(cohort), assign = assign_fn)
}

#' Does a baseline deliver the exceedance rate it claims?
#'
#' A baseline set at the 90th percentile is a claim that one visit in ten will
#' exceed it. The claim is testable on data the baseline was not fitted to, and
#' it is the only check that distinguishes a cohort scheme that helps from one
#' that has fitted noise (§12.2).
#'
#' Read the per-cohort column rather than the total. A scheme can deliver 10%
#' overall while delivering 2% in one cohort and 30% in another, which is the
#' failure a global threshold produces and the reason for conditioning in the
#' first place.
#'
#' @param dwell A holdout data frame from [stage_dwell()], not used to fit.
#' @param cohort Cohort keys for those rows.
#' @param baseline A [baseline_cohort()] result.
#'
#' @return A list with the `overall` exceedance rate, its `target`, the absolute
#'   `error`, a `by_cohort` data frame, and the worst cohort deviation.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 3000, seed = 11)
#' d <- stage_dwell(sim, "released")
#' o <- sim$orders[match(d$order_no, sim$orders$order_no), ]
#' fit <- seq_len(nrow(d)) %% 2 == 0
#' b <- baseline_cohort(d[fit, ], o[fit, "branch", drop = FALSE], p = 0.9)
#' baseline_exceedance(d[!fit, ], o[!fit, "branch", drop = FALSE], b)
#' @export
baseline_exceedance <- function(dwell, cohort, baseline) {
  cohort <- as.data.frame(cohort, stringsAsFactors = FALSE)
  stopifnot(nrow(dwell) == nrow(cohort))
  thr <- baseline$assign(cohort)

  # A still-open visit that has not yet passed the threshold is uninformative:
  # it may exceed later. Counting it as compliant is the censoring error of
  # Section 6.2, so those rows are excluded and reported.
  decided <- !dwell$censored | dwell$dwell_hours > thr
  over <- dwell$dwell_hours > thr
  target <- 1 - baseline$p

  key <- do.call(paste, c(cohort, sep = "\r"))
  rows <- lapply(unique(key), function(k) {
    i <- key == k & decided
    parts <- strsplit(k, "\r", fixed = TRUE)[[1]]
    out <- as.data.frame(as.list(parts), stringsAsFactors = FALSE)
    names(out) <- names(cohort)
    out$n <- sum(i)
    out$exceedance <- if (sum(i)) mean(over[i]) else NA_real_
    out$error <- abs(out$exceedance - target)
    out
  })
  by_cohort <- do.call(rbind, rows)
  by_cohort <- by_cohort[do.call(order, by_cohort[names(cohort)]), ]
  rownames(by_cohort) <- NULL

  list(overall = mean(over[decided]),
       target = target,
       error = abs(mean(over[decided]) - target),
       excluded_undecided = sum(!decided),
       by_cohort = by_cohort,
       worst_cohort_error = max(by_cohort$error, na.rm = TRUE))
}

#' How stable is a baseline at a given window length?
#'
#' A baseline is estimated over a trailing window, and the window length trades
#' responsiveness against stability. A short window tracks a change in the
#' process quickly and moves on sampling noise alone; a long window is steady
#' and slow to notice that the process has changed (§12.6).
#'
#' Both halves of that trade-off are measurable. This reports the dispersion of
#' the estimate across successive windows, which is the stability side. The
#' responsiveness side requires a known change, which is why §12.6 introduces
#' one deliberately rather than waiting for one to occur.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param at Observation time of each row, as a `POSIXct` vector.
#' @param widths Window lengths in days.
#' @param p Quantile to estimate.
#' @param step Days between successive estimates.
#'
#' @return A data frame with one row per window length, giving the number of
#'   estimates made, their median, their standard deviation, and that standard
#'   deviation as a share of the median.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 3000, seed = 11)
#' d <- stage_dwell(sim, "released")
#' at <- sim$orders$entered_at[match(d$order_no, sim$orders$order_no)]
#' baseline_stability(d, at, widths = c(7, 30), p = 0.9, step = 7)
#' @export
baseline_stability <- function(dwell, at, widths = c(7, 14, 30, 60), p = 0.9,
                               step = 7) {
  stopifnot(nrow(dwell) == length(at), all(widths > 0), step > 0)
  t <- as.numeric(at)
  span <- range(t)
  day <- 86400

  rows <- lapply(widths, function(w) {
    ends <- seq(span[1] + w * day, span[2], by = step * day)
    est <- vapply(ends, function(e) {
      i <- t > e - w * day & t <= e
      if (sum(i) < 20) NA_real_ else dwell_quantile(dwell[i, , drop = FALSE], p)
    }, numeric(1))
    est <- est[is.finite(est)]
    data.frame(width_days = w, estimates = length(est),
               median = if (length(est)) stats::median(est) else NA_real_,
               sd = if (length(est) > 1) stats::sd(est) else NA_real_,
               cv = if (length(est) > 1) stats::sd(est) / stats::median(est)
                    else NA_real_)
  })
  do.call(rbind, rows)
}

#' Separate a changing population from a changing process
#'
#' An aggregate dwell figure rises for two different reasons, and the response
#' to each is different. The process may have slowed, or the mix may have
#' shifted toward cohorts that were always slower. Direct standardization
#' separates them by holding the mix fixed at the base period's and letting only
#' the within-cohort figures move (§12.10).
#'
#' The `process` component is what remains after the mix is held constant, and
#' it is the part an operation can act on. The `mix` component is the part
#' attributable to a different population arriving, which calls for a change to
#' the baseline instead of an intervention in the process.
#'
#' @param value_base,value_now Within-cohort summary for each cohort, in the
#'   base and current period.
#' @param weight_base,weight_now Cohort shares in each period. Each is
#'   normalized to sum to one.
#'
#' @return A list with the `base` and `now` aggregates, the total `change`, and
#'   its decomposition into `process` and `mix` components.
#'
#' @examples
#' # A mix shift alone, with every cohort unchanged.
#' baseline_drift_decompose(value_base = c(2, 10), value_now = c(2, 10),
#'                          weight_base = c(0.8, 0.2),
#'                          weight_now = c(0.5, 0.5))
#' @export
baseline_drift_decompose <- function(value_base, value_now,
                                     weight_base, weight_now) {
  stopifnot(length(value_base) == length(value_now),
            length(weight_base) == length(value_base),
            length(weight_now) == length(value_base))

  # Align by name wherever names are present. Matching positionally is the
  # defect this guards against: `tapply` returns its groups in sorted order,
  # so a caller who writes the weights out in the order the cohorts occur to
  # them silently pairs each weight with the wrong cohort. The first draft of
  # Section 12.10 reported a mix effect of +85 hours that way, against a true
  # +12.
  nm <- names(value_base)
  if (!is.null(nm)) {
    pick <- function(x, what) {
      if (is.null(names(x))) return(x)
      if (!setequal(names(x), nm))
        stop(sprintf("names of %s do not match names of value_base", what),
             call. = FALSE)
      x[nm]
    }
    value_now <- pick(value_now, "value_now")
    weight_base <- pick(weight_base, "weight_base")
    weight_now <- pick(weight_now, "weight_now")
  }

  wb <- weight_base / sum(weight_base)
  wn <- weight_now / sum(weight_now)

  base <- sum(wb * value_base)
  now <- sum(wn * value_now)
  # Standardized: the current within-cohort figures on the base period's mix.
  standardized <- sum(wb * value_now)

  list(base = base, now = now, change = now - base,
       standardized = standardized,
       process = standardized - base,
       mix = now - standardized)
}
