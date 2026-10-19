# dwellr (development version)

First scaffold. Implemented and tested:

* `simulate_lifecycles()` — the synthetic order generator. Everything else in
  the package, and every example in the book, runs against its output.
* `stage_dwell()`, `dwell_survival()`, `dwell_quantile()` — stage dwell under
  right-censoring.
* `threshold_from_cost()` — an alerting threshold as the critical fractile of
  the conditional dwell distribution.
* `fill_rate()` — fill on units, lines and value, with the binding dimension.
* `eoq()`, `eoq_cost()`, `nv_critical_fractile()`, `nv_quantity()` — the
  classical results the later chapters reuse. Their tests reproduce the
  instance carried through Chapter 2 (K = 100, h = 1, d = 10), the flatness
  figures of Section 2.3, and the cost-misestimation figures of Section 2.4.

Declared with a roadmap and not yet implemented: the remaining `eoq_*`/`dls_*`
lot-sizing models, the stochastic policies, the `fit_*`/`calibrate_*` modeling
helpers, `baseline_*`, `deadline_*`, `capacity_*`, `route_*` and `evaluate_*`.
Each errors with the chapter it belongs to rather than returning something
plausible.
