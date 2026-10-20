# dwellr (development version)

First scaffold. Implemented and tested:

* `simulate_lifecycles()` — the synthetic order generator. Everything else in
  the package, and every example in the book, runs against its output.
* `stage_dwell()`, `dwell_survival()`, `dwell_quantile()` — stage dwell under
  right-censoring.
* `threshold_from_cost()` — an alerting threshold on a vector of calibrated
  stall probabilities, set at `p* = c_look / (c_look + c_miss)` and reported
  with the expected cost of the rule beside reviewing nothing and reviewing
  everything. This is the newsvendor fractile with the costs in the other
  order, since the decision variable is when to spend attention rather than
  how much to stock, so the threshold falls as a missed stall gets dearer and
  rises as reviewer attention does. `threshold_from_dwell_quantile()` is the
  fallback where no fitted probability exists, applying the same `p*` as a
  fractile of the dwell distribution.
* `dwell_mixture()` now returns the index of the `slow` component and a
  `posterior(time, censored)` function giving the probability that a visit
  belongs to it, which is the input `threshold_from_cost()` expects.
* `order_lines()`, `entity_check()`, `identifier_audit()` and
  `segment_decision_shift()` — the entity decision of Chapter 10.
  `entity_check()` applies the two conditions a candidate entity has to meet,
  that every event attaches to exactly one instance and that an instance can
  say whether work is still owed; on the order population the pick ticket
  fails the first at an attachment rate of 0.857 and the customer fails the
  second. `segment_decision_shift()` judges a segmentation key by how many
  visits it reclassifies under the operating threshold rather than by whether
  its groups differ.
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
