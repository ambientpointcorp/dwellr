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
* `baseline_cohort()`, `baseline_exceedance()`, `baseline_stability()` and
  `baseline_drift_decompose()` — the baselines of Chapter 12, replacing the
  `baseline_cohort` roadmap stub. A cohort scheme is validated on the cohort
  and not on the population: on the order population every granularity from one
  key to four holds an overall exceedance within 1.7 points of its 10% target
  while the worst cohort moves from 2.6 to 90 points off it, so
  `baseline_exceedance()` reports the per-cohort column. `baseline_stability()`
  measures the steadiness half of the window-length trade-off, where a
  seven-day baseline moves 15% from week to week against 1.4% at sixty days.
  `baseline_drift_decompose()` separates a slowing process from a shifting mix,
  which an aggregate cannot: a mix shift and a 25% slowdown produce aggregates
  0.7 hours apart here. It aligns its inputs by name, because matching
  positionally pairs each weight with the wrong cohort whenever the caller
  writes them in a different order from `tapply`, and that produced a mix
  effect of +85 hours against a true +12 in a first draft.
* `order_lines()`, `entity_check()`, `identifier_audit()` and
  `segment_decision_shift()` — the entity decision of Chapter 10.
  `entity_check()` applies the two conditions a candidate entity has to meet,
  that every event attaches to exactly one instance and that an instance can
  say whether work is still owed; on the order population the pick ticket
  fails the first at an attachment rate of 0.857 and the customer fails the
  second. `segment_decision_shift()` judges a segmentation key by how many
  visits it reclassifies under the operating threshold rather than by whether
  its groups differ.
* `fill_rate()`, `fill_dimensions()` — fill on units, lines and value, with the
  binding dimension, for Chapter 11. `simulate_lifecycles()` now returns a
  `lines` table carrying an ordered quantity and a unit price per line, and the
  three `fill_*` columns are computed from it rather than drawn separately, so
  the dimensions describe one population and can disagree the way they do in
  an order book. They are `NA` where the order has not reached a shipping
  stage: an order that has not shipped has no fill rate, and crediting it with
  one overstated unit fill on this population by 0.3 points while pooling every
  line in the extract understated it by 4.6. The new line draws are vectorized
  and happen after every other draw, so the rest of the population is
  byte-identical, verified across three seeds and two sizes.
* `eoq()`, `eoq_cost()`, `nv_critical_fractile()`, `nv_quantity()` — the
  classical results the later chapters reuse. Their tests reproduce the
  instance carried through Chapter 2 (K = 100, h = 1, d = 10), the flatness
  figures of Section 2.3, and the cost-misestimation figures of Section 2.4.

Declared with a roadmap and not yet implemented: the remaining `eoq_*`/`dls_*`
lot-sizing models, the stochastic policies, the `fit_*`/`calibrate_*` modeling
helpers, `baseline_*`, `deadline_*`, `capacity_*`, `route_*` and `evaluate_*`.
Each errors with the chapter it belongs to rather than returning something
plausible.
