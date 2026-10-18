# dwellr

Lifecycle monitoring analytics for supply chain operations.

Companion R package to the book *Lifecycle Monitoring Analytics: A toolkit for
continuous supply chain operations*, by Ariel Rodríguez Herrera (CEO, Ambient
Point Corp.), first edition, 2026.

**Private while the book is drafted.** It goes public, MIT licensed, at
publication.

> **Status: 0.0.1.9000.** The generator, stage dwell estimation, thresholds,
> fill rate and the classical results are implemented and tested. The rest of
> the function families are declared with the chapter they belong to and error
> if called — see [Roadmap](#roadmap).

## What it does

Classical inventory models size safety stock from lead-time variance. This
package measures where that variance actually comes from: the time an order
spends in each stage of its own lifecycle, much of it administrative rather
than physical.

```r
library(dwellr)

# The built-in dataset is one call to the generator.
orders
#> <lifecycle_sim> 2000 orders, 6722 events, as of 2026-05-05 07:00
#>   in flight (censored): 43 (2.1%)
#>   residue -- finished but holding an open ticket: 600

# Stage dwell, with still-open visits flagged rather than dropped.
d <- stage_dwell(orders, stage = "released")

# The operating quantiles a threshold should come from.
dwell_quantile(d, c(0.5, 0.9))
#> [1]  1.97 17.99

# And the threshold itself, derived from a cost ratio rather than chosen:
# the critical fractile of the conditional dwell distribution.
threshold_from_cost(d, cost_miss = 400, cost_look = 10)
#> $fractile
#> [1] 0.9756
#> $hours
#> [1] 108.8
```

### Why censoring is the first thing in the package

An order sitting in a stage right now has not finished waiting. Its dwell is a
lower bound, not an observation. Averaging completed visits only — the obvious
thing to do — removes exactly the orders that are stuck:

```r
d <- stage_dwell(orders)
median(d$dwell_hours[d$censored])    # still open:  29.6 h
median(d$dwell_hours[!d$censored])   # completed:    3.0 h
```

A tenfold optimistic bias, and it is largest when the operation is performing
worst. `dwell_survival()` and `dwell_quantile()` use the censored estimate.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ambientpointcorp/dwellr")
```

## Function families

Named by the chapter family they belong to, which is what keeps one package
spanning EOQ through routing navigable.

| Prefix | Covers |
|---|---|
| `eoq_*` `dls_*` `nv_*` `policy_*` | Classical inventory control (Part I) |
| `simulate_*` `stage_*` `dwell_*` `safety_*` | Lead time as a stage process (Part II) |
| `fit_*` `calibrate_*` | Machine learning (Chapter 9) |
| `fill_*` `baseline_*` `deadline_*` | Measurement method (Part III) |
| `threshold_*` `capacity_*` `route_*` | Allocating attention (Part IV) |
| `evaluate_*` | Evaluation (Part V) |

## Synthetic data, not client data

`simulate_lifecycles()` generates orders from a stated model with documented
parameters. It is not a scrubbed extract, and it reproduces the pathologies the
book is about on purpose: right-censoring, a fast and a stuck population in
every stage, partial fills, orders closed short, `released → backordered`
loops, vendor-sourced special orders, and residue — finished orders still
carrying an open pick ticket, which is what overstates a naive work queue.

The built-in `orders` dataset is one call at `n = 2000`. The full corpus the
book's figures are built from is the same function at a larger `n`, in
`dwellr-examples` — one generator, two scales, so they cannot drift apart.

## Tests

The suite reproduces worked numerical results printed in the book: the EOQ
example and the flatness of its cost curve, the newsvendor critical fractile
and its `Poisson(100)` quantity, and the censoring bias above. A change that
breaks an example in the text fails the build.

```r
devtools::test()
```

## Roadmap

Declared and erroring until their chapter is drafted: the remaining lot-sizing
models (`dls_wagner_whitin()`, `eoq_quantity_discount()`, `epq()`), the
stochastic policies (`policy_base_stock()`, `policy_ss()`, `policy_rs()`),
safety stock from measured dwell (`safety_stock()`,
`dwell_variance_contribution()`), the modeling helpers (`fit_dwell_model()`,
`calibrate_probability()`), and `baseline_cohort()`, `deadline_derive()`,
`capacity_admit()`, `route_action()`, `evaluate_policy()`.

Each errors with the chapter it belongs to. An unimplemented method that
returns a plausible number is worse than one that refuses.

## License

MIT. See [LICENSE.md](LICENSE.md).
