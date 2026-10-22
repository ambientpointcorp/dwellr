# dwellr

Lifecycle monitoring analytics for supply chain operations.

An R package for measuring the time orders spend in each stage of their
lifecycle, and for deciding what to do about it: classical inventory control,
stage dwell estimation under censoring, baselines, alerting thresholds derived
from cost ratios, reviewer capacity, routing, and policy evaluation.

The book *Lifecycle Monitoring Analytics: A toolkit for continuous supply chain
operations*, by Ariel Rodríguez Herrera (CEO, Ambient Point Corp.), first
edition 2026, works through the methods the package implements and is the place
to read why each one is built the way it is. The package does not depend on the
book.

**Private until the book is published**, at which point it goes public under
the MIT license.

> **Status: 0.0.1.9000.** Every function family is implemented and tested: the
> generator, stage dwell estimation under right-censoring, the classical
> inventory models, fill rate on three dimensions, cohort and modeled
> baselines, thresholds from cost ratios, reviewer capacity and admission,
> routing, the decision path, policy evaluation, records, the serial echelon
> simulator and the observation-cost family. 102 exported functions, 680
> passing assertions. The interface is not yet stable, which is what the
> fourth version component marks.

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
#> [1]  1.81 13.68

# A threshold wants a stall probability per order, which the two-component
# mixture supplies: the chance a visit still open at t belongs to the slow
# population rather than the one moving at the intended pace.
mix <- dwell_mixture(d)
q <- mix$posterior(d$dwell_hours, censored = d$censored)

# The threshold itself is then derived from a cost ratio rather than chosen.
# Reviewer attention priced at 10x a missed stall flags 9% of the population,
# and the crossing translates back to an operating rule in hours.
r <- threshold_from_cost(q, cost_miss = 40, cost_look = 400,
                         hours = d$dwell_hours)
r$probability_threshold
#> [1] 0.9091
c(flagged = r$flagged_n, hours = round(r$hours, 2))
#> flagged   hours
#>  193.00   17.31

# And the rule is checked against the two policies it has to beat.
round(r$expected_cost)
#>   rule   none    all
#>   2571   9892 741878

# With no fitted probability available, the fallback reads the same cost ratio
# off the dwell distribution instead, under the assumption its help documents.
threshold_from_dwell_quantile(d, cost_miss = 40, cost_look = 400)$hours
#> [1] 17.77
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

## Stability

Nothing is declared-but-unimplemented any more. Earlier versions carried stubs
that errored with the chapter they belonged to, on the principle that an
unimplemented method returning a plausible number is worse than one that
refuses; all of them now have implementations and tests.

The version stays below 1.0 because the interface is still free to change. Two
things will be settled before it goes to 1.0: the argument names on the newest
families, and whether the cost conventions should be carried in an object
rather than passed to every call.

## License

MIT. See [LICENSE.md](LICENSE.md).
