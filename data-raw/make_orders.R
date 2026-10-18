# Build the package's built-in dataset.
#
# Deliberately one call to the package's own generator, at a size that keeps
# the .rda small enough to ship and the examples fast. The full corpus lives in
# dwellr-examples and comes from the same function at a larger n -- so there is
# one implementation and the two cannot diverge.
#
#   Rscript data-raw/make_orders.R

library(dwellr)
orders <- simulate_lifecycles(n = 2000, seed = 42)
print(orders)
save(orders, file = "data/orders.rda", compress = "xz", version = 3)
cat(sprintf("data/orders.rda: %.0f KB\n", file.size("data/orders.rda") / 1024))
