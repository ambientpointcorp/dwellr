#' Attribute lead-time demand variance, and safety stock, across stages
#'
#' The quantity a replenishment policy consumes is the variance of lead-time
#' demand,
#'
#' \deqn{\sigma_{LTD}^{2} = \bar{L}\,\sigma_d^{2} + \bar{d}^{\,2}\,\sigma_L^{2},}
#'
#' in which the lead-time term is multiplied by the square of mean demand.
#' @param dec A `lead_time_decomposition` from [lead_time_decompose()], whose
#'   dwells are in hours.
#' @param demand_mean,demand_sd Demand per period, in the period named by
#'   `hours_per_period`.
#' @param service_level Target alpha service level.
#' @param hours_per_period Hours in one demand period. The default of 24 pairs
#'   an hourly dwell measurement with a daily demand rate, which is the usual
#'   combination and the usual place to get the units wrong.
#'
#' @details
#' [lead_time_decompose()] splits \eqn{\sigma_L^{2}} across stages by
#' covariance with the total, so each stage's share can be carried straight
#' into the expression above. What cannot be carried straight through is safety
#' stock, which is proportional to the standard deviation and therefore not
#' additive. Two attributions are reported because they answer different
#' questions.
#'
#' `marginal_units` is the safety stock that would be released if that stage's
#' contribution were removed and every other stage left alone. It is the figure
#' to use when deciding whether to work on a stage, and the marginal figures
#' sum to less than the total, because removing variance from a sum of squares
#' returns less than its share.
#'
#' `proportional_units` allocates the total in proportion to variance share. It
#' sums to the total by construction, which makes it the figure for reporting
#' how much of the safety stock each stage is responsible for carrying, and it
#' overstates what any single improvement would release.
#'
#' @return A data frame of class `variance_contribution`, one row per stage,
#'   with the totals in the `total` attribute.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
#' dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3)
#' @export
dwell_variance_contribution <- function(dec, demand_mean, demand_sd,
                                        service_level = 0.95,
                                        hours_per_period = 24) {
  stopifnot(inherits(dec, "lead_time_decomposition"))
  tot <- attr(dec, "total")
  h2 <- hours_per_period^2

  L_mean <- tot[["mean"]] / hours_per_period
  L_var <- tot[["var"]] / h2
  v_demand <- L_mean * demand_sd^2
  v_lead <- demand_mean^2 * L_var
  v_total <- v_demand + v_lead
  z <- stats::qnorm(service_level)
  ss <- z * sqrt(v_total)

  vk <- demand_mean^2 * dec$cov_with_total / h2
  out <- data.frame(
    stage = dec$stage,
    class = dec$class,
    variance = vk,
    share_of_ltd_variance = vk / v_total,
    marginal_units = z * (sqrt(v_total) - sqrt(pmax(v_total - vk, 0))),
    proportional_units = ss * vk / v_total,
    stringsAsFactors = FALSE)
  out <- out[order(-out$variance), ]
  rownames(out) <- NULL

  structure(out, class = c("variance_contribution", "data.frame"),
            total = c(lead_time_mean = L_mean, lead_time_sd = sqrt(L_var),
                      demand_variance = v_demand, lead_variance = v_lead,
                      ltd_variance = v_total, ltd_sd = sqrt(v_total),
                      safety_stock = ss, service_level = service_level))
}

#' @export
print.variance_contribution <- function(x, digits = 3, ...) {
  tot <- attr(x, "total")
  if (!is.null(tot)) {
    cat(sprintf("<variance_contribution> lead time %.2f +/- %.2f periods\n",
                tot[["lead_time_mean"]], tot[["lead_time_sd"]]))
    cat(sprintf("  sigma_LTD %.2f units; demand term %.1f%%, lead-time term %.1f%%\n",
                tot[["ltd_sd"]],
                100 * tot[["demand_variance"]] / tot[["ltd_variance"]],
                100 * tot[["lead_variance"]] / tot[["ltd_variance"]]))
    cat(sprintf("  safety stock at %.0f%%: %.1f units\n",
                100 * tot[["service_level"]], tot[["safety_stock"]]))
  }
  y <- as.data.frame(x)
  num <- vapply(y, is.numeric, TRUE)
  y[num] <- lapply(y[num], round, digits)
  print(y, ...)
  invisible(x)
}

#' What reducing one stage's dwell variance is worth
#'
#' Converts a reduction in a stage's dwell variance into units of safety stock
#' released and into an annual carrying cost, so that the case for working on a
#' stage can be stated in the same terms as the cost of working on it.
#'
#' Three assumptions sit behind the figure and all three fail in recognizable
#' ways. The service level is held fixed, so the saving is a reduction in stock
#' at constant service and not an improvement in service. Demand is held fixed,
#' so a reduction achieved by losing volume does not count. And the reduction
#' is assumed to persist, which is the assumption that fails most often: a
#' stage improved by attention reverts when the attention moves elsewhere,
#' which is why Chapter 21 evaluates a monitoring policy over a period rather
#' than at a point.
#'
#' @param contrib A `variance_contribution` from
#'   [dwell_variance_contribution()].
#' @param stage Stage to reduce.
#' @param reduction Share of that stage's variance removed, between 0 and 1.
#' @param unit_cost Cost of one unit of stock.
#' @param holding_rate Annual holding cost as a share of unit cost.
#'
#' @return A list with the `units` released, the `annual_value`, and the
#'   safety stock before and after.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' dec <- lead_time_decompose(sim, spec = distribution_stage_spec())
#' vc <- dwell_variance_contribution(dec, demand_mean = 10, demand_sd = 3)
#' variance_reduction_value(vc, "received", 0.5, unit_cost = 120)
#' @export
variance_reduction_value <- function(contrib, stage, reduction = 0.5,
                                     unit_cost, holding_rate = 0.22) {
  stopifnot(inherits(contrib, "variance_contribution"),
            stage %in% contrib$stage,
            reduction >= 0, reduction <= 1, unit_cost > 0)
  tot <- attr(contrib, "total")
  z <- stats::qnorm(tot[["service_level"]])
  vk <- contrib$variance[contrib$stage == stage]
  before <- tot[["safety_stock"]]
  after <- z * sqrt(max(tot[["ltd_variance"]] - reduction * vk, 0))
  units <- before - after
  list(stage = stage, reduction = reduction,
       safety_stock_before = before, safety_stock_after = after,
       units = units, annual_value = units * unit_cost * holding_rate)
}

#' Attention a stage justifies, from what its variance costs
#'
#' A monitoring budget is the point at which the carrying cost released by
#' watching a stage stops exceeding the cost of watching it. Below that point
#' the attention pays; above it the reviews cost more than the stock they free.
#'
#' The figure is a ceiling and not a recommendation. It assumes every review
#' is equally productive, which Chapter 15 does not, and it
#' ignores the service consequences of a stall that nobody catches, which are
#' the larger part of the case for monitoring in most operations and are priced
#' in Chapter 14 instead.
#'
#' @param annual_value Annual carrying cost released, from
#'   [variance_reduction_value()].
#' @param cost_per_review Fully loaded cost of one reviewer look.
#' @param reviews_per_year Reviews the stage is expected to need.
#'
#' @return A list with the `break_even_reviews` the value supports, the
#'   `value_per_review` at the proposed volume, and whether the proposal
#'   `justified` itself.
#'
#' @examples
#' monitoring_budget(annual_value = 186, cost_per_review = 4,
#'                   reviews_per_year = 30)
#' @export
monitoring_budget <- function(annual_value, cost_per_review,
                              reviews_per_year = NULL) {
  stopifnot(annual_value >= 0, cost_per_review > 0)
  out <- list(annual_value = annual_value,
              cost_per_review = cost_per_review,
              break_even_reviews = annual_value / cost_per_review)
  if (!is.null(reviews_per_year)) {
    out$reviews_per_year <- reviews_per_year
    out$annual_cost <- reviews_per_year * cost_per_review
    out$value_per_review <- annual_value / reviews_per_year
    out$justified <- out$annual_cost <= annual_value
  }
  out
}

#' Rank the drivers of a stage dwell
#'
#' Stage attribution says which stage holds the variance. Driver attribution
#' asks which recorded attribute of an order predicts it, which is a different
#' question and a weaker one.
#'
#' The result is a ranking and not a decomposition. Each term's share is the
#' variance of its own fitted contribution over the variance of log dwell, and
#' those shares do not sum to the explained share when the covariates are
#' correlated, because the covariance between fitted contributions belongs to
#' no single term. The maximum pairwise correlation is therefore reported
#' beside the ranking: where it is small the ranking is close to a
#' decomposition, and where it is large the shares should be read as
#' indicative only.
#'
#' The unexplained share is the figure to read first. A stage whose dwell is
#' driven by a mixture that no order attribute predicts (§6.5) will leave
#' almost all of its variance unexplained, and no reweighting of these drivers
#' will change that.
#'
#' @param dwell A data frame from [stage_dwell()] with covariates joined on.
#'   Censored visits are dropped, since the model is fitted on log dwell.
#' @param covariates Column names to rank.
#'
#' @return A list with the `ranking`, the `r_squared`, the `unexplained`
#'   share, and `max_correlation` among numeric covariates.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 2000, seed = 11)
#' d <- merge(stage_dwell(sim, "released"),
#'            sim$orders[, c("order_no", "branch", "lines")], by = "order_no")
#' driver_variance(d, c("branch", "lines"))$ranking
#' @export
driver_variance <- function(dwell, covariates) {
  stopifnot(all(covariates %in% names(dwell)))
  d <- dwell[!dwell$censored & dwell$dwell_hours > 0, ]
  d$.y <- log(d$dwell_hours)
  fit <- stats::lm(stats::as.formula(paste(".y ~", paste(covariates, collapse = " + "))),
                   data = d)
  mm <- stats::model.matrix(fit)[, -1, drop = FALSE]
  b <- stats::coef(fit)[-1]
  share <- vapply(seq_along(b), function(j) stats::var(mm[, j] * b[j]), 0) /
    stats::var(d$.y)
  ranking <- data.frame(term = names(b), share_of_log_variance = share,
                        stringsAsFactors = FALSE)
  ranking <- ranking[order(-ranking$share_of_log_variance), ]
  rownames(ranking) <- NULL

  num <- covariates[vapply(d[covariates], is.numeric, TRUE)]
  mx <- if (length(num) > 1) {
    cm <- abs(stats::cor(d[num]))
    max(cm[lower.tri(cm)])
  } else NA_real_


  list(ranking = ranking, r_squared = summary(fit)$r.squared,
       unexplained = 1 - summary(fit)$r.squared, max_correlation = mx,
       n = nrow(d))
}

#' Service level a policy attains when its lead time was wrong
#'
#' A policy is parameterized with a lead-time mean and standard deviation, and
#' it then operates under whatever the lead time actually is. Where the second
#' standard deviation exceeds the first, the policy carries less stock than its
#' own target requires and attains a service level below it (§7.9).
#'
#' Reporting the gap this way attributes it. A fill rate short of target is
#' usually investigated as a forecast problem, and this function prices the
#' alternative explanation, which is that \eqn{\sigma_L} was never measured.
#'
#' @param demand_mean,demand_sd Demand per period.
#' @param assumed_mean,assumed_sd The lead time the policy was sized with.
#' @param actual_mean,actual_sd The lead time measured.
#' @param target Target alpha service level the policy was sized for.
#'
#' @return A list with the safety stock the policy carries, the target and
#'   attained alpha and beta, and the stock the measured inputs require.
#'
#' @examples
#' service_level_gap(demand_mean = 10, demand_sd = 3,
#'                   assumed_mean = 2, assumed_sd = 0.5,
#'                   actual_mean = 2.02, actual_sd = 4.37)
#' @export
service_level_gap <- function(demand_mean, demand_sd,
                              assumed_mean, assumed_sd,
                              actual_mean, actual_sd, target = 0.95) {
  stopifnot(target > 0, target < 1)
  z <- stats::qnorm(target)
  sig <- function(Lm, Ls) sqrt(Lm * demand_sd^2 + demand_mean^2 * Ls^2)
  s_assumed <- sig(assumed_mean, assumed_sd)
  s_actual <- sig(actual_mean, actual_sd)
  carried <- z * s_assumed

  # Standard normal loss function, which turns a safety factor into an
  # expected shortfall per cycle and hence into a fill rate.
  loss <- function(k) stats::dnorm(k) - k * stats::pnorm(k, lower.tail = FALSE)
  k_att <- carried / s_actual

  list(safety_stock_carried = carried,
       sigma_assumed = s_assumed, sigma_actual = s_actual,
       alpha_target = target,
       alpha_attained = stats::pnorm(k_att),
       beta_target = 1 - s_assumed * loss(z) / (demand_mean * assumed_mean),
       beta_attained = 1 - s_actual * loss(k_att) / (demand_mean * actual_mean),
       safety_stock_required = z * s_actual,
       shortfall_multiple = s_actual / s_assumed)
}

#' Lead-time demand by convolution, without the normal assumption
#'
#' The closed form for \eqn{\sigma_{LTD}} is a statement about the first two
#' moments, and turning it into a safety stock multiplies it by a normal
#' quantile. On a dwell distribution as dispersed as the ones Part II measures,
#' the normal that matches those moments puts substantial probability on
#' negative demand, so its shape is wrong even where its upper quantile is
#' close.
#'
#' This function convolves the measured lead-time distribution with demand
#' directly, by drawing a lead time from the observed set and then drawing
#' demand over it. It makes no assumption about the shape of the lead time and
#' the normal assumption about demand over a fixed horizon only.
#'
#' @param lead_times Observed lead times, one per completed instance, in the
#'   same period as the demand rate.
#' @param demand_mean,demand_sd Demand per period.
#' @param service_level Quantile to report.
#' @param n Draws.
#'
#' @return A list with the mean and standard deviation of lead-time demand, the
#'   quantile at `service_level`, the implied `safety_stock`, and the
#'   `normal_safety_stock` the closed form would give, with
#'   `normal_mass_below_zero` as a check on the approximation.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1000, seed = 11)
#' d <- stage_dwell(sim)
#' done <- sim$orders$order_no[sim$orders$stage == "shipped"]
#' L <- tapply(d$dwell_hours[d$order_no %in% done],
#'             d$order_no[d$order_no %in% done], sum) / 24
#' lead_time_demand_empirical(L, 10, 3, n = 20000)
#' @export
lead_time_demand_empirical <- function(lead_times, demand_mean, demand_sd,
                                       service_level = 0.95, n = 2e5) {
  lead_times <- lead_times[is.finite(lead_times) & lead_times > 0]
  stopifnot(length(lead_times) > 0)
  L <- sample(lead_times, n, replace = TRUE)
  D <- stats::rnorm(n, demand_mean * L, demand_sd * sqrt(L))
  q <- stats::quantile(D, service_level, names = FALSE)
  mu <- demand_mean * mean(lead_times)
  s_norm <- sqrt(mean(lead_times) * demand_sd^2 +
                   demand_mean^2 * stats::var(lead_times))
  list(mean = mean(D), sd = stats::sd(D), quantile = q,
       safety_stock = q - mu,
       normal_safety_stock = stats::qnorm(service_level) * s_norm,
       normal_mass_below_zero = stats::pnorm(0, mu, s_norm))
}
