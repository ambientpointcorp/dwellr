# Chapter 4 models. The benchmark forecasts, the accuracy metrics used to
# compare them, rolling-origin evaluation, and the bridge from a demand
# forecast to a safety-stock figure.
#
# ETS and ARIMA are not reimplemented here; the chapter uses the forecast
# package for those, which is well tested and widely used. What this file adds
# is the material the book needs and that package does not supply: a single
# consistent accuracy table, and the lead-time demand calculation of Section
# 4.11 that Part II then takes apart.

#' Benchmark forecasts
#'
#' The four methods any more sophisticated forecast must beat (Section 4.2).
#' Each returns a numeric vector of length `h`.
#'
#' * `fc_mean` — the mean of the history, appropriate when the series has no
#'   trend or seasonality.
#' * `fc_naive` — the last observation, carried forward. Optimal for a random
#'   walk.
#' * `fc_drift` — the last observation extended along the average slope of the
#'   history, equivalent to drawing a line through the first and last points.
#' * `fc_seasonal_naive` — the observation from the same point in the previous
#'   season. Useful only where seasonality is large relative to noise.
#'
#' @param y Observed series.
#' @param h Forecast horizon.
#' @param period Season length, for `fc_seasonal_naive`.
#' @return A numeric vector of length `h`.
#' @examples
#' y <- c(10, 12, 11, 14, 13, 16)
#' fc_naive(y, 3)
#' fc_drift(y, 3)
#' @name fc_benchmarks
NULL

#' @rdname fc_benchmarks
#' @export
fc_mean <- function(y, h) rep(mean(y), h)

#' @rdname fc_benchmarks
#' @export
fc_naive <- function(y, h) rep(y[length(y)], h)

#' @rdname fc_benchmarks
#' @export
fc_drift <- function(y, h) {
  n <- length(y)
  stopifnot(n >= 2)
  y[n] + seq_len(h) * ((y[n] - y[1]) / (n - 1))
}

#' @rdname fc_benchmarks
#' @export
fc_seasonal_naive <- function(y, h, period) {
  n <- length(y)
  stopifnot(n >= period)
  y[n - period + ((seq_len(h) - 1) %% period) + 1]
}

#' Forecast accuracy
#'
#' The standard error measures, computed together so that a comparison is made
#' on one basis (Section 4.3).
#'
#' * `ME` — mean error. Measures bias; a method can have near-zero ME and large
#'   errors.
#' * `RMSE`, `MAE` — scale-dependent. Comparable across methods on one series,
#'   not across series.
#' * `MAPE` — percentage. Comparable across series, undefined at zero actuals
#'   and asymmetric between over- and under-forecasting.
#' * `MASE` — scaled by the in-sample error of a naive forecast. Defined at
#'   zero actuals, symmetric, and comparable across series. A value below 1
#'   beats the naive benchmark on the training data.
#'
#' @param forecast Forecast values.
#' @param actual Observed values, the same length.
#' @param train The training series, used to scale `MASE`.
#' @param scale_lag Lag of the naive forecast used for scaling. Use 1 unless
#'   comparing seasonal methods only; mixing lags in one table makes `MASE`
#'   incomparable between rows.
#' @return A named numeric vector.
#' @examples
#' y <- c(10, 12, 11, 14, 13, 16); a <- c(15, 17, 16)
#' fc_accuracy(fc_naive(y, 3), a, y)
#' @export
fc_accuracy <- function(forecast, actual, train, scale_lag = 1) {
  stopifnot(length(forecast) == length(actual))
  e <- actual - forecast
  scale <- mean(abs(diff(train, lag = scale_lag)))
  c(ME = mean(e),
    RMSE = sqrt(mean(e^2)),
    MAE = mean(abs(e)),
    MAPE = 100 * mean(abs(e / actual)),
    MASE = mean(abs(e)) / scale)
}

#' Rolling-origin evaluation
#'
#' Evaluates a forecasting function at many successive origins rather than on
#' one train/test split, which gives a more stable estimate of accuracy and
#' avoids the result depending on where the single split happened to fall
#' (Section 4.3).
#'
#' @param y The full series.
#' @param fn A function of `(history, h)` returning `h` forecasts.
#' @param h Forecast horizon evaluated at each origin.
#' @param n_origins Number of origins, taken from the end of the series.
#' @return A named numeric vector of `RMSE` and `MAE` over all origins.
#' @examples
#' set.seed(1)
#' y <- cumsum(rnorm(60, 0.2))
#' fc_rolling_origin(y, fc_naive, h = 1, n_origins = 20)
#' @export
fc_rolling_origin <- function(y, fn, h = 1, n_origins = 20) {
  n <- length(y)
  stopifnot(n_origins + h <= n)
  errs <- numeric(0)
  for (i in seq_len(n_origins)) {
    cut <- n - n_origins - h + i
    f <- fn(y[seq_len(cut)], h)
    errs <- c(errs, y[(cut + 1):(cut + h)] - f)
  }
  c(RMSE = sqrt(mean(errs^2)), MAE = mean(abs(errs)))
}

#' Lead-time demand
#'
#' The distribution a reorder point is set against: total demand over a lead
#' time that is itself uncertain. With demand and lead time independent,
#'
#' \deqn{E[D] = \bar{L}\,\bar{d}, \qquad
#'       \mathrm{Var}[D] = \bar{L}\,\sigma_d^{2} + \bar{d}^{\,2}\,\sigma_L^{2}.}
#'
#' The second variance term is quadratic in mean demand, so for a fast-moving
#' item it dominates the first even when the lead time looks reasonably
#' consistent. Section 4.11 works an instance; Part II takes the term apart
#' and asks where it comes from.
#'
#' @param demand_mean,demand_sd Per-period demand.
#' @param lead_time_mean,lead_time_sd Lead time, in periods. A standard
#'   deviation of zero gives the constant-lead-time case of Chapter 2.
#' @return A list with `mean`, `variance`, `sd`, and `demand_share` and
#'   `lead_time_share` — the two components as fractions of the variance.
#' @examples
#' lead_time_demand(demand_mean = 10, demand_sd = 3,
#'                  lead_time_mean = 10, lead_time_sd = 2)
#' @export
lead_time_demand <- function(demand_mean, demand_sd,
                             lead_time_mean, lead_time_sd = 0) {
  stopifnot(demand_mean >= 0, demand_sd >= 0,
            lead_time_mean > 0, lead_time_sd >= 0)
  v_d <- lead_time_mean * demand_sd^2
  v_L <- demand_mean^2 * lead_time_sd^2
  v <- v_d + v_L
  list(mean = lead_time_mean * demand_mean,
       variance = v, sd = sqrt(v),
       demand_share = if (v > 0) v_d / v else NA_real_,
       lead_time_share = if (v > 0) v_L / v else NA_real_)
}

#' Safety stock for a service level
#'
#' The stock held above expected lead-time demand so that a stockout occurs no
#' more often than the target allows. For a normal lead-time demand,
#'
#' \deqn{SS = z_{\alpha}\,\sigma_{LTD},}
#'
#' with \eqn{z_\alpha} the standard normal quantile at the target. Supply
#' either `service_level` directly or a pair of costs, in which case the
#' critical fractile of Section 3.1 supplies it.
#'
#' @param ltd A list from [lead_time_demand()], or a numeric standard
#'   deviation of lead-time demand.
#' @param service_level Target alpha service level.
#' @param holding_cost,shortage_cost Supply instead of `service_level` to use
#'   the critical fractile \eqn{b/(h+b)}.
#' @return The safety stock, in units.
#' @examples
#' ltd <- lead_time_demand(10, 3, 10, 2)
#' safety_stock(ltd, service_level = 0.95)
#' safety_stock(ltd, holding_cost = 1, shortage_cost = 19)
#' @export
safety_stock <- function(ltd, service_level = NULL,
                         holding_cost = NULL, shortage_cost = NULL) {
  sd_ltd <- if (is.list(ltd)) ltd$sd else ltd
  if (is.null(service_level)) {
    stopifnot(!is.null(holding_cost), !is.null(shortage_cost))
    service_level <- shortage_cost / (holding_cost + shortage_cost)
  }
  stopifnot(service_level > 0, service_level < 1)
  stats::qnorm(service_level) * sd_ltd
}
