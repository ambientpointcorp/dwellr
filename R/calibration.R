# Chapter 9 models. A probability that ranks well and reports the wrong scale
# is useless to a threshold, because the threshold compares against a number
# (§9.14). These functions measure that error and repair it.

#' Measure how far a predicted probability is from the observed rate
#'
#' A ranking metric cannot see a calibration error: a monotone transform of a
#' score leaves AUC untouched and moves every probability. So calibration has
#' to be measured separately, by binning the predictions and comparing the
#' mean prediction in each bin against the rate actually observed there.
#'
#' Three summaries come out of it. The *expected calibration error* is the
#' bin-weighted mean absolute gap, which is the single number to watch. The
#' *Brier score* is the mean squared error of the probabilities, which rewards
#' calibration and discrimination together. And the gap between the overall
#' mean prediction and the overall rate is the crudest check, worth reading
#' first because a model that fails it fails everywhere.
#'
#' @param p Predicted probabilities.
#' @param outcome Observed outcomes, logical or 0/1.
#' @param bins Number of equal-count bins.
#'
#' @return A list with the `reliability` table, `ece`, `brier`,
#'   `mean_predicted` and `observed`.
#'
#' @examples
#' set.seed(1)
#' truth <- runif(500) < 0.3
#' calibrate_reliability(runif(500) * 0.6, truth, bins = 5)$ece
#' @export
calibrate_reliability <- function(p, outcome, bins = 10) {
  outcome <- as.numeric(outcome)
  stopifnot(length(p) == length(outcome), length(p) > 0,
            all(p >= 0 & p <= 1), all(outcome %in% c(0, 1)))
  bins <- min(as.integer(bins), length(unique(p)))

  cuts <- stats::quantile(p, seq(0, 1, length.out = bins + 1), names = FALSE)
  cuts <- unique(cuts)
  grp  <- if (length(cuts) < 2) factor(rep(1, length(p))) else
    cut(p, cuts, include.lowest = TRUE)

  tab <- data.frame(
    bin = levels(grp),
    n = as.integer(table(grp)),
    predicted = round(as.numeric(tapply(p, grp, mean)), 4),
    observed = round(as.numeric(tapply(outcome, grp, mean)), 4),
    stringsAsFactors = FALSE)
  tab <- tab[!is.na(tab$predicted), ]
  tab$gap <- round(tab$predicted - tab$observed, 4)
  rownames(tab) <- NULL

  list(reliability = tab,
       ece = sum(tab$n * abs(tab$gap)) / sum(tab$n),
       brier = mean((p - outcome)^2),
       mean_predicted = mean(p),
       observed = mean(outcome))
}

#' Repair a probability scale
#'
#' Fits a map from predicted probability to observed rate, which is what
#' Chapter 14 needs before a score can be compared against a cost ratio
#' (§9.14).
#'
#' *Isotonic regression* fits a monotone step function. It is flexible and it
#' ties predictions the raw score separated, so it can cost a little
#' discrimination: the common claim that isotonic calibration cannot change the
#' ranking holds only up to those ties. *Platt scaling* fits a two-parameter
#' logistic on the log odds, which is strictly increasing and therefore cannot
#' reorder anything, and is the better choice when the calibration set is
#' small.
#'
#' Fit this on data the model did not train on and the evaluation did not use.
#' A calibration fitted on the evaluation period reports a flattering figure
#' for the reason §9.12 gives about any quantity estimated where it is tested.
#'
#' @param p Predicted probabilities from the model being repaired.
#' @param outcome Observed outcomes on the same rows, logical or 0/1.
#' @param method `"isotonic"` or `"platt"`.
#' @param eps Clamp applied before taking log odds, so that a prediction of
#'   exactly zero or one does not produce an infinite logit.
#'
#' @return An object of class `dwellr_calibration`, with a `predict()` method
#'   that applies the map to new predictions. Carries the method, the number of
#'   rows it was fitted on, and the reliability of the fitting set before and
#'   after, so that a map can be versioned with the model as §9.20 requires.
#'
#' @examples
#' set.seed(1)
#' truth <- runif(2000) < 0.2
#' raw <- pmin(1, pmax(0, 0.2 + 0.6 * truth + rnorm(2000, 0, 0.2)))
#' inflated <- raw^0.5                      # ranks the same, scale is wrong
#' cal <- calibrate_probability(inflated, truth, method = "platt")
#' c(before = mean(inflated), after = mean(predict(cal, inflated)),
#'   observed = mean(truth))
#' @export
calibrate_probability <- function(p, outcome,
                                  method = c("isotonic", "platt"),
                                  eps = 1e-6) {
  method <- match.arg(method)
  outcome <- as.numeric(outcome)
  stopifnot(length(p) == length(outcome), length(p) > 1,
            all(p >= 0 & p <= 1), all(outcome %in% c(0, 1)),
            eps > 0, eps < 0.5)
  if (length(unique(outcome)) < 2)
    stop("calibration needs both outcomes present; the fitting set has one.",
         call. = FALSE)

  logit <- function(x) log(pmin(1 - eps, pmax(eps, x)) /
                           (1 - pmin(1 - eps, pmax(eps, x))))

  if (method == "isotonic") {
    o  <- order(p)
    ir <- stats::isoreg(p[o], outcome[o])
    # Interpolate between the isotonic knots, clamping outside the fitted
    # range. Flat stretches of the fit are where the ties come from.
    map <- stats::approxfun(ir$x, ir$yf, method = "linear", rule = 2,
                            ties = "ordered")
    fit <- NULL
  } else {
    fit <- stats::glm(outcome ~ logit(p), family = stats::binomial())
    map <- function(x) as.numeric(stats::predict(
      fit, newdata = data.frame(p = x), type = "response"))
  }

  out <- list(method = method, map = map, model = fit, n = length(p),
              eps = eps,
              before = calibrate_reliability(p, outcome),
              after = calibrate_reliability(pmin(1, pmax(0, map(p))), outcome))
  class(out) <- "dwellr_calibration"
  out
}

#' @export
predict.dwellr_calibration <- function(object, newdata, ...) {
  stopifnot(is.numeric(newdata), all(newdata >= 0 & newdata <= 1,
                                     na.rm = TRUE))
  pmin(1, pmax(0, object$map(newdata)))
}

#' @export
print.dwellr_calibration <- function(x, ...) {
  cat(sprintf("Probability calibration (%s), fitted on %d rows\n",
              x$method, x$n))
  cat(sprintf("  mean predicted  %.4f -> %.4f  (observed %.4f)\n",
              x$before$mean_predicted, x$after$mean_predicted,
              x$before$observed))
  cat(sprintf("  expected calibration error  %.4f -> %.4f\n",
              x$before$ece, x$after$ece))
  cat(sprintf("  Brier score                 %.4f -> %.4f\n",
              x$before$brier, x$after$brier))
  invisible(x)
}
