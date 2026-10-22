# Chapter 9 models. The value here is the protocol rather than the estimator:
# a split that respects time, metrics that match the decision, and a
# calibration check. The estimator itself is a few lines either way, and the
# chapter prints those lines in full because choosing one is the reader's job.

#' Fit a model for a stage outcome under the book's validation protocol
#'
#' Three choices decide whether a fitted model means anything, and this
#' function makes all three explicitly rather than by default.
#'
#' The split respects time where a timestamp is supplied: fit on the earlier
#' period and evaluate on the later one. A random split lets a model learn from
#' the period it is tested on and reports a figure answering a question nobody
#' asks (§9.12).
#'
#' The metrics are the ones the decision consumes. Discrimination is reported
#' as AUC, which is what a ranking needs, alongside precision at a stated
#' review capacity, which is what Chapter 15 actually acts on, and
#' the calibration summaries of [calibrate_reliability()], since a threshold
#' compares a probability against a number.
#'
#' The estimator is `"logistic"` by default because it needs nothing beyond
#' base R and because a model that cannot beat it is not worth operating.
#' `"forest"` requires `ranger`.
#'
#' @param data A data frame with one row per instance.
#' @param target Name of the outcome column: logical, 0/1, or numeric when
#'   `horizon` is supplied.
#' @param features Names of the predictor columns.
#' @param at Optional name of a timestamp column. Supplying it makes the split
#'   chronological; omitting it makes it random and says so in the result.
#' @param horizon Threshold applied to a numeric `target` to form the outcome,
#'   as in "breached 120 hours".
#' @param prop Share of rows used for fitting.
#' @param capacity Number of instances a reviewer can work in the evaluation
#'   window, for precision at capacity. Defaults to a tenth of the holdout.
#' @param method `"logistic"` or `"forest"`.
#' @param seed Seed for the random split and the forest.
#' @param ... Passed to `ranger::ranger()`.
#'
#' @return A list with the `fit`, the holdout `predictions` and `outcome`, a
#'   one-row `metrics` frame, the `split` description, and the `calibration`
#'   summary of the holdout.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 600, seed = 11)
#' d <- stage_dwell(sim)
#' total <- tapply(d$dwell_hours, d$order_no, sum)
#' o <- sim$orders
#' o$lead_hours <- as.numeric(total[o$order_no])
#' o$log_value <- log(o$value)
#' m <- fit_dwell_model(o, target = "lead_hours",
#'                      features = c("special_order", "log_value", "lines"),
#'                      at = "entered_at", horizon = 120)
#' m$metrics
#' @export
fit_dwell_model <- function(data, target, features, at = NULL,
                            horizon = NULL, prop = 0.6, capacity = NULL,
                            method = c("logistic", "forest"), seed = 1, ...) {
  method <- match.arg(method)
  stopifnot(is.data.frame(data), nrow(data) > 10,
            length(target) == 1, target %in% names(data),
            length(features) >= 1, all(features %in% names(data)),
            prop > 0, prop < 1)
  if (!is.null(at)) stopifnot(length(at) == 1, at %in% names(data))

  y <- data[[target]]
  if (!is.null(horizon)) {
    stopifnot(is.numeric(y))
    y <- y > horizon
  }
  y <- as.integer(as.logical(y))
  if (anyNA(y))
    stop("the outcome has ", sum(is.na(y)), " missing values; decide what ",
         "they mean before fitting.", call. = FALSE)
  if (length(unique(y)) < 2)
    stop("the outcome takes one value; there is nothing to fit.",
         call. = FALSE)

  n <- nrow(data)
  k <- max(1L, floor(prop * n))
  if (is.null(at)) {
    set.seed(seed)
    idx <- sample(n)
    split_kind <- "random"
    boundary <- NA_character_
  } else {
    idx <- order(data[[at]])
    split_kind <- "chronological"
    boundary <- as.character(sort(data[[at]])[k])
  }
  tr <- idx[seq_len(k)]
  te <- idx[-seq_len(k)]
  if (length(te) < 5)
    stop("the holdout has ", length(te), " rows; lower `prop`.", call. = FALSE)

  frame <- function(rows) {
    out <- data[rows, features, drop = FALSE]
    chr <- vapply(out, is.character, TRUE)
    out[chr] <- lapply(out[chr], factor)
    out
  }
  train <- frame(tr); test <- frame(te)
  train$.y <- y[tr]

  if (method == "forest") {
    if (!requireNamespace("ranger", quietly = TRUE))
      stop('method = "forest" needs the ranger package; install it or use ',
           'method = "logistic".', call. = FALSE)
    # ranger needs a factor outcome to return named probability columns; given
    # an integer it fits a probability forest whose predictions have no
    # dimnames, and the column cannot be selected by name.
    train$.y <- factor(train$.y, levels = c(0, 1))
    fit <- ranger::ranger(stats::as.formula(".y ~ ."), data = train,
                          probability = TRUE, seed = seed, ...)
    p <- stats::predict(fit, test)$predictions[, "1"]
  } else {
    fit <- stats::glm(stats::as.formula(".y ~ ."), data = train,
                      family = stats::binomial())
    p <- as.numeric(stats::predict(fit, newdata = test, type = "response"))
  }

  yte <- y[te]
  cap <- if (is.null(capacity)) max(1L, floor(0.1 * length(yte))) else
    min(as.integer(capacity), length(yte))
  top <- order(-p)[seq_len(cap)]

  r <- rank(p); n1 <- sum(yte); n0 <- length(yte) - n1
  auc <- if (n1 > 0 && n0 > 0)
    (sum(r[yte == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0) else NA_real_

  cal <- calibrate_reliability(p, yte)
  metrics <- data.frame(
    n_train = length(tr), n_test = length(te),
    base_rate = round(mean(yte), 4),
    auc = round(auc, 4),
    capacity = cap,
    precision_at_capacity = round(mean(yte[top]), 4),
    recall_at_capacity = round(sum(yte[top]) / max(1, sum(yte)), 4),
    mean_predicted = round(cal$mean_predicted, 4),
    ece = round(cal$ece, 4), brier = round(cal$brier, 4))

  list(fit = fit, method = method,
       predictions = p, outcome = yte, rows = te,
       metrics = metrics,
       split = list(kind = split_kind, prop = prop, boundary = boundary),
       calibration = cal)
}
