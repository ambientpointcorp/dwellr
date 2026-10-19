#' Re-cut the observation window on an event log
#'
#' Returns the same population as it would have appeared at an earlier instant:
#' events after `as_of` are discarded, and each instance's stage is whatever it
#' had reached by then. Instances that had not yet been created are dropped.
#'
#' This is what makes the censoring bias of Chapter 6 measurable rather than
#' argued. The same corpus, observed at a sequence of cutoffs, gives a sequence
#' of in-flight sets, and the naive average over completed visits can be
#' compared against an estimator that uses the censored ones at each.
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param as_of Observation instant, a `POSIXct` at or before `sim$as_of`.
#'
#' @return A `lifecycle_sim` observed at `as_of`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 500, seed = 1)
#' earlier <- observe_at(sim, sim$as_of - 30 * 24 * 3600)
#' earlier
#' @export
observe_at <- function(sim, as_of) {
  stopifnot(inherits(as_of, "POSIXct"))
  ev <- sim$events[sim$events$occurred_at <= as_of, ]
  if (!nrow(ev)) stop("no events at or before as_of", call. = FALSE)
  ev <- ev[order(ev$order_no, ev$occurred_at), ]

  last <- !duplicated(ev$order_no, fromLast = TRUE)
  now <- data.frame(order_no = ev$order_no[last], stage = ev$stage[last],
                    stringsAsFactors = FALSE)

  o <- sim$orders[match(now$order_no, sim$orders$order_no), ]
  o$stage <- now$stage
  o$is_terminal <- o$stage %in% c("shipped", "cancelled")
  o$finished_at[!o$is_terminal] <- as.POSIXct(NA, tz = "UTC")
  rownames(o) <- NULL
  rownames(ev) <- NULL

  structure(list(orders = o, events = ev, as_of = as_of),
            class = "lifecycle_sim")
}

#' Naive and censoring-aware averages of a stage dwell
#'
#' The average dwell computed over completed visits only is the figure an
#' operational report usually carries, and it is biased low. The visits it
#' omits are the ones still open, which are the longest waits in the stage, so
#' the omission is not random with respect to the quantity being averaged
#' (§6.2).
#'
#' The censoring-aware figure is a **restricted** mean: the area under the
#' Kaplan-Meier curve out to `restrict_to`. Restriction is not a convenience.
#' A heavy-tailed dwell observed over a finite window leaves the curve above
#' zero at the last event, so the unrestricted mean is not identified from the
#' data and any single number claiming to be it has made an assumption about
#' the tail. Reporting the horizon alongside the mean is what makes the figure
#' checkable.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param restrict_to Horizon in hours for the restricted mean. Defaults to the
#'   largest observed dwell, which is the longest horizon the data supports.
#'
#' @return A list with `n`, `censored`, the `naive` mean over completed visits,
#'   the `restricted` mean from the Kaplan-Meier curve, the `horizon` used, and
#'   the `bias` of the naive figure as a share of the restricted one.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' unlist(dwell_mean(stage_dwell(sim, "awaiting_supply")))
#' @export
dwell_mean <- function(dwell, restrict_to = NULL) {
  stopifnot(nrow(dwell) > 0)
  if (is.null(restrict_to)) restrict_to <- max(dwell$dwell_hours)
  fit <- dwell_survival(dwell)

  # Area under the step function out to the horizon, which is the restricted
  # mean. The curve is held at its last value where it stops before the
  # horizon, which is the only assumption the estimator needs.
  t <- c(0, fit$time[fit$time <= restrict_to], restrict_to)
  s <- c(1, fit$surv[fit$time <= restrict_to])
  s <- c(s, s[length(s)])
  area <- sum(diff(t) * s[-length(s)])

  naive <- mean(dwell$dwell_hours[!dwell$censored])
  list(n = nrow(dwell), censored = sum(dwell$censored),
       naive = naive, restricted = area, horizon = restrict_to,
       bias = naive / area - 1)
}

#' Hazard of leaving a stage, by interval
#'
#' The survival curve answers how many instances are still waiting. The hazard
#' answers the question an operator actually asks, which is whether an instance
#' that has already waited this long is about to move: of those still in the
#' stage at the start of an interval, what share leaves during it (§6.3).
#'
#' A hazard that falls with elapsed time is the signature of a mixed
#' population, because the fast instances leave early and what remains is
#' progressively more stuck. A hazard that rises is the signature of a deadline
#' or a scheduled sweep.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param breaks Interval boundaries in hours.
#'
#' @return A data frame with one row per interval: `from`, `to`, `at_risk`,
#'   `left`, the `hazard` over the interval, and `per_hour`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' dwell_hazard(stage_dwell(sim, "released"), c(0, 4, 8, 24, 72, Inf))
#' @export
dwell_hazard <- function(dwell, breaks = c(0, 1, 2, 4, 8, 24, 48, 72, Inf)) {
  stopifnot(length(breaks) >= 2, !is.unsorted(breaks))
  t <- dwell$dwell_hours
  ev <- !dwell$censored
  out <- lapply(seq_len(length(breaks) - 1), function(i) {
    lo <- breaks[i]; hi <- breaks[i + 1]
    at_risk <- sum(t >= lo)
    left <- sum(ev & t >= lo & t < hi)
    width <- min(hi, max(t)) - lo
    data.frame(from = lo, to = hi, at_risk = at_risk, left = left,
               hazard = if (at_risk) left / at_risk else NA_real_,
               per_hour = if (at_risk && width > 0)
                 left / at_risk / width else NA_real_)
  })
  do.call(rbind, out)
}

#' How each visit to a stage ended
#'
#' An instance can leave a stage by advancing, by going backwards, or by being
#' cancelled, and those exits have different causes and different remedies.
#' Treating them as one event confounds the hazard, because the estimator is
#' then answering a question about a mixture of outcomes nobody asked about
#' (§6.8).
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param stage Stage to restrict to.
#' @param spec Optional [stage_spec()]. When supplied, an exit is classed
#'   `"backward"` if the stage it moves to appears earlier in the declared
#'   stage order, and `"forward"` otherwise.
#'
#' @return The [stage_dwell()] data frame for that stage with two extra
#'   columns: `exit_to`, the stage moved to, and `exit_type`, one of
#'   `"forward"`, `"backward"`, `"cancelled"` or `"censored"`.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' x <- stage_exit_type(sim, "released", distribution_stage_spec())
#' table(x$exit_type)
#' @export
stage_exit_type <- function(sim, stage, spec = NULL) {
  ev <- sim$events[order(sim$events$order_no, sim$events$occurred_at), ]
  nr <- nrow(ev)
  same <- c(ev$order_no[-1] == ev$order_no[-nr], FALSE)
  nxt <- rep(NA_character_, nr)
  nxt[same] <- ev$stage[c(FALSE, same[-nr])]

  key <- paste(ev$order_no, ev$stage, format(ev$occurred_at, "%Y%m%d%H%M%OS3"))
  d <- stage_dwell(sim, stage)
  dkey <- paste(d$order_no, d$stage, format(d$entered_at, "%Y%m%d%H%M%OS3"))
  d$exit_to <- nxt[match(dkey, key)]

  order_of <- if (is.null(spec)) NULL else stats::setNames(
    seq_along(spec$stages), spec$stages)
  d$exit_type <- ifelse(
    d$censored | is.na(d$exit_to), "censored",
    ifelse(d$exit_to == "cancelled", "cancelled",
           if (is.null(order_of)) "forward"
           else ifelse(order_of[d$exit_to] < order_of[d$stage],
                       "backward", "forward")))
  d
}

#' Backlog in a stage at the instant an instance entered it
#'
#' A queueing dwell depends on how much other work was present on arrival,
#' whereas a handling dwell depends on the instance itself (§5.5). The
#' distinction is testable once the backlog is available as a covariate, which
#' is what this function supplies.
#'
#' The count is of instances already in the stage at that instant and not yet
#' out of it, which is the quantity a queue discipline acts on.
#'
#' @param sim A `lifecycle_sim` from [simulate_lifecycles()].
#' @param stage Stage to restrict to.
#'
#' @return The [stage_dwell()] data frame for that stage with a `backlog`
#'   column.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 400, seed = 1)
#' summary(stage_backlog(sim, "released")$backlog)
#' @export
stage_backlog <- function(sim, stage) {
  d <- stage_dwell(sim, stage)
  d <- d[order(d$entered_at), ]
  entry <- as.numeric(d$entered_at)
  exit <- as.numeric(d$left_at)
  # Instances already in the stage = those that entered at or before this
  # instant, less those that had already left it. Counting that with a pair of
  # sorted searches rather than a comparison against every other visit keeps
  # this linear in the log of the population rather than quadratic in it: the
  # released stage of the book's corpus holds over five thousand visits.
  entered_by <- findInterval(entry, sort(entry))
  left_by <- findInterval(entry, sort(exit))
  d$backlog <- as.integer(entered_by - left_by - 1L)
  rownames(d) <- NULL
  d
}

#' Fit parametric families to a stage dwell
#'
#' The Kaplan-Meier estimate makes no assumption about shape, which is its
#' merit, and it stops at the last observed event, which is its limit. A
#' parametric fit extrapolates past that point and reduces the distribution to
#' two or three numbers that can be carried into a policy, at the price of an
#' assumption that has to be checked rather than adopted (§6.4).
#'
#' The families differ in the shape of hazard they can represent, and that is
#' the property to choose on. An exponential fit asserts a constant hazard, so
#' that an order which has waited a day is no more and no less likely to move
#' in the next hour than one that arrived a minute ago. A Weibull fit allows
#' the hazard to rise or fall monotonically, with its shape parameter below one
#' meaning a falling hazard. A log-normal fit allows a hazard that rises and
#' then falls.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param dists Families to fit, passed to [survival::survreg()].
#'
#' @return A data frame with one row per family: the `dist`, its
#'   log-likelihood, the number of parameters, `AIC`, and `shape` where the
#'   family has one, with `hazard` naming the shape of hazard implied.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 800, seed = 1)
#' dwell_parametric(stage_dwell(sim, "released"))
#' @export
dwell_parametric <- function(dwell,
                             dists = c("exponential", "weibull", "lognormal")) {
  s <- survival::Surv(pmax(dwell$dwell_hours, 1e-6),
                      as.integer(!dwell$censored))
  rows <- lapply(dists, function(dd) {
    fit <- survival::survreg(s ~ 1, dist = dd)
    k <- length(fit$coefficients) + (dd != "exponential")
    shape <- if (dd == "weibull") 1 / fit$scale
             else if (dd == "lognormal") fit$scale else NA_real_
    hz <- if (dd == "exponential") "constant"
          else if (dd == "weibull")
            if (1 / fit$scale < 1) "falling" else "rising"
          else "rises then falls"
    data.frame(dist = dd, logLik = as.numeric(fit$loglik[2]), params = k,
               AIC = -2 * as.numeric(fit$loglik[2]) + 2 * k,
               shape = shape, hazard = hz, stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$AIC), ]
  rownames(out) <- NULL
  out
}

#' Fit a two-component mixture to a stage dwell
#'
#' Most stages hold two populations rather than one: orders that move through
#' at the pace the process intends, and orders that have stopped. A single
#' distribution fitted to the pool describes neither of them, and a single
#' quantile taken from it is too late to catch the stalls and too early to
#' leave the normal work alone (§6.5).
#'
#' The fit is by expectation-maximization, and it uses the censored visits
#' rather than discarding them: a visit still open at `t` contributes each
#' component's survival at `t` to the responsibility, in place of its density.
#' Dropping those visits would reintroduce exactly the bias of §6.2 into the
#' estimate of the component that matters most, since an open visit is
#' overwhelmingly likely to belong to the stuck population.
#'
#' @param dwell A data frame from [stage_dwell()].
#' @param families Length-two character vector, each `"exp"` or `"lnorm"`,
#'   giving the family of the fast and the slow component.
#' @param iter Maximum EM iterations.
#' @param tol Convergence tolerance on the log-likelihood.
#'
#' @return A list with the mixing `weight` of each component, the fitted
#'   `params`, the `logLik`, the iterations run, whether the fit `converged`,
#'   and a `quantile` function giving quantiles of the mixture.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 2000, seed = 1)
#' fit <- dwell_mixture(stage_dwell(sim, "released"))
#' fit$weight
#' fit$quantile(c(0.5, 0.9))
#' @export
dwell_mixture <- function(dwell, families = c("exp", "lnorm"),
                          iter = 500, tol = 1e-7) {
  stopifnot(length(families) == 2, all(families %in% c("exp", "lnorm")))
  t <- pmax(dwell$dwell_hours, 1e-6)
  obs <- !dwell$censored
  n <- length(t)

  dens <- function(fam, par, x)
    if (fam == "exp") stats::dexp(x, 1 / par[1])
    else stats::dlnorm(x, par[1], par[2])
  surv <- function(fam, par, x)
    if (fam == "exp") stats::pexp(x, 1 / par[1], lower.tail = FALSE)
    else stats::plnorm(x, par[1], par[2], lower.tail = FALSE)
  quant <- function(fam, par, p)
    if (fam == "exp") stats::qexp(p, 1 / par[1])
    else stats::qlnorm(p, par[1], par[2])

  # Start the fast component below the median and the slow one above it, so the
  # two do not collapse onto the same solution.
  med <- stats::median(t)
  start <- function(fam, slow) {
    x <- if (slow) t[t > med] else t[t <= med]
    if (fam == "exp") c(mean(x), NA) else c(mean(log(x)), stats::sd(log(x)))
  }
  par <- list(start(families[1], FALSE), start(families[2], TRUE))
  w <- c(0.8, 0.2)

  loglik <- function(w, par) {
    f <- cbind(dens(families[1], par[[1]], t), dens(families[2], par[[2]], t))
    sv <- cbind(surv(families[1], par[[1]], t), surv(families[2], par[[2]], t))
    m <- ifelse(obs, f %*% w, sv %*% w)
    sum(log(pmax(m, 1e-300)))
  }

  ll_old <- loglik(w, par); it <- 0
  for (it in seq_len(iter)) {
    # E step: responsibility of each component for each visit.
    f <- cbind(dens(families[1], par[[1]], t), dens(families[2], par[[2]], t))
    sv <- cbind(surv(families[1], par[[1]], t), surv(families[2], par[[2]], t))
    num <- ifelse(cbind(obs, obs), f, sv) * rep(w, each = n)
    r <- num / pmax(rowSums(num), 1e-300)

    # M step. A censored visit contributes the expected value of the complete
    # data given that it exceeds its observed time, which for the log-normal is
    # the truncated-normal moment and for the exponential is the memoryless
    # remainder.
    w <- colMeans(r)
    for (k in 1:2) {
      rk <- r[, k]
      if (families[k] == "exp") {
        # E[T | T > t] = t + mean for the exponential.
        contrib <- ifelse(obs, t, t + par[[k]][1])
        par[[k]][1] <- sum(rk * contrib) / sum(rk)
      } else {
        mu <- par[[k]][1]; sg <- par[[k]][2]
        z <- (log(t) - mu) / sg
        lam <- stats::dnorm(z) / pmax(stats::pnorm(z, lower.tail = FALSE), 1e-12)
        y1 <- ifelse(obs, log(t), mu + sg * lam)
        y2 <- ifelse(obs, (log(t) - mu)^2, sg^2 * (1 + z * lam))
        par[[k]][1] <- sum(rk * y1) / sum(rk)
        par[[k]][2] <- sqrt(max(sum(rk * y2) / sum(rk), 1e-8))
      }
    }
    ll <- loglik(w, par)
    done <- abs(ll - ll_old) < tol
    ll_old <- ll
    if (done) break
  }
  # Reported rather than silent. A mixture that has not converged is still a
  # usable description of the data, but the component parameters should not be
  # quoted to three figures when the likelihood is still moving.
  converged <- it < iter

  qmix <- function(p) {
    vapply(p, function(pi) {
      f <- function(x) w[1] * surv(families[1], par[[1]], x) +
                       w[2] * surv(families[2], par[[2]], x) - (1 - pi)
      stats::uniroot(f, c(1e-8, 1e6), extendInt = "downX")$root
    }, numeric(1))
  }

  list(weight = stats::setNames(w, families), params = par,
       families = families, logLik = ll_old, iterations = it,
       converged = converged,
       quantile = qmix,
       component_quantile = function(p) rbind(
         vapply(p, function(pi) quant(families[1], par[[1]], pi), numeric(1)),
         vapply(p, function(pi) quant(families[2], par[[2]], pi), numeric(1))))
}

#' Cumulative incidence of each way out of a stage
#'
#' With more than one way to leave a stage, the share that has left by a given
#' time has to be split by how they left. Treating every exit as the same event
#' answers a question about a mixture of outcomes, and estimating one exit by
#' treating the others as censored overstates it, because an order that was
#' cancelled is not an order still waiting to advance (§6.8).
#'
#' The estimator is the Aalen-Johansen cumulative incidence, which keeps the
#' competing exits in the risk set where they belong.
#'
#' @param exits A data frame from [stage_exit_type()].
#' @param times Times in hours at which to report incidence.
#'
#' @return A data frame with one row per time and one column per exit type,
#'   giving the share of visits that had left that way by then, plus the share
#'   still waiting.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1500, seed = 1)
#' dwell_competing(stage_exit_type(sim, "received", distribution_stage_spec()))
#' @export
dwell_competing <- function(exits, times = c(4, 8, 24, 72, 168)) {
  stopifnot("exit_type" %in% names(exits))
  types <- setdiff(sort(unique(exits$exit_type)), "censored")
  ev <- factor(ifelse(exits$exit_type == "censored", "none", exits$exit_type),
               levels = c("none", types))
  s <- survival::Surv(pmax(exits$dwell_hours, 1e-6), ev)
  fit <- survival::survfit(s ~ 1)
  sm <- summary(fit, times = times, extend = TRUE)

  # survfit for competing risks returns one probability column per state, the
  # last being the "still here" state.
  p <- as.matrix(sm$pstate)
  colnames(p) <- sm$states
  out <- data.frame(hours = sm$time, p[, types, drop = FALSE],
                    still_waiting = p[, "(s0)"], check.names = FALSE)
  rownames(out) <- NULL
  out
}

#' Regression of dwell on order attributes
#'
#' Covariates turn one dwell distribution for a stage into a distribution per
#' order, which is what Chapter 7 needs in order
#' to attribute variance to drivers rather than only to stages (§6.9).
#'
#' Two model families answer the question differently and both are reported.
#' A **Cox** model is multiplicative on the hazard and assumes that a
#' covariate's effect on it does not change with elapsed time, which is the
#' proportional-hazards assumption and is testable. An **accelerated failure
#' time** model is multiplicative on the dwell itself, so its coefficients read
#' directly as "this group waits x times as long", and it does not require
#' proportional hazards.
#'
#' The proportional-hazards test is reported alongside the Cox fit rather than
#' left to the reader to remember. Where it fails, the usual remedies are to
#' stratify on the offending covariate, to let its effect vary with time, or to
#' use the accelerated failure time fit instead.
#'
#' @param dwell A data frame from [stage_dwell()] with covariate columns joined
#'   on.
#' @param covariates Right-hand side of the model, as a character vector of
#'   column names.
#' @param dist Family for the accelerated failure time fit.
#'
#' @return A list with the `cox` and `aft` fits, a `table` of each covariate's
#'   hazard ratio and time ratio, and `ph_test`, the proportional-hazards test.
#'
#' @examples
#' sim <- simulate_lifecycles(n = 1500, seed = 1)
#' d <- merge(stage_dwell(sim, "released"),
#'            sim$orders[, c("order_no", "branch", "lines")], by = "order_no")
#' fit <- dwell_regression(d, c("branch", "lines"))
#' fit$table
#' @export
dwell_regression <- function(dwell, covariates, dist = "lognormal") {
  stopifnot(all(covariates %in% names(dwell)))
  rhs <- paste(covariates, collapse = " + ")
  dwell$.t <- pmax(dwell$dwell_hours, 1e-6)
  dwell$.e <- as.integer(!dwell$censored)
  f <- stats::as.formula(paste("survival::Surv(.t, .e) ~", rhs))

  cox <- survival::coxph(f, data = dwell)
  aft <- survival::survreg(f, data = dwell, dist = dist)
  ph <- survival::cox.zph(cox)

  cs <- summary(cox)$coefficients
  av <- summary(aft)$table
  av <- av[rownames(av) %in% rownames(cs), , drop = FALSE]

  tab <- data.frame(
    term = rownames(cs),
    hazard_ratio = exp(cs[, "coef"]),
    cox_p = cs[, ncol(cs)],
    time_ratio = exp(av[rownames(cs), "Value"]),
    aft_p = av[rownames(cs), "p"],
    stringsAsFactors = FALSE)
  rownames(tab) <- NULL

  list(cox = cox, aft = aft, table = tab, ph_test = ph)
}
