#' Reasons a decision was made without the model tier
#'
#' "Not configured to use a model" and "could not reach one" look identical
#' from outside the decision path and mean entirely different things: the
#' first is a choice and the second is an incident (§19.9). A single `NA` in
#' place of a model score collapses them, and collapsing them means an outage
#' is indistinguishable from a configuration.
#'
#' These are the reasons [decision_path()] accepts. `model_disabled` is the
#' deliberate one; the rest are degradations, each with the consequence
#' Section 19.10 assigns it.
#'
#' @return A character vector of accepted reasons.
#' @examples
#' fallback_reasons()
#' @export
fallback_reasons <- function() {
  c("model_disabled",          # configured off for this item or population
    "model_unreachable",       # no response at all
    "model_timeout",           # response too late to use
    "model_throttled",         # refused for rate or quota
    "model_malformed",         # response not parseable into a severity
    "inputs_stale",            # features older than the model's assumption
    "budget_exhausted",        # metered spend cap reached (§19.8)
    "below_confidence_floor")  # model answered, not confidently enough
}

#' Compose a rule tier and a model tier into one decision
#'
#' The deterministic tier decides every item, costs nothing per decision and
#' runs whatever else is unavailable (§19.2). A model tier, where one is
#' reachable, proposes a severity of its own. What the path does with the two
#' proposals is a policy, and the policy is the part worth constraining:
#' `raise_only` lets the model escalate an item the rule would have left alone
#' but never de-escalate one the rule flagged, which puts a floor under the
#' worst case that no property of the model itself can provide (§19.4).
#'
#' Both proposals are returned, not just the outcome. A review that can see
#' only what was decided cannot tell a model that agreed from a model that was
#' overruled, and the difference is the whole of the evidence for promoting or
#' demoting the tier (§19.5).
#'
#' An item whose model proposal is `NA` must carry a reason from
#' [fallback_reasons()]. Supplying none is an error rather than a default,
#' because the state that needs naming is exactly the one nobody names.
#'
#' @param rule Severity proposed by the deterministic tier, one per item.
#' @param model Optional severity proposed by the model tier. `NA` marks an
#'   item the model did not decide.
#' @param policy `"raise_only"` takes the higher of the two proposals,
#'   `"replace"` takes the model's where it has one, and `"advise"` records
#'   the model's proposal without ever acting on it.
#' @param levels The severity scale, ordered from least to most severe.
#' @param reason Fallback reason for items with no model proposal, recycled
#'   from length one. Required whenever `model` contains `NA`.
#'
#' @return A data frame with one row per item: `rule`, `model`, `decided`,
#'   `source` (which tier the decision came from), `raised`, `lowered` and
#'   `reason`.
#'
#' @examples
#' decision_path(rule = c("notify", "escalate", "none"),
#'               model = c("escalate", "none", NA),
#'               reason = "model_timeout")
#' @export
decision_path <- function(rule, model = NULL,
                          policy = c("raise_only", "replace", "advise"),
                          levels = c("none", "notify", "escalate"),
                          reason = NULL) {
  policy <- match.arg(policy)
  stopifnot(length(rule) > 0, !anyNA(rule), all(rule %in% levels))

  n <- length(rule)
  if (is.null(model)) model <- rep(NA_character_, n)
  stopifnot(length(model) == n)
  if (!all(model[!is.na(model)] %in% levels))
    stop("model proposed a severity outside `levels`: ",
         paste(setdiff(unique(model[!is.na(model)]), levels), collapse = ", "),
         call. = FALSE)

  absent <- is.na(model)
  if (any(absent)) {
    if (is.null(reason))
      stop("model has no proposal for ", sum(absent), " of ", n,
           " items and no `reason` was given. See fallback_reasons().",
           call. = FALSE)
    if (length(reason) == 1) reason <- rep(reason, n)
    stopifnot(length(reason) == n)
    bad <- setdiff(unique(reason[absent]), fallback_reasons())
    if (length(bad))
      stop("reason not in fallback_reasons(): ",
           paste(bad, collapse = ", "), call. = FALSE)
    if (anyNA(reason[absent]))
      stop("every item without a model proposal needs a reason.",
           call. = FALSE)
  } else if (is.null(reason)) {
    reason <- rep(NA_character_, n)
  } else if (length(reason) == 1) {
    reason <- rep(reason, n)
  }
  reason[!absent] <- NA_character_

  ri <- match(rule, levels)
  mi <- match(model, levels)
  di <- switch(policy,
               raise_only = ifelse(absent, ri, pmax(ri, mi)),
               replace    = ifelse(absent, ri, mi),
               advise     = ri)

  data.frame(rule = rule, model = model, decided = levels[di],
             source = ifelse(absent | di == ri, "rule", "model"),
             raised = di > ri, lowered = di < ri,
             reason = reason, stringsAsFactors = FALSE)
}

#' What the decision path did, and how often it ran without the model
#'
#' Two numbers decide whether a model tier is earning its place: the share of
#' decisions it actually changed, and the share of decisions made without it.
#' A tier that changes nothing is cost without effect, and a tier unreachable
#' for a quarter of the population is not the tier that was evaluated.
#'
#' Reasons are reported separately rather than summed, for the reason
#' [fallback_reasons()] gives.
#'
#' @param path A data frame returned by [decision_path()].
#'
#' @return A list with `decided` (a table of outcomes), `changed` (the share
#'   the model moved), `raised`, `lowered`, `without_model` and a `reasons`
#'   data frame of counts and shares.
#'
#' @examples
#' p <- decision_path(rule = c("notify", "escalate", "none"),
#'                    model = c("escalate", "none", NA),
#'                    reason = "model_timeout")
#' decision_audit(p)
#' @export
decision_audit <- function(path) {
  need <- c("rule", "model", "decided", "source", "raised", "lowered",
            "reason")
  stopifnot(all(need %in% names(path)), nrow(path) > 0)

  absent <- is.na(path$model)
  rs <- if (any(absent)) {
    tb <- table(path$reason[absent])
    data.frame(reason = names(tb), items = as.integer(tb),
               share = round(as.integer(tb) / nrow(path), 4),
               stringsAsFactors = FALSE)
  } else {
    data.frame(reason = character(0), items = integer(0), share = numeric(0))
  }
  if (nrow(rs)) rs <- rs[order(-rs$items), ]
  rownames(rs) <- NULL

  list(decided = table(path$decided),
       changed = mean(path$raised | path$lowered),
       raised = mean(path$raised),
       lowered = mean(path$lowered),
       without_model = mean(absent),
       reasons = rs)
}

#' Meter metered inference against a daily cap
#'
#' Inference that is paid for per call is a resource like any other, and the
#' cap belongs in the design rather than in the invoice (§19.8). The question
#' the cap raises is not whether it will be reached but what reaching it
#' costs: a path that drops items when the budget runs out has made coverage
#' depend on spend, while a path that drops the drafted explanation and keeps
#' the deterministic decision has not.
#'
#' @param calls Calls the path would make in the period.
#' @param cost_per_call Metered cost of one call.
#' @param cap Spend permitted in the period.
#' @param essential Calls that must be paid for before any discretionary
#'   ones, such as the explanation on an escalated item.
#'
#' @return A list with `affordable` calls, the `exhausted_after` call at which
#'   the cap binds, `cost`, `share_metered`, and `essential_covered`.
#'
#' @examples
#' inference_budget(calls = 400, cost_per_call = 0.02, cap = 5,
#'                  essential = 120)
#' @export
inference_budget <- function(calls, cost_per_call, cap, essential = 0) {
  stopifnot(calls >= 0, cost_per_call > 0, cap >= 0, essential >= 0,
            essential <= calls)
  affordable <- min(calls, floor(cap / cost_per_call))
  list(calls = calls,
       affordable = affordable,
       exhausted_after = if (affordable < calls) affordable else NA_integer_,
       cost = affordable * cost_per_call,
       share_metered = if (calls > 0) affordable / calls else NA_real_,
       essential_covered = if (essential > 0) min(1, affordable / essential)
                           else NA_real_)
}
