# Chapter 21 models. Evaluating a monitoring policy rather than the model
# inside it. The asymmetry the whole chapter turns on is that a false positive
# leaves a record and a false negative leaves nothing, so anything measured
# from the program's own output is measured on the half that is visible.

#' Compare a proposed monitoring policy against the one running today
#'
#' Three lists decide whether a change is safe to publish: what the proposal
#' surfaces that the current policy does not, what it stops surfacing, and
#' where the urgency changes. The middle list is the one no reporting tool
#' produces on its own, because an item that stops being flagged produces no
#' output at all (§21.6).
#'
#' Both policies are evaluated on the same population, which should be the
#' live one rather than a sample. An item ceasing to surface is reported with
#' its exposure, since the question a reviewer will ask about the list is what
#' it is worth.
#'
#' @param current,proposed Severity decided by each policy, one entry per item,
#'   drawn from `levels`.
#' @param id Optional item identifier, used to label the returned lists.
#' @param value Optional exposure per item.
#' @param outcome Optional logical: whether the item turned out to need the
#'   attention. Where it is supplied, precision and recall are reported for
#'   both policies, with the caveat of §21.3 attached to any outcome collected
#'   under the current policy.
#' @param levels The severity scale, ordered from least to most severe.
#' @param acted Levels that count as acting on an item.
#'
#' @return A list with the `transition` table, `volume` under each policy, the
#'   `newly_surfacing`, `ceasing` and `urgency_changed` item sets with their
#'   exposure, and, when `outcome` is given, a `quality` data frame.
#'
#' @examples
#' evaluate_policy(current  = c("none", "notify", "escalate", "notify"),
#'                 proposed = c("notify", "notify", "notify", "none"),
#'                 value = c(100, 200, 9000, 50))
#' @export
evaluate_policy <- function(current, proposed, id = NULL, value = NULL,
                            outcome = NULL,
                            levels = c("none", "notify", "escalate"),
                            acted = c("notify", "escalate")) {
  n <- length(current)
  stopifnot(n > 0, length(proposed) == n,
            all(current %in% levels), all(proposed %in% levels))
  if (is.null(id)) id <- seq_len(n)
  stopifnot(length(id) == n)
  if (!is.null(value)) stopifnot(length(value) == n)
  if (!is.null(outcome)) stopifnot(length(outcome) == n, is.logical(outcome))

  cur_act <- current %in% acted
  pro_act <- proposed %in% acted
  ci <- match(current, levels); pi_ <- match(proposed, levels)

  new   <- !cur_act & pro_act
  cease <- cur_act & !pro_act
  moved <- cur_act & pro_act & ci != pi_

  set <- function(k) {
    out <- data.frame(id = id[k], from = current[k], to = proposed[k],
                      stringsAsFactors = FALSE)
    if (!is.null(value)) out$value <- value[k]
    if (!is.null(outcome)) out$outcome <- outcome[k]
    if (!is.null(value) && nrow(out)) out <- out[order(-out$value), ]
    rownames(out) <- NULL
    out
  }
  exposure <- function(k) if (is.null(value)) NA_real_ else sum(value[k])

  quality <- NULL
  if (!is.null(outcome)) {
    quality <- do.call(rbind, lapply(
      list(current = cur_act, proposed = pro_act), function(a)
        data.frame(acted = sum(a),
                   precision = if (sum(a)) mean(outcome[a]) else NA_real_,
                   recall = if (sum(outcome)) sum(outcome & a) / sum(outcome)
                            else NA_real_)))
    quality$policy <- rownames(quality); rownames(quality) <- NULL
    quality <- quality[, c("policy", "acted", "precision", "recall")]
  }

  list(transition = table(current = current, proposed = proposed),
       volume = c(current = sum(cur_act), proposed = sum(pro_act),
                  change = sum(pro_act) - sum(cur_act)),
       newly_surfacing = set(new),
       ceasing = set(cease),
       urgency_changed = set(moved),
       exposure = c(newly_surfacing = exposure(new),
                    ceasing = exposure(cease),
                    urgency_changed = exposure(moved)),
       quality = quality)
}

#' Can a rule fire at all?
#'
#' A rule that did not fire because nothing qualified and a rule that cannot
#' fire read identically in a report: both produce no alerts (§21.9). The
#' difference is worth a test, because the second kind has been seen to sit in
#' production for months -- a stage list that never contained credit holds
#' made every credit-hold rule unreachable from the day it was written, and a
#' value field spelled differently from what the evaluator read returned null
#' on every item and disabled the rule that depended on it.
#'
#' Each rule is checked in three steps: the field it reads has to exist, it
#' has to be populated on at least one row, and at least one row has to
#' satisfy the condition.
#'
#' @param rules A data frame with columns `name`, `field`, `op` (one of
#'   `">"`, `">="`, `"<"`, `"<="`, `"=="`, `"!="`, `"in"`) and `value`. For
#'   `"in"`, `value` holds a comma-separated set.
#' @param data The population the rules are meant to fire on.
#'
#' @return A data frame with one row per rule: whether it is `reachable`, the
#'   `reason` when it is not, how many rows have the field `populated`, and
#'   how many rows `match`.
#'
#' @examples
#' rules <- data.frame(
#'   name = c("aged hold", "high value"),
#'   field = c("stage", "order_value"),
#'   op = c("in", ">"),
#'   value = c("credit_hold", "1000"), stringsAsFactors = FALSE)
#' rule_reachable(rules, data.frame(stage = c("received", "released"),
#'                                  value = c(2000, 50)))
#' @export
rule_reachable <- function(rules, data) {
  need <- c("name", "field", "op", "value")
  stopifnot(all(need %in% names(rules)), nrow(rules) > 0, nrow(data) > 0)
  ops <- c(">", ">=", "<", "<=", "==", "!=", "in")
  bad <- setdiff(unique(rules$op), ops)
  if (length(bad))
    stop("unsupported operator: ", paste(bad, collapse = ", "), call. = FALSE)

  do.call(rbind, lapply(seq_len(nrow(rules)), function(i) {
    f <- rules$field[i]; op <- rules$op[i]; v <- rules$value[i]
    row <- data.frame(name = rules$name[i], reachable = FALSE,
                      reason = NA_character_, populated = 0L, match = 0L,
                      stringsAsFactors = FALSE)
    if (!f %in% names(data)) {
      row$reason <- paste0("field absent: ", f)
      return(row)
    }
    x <- data[[f]]
    row$populated <- sum(!is.na(x))
    if (row$populated == 0L) {
      row$reason <- paste0("field never populated: ", f)
      return(row)
    }
    hit <- if (op == "in") {
      x %in% trimws(strsplit(v, ",")[[1]])
    } else {
      num <- suppressWarnings(as.numeric(v))
      cmp <- get(op, envir = baseenv())
      if (!is.na(num) && is.numeric(x)) cmp(x, num) else cmp(as.character(x), v)
    }
    hit[is.na(hit)] <- FALSE
    row$match <- sum(hit)
    if (row$match == 0L) {
      row$reason <- "no row matches the condition"
      return(row)
    }
    row$reachable <- TRUE
    row
  }))
}

#' Off-policy value of a proposed policy from logged decisions
#'
#' A proposed policy's value can be estimated from decisions logged under a
#' different one, provided the logged policy had some chance of taking the
#' action the proposal would take. Inverse propensity weighting reweights each
#' logged decision by that chance; the doubly robust form subtracts a reward
#' model and adds it back, which leaves the estimate correct if either the
#' propensities or the model are (§21.4).
#'
#' The assumption that fails in practice is positivity, and it fails silently.
#' A deterministic logged policy took what it took with probability one, so
#' nothing in `propensity` looks wrong, while the items where the proposal
#' would have acted differently carry no weight and simply drop out of the
#' average. `evidence` reports the share of items the estimate actually rests
#' on, and `positivity` reports the share whose target action had a nonzero
#' chance of being logged, which needs `target_propensity` to be known.
#'
#' @param action Action taken under the logged policy, one per item.
#' @param reward Observed reward for the logged action.
#' @param propensity Probability the logged policy took the action it took.
#' @param target Action the proposed policy would take.
#' @param target_propensity Optional probability the logged policy would have
#'   taken `target`. Supplying it is what makes a positivity failure visible
#'   rather than inferred.
#' @param reward_model Optional matrix of predicted rewards with one column
#'   per action, named by action, used for the doubly robust estimate.
#'
#' @return A list with the `ips` and, when a model is supplied, `dr`
#'   estimates, their standard errors, the `effective_n` of the weighting, the
#'   `evidence` share, `positivity` where it can be computed, and the naive
#'   `on_policy` mean for comparison.
#'
#' @examples
#' set.seed(1)
#' a <- sample(c("look", "pass"), 200, TRUE)
#' r <- ifelse(a == "look", rnorm(200, 1), rnorm(200, 0.5))
#' offpolicy_value(a, r, propensity = rep(0.5, 200),
#'                 target = rep("look", 200),
#'                 target_propensity = rep(0.5, 200))
#' @export
offpolicy_value <- function(action, reward, propensity, target,
                            target_propensity = NULL, reward_model = NULL) {
  n <- length(action)
  stopifnot(n > 0, length(reward) == n, length(propensity) == n,
            length(target) == n, all(propensity >= 0), all(propensity <= 1))
  if (!is.null(target_propensity))
    stopifnot(length(target_propensity) == n, all(target_propensity >= 0),
              all(target_propensity <= 1))

  agree <- action == target
  w <- ifelse(agree & propensity > 0, 1 / propensity, 0)
  contrib <- w * reward
  ips <- mean(contrib)

  out <- list(ips = ips,
              ips_se = stats::sd(contrib) / sqrt(n),
              effective_n = if (sum(w^2) > 0) sum(w)^2 / sum(w^2) else 0,
              evidence = mean(w > 0),
              positivity = if (is.null(target_propensity)) NA_real_
                           else mean(target_propensity > 0),
              on_policy = mean(reward))

  if (!is.null(reward_model)) {
    stopifnot(is.matrix(reward_model), nrow(reward_model) == n,
              !is.null(colnames(reward_model)),
              all(target %in% colnames(reward_model)),
              all(action %in% colnames(reward_model)))
    pick <- function(a) reward_model[cbind(seq_len(n),
                                           match(a, colnames(reward_model)))]
    dr_terms <- pick(target) + w * (reward - pick(action))
    out$dr <- mean(dr_terms)
    out$dr_se <- stats::sd(dr_terms) / sqrt(n)
  }
  out
}
