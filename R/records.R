# Chapter 22 models. A decision record that supports review, and the narrow
# configuration grammar that keeps a written rule checkable.
#
# The requirement throughout is that a decision resolve to the thing that
# decided it. A record saying what happened, without saying what the policy
# was at the time, cannot answer whether the decision was right.

#' Stamp a set of decisions with the provenance review needs
#'
#' A record of what the system did is not enough to review it. The question a
#' reviewer asks is whether the decision was right given what was known and
#' what the policy was, and answering it needs the policy version, the model
#' version where one applied, and the basis the decision rested on (§22.1).
#'
#' Every field is required rather than optional, and a missing one is an error.
#' A provenance field that is sometimes absent is one nobody can rely on, and
#' the absence always turns out to be concentrated in the decisions somebody
#' later wants to examine.
#'
#' @param decided Severity or action decided, one entry per item.
#' @param id Item identifier.
#' @param at Decision timestamps, recycled from length one for a batch
#'   decided at a single epoch.
#' @param policy_version Version of the configuration that decided, recycled
#'   from length one.
#' @param source Which tier decided, as returned by [decision_path()].
#' @param model_version Model version where the model tier decided. `NA` is
#'   permitted only where `source` is not `"model"`.
#' @param score Model score where one applied.
#' @param reason Fallback reason from [fallback_reasons()] where no model
#'   proposal was available.
#' @param basis Optional free text recording what the decision was measured
#'   against, such as the baseline and window in force.
#'
#' @return A data frame with one row per decision and the provenance columns
#'   the conformance check of [record_conformance()] requires.
#'
#' @examples
#' decision_record(decided = c("notify", "escalate"), id = c("A", "B"),
#'                 at = as.POSIXct(c("2026-01-01", "2026-01-01"), tz = "UTC"),
#'                 policy_version = "v4", source = c("rule", "model"),
#'                 model_version = c(NA, "forest-2026-01"),
#'                 score = c(NA, 0.81), reason = c("model_disabled", NA))
#' @export
decision_record <- function(decided, id, at, policy_version, source,
                            model_version = NA_character_, score = NA_real_,
                            reason = NA_character_, basis = NA_character_) {
  n <- length(decided)
  stopifnot(n > 0, length(id) == n)
  rec <- function(x) if (length(x) == 1) rep(x, n) else x
  # A batch decided at one epoch carries one timestamp, so `at` recycles the
  # way the provenance fields do.
  at <- rec(at)
  stopifnot(length(at) == n)
  policy_version <- rec(policy_version); source <- rec(source)
  model_version <- rec(model_version); score <- rec(score)
  reason <- rec(reason); basis <- rec(basis)
  stopifnot(length(policy_version) == n, length(source) == n,
            length(model_version) == n, length(score) == n,
            length(reason) == n, length(basis) == n)

  if (anyNA(policy_version) || any(!nzchar(policy_version)))
    stop("every decision needs a policy version; ",
         sum(is.na(policy_version) | !nzchar(policy_version)),
         " of ", n, " have none.", call. = FALSE)
  if (anyNA(source))
    stop("every decision needs a source tier.", call. = FALSE)
  bad <- source == "model" & is.na(model_version)
  if (any(bad))
    stop(sum(bad), " decisions came from the model tier with no model ",
         "version recorded.", call. = FALSE)
  bad <- source != "model" & !is.na(score) & is.na(model_version)
  if (any(bad))
    stop(sum(bad), " decisions record a score with no model version.",
         call. = FALSE)
  bad <- !is.na(reason) & !reason %in% fallback_reasons()
  if (any(bad))
    stop("reason not in fallback_reasons(): ",
         paste(unique(reason[bad]), collapse = ", "), call. = FALSE)

  data.frame(id = id, at = at, decided = decided,
             policy_version = policy_version, source = source,
             model_version = model_version, score = score,
             reason = reason, basis = basis, stringsAsFactors = FALSE)
}

#' The contract a decision record has to satisfy
#'
#' Everything downstream of the decision path reads its output, so the shape
#' of that output is an interface and benefits from being versioned like one
#' (§22.7). A conformance check establishes mechanically that the columns are
#' present, that the enumerated ones carry only declared values, and that the
#' required ones are populated.
#'
#' What it cannot establish is that the values are correct. A record declaring
#' policy version `v4` conforms whether or not `v4` is the version that ran,
#' which is why §22.6 replays rather than inspects.
#'
#' @param record A data frame as returned by [decision_record()].
#' @param levels Permitted values of `decided`.
#' @param sources Permitted values of `source`.
#' @param required Columns that may not be missing on any row.
#'
#' @return A list with `ok`, and a `problems` data frame naming each failure.
#'
#' @examples
#' r <- decision_record("notify", "A", Sys.time(), "v4", "rule")
#' record_conformance(r)
#' @export
record_conformance <- function(record,
                               levels = c("none", "notify", "escalate"),
                               sources = c("rule", "model"),
                               required = c("id", "at", "decided",
                                            "policy_version", "source")) {
  contract <- c("id", "at", "decided", "policy_version", "source",
                "model_version", "score", "reason", "basis")
  problems <- list()
  add <- function(what, detail, rows = NA_integer_)
    problems[[length(problems) + 1L]] <<-
      data.frame(check = what, detail = detail, rows = rows,
                 stringsAsFactors = FALSE)

  for (col in setdiff(contract, names(record)))
    add("column absent", col)
  for (col in intersect(required, names(record))) {
    k <- sum(is.na(record[[col]]))
    if (k) add("required column not populated", col, k)
  }
  if ("decided" %in% names(record)) {
    bad <- setdiff(unique(record$decided), levels)
    if (length(bad)) add("value outside the declared scale",
                         paste(bad, collapse = ", "),
                         sum(record$decided %in% bad))
  }
  if ("source" %in% names(record)) {
    bad <- setdiff(unique(record$source), sources)
    if (length(bad)) add("source outside the declared set",
                         paste(bad, collapse = ", "),
                         sum(record$source %in% bad))
  }
  if ("reason" %in% names(record)) {
    bad <- setdiff(stats::na.omit(unique(record$reason)), fallback_reasons())
    if (length(bad)) add("reason outside fallback_reasons()",
                         paste(bad, collapse = ", "), length(bad))
  }
  out <- if (length(problems)) do.call(rbind, problems) else
    data.frame(check = character(0), detail = character(0),
               rows = integer(0))
  list(ok = nrow(out) == 0L, problems = out)
}

#' Compare a replay against the record it is meant to reproduce
#'
#' A replay is faithful when it returns what the record says happened. Running
#' the current configuration over historical data is a different exercise and
#' answers a different question, so the two are worth keeping apart (§22.6).
#'
#' Divergence is reported per item rather than as a count, because the useful
#' question about a replay that failed is which decisions moved.
#'
#' @param record A record from [decision_record()].
#' @param replayed Decisions produced by re-execution, in the same order, or a
#'   data frame with `id` and `decided` columns to be matched on `id`.
#'
#' @return A list with the `agreement` share, the `diverged` rows, and
#'   `missing`, the recorded ids the replay did not produce.
#'
#' @examples
#' r <- decision_record(c("notify", "none"), c("A", "B"), Sys.time(),
#'                      "v4", "rule")
#' replay_check(r, c("notify", "escalate"))
#' @export
replay_check <- function(record, replayed) {
  stopifnot(all(c("id", "decided") %in% names(record)), nrow(record) > 0)
  if (is.data.frame(replayed)) {
    stopifnot(all(c("id", "decided") %in% names(replayed)))
    i <- match(record$id, replayed$id)
    got <- replayed$decided[i]
  } else {
    stopifnot(length(replayed) == nrow(record))
    got <- replayed
    i <- seq_len(nrow(record))
  }
  diverged <- which(!is.na(got) & got != record$decided)
  out <- data.frame(id = record$id[diverged],
                    recorded = record$decided[diverged],
                    replayed = got[diverged],
                    policy_version = record$policy_version[diverged],
                    stringsAsFactors = FALSE)
  list(agreement = mean(!is.na(got) & got == record$decided),
       diverged = out,
       missing = record$id[is.na(i) | is.na(got)])
}

#' The configuration grammar, as a value rather than a paragraph
#'
#' An admission rule is the one setting that resists being reduced to a form,
#' so it gets written. A deliberately small grammar is what keeps a written
#' rule checkable: a short list of operators, two connectives, no negation,
#' no arithmetic and no function calls (§22.9).
#'
#' The limits are returned alongside the vocabulary, because a limit nobody
#' wrote down becomes a workaround nobody remembers (§22.10).
#'
#' @return A list with the permitted `operators`, `connectives`, and the
#'   `cannot_express` constructs, each with the usual workaround named.
#'
#' @examples
#' rule_grammar()$cannot_express
#' @export
rule_grammar <- function() {
  list(
    operators = c(">", ">=", "<", "<=", "==", "!=", "in"),
    connectives = c("and", "or"),
    cannot_express = data.frame(
      construct = c("negation", "arithmetic on a field",
                    "a field compared to another field", "grouping",
                    "a function call"),
      example = c("not stage in shipped", "due_date - 2",
                  "promised_date < requested_date", "(a or b) and c",
                  "median(dwell_hours)"),
      workaround = c("state the complement as an explicit set",
                     "add the derived field upstream and declare it",
                     "add a derived comparison field upstream",
                     "write two rules",
                     "add the aggregate as a declared field"),
      stringsAsFactors = FALSE))
}

#' Compile a written rule against a declared vocabulary of fields
#'
#' The three controls that make a written rule safe are validation by the same
#' evaluator production uses, a visible vocabulary of the fields available,
#' and an evaluation showing which items the rule catches (§22.9). This
#' supplies the first two; [rule_apply()] and [evaluate_policy()] supply the
#' third.
#'
#' A construct the grammar does not have is reported by name rather than
#' ignored, so that a rule which cannot mean what its author intended fails
#' when it is written instead of returning nothing in production.
#'
#' @param text The rule, as `field op value` terms joined by `and` or `or`.
#' @param fields Named character vector or list mapping each available field to
#'   its type, `"numeric"` or `"character"`. This is the vocabulary a user is
#'   shown.
#'
#' @return A list with `ok`, the parsed `terms`, the `connective`, and
#'   `errors` naming each problem.
#'
#' @examples
#' rule_compile("stage in credit_hold,backordered and value > 1000",
#'              fields = c(stage = "character", value = "numeric"))
#' @export
rule_compile <- function(text, fields) {
  stopifnot(length(text) == 1, is.character(text), length(fields) > 0,
            !is.null(names(fields)))
  g <- rule_grammar()
  errors <- character(0)

  for (bad in c("not ", "(", ")", "+", "*", "/"))
    if (grepl(bad, text, fixed = TRUE))
      errors <- c(errors, paste0("grammar has no ", trimws(bad), " -- see ",
                                 "rule_grammar()$cannot_express"))
  if (grepl("-", text) && !grepl("[A-Za-z_]-[A-Za-z_]", text))
    errors <- c(errors, "grammar has no arithmetic -- see rule_grammar()")

  conn <- if (grepl("\\band\\b", text)) "and" else
          if (grepl("\\bor\\b", text)) "or" else "none"
  if (grepl("\\band\\b", text) && grepl("\\bor\\b", text))
    errors <- c(errors, "mixing and with or needs grouping, which the ",
                "grammar has no parentheses for; write two rules")

  parts <- trimws(strsplit(text, "\\b(and|or)\\b")[[1]])
  parts <- parts[nzchar(parts)]
  terms <- list()
  for (part in parts) {
    m <- regmatches(part, regexec(
      "^([A-Za-z_][A-Za-z_0-9]*)\\s*(>=|<=|!=|==|>|<|in)\\s*(.+)$", part))[[1]]
    if (!length(m)) {
      errors <- c(errors, paste0("cannot parse term: ", part))
      next
    }
    f <- m[2]; op <- m[3]; v <- trimws(m[4])
    if (!f %in% names(fields))
      errors <- c(errors, paste0("unknown field: ", f, " -- available: ",
                                 paste(names(fields), collapse = ", ")))
    else if (fields[[f]] == "numeric" && op != "in" &&
             is.na(suppressWarnings(as.numeric(v))))
      errors <- c(errors, paste0("field ", f, " is numeric; value is not: ", v))
    if (!op %in% g$operators)
      errors <- c(errors, paste0("unknown operator: ", op))
    terms[[length(terms) + 1L]] <-
      data.frame(field = f, op = op, value = v, stringsAsFactors = FALSE)
  }

  list(ok = length(errors) == 0L,
       terms = if (length(terms)) do.call(rbind, terms) else
         data.frame(field = character(0), op = character(0),
                    value = character(0)),
       connective = conn,
       errors = errors)
}

#' Apply a compiled rule to a population
#'
#' @param compiled A rule from [rule_compile()].
#' @param data The population.
#'
#' @return A logical vector, one entry per row.
#'
#' @examples
#' r <- rule_compile("value > 1000",
#'                   fields = c(value = "numeric"))
#' rule_apply(r, data.frame(value = c(500, 5000)))
#' @export
rule_apply <- function(compiled, data) {
  if (!isTRUE(compiled$ok))
    stop("rule did not compile: ",
         paste(compiled$errors, collapse = "; "), call. = FALSE)
  hits <- lapply(seq_len(nrow(compiled$terms)), function(i) {
    f <- compiled$terms$field[i]; op <- compiled$terms$op[i]
    v <- compiled$terms$value[i]
    if (!f %in% names(data))
      stop("field absent from the population: ", f, call. = FALSE)
    x <- data[[f]]
    h <- if (op == "in") x %in% trimws(strsplit(v, ",")[[1]]) else {
      num <- suppressWarnings(as.numeric(v))
      cmp <- get(op, envir = baseenv())
      if (!is.na(num) && is.numeric(x)) cmp(x, num) else
        cmp(as.character(x), v)
    }
    h[is.na(h)] <- FALSE
    h
  })
  if (!length(hits)) return(rep(FALSE, nrow(data)))
  Reduce(if (compiled$connective == "or") `|` else `&`, hits)
}
