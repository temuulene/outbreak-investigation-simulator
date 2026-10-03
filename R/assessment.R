assessment_profile <- function(s) {
  data <- line_list(s)
  comparisons <- any(data$case_status == "Non-case")
  calculated <- any(vapply(s$checks, function(x) isTRUE(x$result$ok), logical(1)))
  checkpoints <- length(s$checkpoints) > 0
  communicated <- length(s$media_responses) > 0
  labels <- c("Comparison group", "Case definition", "Calculation", "Evidence synthesis", "Action and communication")
  status <- c(if (comparisons) "Evidence recorded" else "Needs attention", "Instructor review",
    if (calculated) "Evidence recorded" else "Needs attention", if (checkpoints) "Instructor review" else "Needs attention",
    if (communicated) "Instructor review" else "Needs attention")
  criteria <- data.frame(id = c("planning", "definition", "calculation", "synthesis", "communication"),
    label = labels, status = status,
    feedback = c(if (comparisons) "Classified comparison guests are present; discuss representativeness and missingness." else "Collect and classify comparison guests before estimating attack rates.",
      paste(length(s$case_defs), "definition versions;", sum(data$case_status == "Unknown"), "unknown classifications. Explain population, clinical, and time criteria."),
      if (calculated) "A frozen-snapshot calculation passed. Precision and interpretation still require review." else "No successful frozen-snapshot calculation is recorded.",
      if (checkpoints) "Review supporting and conflicting evidence at each checkpoint, including what would change the conclusion." else "Record supporting evidence, contrary evidence, and what could change the conclusion.",
      if (communicated) "Review whether the response separates facts, uncertainty, actions, and evidence references." else "Respond to an available media request using current evidence and explicit uncertainty."),
    evidence = c(paste("Collected records:", nrow(data)), paste("Definition versions:", length(s$case_defs)),
      paste("Calculation checks:", length(s$checks)), paste("Checkpoints:", length(s$checkpoints)),
      paste("Media responses:", length(s$media_responses))), stringsAsFactors = FALSE)
  excerpt <- function(x) if (is.null(x)) "Not recorded" else substr(x, 1, 180)
  if (checkpoints) {
    cp <- s$checkpoints[[length(s$checkpoints)]]
    criteria$feedback[4] <- paste("Latest checkpoint:", cp$stage, "at", game_time(cp$time),
      "Supporting:", excerpt(cp$supporting), "Against:", excerpt(cp$against),
      "Would change:", excerpt(cp$change), "Proposed action:", excerpt(cp$action),
      "Instructor must assess whether these statements follow from the evidence available then.")
    criteria$evidence[4] <- paste("Checkpoint", length(s$checkpoints), "with", length(cp$evidence),
      "contemporaneous evidence entries:", paste(vapply(cp$evidence, `[[`, character(1), "source"), collapse = "; "))
  }
  references <- sum(vapply(s$media_responses, function(x) length(x$evidence_ids), integer(1)))
  unsupported <- sum(vapply(s$actions, function(x) !isTRUE(x$justified), logical(1)))
  criteria$feedback[5] <- paste(criteria$feedback[5], length(s$actions), "actions recorded;", unsupported,
    "did not meet the configured evidence condition at the time.", references, "media evidence references recorded.",
    if (communicated) paste("Latest response:", excerpt(s$media_responses[[length(s$media_responses)]]$text)) else "")
  criteria$evidence[5] <- paste("Actions:", paste(vapply(s$actions, function(x) paste(x$label, "at", game_time(x$time), "rationale:", excerpt(x$reason)), character(1)), collapse = "; "),
    "Media evidence IDs:", paste(unique(unlist(lapply(s$media_responses, `[[`, "evidence_ids"))), collapse = ", "))
  criteria$rule <- c("At least one classified non-case; representativeness requires review.",
    "Count definition versions and unknown classifications; inspect the written rationale.",
    "At least one successful frozen-snapshot calculation check.",
    "Show checkpoint supporting, contrary, change-trigger, and action text alongside contemporaneous evidence; no semantic score.",
    "Show action evidence-condition results, rationales, communication text, and cited evidence IDs; no semantic score.")
  for (review in s$instructor_reviews) {
    row <- match(review$criterion, criteria$id)
    criteria$status[row] <- review$status
    criteria$feedback[row] <- paste(criteria$feedback[row], "Instructor note:", review$reason)
  }
  list(criteria = criteria, caveat = "Formative indicators are observable process checks, not validated educational scores. Evidence quality and causal reasoning require instructor review; naming the hidden source is never scored.")
}

instructor_override <- function(s, criterion, status, reason) {
  ids <- c("planning", "definition", "calculation", "synthesis", "communication")
  if (length(criterion) != 1 || !criterion %in% ids || length(status) != 1 || !status %in% c("Evidence recorded", "Needs attention", "Instructor review")) stop("Invalid instructor criterion or status.")
  if (length(reason) != 1 || !nzchar(trimws(reason)) || nchar(reason) > 4000) stop("Record an instructor rationale of 1 to 4000 characters.")
  entry <- list(time = s$clock, criterion = criterion, status = status, reason = trimws(reason))
  s$instructor_reviews <- append(s$instructor_reviews, list(entry))
  record_event(s, "instructor_review", entry)
}
