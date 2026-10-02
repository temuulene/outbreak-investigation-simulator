save_checkpoint <- function(s, stage, suspect, confidence, supporting, against, change, action) {
  fields <- c(suspect, supporting, against, change, action)
  if (any(!nzchar(trimws(fields)))) stop("Complete each reasoning field; 'none yet' is a valid evidence statement.")
  entry <- list(time = s$clock, stage = stage, suspect = suspect, confidence = confidence,
    supporting = supporting, against = against, change = change, action = action, evidence = s$evidence)
  s$checkpoints <- append(s$checkpoints, list(entry))
  record_event(s, "checkpoint", entry)
}

learning_profile <- function(s) {
  data <- line_list(s)
  c(
    Planning = if (any(data$case_status == "Non-case")) "Comparison guests are present. Review coverage and unasked exposures with your facilitator." else "No classified non-cases yet. Consider whether you can estimate attack rates in both groups.",
    `Case classification` = paste(length(s$case_defs), "definition version(s);", sum(data$case_status == "Unknown"), "records remain unclassified. Discuss the reasons for your criteria."),
    Analysis = if (any(vapply(s$checks, function(x) isTRUE(x$result$ok), logical(1)))) "At least one calculation check passed. Review missingness and uncertainty separately." else "No successful calculation check recorded yet. Calculation accuracy is separate from reasoning.",
    `Evidence synthesis` = "Instructor review: compare the recommendation with the evidence available at each checkpoint; do not grade on naming the hidden vehicle.",
    `Action and communication` = paste(length(s$actions), "action(s) recorded;", sum(vapply(s$actions, function(x) !x$justified, logical(1))), "did not meet the scenario's evidence condition. Review their rationale."))
}
