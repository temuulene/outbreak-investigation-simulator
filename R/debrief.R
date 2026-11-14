save_checkpoint <- function(s, stage, suspect, confidence, supporting, against, change, action) {
  fields <- c(suspect, supporting, against, change, action)
  if (any(!nzchar(trimws(fields)))) stop("Complete each reasoning field; 'none yet' is a valid evidence statement.")
  entry <- list(time = s$clock, stage = stage, suspect = suspect, confidence = confidence,
    supporting = supporting, against = against, change = change, action = action, evidence = s$evidence)
  s$checkpoints <- append(s$checkpoints, list(entry))
  record_event(s, "checkpoint", entry)
}

# Reference tables for the optional reveal: what complete, accurate histories
# show for the simulated vehicle, and what complete histories show as guests
# recalled them. The gap between the two is the cost of recall error.
truth_summary <- function(s) {
  vehicle <- s$sc$truth$vehicle
  table <- function(records) {
    records$case_status <- ifelse(records$ill, "Case", "Non-case")
    attack_rates(records, vehicle)
  }
  list(actual = table(s$truth), recalled = table(s$reported))
}
