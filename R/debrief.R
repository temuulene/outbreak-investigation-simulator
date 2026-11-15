save_checkpoint <- function(s, stage, suspect, confidence, supporting, against, change, action) {
  for (field in list(suspect, supporting, against, change, action)) {
    check_text(field, "Complete each reasoning field; 'none yet' is a valid evidence statement.")
  }
  entry <- list(time = s$clock, stage = stage, suspect = suspect, confidence = confidence,
    supporting = supporting, against = against, change = change, action = action, evidence = s$evidence)
  s$checkpoints <- append(s$checkpoints, list(entry))
  record_event(s, "checkpoint", entry)
}

# Reference tables for the optional reveal: what complete, accurate histories
# show, and what complete histories show as guests recalled them. The gap
# between the two is the cost of recall error. A rumoured food is included so
# learners can see an association that recall alone created.
truth_summary <- function(s) {
  table <- function(records, food) {
    records$case_status <- ifelse(records$ill, "Case", "Non-case")
    attack_rates(records, food)
  }
  vehicle <- s$sc$truth$vehicle
  result <- list(actual = table(s$truth, vehicle), recalled = table(s$reported, vehicle))
  rumour <- s$sc$recall$rumour$food
  if (!is.null(rumour)) {
    result$rumour <- list(food = rumour, actual = table(s$truth, rumour), recalled = table(s$reported, rumour))
  }
  result
}
