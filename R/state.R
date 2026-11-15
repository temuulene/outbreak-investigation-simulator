game_time <- function(hours) {
  minutes <- round(hours * 60) + 9 * 60
  days <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  paste0(days[(minutes %/% 1440) %% 7 + 1], " ", sprintf("%02d:%02d", (minutes %% 1440) %/% 60, minutes %% 60))
}

# The longest free-text answer a session keeps. Restore applies the same limit,
# so anything the app accepts can be saved and resumed.
max_text_length <- function() 20000L

# Stop with a learner-facing message unless `x` is one non-blank string within the limit.
check_text <- function(x, blank_message, limit = max_text_length()) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(trimws(x))) stop(blank_message, call. = FALSE)
  if (nchar(x) > limit) {
    stop("Keep each answer within ", format(limit, big.mark = ","), " characters.", call. = FALSE)
  }
  invisible(x)
}

# State fields that hold simulated truth. They are rebuilt from the scenario and
# seed, and never saved, exported, or shown.
private_fields <- function() c("sc", "truth", "reported", "lab")

new_state <- function(sc) {
  data <- generate_data(sc)
  list(
    sc = sc, truth = data$truth, reported = data$reported, lab = data$lab, clock = 0,
    collected = data.frame(id = integer(), on_list = logical()), queue = list(), completed = character(),
    evidence = list(list(time = 0, source = sc$briefing$source, finding = sc$briefing$finding)),
    log = list(), chats = list(), interviewed = character(), menu = FALSE, walk_ins = FALSE,
    process_known = FALSE, leftovers_home = FALSE, late_event = FALSE, checkpoints = list(), actions = list(),
    definition = default_definition(), case_defs = list(default_definition()), downloads = list(), checks = list(),
    recommendation = NULL, retry = NULL, revealed = FALSE, fired_events = character(),
    media_events = list(), media_responses = list(), instructor_reviews = list()
  )
}

record_event <- function(s, type, detail) {
  s$log <- append(s$log, list(list(time = s$clock, type = type, detail = detail)))
  s
}

add_evidence <- function(s, source, finding) {
  s$evidence <- append(s$evidence, list(list(time = s$clock, source = source, finding = finding)))
  record_event(s, "evidence", list(source = source, finding = finding))
}

advance_time <- function(s, hours) {
  stopifnot(length(hours) == 1, is.finite(hours), hours >= 0)
  target <- s$clock + hours
  repeat {
    due <- vapply(s$queue, `[[`, numeric(1), "due")
    next_due <- if (length(due)) min(due) else Inf
    events <- s$sc$events
    pending <- which(!vapply(events, `[[`, character(1), "id") %in% s$fired_events)
    event_due <- if (length(pending)) min(vapply(events[pending], `[[`, numeric(1), "at_hour")) else Inf
    at <- min(next_due, event_due)
    if (at > target) break
    s$clock <- at
    if (event_due == at) {
      for (idx in pending[vapply(events[pending], `[[`, numeric(1), "at_hour") == at]) {
        event <- events[[idx]]
        s$fired_events <- c(s$fired_events, event$id)
        if (event$type == "media") s$media_events <- append(s$media_events, list(event[media_event_fields()]))
        if (event$type == "reports") s$late_event <- TRUE
        s <- add_evidence(s, event$source, event$text)
        s <- record_event(s, "scheduled_event", event)
      }
    }
    ready <- which(due == at)
    jobs <- s$queue[ready]
    if (length(ready)) s$queue <- s$queue[-ready]
    for (job in jobs) s <- complete_task(s, job)
  }
  s$clock <- target
  s
}

media_event_fields <- function() c("id", "type", "at_hour", "source", "text")

complete_task <- function(s, job) {
  s$completed <- unique(c(s$completed, job$id))
  message <- switch(job$id,
    team_interviews = {
      reached <- responders(s, job$people)
      s <- collect_records(s, reached, job$fields)
      silent <- length(job$people) - length(reached)
      paste0(
        length(reached), " of ", length(job$people), " guests on the plan answered. ",
        if (silent) paste0(silent, " did not respond and are not in the line list. ") else "",
        "Only the questionnaire fields were collected."
      )
    },
    guest_list = paste0(
      "Pat sends ", sum(s$reported$on_list),
      " RSVP names. 'This is everyone who RSVP'd; a few people came without replying.'"
    ),
    food_prep_review = {
      s$process_known <- TRUE
      paste(s$sc$truth$process_failure, "A process failure is not proof of the source.")
    },
    stool_samples = {
      s$collected$stool_positive <- rep(NA, nrow(s$collected))
      sampled <- head(s$reported$id[which(has_reported(s, job$requested))], 3)
      if (length(sampled)) {
        s <- collect_records(s, sampled, character())
        positive <- s$reported$stool_detectable[match(sampled, s$reported$id)]
        s$collected$stool_positive[match(sampled, s$collected$id)] <- positive
        stool_message(s, sum(positive), length(sampled))
      } else {
        "No guest had reported illness when samples were requested, so none were collected."
      }
    },
    leftover_testing = leftover_message(s),
    "Pat labels and preserves the leftovers for environmental health sampling."
  )
  add_evidence(s, job$label, message)
}

stool_message <- function(s, positive, sampled) {
  if (!positive) {
    return(paste0(
      "No pathogen was isolated from ", sampled, " stool samples. ",
      "A negative culture does not rule out infection."
    ))
  }
  matched <- "leftover_testing" %in% s$completed && s$lab$food_detected
  paste0(
    "The lab isolates ", s$sc$pathogen$name, " from ", positive, " of ", sampled, " stool samples. ",
    if (matched) "The isolates match the one from the preserved leftovers. " else "",
    if (positive < sampled) "A negative culture does not rule out infection. " else "",
    "A stool result confirms the illness; it does not identify a food vehicle."
  )
}

leftover_message <- function(s) {
  if (!s$lab$food_detected) {
    return(paste(
      "No pathogen was isolated from the preserved leftovers.",
      "A negative food sample does not rule a food out: organisms can be unevenly spread or no longer viable."
    ))
  }
  # Only claim a match when there are stool isolates to compare with.
  stool <- isTRUE(any(s$collected$stool_positive))
  paste0(
    "Among the preserved leftovers, only ", food_labels(s$sc)[[s$sc$truth$vehicle]], " tests positive for ",
    s$sc$pathogen$name, ". ",
    if (stool) "The isolate matches the stool isolates. " else "No stool isolates are available for comparison yet. ",
    "A negative food sample does not rule that food out."
  )
}

request_task <- function(s, id, audience = "all", domains = character(), foods = character()) {
  task <- Filter(function(x) identical(x$id, id), s$sc$tasks)
  if (!length(task)) stop("Unknown task.")
  task <- task[[1]]
  busy <- c(if (id != "team_interviews") s$completed, vapply(s$queue, `[[`, character(1), "id"))
  if (id %in% busy) stop("That task has already been requested.")
  if (!is.null(task$requires) && !task$requires %in% s$completed) {
    stop("Preserve the leftovers before requesting testing.")
  }
  if (!is.null(task$window_closes_hour) && s$clock >= task$window_closes_hour) {
    stop("The fridge has already been cleared; leftovers are unavailable.")
  }
  job <- list(
    id = id, label = task$label, requested = s$clock,
    due = s$clock + task$effort_min / 60 + task$turnaround_hours
  )
  if (id == "team_interviews") {
    job$people <- plan_people(s, audience)
    if (!length(job$people)) stop("Request and receive the guest list first.")
    job$fields <- domain_fields(domains, if (s$menu) foods else character())
    if (!length(job$fields)) stop("Choose at least one questionnaire field.")
    job$audience <- audience
  }
  s$queue <- append(s$queue, list(job))
  s <- record_event(s, "task_requested", job)
  advance_time(s, task$effort_min / 60)
}

# Each condition is judged from what the investigation has established so far,
# with a note for the learner when it is not met.
action_conditions <- function() {
  c("cluster_confirmed", "process_failure_known", "leftovers_taken_home", "ongoing_exposure")
}

action_condition_met <- function(s, condition) {
  switch(condition,
    cluster_confirmed = sum(has_reported(s)) >= 2,
    process_failure_known = s$process_known,
    leftovers_taken_home = s$leftovers_home,
    ongoing_exposure = isTRUE(s$sc$setting$ongoing_exposure),
    FALSE
  )
}

action_condition_note <- function(condition) {
  switch(condition,
    cluster_confirmed = "Fewer than two illness reports are on record.",
    process_failure_known = "No preparation or storage failure is on record yet.",
    leftovers_taken_home = "You have not yet established that any leftovers left the hall.",
    ongoing_exposure = "Exposure at the hall ended Saturday; nothing on record shows it continuing.",
    "The evidence condition for this action is not met."
  )
}

take_action <- function(s, id, reason) {
  check_text(reason, "Record why this action is justified now.")
  action <- Filter(function(x) identical(x$id, id), s$sc$actions)
  if (!length(action)) stop("Choose an available action.")
  action <- action[[1]]
  entry <- list(
    time = s$clock, id = id, label = action$label, reason = reason,
    justified = isTRUE(action_condition_met(s, action$justified_if)), evidence = s$evidence
  )
  s$actions <- append(s$actions, list(entry))
  record_event(s, "action", entry)
}

submit_media_response <- function(s, id, text, evidence_ids = integer()) {
  if (length(id) != 1 || !id %in% vapply(s$media_events, `[[`, character(1), "id")) {
    stop("That media event is not available yet.")
  }
  if (length(text) != 1 || !nzchar(trimws(text)) || nchar(text) > 4000) {
    stop("Write a response of 1 to 4000 characters.")
  }
  known <- !anyNA(evidence_ids) && all(evidence_ids == floor(evidence_ids)) &&
    all(evidence_ids %in% seq_along(s$evidence))
  if (!known) stop("Choose available evidence references.")
  entry <- list(
    time = s$clock, id = id, text = trimws(text), evidence_ids = unique(evidence_ids),
    evidence = s$evidence[evidence_ids]
  )
  s$media_responses <- append(s$media_responses, list(entry))
  record_event(s, "media_response", entry)
}
