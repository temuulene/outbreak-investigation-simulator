game_time <- function(hours) {
  minutes <- round(hours * 60) + 9 * 60
  paste0(c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")[(minutes %/% 1440) %% 7 + 1],
    " ", sprintf("%02d:%02d", (minutes %% 1440) %/% 60, minutes %% 60))
}

new_state <- function(sc) {
  data <- generate_data(sc)
  list(sc = sc, truth = data$truth, reported = data$reported, clock = 0,
    collected = data.frame(id = integer(), on_list = logical()), queue = list(), completed = character(),
    evidence = list(list(time = 0, source = "Pat · initial call", finding = "Several guests report illness after Saturday's potluck. Exposure ended Saturday night. The fridge will be cleared Tuesday at 09:00.")),
    log = list(), chats = list(), interviewed = character(), menu = FALSE, walk_ins = FALSE,
    process_known = FALSE, late_event = FALSE, checkpoints = list(), actions = list(),
    definition = default_definition(), case_defs = list(default_definition()), downloads = list(), checks = list(),
    recommendation = NULL, retry = NULL, revealed = FALSE, fired_events = character(),
    media_events = list(), media_responses = list(), instructor_reviews = list())
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
        if (event$type == "media") s$media_events <- append(s$media_events, list(event))
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

complete_task <- function(s, job) {
  s$completed <- unique(c(s$completed, job$id))
  if (job$id == "team_interviews") {
    s <- collect_records(s, job$people, job$fields)
    message <- paste(length(job$people), "guests reached. Only the questionnaire fields were collected.")
  } else if (job$id == "guest_list") {
    message <- paste("Pat sends", sum(s$reported$on_list), "RSVP names. 'This is everyone who RSVP'd; a few people came without replying.'")
  } else if (job$id == "food_prep_review") {
    s$process_known <- TRUE
    message <- paste(s$sc$truth$process_failure, "A process failure is not proof of the source.")
  } else if (job$id == "stool_samples") {
    message <- paste("The lab identifies", s$sc$pathogen$name, "in sampled ill guests. This does not identify a food vehicle.")
    s$collected$stool_positive <- rep(NA, nrow(s$collected))
    sampled <- head(s$reported$id[which(s$reported$ill & s$reported$report_hour <= job$requested)], 3)
    s <- collect_records(s, sampled, character())
    s$collected$stool_positive[s$collected$id %in% sampled] <- TRUE
  } else if (job$id == "leftover_testing") {
    message <- paste("Preserved", food_labels(s$sc)[[s$sc$truth$vehicle]], "tests positive for", s$sc$pathogen$name, "matching the stool isolates.")
  } else message <- "Pat labels and preserves the leftovers for environmental health sampling."
  add_evidence(s, job$label, message)
}

request_task <- function(s, id, audience = "all", domains = character(), foods = character()) {
  task <- Filter(function(x) x$id == id, s$sc$tasks)
  if (!length(task)) stop("Unknown task.")
  task <- task[[1]]
  if (id %in% c(if (id != "team_interviews") s$completed, vapply(s$queue, `[[`, character(1), "id"))) stop("That task has already been requested.")
  if (!is.null(task$requires) && !task$requires %in% s$completed) stop("Preserve the leftovers before requesting testing.")
  if (!is.null(task$window_closes_hour) && s$clock >= task$window_closes_hour) stop("The fridge has already been cleared; leftovers are unavailable.")
  job <- list(id = id, label = task$label, requested = s$clock,
    due = s$clock + task$effort_min / 60 + task$turnaround_hours)
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

take_action <- function(s, id, reason) {
  if (!nzchar(trimws(reason))) stop("Record why this action is justified now.")
  action <- Filter(function(x) x$id == id, s$sc$actions)[[1]]
  justified <- switch(action$justified_if, cluster_confirmed = TRUE,
    process_failure_known = s$process_known, ongoing_exposure = FALSE, FALSE)
  entry <- list(time = s$clock, id = id, label = action$label, reason = reason,
    justified = justified, evidence = s$evidence)
  s$actions <- append(s$actions, list(entry))
  record_event(s, "action", entry)
}


submit_media_response <- function(s, id, text, evidence_ids = integer()) {
  if (length(id) != 1 || !id %in% vapply(s$media_events, `[[`, character(1), "id")) stop("That media event is not available yet.")
  if (length(text) != 1 || !nzchar(trimws(text)) || nchar(text) > 4000) stop("Write a response of 1 to 4000 characters.")
  if (anyNA(evidence_ids) || any(evidence_ids != floor(evidence_ids)) || any(!evidence_ids %in% seq_along(s$evidence))) stop("Choose available evidence references.")
  entry <- list(time = s$clock, id = id, text = trimws(text), evidence_ids = unique(evidence_ids), evidence = s$evidence[evidence_ids])
  s$media_responses <- append(s$media_responses, list(entry))
  record_event(s, "media_response", entry)
}
