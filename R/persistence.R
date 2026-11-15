# A typed JSON tree preserves empty vectors and data frames without evaluating code.
encode_session_value <- function(x) {
  if (is.null(x)) return(list(type = "null"))
  if (is.data.frame(x)) return(list(type = "frame", columns = lapply(x, encode_session_value)))
  if (is.list(x)) return(list(type = "list", names = names(x), values = lapply(unname(x), encode_session_value)))
  if (!typeof(x) %in% c("logical", "integer", "double", "character")) stop("Unsupported session value.")
  values <- if (is.double(x)) ifelse(is.na(x), NA_character_, sprintf("%.17g", x)) else as.character(x)
  list(type = typeof(x), names = names(x), values = values)
}

decode_session_value <- function(x, depth = 0L) {
  if (depth > 30 || !is.list(x) || length(x$type) != 1 || !is.character(x$type)) stop("Invalid session structure.")
  type <- x$type
  allowed <- switch(type, null = "type", frame = c("type", "columns"), c("type", "names", "values"))
  if (any(!names(x) %in% allowed) || anyDuplicated(names(x))) stop("Unknown saved node field.")
  if (type == "null") return(NULL)
  if (type == "frame") {
    columns <- lapply(x$columns, decode_session_value, depth = depth + 1L)
    if (!length(columns) || is.null(names(columns)) || anyDuplicated(names(columns)) || any(vapply(columns, is.list, logical(1))) || length(unique(lengths(columns))) != 1) stop("Invalid saved data frame.")
    return(as.data.frame(columns, stringsAsFactors = FALSE))
  }
  if (type == "list") {
    result <- lapply(x$values, decode_session_value, depth = depth + 1L)
  } else {
    values <- vapply(x$values, function(v) if (is.null(v)) NA_character_ else {
      if (!is.character(v) || length(v) != 1) stop("Invalid session scalar.")
      v
    }, character(1), USE.NAMES = FALSE)
    result <- switch(type, logical = as.logical(values), integer = suppressWarnings(as.integer(values)),
      double = suppressWarnings(as.numeric(values)), character = values, stop("Unsupported saved type."))
    if (is.numeric(result) && any(is.infinite(result))) stop("Invalid non-finite saved number.")
    if (type != "character" && any(!is.na(values) & is.na(result))) stop("Invalid typed session value.")
  }
  if (!is.null(x$names)) {
    nms <- unlist(x$names, use.names = FALSE)
    if (length(nms) != length(result) || anyNA(nms) || anyDuplicated(nms)) stop("Invalid session names.")
    names(result) <- nms
  }
  result
}

save_session <- function(s, path, guide = list()) {
  public <- s[setdiff(names(s), private_fields())]
  record <- list(
    format = "fieldnotes-session", version = 1L, scenario = s$sc$id, seed = s$sc$seed,
    state = encode_session_value(public), guide = encode_session_value(guide)
  )
  jsonlite::write_json(record, path, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  invisible(path)
}

# State fields added after the first release. A file saved before a field
# existed restores with that field's starting value.
later_state_fields <- function() "leftovers_home"

restore_session <- function(path, directory = "scenarios") {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size > 5000000) {
    stop("Session file is missing or exceeds 5 MB.")
  }
  record <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) stop("Invalid session JSON."))
  fields <- c("format", "version", "scenario", "seed", "state", "guide")
  if (!is.list(record) || anyDuplicated(names(record)) || !setequal(names(record), fields) ||
      !identical(record$format, "fieldnotes-session") || !identical(record$version, 1L)) {
    stop("Unsupported session file.")
  }
  s <- start_session(record$scenario, seed = record$seed, directory = directory)
  saved <- decode_session_value(record$state)
  allowed <- setdiff(names(s), private_fields())
  if (!is.list(saved) || any(!names(saved) %in% allowed) ||
      any(!setdiff(allowed, names(saved)) %in% later_state_fields())) {
    stop("Unknown or missing saved state field.")
  }
  s[names(saved)] <- saved
  validate_session_shapes(s)
  validate_restored_state(s)
  guide <- decode_session_value(record$guide)
  validate_guide(guide)
  list(state = s, guide = guide)
}

# Fields a learner's records may hold: everything asked of guests, never simulated truth.
collectable_fields <- function(s) c(setdiff(names(s$reported), hidden_fields()), "stool_positive")

validate_restored_state <- function(s) {
  number <- function(x, minimum = 0, maximum = 100000) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x >= minimum && x <= maximum
  }
  if (!number(s$clock)) stop("Invalid saved clock.")
  ids_ok <- is.data.frame(s$collected) && all(c("id", "on_list") %in% names(s$collected)) &&
    !anyNA(s$collected$id) && !anyDuplicated(s$collected$id) && all(s$collected$id %in% s$reported$id)
  if (!ids_ok) stop("Invalid collected guest identifiers.")
  allowed <- collectable_fields(s)
  if (any(!names(s$collected) %in% allowed)) stop("Unknown collected field.")
  tasks <- vapply(s$sc$tasks, `[[`, character(1), "id")
  events <- vapply(s$sc$events, `[[`, character(1), "id")
  if (any(!s$completed %in% tasks) || any(!s$fired_events %in% events)) stop("Unknown saved task or event.")
  expected_events <- vapply(Filter(function(e) e$at_hour <= s$clock, s$sc$events), `[[`, character(1), "id")
  if (!setequal(s$fired_events, expected_events)) stop("Inconsistent saved event timeline.")
  if (anyDuplicated(vapply(s$queue, `[[`, character(1), "id"))) stop("Duplicate queued task.")
  questionnaire <- setdiff(allowed, c("id", "on_list", "stool_positive"))
  for (job in s$queue) {
    timeline_ok <- job$id %in% tasks && number(job$requested) && number(job$due) &&
      job$requested <= s$clock && job$due > s$clock
    if (!timeline_ok) stop("Invalid queued task timeline.")
    task <- s$sc$tasks[[match(job$id, tasks)]]
    expected_due <- job$requested + task$effort_min / 60 + task$turnaround_hours
    if (abs(job$due - expected_due) > 1e-8) stop("Invalid queued task duration.")
    if (job$id == "team_interviews" && (any(!job$people %in% s$reported$id) || any(!job$fields %in% questionnaire))) {
      stop("Invalid queued collection.")
    }
  }
  for (entry in c(s$evidence, s$log, s$checkpoints, s$actions, s$media_responses, s$instructor_reviews)) {
    if (!number(entry$time) || entry$time > s$clock) stop("Invalid saved record timeline.")
  }
  for (field in c("menu", "walk_ins", "process_known", "leftovers_home", "late_event", "revealed")) {
    if (!is.logical(s[[field]]) || length(s[[field]]) != 1 || is.na(s[[field]])) stop("Invalid saved flag.")
  }
  for (def in c(list(s$definition), s$case_defs)) {
    definition_ok <- def$person %in% c("all", "rsvp") && def$clinical %in% c("standard", "diarrhea", "any") &&
      number(def$start) && number(def$end) && def$start < def$end &&
      is.logical(def$lab) && length(def$lab) == 1 && !is.na(def$lab)
    if (!definition_ok) stop("Invalid saved definition.")
  }
  for (response in s$media_responses) {
    if (!response$id %in% events || any(!response$evidence_ids %in% seq_along(s$evidence))) {
      stop("Invalid communication reference.")
    }
  }
  # Revalidate review values; imported self-study notes are not authenticated grades.
  for (review in s$instructor_reviews) instructor_override(s, review$criterion, review$status, review$reason)
  invisible(TRUE)
}

validate_session_shapes <- function(s) {
  fail <- function() stop("Invalid saved state shape or value.")
  check <- function(ok) if (!isTRUE(ok)) fail()
  text <- function(x, null = FALSE) {
    (null && is.null(x)) || (is.character(x) && length(x) == 1 && !is.na(x) && nchar(x) <= max_text_length())
  }
  strings <- function(x) is.character(x) && !anyNA(x) && length(x) <= 5000
  number <- function(x, min = 0, max = 100000) {
    is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x >= min && x <= max
  }
  flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
  shape <- function(x, required, optional = character()) {
    is.list(x) && !is.data.frame(x) && all(required %in% names(x)) &&
      all(names(x) %in% c(required, optional)) && !anyDuplicated(names(x))
  }
  entries <- function(x) is.list(x) && !is.data.frame(x) && length(x) <= 5000
  evidence <- function(x) {
    check(entries(x))
    for (e in x) {
      check(shape(e, c("time", "source", "finding")) && number(e$time, max = s$clock) && text(e$source) && text(e$finding))
    }
  }
  definition <- function(d) {
    check(shape(d, names(default_definition()), "time") && text(d$person) && d$person %in% c("all", "rsvp") &&
      text(d$clinical) && d$clinical %in% c("standard", "diarrhea", "any") && text(d$place) && text(d$reason) &&
      flag(d$lab) && number(d$start) && number(d$end) && d$start < d$end)
  }
  # Onset may fall before the meal for unrelated illness; other numbers are never negative.
  lowest <- c(age = 0, id = 0, onset_hours = -1000, report_hour = 0)
  table <- function(d, snapshot = FALSE) {
    check(is.data.frame(d) && all(c("id", "on_list") %in% names(d)) && nrow(d) <= nrow(s$reported) &&
      is.numeric(d$id) && !anyNA(d$id) && !anyDuplicated(d$id) && all(d$id %in% s$reported$id))
    check(all(names(d) %in% c(collectable_fields(s), if (snapshot) "case_status")))
    for (name in setdiff(names(d), c("sex", "case_status", names(lowest)))) check(is.logical(d[[name]]))
    for (name in intersect(names(d), names(lowest))) {
      x <- d[[name]]
      check(is.numeric(x) && all(is.na(x) | (is.finite(x) & x >= lowest[[name]] & x <= 100000)))
    }
    if ("sex" %in% names(d)) check(is.character(d$sex) && all(is.na(d$sex) | d$sex %in% c("F", "M")))
    if (snapshot) {
      check("case_status" %in% names(d) && is.character(d$case_status) && !anyNA(d$case_status) &&
        all(d$case_status %in% case_status_levels()))
    }
  }
  lists <- c(
    "queue", "evidence", "log", "chats", "checkpoints", "actions", "case_defs", "downloads", "checks",
    "media_events", "media_responses", "instructor_reviews"
  )
  for (field in lists) check(entries(s[[field]]))
  for (field in c("completed", "fired_events", "interviewed")) check(strings(s[[field]]))
  contacts <- c("organizer", "cook", paste0("guest_", s$reported$id))
  check(all(s$interviewed %in% contacts) && !anyDuplicated(s$interviewed) && length(s$interviewed) <= s$sc$interviews$max)
  if (length(s$chats)) check(!is.null(names(s$chats)) && !anyDuplicated(names(s$chats)) && all(names(s$chats) %in% contacts))
  table(s$collected)
  definition(s$definition)
  evidence(s$evidence)
  for (d in s$case_defs) definition(d)
  check(text(s$recommendation, TRUE))
  if (!is.null(s$retry)) {
    check(shape(s$retry, c("checkpoint", "reason", "revision")) &&
      number(s$retry$checkpoint, 1, length(s$checkpoints)) && text(s$retry$reason) && text(s$retry$revision))
  }
  for (job in s$queue) {
    check(shape(job, c("id", "label", "requested", "due"), c("people", "fields", "audience")) &&
      text(job$id) && text(job$label) && number(job$requested) && number(job$due))
    if (job$id == "team_interviews") {
      check(is.numeric(job$people) && !anyNA(job$people) && !anyDuplicated(job$people) && strings(job$fields) &&
        text(job$audience) && job$audience %in% c("all", "ill"))
    }
  }
  for (chat in s$chats) {
    check(entries(chat))
    for (entry in chat) {
      check(shape(entry, c("time", "question", "topics", "reply"), c("dialogue_status", "authored_reply")) &&
        number(entry$time, max = s$clock) && text(entry$question) && strings(entry$topics) && text(entry$reply) &&
        text(entry$dialogue_status, TRUE) && text(entry$authored_reply, TRUE))
    }
  }
  reasoning <- c("stage", "suspect", "confidence", "supporting", "against", "change", "action")
  for (cp in s$checkpoints) {
    check(shape(cp, c("time", reasoning, "evidence")) && number(cp$time, max = s$clock) &&
      all(vapply(cp[reasoning], text, logical(1))))
    evidence(cp$evidence)
  }
  action_ids <- vapply(s$sc$actions, `[[`, character(1), "id")
  for (action in s$actions) {
    check(shape(action, c("time", "id", "label", "reason", "justified", "evidence")) &&
      number(action$time, max = s$clock) && text(action$id) && action$id %in% action_ids &&
      text(action$label) && text(action$reason) && flag(action$justified))
    evidence(action$evidence)
  }
  for (snap in s$downloads) {
    check(shape(snap, c("id", "time", "definition", "data")) && number(snap$id, 1) && snap$id == floor(snap$id) &&
      number(snap$time, max = s$clock))
    definition(snap$definition)
    table(snap$data, TRUE)
  }
  snapshot_ids <- vapply(s$downloads, function(snap) as.numeric(snap$id), numeric(1))
  for (item in s$checks) {
    check(shape(item, c("snapshot", "food", "result")) && number(item$snapshot, 1) && item$snapshot %in% snapshot_ids &&
      text(item$food) && item$food %in% names(food_labels(s$sc)) && shape(item$result, c("ok", "message")) &&
      flag(item$result$ok) && text(item$result$message))
  }
  for (entry in s$log) {
    check(shape(entry, c("time", "type", "detail")) && number(entry$time, max = s$clock) && text(entry$type))
  }
  for (e in s$media_events) {
    check(shape(e, media_event_fields()) && text(e$id) && text(e$type) && e$type == "media" &&
      number(e$at_hour, max = s$clock) && text(e$source) && text(e$text))
  }
  media_ids <- vapply(s$media_events, `[[`, character(1), "id")
  for (e in s$media_responses) {
    check(shape(e, c("time", "id", "text", "evidence_ids", "evidence")) && text(e$id) && e$id %in% media_ids &&
      number(e$time, max = s$clock) && text(e$text) && is.numeric(e$evidence_ids) && !anyNA(e$evidence_ids) &&
      all(e$evidence_ids == floor(e$evidence_ids)))
    evidence(e$evidence)
  }
  for (review in s$instructor_reviews) {
    check(shape(review, c("time", "criterion", "status", "reason")) && number(review$time, max = s$clock) &&
      text(review$criterion) && text(review$status) && text(review$reason))
  }
  invisible(TRUE)
}
