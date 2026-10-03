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
  public <- s[setdiff(names(s), c("sc", "truth", "reported"))]
  record <- list(format = "fieldnotes-session", version = 1L, scenario = s$sc$id, seed = s$sc$seed,
    state = encode_session_value(public), guide = encode_session_value(guide))
  jsonlite::write_json(record, path, auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE)
  invisible(path)
}

validate_restored_state <- function(s) {
  scalar_number <- function(x, minimum = 0, maximum = 100000) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x >= minimum && x <= maximum
  if (!scalar_number(s$clock)) stop("Invalid saved clock.")
  if (!is.data.frame(s$collected) || !all(c("id", "on_list") %in% names(s$collected)) || anyNA(s$collected$id) || anyDuplicated(s$collected$id) || any(!s$collected$id %in% s$reported$id)) stop("Invalid collected guest identifiers.")
  allowed <- c(setdiff(names(s$reported), "ill"), "stool_positive")
  if (any(!names(s$collected) %in% allowed)) stop("Unknown collected field.")
  tasks <- vapply(s$sc$tasks, `[[`, character(1), "id")
  events <- vapply(s$sc$events, `[[`, character(1), "id")
  if (any(!s$completed %in% tasks) || any(!s$fired_events %in% events)) stop("Unknown saved task or event.")
  expected_events <- vapply(Filter(function(e) e$at_hour <= s$clock, s$sc$events), `[[`, character(1), "id")
  if (!setequal(s$fired_events, expected_events)) stop("Inconsistent saved event timeline.")
  if (anyDuplicated(vapply(s$queue, `[[`, character(1), "id"))) stop("Duplicate queued task.")
  for (job in s$queue) {
    if (!job$id %in% tasks || !scalar_number(job$requested) || !scalar_number(job$due) || job$requested > s$clock || job$due <= s$clock) stop("Invalid queued task timeline.")
    task <- s$sc$tasks[[match(job$id, tasks)]]
    expected_due <- job$requested + task$effort_min / 60 + task$turnaround_hours
    if (abs(job$due - expected_due) > 1e-8) stop("Invalid queued task duration.")
    if (job$id == "team_interviews" && (any(!job$people %in% s$reported$id) || any(!job$fields %in% setdiff(allowed, c("id", "on_list", "stool_positive"))))) stop("Invalid queued collection.")
  }
  for (entry in c(s$evidence, s$log, s$checkpoints, s$actions, s$media_responses, s$instructor_reviews)) {
    if (!scalar_number(entry$time) || entry$time > s$clock) stop("Invalid saved record timeline.")
  }
  for (field in c("menu", "walk_ins", "process_known", "late_event", "revealed")) {
    if (!is.logical(s[[field]]) || length(s[[field]]) != 1 || is.na(s[[field]])) stop("Invalid saved flag.")
  }
  for (def in c(list(s$definition), s$case_defs)) {
    if (!def$person %in% c("all", "rsvp") || !def$clinical %in% c("standard", "diarrhea", "any") || !scalar_number(def$start) || !scalar_number(def$end) || def$start >= def$end || !is.logical(def$lab) || length(def$lab) != 1 || is.na(def$lab)) stop("Invalid saved definition.")
  }
  for (response in s$media_responses) {
    if (!response$id %in% events || any(!response$evidence_ids %in% seq_along(s$evidence))) stop("Invalid communication reference.")
  }
  # Revalidate review values; imported self-study notes are not authenticated grades.
  for (review in s$instructor_reviews) instructor_override(s, review$criterion, review$status, review$reason)
  invisible(TRUE)
}

restore_session <- function(path, directory = "scenarios") {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size > 5000000) stop("Session file is missing or exceeds 5 MB.")
  record <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE), error = function(e) stop("Invalid session JSON."))
  if (!is.list(record) || anyDuplicated(names(record)) || !setequal(names(record), c("format", "version", "scenario", "seed", "state", "guide")) || !identical(record$format, "fieldnotes-session") || !identical(record$version, 1L)) stop("Unsupported session file.")
  s <- start_session(record$scenario, seed = record$seed, directory = directory)
  saved <- decode_session_value(record$state)
  allowed <- setdiff(names(s), c("sc", "truth", "reported"))
  if (!is.list(saved) || !setequal(names(saved), allowed)) stop("Unknown or missing saved state field.")
  s[allowed] <- saved[allowed]
  validate_session_shapes(s)
  validate_restored_state(s)
  guide <- decode_session_value(record$guide)
  validate_saved_guide(guide)
  list(state = s, guide = guide)
}


validate_session_shapes <- function(s) {
  fail <- function() stop("Invalid saved state shape or value.")
  text <- function(x, null = FALSE) (null && is.null(x)) || (is.character(x) && length(x) == 1 && !is.na(x) && nchar(x) <= 20000)
  strings <- function(x) is.character(x) && !anyNA(x) && length(x) <= 5000
  number <- function(x, min = 0, max = 100000) is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x >= min && x <= max
  flag <- function(x) is.logical(x) && length(x) == 1 && !is.na(x)
  shape <- function(x, required, optional = character()) is.list(x) && !is.data.frame(x) && all(required %in% names(x)) && all(names(x) %in% c(required, optional)) && !anyDuplicated(names(x))
  entries <- function(x) is.list(x) && !is.data.frame(x) && length(x) <= 5000
  evidence <- function(x) {
    if (!entries(x)) fail()
    for (e in x) if (!shape(e, c("time", "source", "finding")) || !number(e$time, max = s$clock) || !text(e$source) || !text(e$finding)) fail()
  }
  definition <- function(d) {
    if (!shape(d, names(default_definition()), "time") || !text(d$person) || !d$person %in% c("all", "rsvp") || !text(d$clinical) || !d$clinical %in% c("standard", "diarrhea", "any") || !text(d$place) || !text(d$reason) || !flag(d$lab) || !number(d$start) || !number(d$end) || d$start >= d$end) fail()
  }
  table <- function(d, snapshot = FALSE) {
    if (!is.data.frame(d) || !all(c("id", "on_list") %in% names(d)) || nrow(d) > nrow(s$reported) || !is.numeric(d$id) || anyNA(d$id) || anyDuplicated(d$id) || any(!d$id %in% s$reported$id)) fail()
    if (any(!names(d) %in% c(setdiff(names(s$reported), "ill"), "stool_positive", if (snapshot) "case_status"))) fail()
    for (name in setdiff(names(d), c("sex", "case_status", "age", "id", "onset_hours", "report_hour"))) if (!is.logical(d[[name]])) fail()
    for (name in intersect(names(d), c("age", "id", "onset_hours", "report_hour"))) if (!is.numeric(d[[name]]) || any(!is.na(d[[name]]) & (!is.finite(d[[name]]) | d[[name]] < 0 | d[[name]] > 100000))) fail()
    if ("sex" %in% names(d) && (!is.character(d$sex) || any(!is.na(d$sex) & !d$sex %in% c("F", "M")))) fail()
    if (snapshot && (!"case_status" %in% names(d) || !is.character(d$case_status) || anyNA(d$case_status) || any(!d$case_status %in% c("Case", "Non-case", "Unknown")))) fail()
  }
  for (field in c("queue", "evidence", "log", "chats", "checkpoints", "actions", "case_defs", "downloads", "checks", "media_events", "media_responses", "instructor_reviews")) if (!entries(s[[field]])) fail()
  for (field in c("completed", "fired_events", "interviewed")) if (!strings(s[[field]])) fail()
  contacts <- c("organizer", "cook", paste0("guest_", s$reported$id))
  if (any(!s$interviewed %in% contacts) || anyDuplicated(s$interviewed) || length(s$interviewed) > s$sc$interviews$max) fail()
  if (length(s$chats) && (is.null(names(s$chats)) || anyDuplicated(names(s$chats)) || any(!names(s$chats) %in% contacts))) fail()
  table(s$collected); definition(s$definition); evidence(s$evidence)
  for (d in s$case_defs) definition(d)
  if (!text(s$recommendation, TRUE)) fail()
  if (!is.null(s$retry) && (!shape(s$retry, c("checkpoint", "reason", "revision")) || !number(s$retry$checkpoint, 1, length(s$checkpoints)) || !text(s$retry$reason) || !text(s$retry$revision))) fail()
  for (job in s$queue) {
    if (!shape(job, c("id", "label", "requested", "due"), c("people", "fields", "audience")) || !text(job$id) || !text(job$label) || !number(job$requested) || !number(job$due)) fail()
    if (job$id == "team_interviews" && (!is.numeric(job$people) || anyNA(job$people) || anyDuplicated(job$people) || !strings(job$fields) || !text(job$audience) || !job$audience %in% c("all", "ill"))) fail()
  }
  for (chat in s$chats) {
    if (!entries(chat)) fail()
    for (entry in chat) if (!shape(entry, c("time", "question", "topics", "reply"), "dialogue_status") || !number(entry$time, max = s$clock) || !text(entry$question) || !strings(entry$topics) || !text(entry$reply) || (!is.null(entry$dialogue_status) && !text(entry$dialogue_status))) fail()
  }
  for (cp in s$checkpoints) {
    if (!shape(cp, c("time", "stage", "suspect", "confidence", "supporting", "against", "change", "action", "evidence")) || !number(cp$time, max = s$clock) || !all(vapply(cp[c("stage", "suspect", "confidence", "supporting", "against", "change", "action")], text, logical(1)))) fail()
    evidence(cp$evidence)
  }
  for (action in s$actions) {
    if (!shape(action, c("time", "id", "label", "reason", "justified", "evidence")) || !number(action$time, max = s$clock) || !text(action$id) || !action$id %in% vapply(s$sc$actions, `[[`, character(1), "id") || !text(action$label) || !text(action$reason) || !flag(action$justified)) fail()
    evidence(action$evidence)
  }
  for (snap in s$downloads) {
    if (!shape(snap, c("id", "time", "definition", "data")) || !number(snap$id, 1) || snap$id != floor(snap$id) || !number(snap$time, max = s$clock)) fail()
    definition(snap$definition); table(snap$data, TRUE)
  }
  for (check in s$checks) if (!shape(check, c("snapshot", "food", "result")) || !number(check$snapshot, 1) || !check$snapshot %in% vapply(s$downloads, `[[`, numeric(1), "id") || !text(check$food) || !check$food %in% names(food_labels(s$sc)) || !shape(check$result, c("ok", "message")) || !flag(check$result$ok) || !text(check$result$message)) fail()
  for (entry in s$log) if (!shape(entry, c("time", "type", "detail")) || !number(entry$time, max = s$clock) || !text(entry$type)) fail()
  for (e in s$media_events) if (!shape(e, c("id", "type", "at_hour", "source", "text")) || !text(e$id) || !text(e$type) || e$type != "media" || !number(e$at_hour, max = s$clock) || !text(e$source) || !text(e$text)) fail()
  for (e in s$media_responses) {
    if (!shape(e, c("time", "id", "text", "evidence_ids", "evidence")) || !text(e$id) || !e$id %in% vapply(s$media_events, `[[`, character(1), "id") || !number(e$time, max = s$clock) || !text(e$text) || !is.numeric(e$evidence_ids) || anyNA(e$evidence_ids) || any(e$evidence_ids != floor(e$evidence_ids))) fail()
    evidence(e$evidence)
  }
  for (review in s$instructor_reviews) if (!shape(review, c("time", "criterion", "status", "reason")) || !number(review$time, max = s$clock) || !text(review$criterion) || !text(review$status) || !text(review$reason)) fail()
  invisible(TRUE)
}

validate_saved_guide <- function(g) {
  if (!is.list(g) || any(!names(g) %in% c("step", "furthest", "drafts", "current_fields", "recommendation_draft"))) stop("Invalid saved guide.")
  for (field in intersect(names(g), c("step", "furthest"))) {
    x <- g[[field]]
    if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x != floor(x) || x < 1 || x > 10) stop("Invalid saved guide step.")
  }
  if (!is.null(g$step) && !is.null(g$furthest) && g$step > g$furthest) stop("Invalid saved guide progress.")
  fields <- function(x) {
    if (!is.list(x) || any(!names(x) %in% checkpoint_fields()) || any(!vapply(x, function(v) is.null(v) || (is.character(v) && length(v) == 1 && !is.na(v) && nchar(v) <= 20000), logical(1)))) stop("Invalid saved guide draft.")
  }
  if (!is.null(g$current_fields)) fields(g$current_fields)
  if (!is.null(g$drafts)) {
    if (!is.list(g$drafts) || any(!names(g$drafts) %in% c("2", "4", "8"))) stop("Invalid saved checkpoint draft.")
    for (draft in g$drafts) fields(draft)
  }
  if (!is.null(g$recommendation_draft) && (!is.character(g$recommendation_draft) || length(g$recommendation_draft) != 1 || is.na(g$recommendation_draft) || nchar(g$recommendation_draft) > 20000)) stop("Invalid recommendation draft.")
  invisible(TRUE)
}
