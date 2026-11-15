# Words and phrases that come from scenario files. Topic matching and the
# optional dialogue reviewer use them, so no food or clue is named in R code.
# Reading a scenario adds its terms; terms only ever widen what is recognized.
scenario_vocabulary <- new.env(parent = emptyenv())
scenario_vocabulary$food_words <- character()
scenario_vocabulary$locked_terms <- character()

register_vocabulary <- function(sc) {
  labels <- vapply(sc$foods, `[[`, character(1), "label")
  words <- unlist(strsplit(tolower(labels), "[^a-z]+"))
  words <- words[nchar(words) >= 3]
  # Accept a singular form of a plural label word ("sandwiches" -> "sandwich").
  words <- unique(c(words, sub("(?<=[sxz]|ch|sh)es$|(?<![s])s$", "", words, perl = TRUE)))
  scenario_vocabulary$food_words <- union(scenario_vocabulary$food_words, words)
  scenario_vocabulary$locked_terms <- union(scenario_vocabulary$locked_terms, c(labels, unlist(sc$cook$locked_terms)))
  invisible(sc)
}

read_scenario <- function(path = "scenarios/potluck-01/scenario.yaml") {
  sc <- yaml::read_yaml(path)
  # Name the offending field so an instructor can fix a scenario file quickly.
  fail <- function(...) stop("Scenario ", basename(dirname(path)), ": ", ..., call. = FALSE)
  required <- c(
    "id", "title", "seed", "setting", "briefing", "pathogen", "foods", "truth", "recall",
    "cook", "interviews", "tasks", "actions"
  )
  missing <- setdiff(required, names(sc))
  if (length(missing)) fail("missing required sections: ", paste(missing, collapse = ", "), ".")
  scalar <- function(x, lo, hi) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x >= lo && x <= hi
  }
  whole <- function(x, lo, hi) scalar(x, lo, hi) && x == floor(x)
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  proportion <- function(x) scalar(x, 0, 1)
  optional <- function(x, check) is.null(x) || check(x)

  if (!string(sc$id) || !grepl("^[a-z][a-z0-9-]*$", sc$id)) fail("id must use lower-case letters, digits, and hyphens.")
  if (!string(sc$title)) fail("title must be a non-empty string.")
  if (!whole(sc$seed, 0, 2147483000)) fail("seed must be a whole number from 0 to 2147483000.")
  if (!whole(sc$setting$attendees, 3, 10000)) fail("setting.attendees must be a whole number from 3 to 10000.")
  if (!proportion(sc$setting$guest_list_completeness)) {
    fail("setting.guest_list_completeness must be a proportion from 0 to 1.")
  }
  if (is.null(sc$setting$meal_date)) sc$setting$meal_date <- "2026-06-06"
  meal <- tryCatch(as.Date(sc$setting$meal_date, format = "%Y-%m-%d"), error = function(e) NA)
  # The story is fixed: a Saturday 18:00 meal and a Monday 09:00 call.
  if (!string(sc$setting$meal_date) || is.na(meal) || as.POSIXlt(meal)$wday != 6) {
    fail("setting.meal_date must be a Saturday written as YYYY-MM-DD.")
  }
  for (flag in c("leftovers_taken_home", "ongoing_exposure")) {
    if (!optional(sc$setting[[flag]], function(x) is.logical(x) && length(x) == 1L && !is.na(x))) {
      fail("setting.", flag, " must be true or false.")
    }
  }
  if (!string(sc$briefing$source) || !string(sc$briefing$finding)) fail("briefing needs a source and a finding.")
  if (!string(sc$pathogen$name)) fail("pathogen.name must be a non-empty string.")
  inc <- sc$pathogen$incubation_hours
  if (!scalar(inc$median, .01, 10000) || !scalar(inc$p95, .01, 10000) || inc$p95 <= inc$median) {
    fail("pathogen.incubation_hours needs a median and a larger p95, in hours.")
  }
  # The questionnaire, case definitions, and interviews expect this symptom set.
  symptom_set <- function(x) {
    length(x) && !is.null(names(x)) && setequal(names(x), domain_fields("symptoms")) &&
      all(vapply(x, proportion, logical(1)))
  }
  if (!symptom_set(sc$pathogen$symptoms)) {
    fail("pathogen.symptoms must give a proportion for exactly: ", paste(domain_fields("symptoms"), collapse = ", "), ".")
  }
  bg <- sc$background_illness
  if (!is.null(bg)) {
    if (!proportion(bg$p_symptomatic) || !proportion(bg$p_self_report) || !symptom_set(bg$symptoms) ||
        !scalar(bg$onset_hours$from, -1000, 10000) || !scalar(bg$onset_hours$to, -1000, 10000) ||
        bg$onset_hours$to <= bg$onset_hours$from) {
      fail("background_illness needs p_symptomatic, p_self_report, symptoms, and onset_hours from/to.")
    }
  }
  food_ok <- function(f) string(f$id) && string(f$label) && proportion(f$p_eaten)
  if (!length(sc$foods) || !all(vapply(sc$foods, food_ok, logical(1)))) {
    fail("each food needs an id, a label, and p_eaten from 0 to 1.")
  }
  ids <- vapply(sc$foods, `[[`, character(1), "id")
  if (anyDuplicated(ids) || any(!grepl("^[a-z][a-z0-9_]*$", ids))) fail("food ids must be unique lower-case identifiers.")
  if (!all(vapply(sc$recall[c("p_forget_item", "p_false_item")], proportion, logical(1)))) {
    fail("recall needs p_forget_item and p_false_item from 0 to 1.")
  }
  rumour <- sc$recall$rumour
  if (!is.null(rumour) && (!string(rumour$food) || !rumour$food %in% ids || !proportion(rumour$p_false_if_ill))) {
    fail("recall.rumour needs a food id and p_false_if_ill from 0 to 1.")
  }
  options <- list(
    reporting.p_self_report = sc$reporting$p_self_report, response.p_respond_ill = sc$response$p_respond_ill,
    response.p_respond_well = sc$response$p_respond_well, lab.stool_sensitivity = sc$lab$stool_sensitivity,
    lab.food_sensitivity = sc$lab$food_sensitivity
  )
  for (name in names(options)) if (!optional(options[[name]], proportion)) fail(name, " must be a proportion from 0 to 1.")
  if (!whole(sc$interviews$max, 1, 100) || !scalar(sc$interviews$minutes_per_topic, 0, 600)) {
    fail("interviews needs a whole-number max and minutes_per_topic.")
  }
  if (!string(sc$truth$vehicle) || !string(sc$truth$process_failure)) {
    fail("truth needs a vehicle and a process_failure description.")
  }
  if (!sc$truth$vehicle %in% ids) fail("truth.vehicle must be one of the food ids.")
  if (!proportion(sc$truth$attack_rate_exposed) || !proportion(sc$truth$attack_rate_unexposed)) {
    fail("truth attack rates must be proportions from 0 to 1.")
  }
  if (!string(sc$cook$dish) || !sc$cook$dish %in% ids || !string(sc$cook$preparation) ||
      !optional(sc$cook$locked_terms, function(x) is.character(x) && !anyNA(x))) {
    fail("cook needs a dish (a food id), a preparation account, and optional locked_terms.")
  }
  # The menu order is the order learners see. Keeping the vehicle out of first
  # place avoids anchoring; analysis tools follow this order too.
  if (is.null(sc$menu_order)) sc$menu_order <- ids
  if (!is.character(sc$menu_order) || !setequal(sc$menu_order, ids) || anyDuplicated(sc$menu_order)) {
    fail("menu_order must list every food id exactly once.")
  }
  for (i in seq_along(sc$foods)) {
    f <- sc$foods[[i]]
    if (is.null(f$conditional_on)) next
    if (!f$conditional_on %in% head(ids, i - 1L)) fail("food ", f$id, ": conditional_on must reference an earlier food.")
    if (!proportion(f$p_if_exposed) || !proportion(f$p_if_unexposed)) {
      fail("food ", f$id, ": p_if_exposed and p_if_unexposed must be proportions.")
    }
  }
  for (i in seq_along(sc$events)) {
    e <- sc$events[[i]]
    if (is.null(e$id)) e$id <- paste0("event-", i)
    if (is.null(e$type)) e$type <- "reports"
    if (is.null(e$source)) e$source <- if (e$type == "media") "Local reporter" else "Guest reports"
    if (!scalar(e$at_hour, 0, 100000) || !string(e$type) || !e$type %in% c("reports", "media", "evidence") ||
        !string(e$text) || !string(e$source)) {
      fail("event ", e$id, " needs at_hour, text, and a type of reports, media, or evidence.")
    }
    late <- e$late_reporters
    if (!is.null(late) && (!whole(late$count, 1, 1000) || !scalar(late$onset_from, 0, 10000) ||
        !scalar(late$onset_to, 0, 10000) || late$onset_to <= late$onset_from)) {
      fail("event ", e$id, ": late_reporters needs count, onset_from, and a larger onset_to.")
    }
    sc$events[[i]] <- e
  }
  if (anyDuplicated(vapply(sc$events, `[[`, character(1), "id"))) fail("event ids must be unique.")
  if (!all(vapply(sc$tasks, function(task) string(task$id) && string(task$label), logical(1)))) {
    fail("each task needs an id and a label.")
  }
  task_ids <- vapply(sc$tasks, `[[`, character(1), "id")
  supported <- c("guest_list", "team_interviews", "food_prep_review", "stool_samples", "hold_leftovers", "leftover_testing")
  if (anyDuplicated(task_ids) || any(!task_ids %in% supported)) {
    fail("task ids must be unique and one of: ", paste(supported, collapse = ", "), ".")
  }
  for (task in sc$tasks) {
    if (!scalar(task$effort_min, 0, 10000) || !scalar(task$turnaround_hours, 0, 10000) ||
        !optional(task$window_closes_hour, function(x) scalar(x, 0, 100000)) ||
        !optional(task$requires, function(x) string(x) && x %in% task_ids)) {
      fail("task ", task$id, " has invalid timing or an unknown dependency.")
    }
  }
  action_ok <- function(a) {
    string(a$id) && string(a$label) && string(a$justified_if) && a$justified_if %in% action_conditions()
  }
  if (!all(vapply(sc$actions, action_ok, logical(1)))) {
    fail("each action needs an id, a label, and justified_if of: ", paste(action_conditions(), collapse = ", "), ".")
  }
  if (anyDuplicated(vapply(sc$actions, `[[`, character(1), "id"))) fail("action ids must be unique.")
  if (!is.null(sc$analysis$stratifier) && !sc$analysis$stratifier %in% ids) {
    fail("analysis.stratifier must be one of the food ids.")
  }
  if (is.null(sc$difficulty)) sc$difficulty <- "Introductory"
  register_vocabulary(sc)
}

# Foods in menu order: the order used for questionnaires, tables, and downloads.
food_labels <- function(sc) {
  labels <- setNames(vapply(sc$foods, `[[`, character(1), "label"), vapply(sc$foods, `[[`, character(1), "id"))
  if (is.null(sc$menu_order)) labels else labels[sc$menu_order]
}

# A food label as it reads mid-sentence: "the chicken salad sandwiches".
dish_phrase <- function(sc, id) {
  label <- food_labels(sc)[[id]]
  paste0(tolower(substr(label, 1, 1)), substring(label, 2))
}

# The Saturday 18:00 meal as a date-time. Times are local to the story; UTC
# keeps arithmetic free of daylight-saving shifts.
meal_time <- function(sc) as.POSIXct(paste(sc$setting$meal_date, "18:00"), tz = "UTC")

scenario_catalog <- function(directory = "scenarios") {
  paths <- list.files(directory, "scenario.yaml$", recursive = TRUE, full.names = TRUE)
  dplyr::bind_rows(lapply(paths, function(path) {
    sc <- read_scenario(path)
    data.frame(id = sc$id, title = sc$title, difficulty = sc$difficulty)
  }))
}

start_session <- function(id = "potluck-01", randomize = FALSE, seed = NULL, directory = "scenarios") {
  catalog <- scenario_catalog(directory)
  if (length(id) != 1L || !id %in% catalog$id) stop("Unknown scenario.")
  sc <- read_scenario(file.path(directory, id, "scenario.yaml"))
  if (!is.null(seed)) {
    if (length(seed) != 1 || !is.finite(seed) || seed < 0 || seed > 2147483000 || seed != floor(seed)) {
      stop("Invalid seed.")
    }
    sc$seed <- as.integer(seed)
    return(new_state(sc))
  }
  if (!isTRUE(randomize)) return(new_state(sc))
  first <- sample.int(2000000000L, 1)
  for (offset in 0:99) {
    sc$seed <- first + offset
    candidate <- new_state(sc)
    if (teaching_signal(candidate)) return(candidate)
  }
  stop("No suitable teaching seed found in 100 attempts. Use the authored scenario seed.")
}

# Does a seed give a readable lesson? Evaluated on complete follow-up of every
# attendee at 72 hours, as recalled, under the starting case definition.
teaching_signal <- function(s) {
  s$clock <- 72
  d <- reported_as_of(s)
  reports <- s$reported$report_hour[s$reported$ill]
  if (sum(reports == 0, na.rm = TRUE) < 2) return(FALSE)
  for (event in s$sc$events) {
    if (is.null(event$late_reporters)) next
    if (sum(reports == event$at_hour, na.rm = TRUE) != event$late_reporters$count) return(FALSE)
  }
  d$case_status <- classify_cases(d, s$definition)
  a <- attack_rates(d, names(food_labels(s$sc)))
  v <- a[a$food == s$sc$truth$vehicle, ]
  basic <- nrow(v) == 1 && is.finite(v$relative_risk) && v$relative_risk >= 3 &&
    v$exposed_total >= 10 && v$unexposed_total >= 10 && a$food[which.max(a$relative_risk)] == s$sc$truth$vehicle
  if (!isTRUE(basic)) return(FALSE)
  if (is.null(s$sc$analysis$stratifier)) return(TRUE)
  strata <- stratified_rates(d, s$sc$truth$vehicle, s$sc$analysis$stratifier)
  confounded <- a[a$food == s$sc$analysis$stratifier, ]
  adjusted <- stratified_rates(d, s$sc$analysis$stratifier, s$sc$truth$vehicle)
  nrow(strata) == 2 && all(strata$exposed_total >= 5 & strata$unexposed_total >= 5) &&
    all(!is.na(strata$relative_risk) & strata$relative_risk > 2) &&
    is.finite(confounded$relative_risk) && confounded$relative_risk >= 1.5 &&
    all(is.finite(adjusted$relative_risk) & adjusted$relative_risk < confounded$relative_risk &
      adjusted$relative_risk <= 1.5)
}
