read_scenario <- function(path = "scenarios/potluck-01/scenario.yaml") {
  sc <- yaml::read_yaml(path)
  # Name the offending field so an instructor can fix a scenario file quickly.
  fail <- function(...) stop("Scenario ", basename(dirname(path)), ": ", ..., call. = FALSE)
  required <- c("id", "title", "seed", "setting", "pathogen", "foods", "truth", "recall", "interviews", "tasks", "actions")
  missing <- setdiff(required, names(sc))
  if (length(missing)) fail("missing required sections: ", paste(missing, collapse = ", "), ".")
  scalar <- function(x, lo, hi) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x >= lo && x <= hi
  whole <- function(x, lo, hi) scalar(x, lo, hi) && x == floor(x)
  string <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))
  if (!string(sc$id) || !grepl("^[a-z][a-z0-9-]*$", sc$id)) fail("id must use lower-case letters, digits, and hyphens.")
  if (!string(sc$title)) fail("title must be a non-empty string.")
  if (!whole(sc$seed, 0, 2147483000)) fail("seed must be a whole number from 0 to 2147483000.")
  if (!whole(sc$setting$attendees, 3, 10000)) fail("setting.attendees must be a whole number from 3 to 10000.")
  if (!scalar(sc$setting$guest_list_completeness, 0, 1)) fail("setting.guest_list_completeness must be a proportion from 0 to 1.")
  if (!string(sc$pathogen$name)) fail("pathogen.name must be a non-empty string.")
  inc <- sc$pathogen$incubation_hours
  if (!scalar(inc$median, .01, 10000) || !scalar(inc$p95, .01, 10000) || inc$p95 <= inc$median) fail("pathogen.incubation_hours needs a median and a larger p95, in hours.")
  if (!length(sc$pathogen$symptoms) || is.null(names(sc$pathogen$symptoms))) fail("pathogen.symptoms must name at least one symptom.")
  # The questionnaire, case definitions, and interviews expect this symptom set.
  if (!setequal(names(sc$pathogen$symptoms), domain_fields("symptoms"))) fail("pathogen.symptoms must be exactly: ", paste(domain_fields("symptoms"), collapse = ", "), ".")
  if (!length(sc$foods) || !all(vapply(sc$foods, function(f) string(f$id) && string(f$label) && scalar(f$p_eaten, 0, 1), logical(1)))) fail("each food needs an id, a label, and p_eaten from 0 to 1.")
  if (!all(vapply(sc$recall[c("p_forget_item", "p_false_item")], scalar, logical(1), 0, 1))) fail("recall needs p_forget_item and p_false_item from 0 to 1.")
  if (!whole(sc$interviews$max, 1, 100) || !scalar(sc$interviews$minutes_per_topic, 0, 600)) fail("interviews needs a whole-number max and minutes_per_topic.")
  if (!string(sc$truth$vehicle) || !string(sc$truth$process_failure)) fail("truth needs a vehicle and a process_failure description.")
  ids <- vapply(sc$foods, `[[`, character(1), "id")
  if (anyDuplicated(ids) || any(!grepl("^[a-z][a-z0-9_]*$", ids))) fail("food ids must be unique lower-case identifiers.")
  if (!sc$truth$vehicle %in% ids) fail("truth.vehicle must be one of the food ids.")
  # The menu order is the order learners see. Keeping the vehicle out of first
  # place avoids anchoring; analysis tools follow this order too.
  if (is.null(sc$menu_order)) sc$menu_order <- ids
  if (!is.character(sc$menu_order) || !setequal(sc$menu_order, ids) || anyDuplicated(sc$menu_order)) fail("menu_order must list every food id exactly once.")
  probabilities <- c(unlist(sc$pathogen$symptoms), sc$truth$attack_rate_exposed, sc$truth$attack_rate_unexposed)
  if (length(probabilities) != length(sc$pathogen$symptoms) + 2L || !is.numeric(probabilities) || any(!is.finite(probabilities) | probabilities < 0 | probabilities > 1)) fail("symptom frequencies and truth attack rates must be proportions from 0 to 1.")
  for (i in seq_along(sc$foods)) {
    f <- sc$foods[[i]]
    if (!is.null(f$conditional_on)) {
      if (!f$conditional_on %in% head(ids, i - 1L)) fail("food ", f$id, ": conditional_on must reference an earlier food.")
      p <- c(f$p_if_exposed, f$p_if_unexposed)
      if (length(p) != 2 || any(!is.finite(p) | p < 0 | p > 1)) fail("food ", f$id, ": p_if_exposed and p_if_unexposed must be proportions.")
    }
  }
  for (i in seq_along(sc$events)) {
    e <- sc$events[[i]]
    if (is.null(e$id)) e$id <- paste0("event-", i)
    if (is.null(e$type)) e$type <- "reports"
    if (is.null(e$source)) e$source <- if (e$type == "media") "Local reporter" else "Guest reports"
    if (!scalar(e$at_hour, 0, 100000) || !string(e$type) || !e$type %in% c("reports", "media", "evidence") || !string(e$text)) fail("event ", e$id, " needs at_hour, text, and a type of reports, media, or evidence.")
    sc$events[[i]] <- e
  }
  if (!all(vapply(sc$tasks, function(task) string(task$id) && string(task$label), logical(1)))) fail("each task needs an id and a label.")
  task_ids <- vapply(sc$tasks, `[[`, character(1), "id")
  supported <- c("guest_list", "team_interviews", "food_prep_review", "stool_samples", "hold_leftovers", "leftover_testing")
  if (anyDuplicated(task_ids) || any(!task_ids %in% supported)) fail("task ids must be unique and one of: ", paste(supported, collapse = ", "), ".")
  for (task in sc$tasks) {
    if (!scalar(task$effort_min, 0, 10000) || !scalar(task$turnaround_hours, 0, 10000) ||
        (!is.null(task$window_closes_hour) && !scalar(task$window_closes_hour, 0, 100000)) ||
        (!is.null(task$requires) && !task$requires %in% task_ids)) fail("task ", task$id, " has invalid timing or an unknown dependency.")
  }
  conditions <- c("cluster_confirmed", "process_failure_known", "ongoing_exposure")
  if (!all(vapply(sc$actions, function(a) string(a$id) && string(a$label) && string(a$justified_if) && a$justified_if %in% conditions, logical(1)))) fail("each action needs an id, a label, and justified_if of: ", paste(conditions, collapse = ", "), ".")
  if (anyDuplicated(vapply(sc$actions, `[[`, character(1), "id"))) fail("action ids must be unique.")
  if (!is.null(sc$analysis$stratifier) && !sc$analysis$stratifier %in% ids) fail("analysis.stratifier must be one of the food ids.")
  if (anyDuplicated(vapply(sc$events, `[[`, character(1), "id"))) fail("event ids must be unique.")
  sc
}

# Foods in menu order: the order used for questionnaires, tables, and downloads.
food_labels <- function(sc) {
  labels <- setNames(vapply(sc$foods, `[[`, character(1), "label"), vapply(sc$foods, `[[`, character(1), "id"))
  if (is.null(sc$menu_order)) labels else labels[sc$menu_order]
}

scenario_catalog <- function(directory = "scenarios") {
  paths <- list.files(directory, "scenario.yaml$", recursive = TRUE, full.names = TRUE)
  dplyr::bind_rows(lapply(paths, function(path) {
    sc <- read_scenario(path)
    data.frame(id = sc$id, title = sc$title, difficulty = if (is.null(sc$difficulty)) "Introductory" else sc$difficulty)
  }))
}

start_session <- function(id = "potluck-01", randomize = FALSE, seed = NULL, directory = "scenarios") {
  catalog <- scenario_catalog(directory)
  if (length(id) != 1L || !id %in% catalog$id) stop("Unknown scenario.")
  sc <- read_scenario(file.path(directory, id, "scenario.yaml"))
  if (!is.null(seed)) {
    if (length(seed) != 1 || !is.finite(seed) || seed < 0 || seed > 2147483000 || seed != floor(seed)) stop("Invalid seed.")
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

teaching_signal <- function(s) {
  # Evaluate the reported, classified dataset a complete collection can reveal.
  s$clock <- 72
  d <- reported_as_of(s)
  if (sum(s$reported$ill & s$reported$report_hour == 0, na.rm = TRUE) < 2 ||
      sum(s$reported$ill & s$reported$report_hour == 28, na.rm = TRUE) != 3) return(FALSE)
  d$case_status <- classify_cases(d, s$definition)
  a <- attack_rates(d, names(food_labels(s$sc)))
  v <- a[a$food == s$sc$truth$vehicle, ]
  basic <- nrow(v) == 1 && is.finite(v$relative_risk) && v$relative_risk >= 3 &&
    v$exposed_total >= 10 && v$unexposed_total >= 10 && a$food[which.max(a$relative_risk)] == s$sc$truth$vehicle
  if (!isTRUE(basic)) return(FALSE)
  if (!is.null(s$sc$analysis$stratifier)) {
    strata <- stratified_rates(d, s$sc$truth$vehicle, s$sc$analysis$stratifier)
    confounded <- a[a$food == s$sc$analysis$stratifier, ]
    adjusted <- stratified_rates(d, s$sc$analysis$stratifier, s$sc$truth$vehicle)
    return(nrow(strata) == 2 && all(strata$exposed_total >= 5 & strata$unexposed_total >= 5) &&
      all(!is.na(strata$relative_risk) & strata$relative_risk > 2) &&
      is.finite(confounded$relative_risk) && confounded$relative_risk >= 1.5 &&
      all(is.finite(adjusted$relative_risk) & adjusted$relative_risk < confounded$relative_risk & adjusted$relative_risk <= 1.5))
  }
  TRUE
}
