read_scenario <- function(path = "scenarios/potluck-01/scenario.yaml") {
  sc <- yaml::read_yaml(path)
  required <- c("id", "seed", "setting", "pathogen", "foods", "truth", "tasks", "actions")
  if (!all(required %in% names(sc))) stop("Scenario is missing required sections.")
  scalar <- function(x, lo, hi) is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x >= lo && x <= hi
  if (!is.character(sc$id) || length(sc$id) != 1L || !grepl("^[a-z][a-z0-9-]*$", sc$id) ||
      !is.character(sc$title) || length(sc$title) != 1L || !nzchar(sc$title) ||
      !scalar(sc$seed, 0, 2147483000) || sc$seed != floor(sc$seed) ||
      !scalar(sc$setting$attendees, 3, 10000) || sc$setting$attendees != floor(sc$setting$attendees) ||
      !scalar(sc$pathogen$incubation_hours$median, .01, 10000) ||
      !scalar(sc$pathogen$incubation_hours$p95, .01, 10000)) stop("Invalid scenario identity, seed, population, or incubation.")
  ids <- vapply(sc$foods, `[[`, character(1), "id")
  if (anyDuplicated(ids) || !sc$truth$vehicle %in% ids) stop("Invalid food identifiers or vehicle.")
  probabilities <- c(vapply(sc$foods, `[[`, numeric(1), "p_eaten"),
    unlist(sc$pathogen$symptoms), unlist(sc$recall), sc$setting$guest_list_completeness,
    sc$truth$attack_rate_exposed, sc$truth$attack_rate_unexposed)
  if (any(!is.finite(probabilities) | probabilities < 0 | probabilities > 1)) stop("Invalid probability.")
  if (sc$setting$attendees < 3 || sc$pathogen$incubation_hours$p95 <= sc$pathogen$incubation_hours$median) stop("Invalid population or incubation.")
  if (any(!grepl("^[a-z][a-z0-9_]*$", ids))) stop("Invalid food identifiers.")
  for (i in seq_along(sc$foods)) {
    f <- sc$foods[[i]]
    if (!is.null(f$conditional_on)) {
      if (!f$conditional_on %in% head(ids, i - 1L)) stop("Conditional exposure must reference an earlier food.")
      p <- c(f$p_if_exposed, f$p_if_unexposed)
      if (length(p) != 2 || any(!is.finite(p) | p < 0 | p > 1)) stop("Invalid conditional probability.")
    }
  }
  for (i in seq_along(sc$events)) {
    e <- sc$events[[i]]
    if (is.null(e$id)) e$id <- paste0("event-", i)
    if (is.null(e$type)) e$type <- "reports"
    if (is.null(e$source)) e$source <- if (e$type == "media") "Local reporter" else "Guest reports"
    if (length(e$at_hour) != 1 || !is.finite(e$at_hour) || e$at_hour < 0 || !e$type %in% c("reports", "media", "evidence")) stop("Invalid scheduled event.")
    sc$events[[i]] <- e
  }
  task_ids <- vapply(sc$tasks, `[[`, character(1), "id")
  supported <- c("guest_list", "team_interviews", "food_prep_review", "stool_samples", "hold_leftovers", "leftover_testing")
  if (anyDuplicated(task_ids) || any(!task_ids %in% supported)) stop("Invalid task identifiers.")
  for (task in sc$tasks) {
    if (!scalar(task$effort_min, 0, 10000) || !scalar(task$turnaround_hours, 0, 10000) ||
        (!is.null(task$window_closes_hour) && !scalar(task$window_closes_hour, 0, 100000)) ||
        (!is.null(task$requires) && !task$requires %in% task_ids)) stop("Invalid task timing or dependency.")
  }
  action_ids <- vapply(sc$actions, `[[`, character(1), "id")
  if (anyDuplicated(action_ids)) stop("Duplicate action identifier.")
  if (any(!vapply(sc$actions, `[[`, character(1), "justified_if") %in% c("cluster_confirmed", "process_failure_known", "ongoing_exposure"))) stop("Invalid action condition.")
  if (!is.null(sc$analysis$stratifier) && !sc$analysis$stratifier %in% ids) stop("Invalid stratifier.")
  if (anyDuplicated(vapply(sc$events, `[[`, character(1), "id"))) stop("Duplicate event identifier.")
  sc
}

food_labels <- function(sc) setNames(vapply(sc$foods, `[[`, character(1), "label"),
  vapply(sc$foods, `[[`, character(1), "id"))

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
