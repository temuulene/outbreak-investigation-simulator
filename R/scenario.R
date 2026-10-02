read_scenario <- function(path = "scenarios/potluck-01/scenario.yaml") {
  sc <- yaml::read_yaml(path)
  required <- c("id", "seed", "setting", "pathogen", "foods", "truth", "tasks", "actions")
  if (!all(required %in% names(sc))) stop("Scenario is missing required sections.")
  ids <- vapply(sc$foods, `[[`, character(1), "id")
  if (anyDuplicated(ids) || !sc$truth$vehicle %in% ids) stop("Invalid food identifiers or vehicle.")
  probabilities <- c(vapply(sc$foods, `[[`, numeric(1), "p_eaten"),
    unlist(sc$pathogen$symptoms), unlist(sc$recall), sc$setting$guest_list_completeness,
    sc$truth$attack_rate_exposed, sc$truth$attack_rate_unexposed)
  if (any(!is.finite(probabilities) | probabilities < 0 | probabilities > 1)) stop("Invalid probability.")
  if (sc$setting$attendees < 3 || sc$pathogen$incubation_hours$p95 <= sc$pathogen$incubation_hours$median) stop("Invalid population or incubation.")
  sc
}

food_labels <- function(sc) setNames(vapply(sc$foods, `[[`, character(1), "label"),
  vapply(sc$foods, `[[`, character(1), "id"))
