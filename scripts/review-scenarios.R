# Instructor reference for every authored scenario. Contains spoilers.
if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, encoding = "UTF-8")

table_rows <- function(rates, name) {
  vapply(seq_len(nrow(rates)), function(i) {
    r <- rates[i, ]
    sprintf(
      "| %s | %d / %d | %d / %d | %.2f |", r[[name]], r$exposed_cases, r$exposed_total, r$unexposed_cases,
      r$unexposed_total, r$relative_risk
    )
  }, character(1))
}
two_by_two <- function(r) {
  sprintf(
    "%d / %d against %d / %d (RR %.2f)", r$exposed_cases, r$exposed_total, r$unexposed_cases, r$unexposed_total,
    r$relative_risk
  )
}

lines <- c(
  "# Scenario reference review", "",
  "Instructor reference; contains scenario spoilers. Each authored seed is evaluated",
  "at 72 hours of game time with every attendee and all symptom, onset, and food fields.",
  "This complete follow-up reference differs from an earlier or incomplete learner collection:",
  "learners reach only guests who respond, and see only cases that have phoned in or been interviewed.", ""
)
for (id in scenario_catalog()$id) {
  s <- start_session(id)
  s <- advance_time(s, 72)
  s <- collect_records(s, s$reported$id, domain_fields(c("symptoms", "onset"), names(food_labels(s$sc))))
  truth <- s$truth
  reference <- truth_summary(s)
  rates <- attack_rates(line_list(s), names(food_labels(s$sc)))
  lines <- c(
    lines, paste("##", s$sc$title), "",
    paste0(
      s$sc$difficulty, " | ", s$sc$pathogen$name, " | Seed: ", s$sc$seed, " | Attendees: ", nrow(truth),
      " | Teaching signal accepted: ", teaching_signal(s)
    ), "",
    paste0(
      "Simulated cases: ", sum(truth$ill), ". Unrelated illness: ", sum(truth$background), ". Cases who phone in: ",
      sum(truth$ill & !is.na(truth$report_hour)), ". Guests who answer the team: ", sum(truth$responds), ". ",
      "Cases a stool culture would detect: ", sum(truth$stool_detectable), ". Leftover test detects the vehicle: ",
      s$lab$food_detected, "."
    ), "",
    paste0("With perfect recall the vehicle table is ", two_by_two(reference$actual), "."),
    "Recall error and unrelated illness account for the gap from the crude RR below.", "",
    "| Exposure | Exposed cases / total | Unexposed cases / total | Crude RR |", "| --- | ---: | ---: | ---: |",
    table_rows(rates, "food")
  )
  if (!is.null(reference$rumour)) {
    lines <- c(
      lines, "",
      paste0(
        "Rumoured food ", reference$rumour$food, ": truly ", two_by_two(reference$rumour$actual), "; as recalled ",
        two_by_two(reference$rumour$recalled), ". The association comes from differential recall."
      )
    )
  }
  lines <- c(
    lines, "", "Clinical criteria against simulated truth, any onset time:", "",
    "| Symptoms criterion | Sensitivity | Specificity |", "| --- | ---: | ---: |"
  )
  for (clinical in c("diarrhea", "standard", "any")) {
    definition <- default_definition()
    definition$clinical <- clinical
    definition$start <- -1000
    definition$end <- 1000
    case <- classify_cases(s$reported, definition) == "Case"
    lines <- c(lines, sprintf("| %s | %.2f | %.2f |", clinical, mean(case[truth$ill]), mean(!case[!truth$ill])))
  }
  if (!is.null(s$sc$analysis$stratifier)) {
    stratifier <- s$sc$analysis$stratifier
    vehicle <- s$sc$truth$vehicle
    strata <- stratified_rates(line_list(s), stratifier, vehicle)
    lines <- c(
      lines, "", paste("Association for", stratifier, "within levels of", vehicle, ":"), "",
      "| Stratum | Exposed cases / total | Unexposed cases / total | RR |", "| --- | ---: | ---: | ---: |",
      table_rows(strata, "stratum")
    )
    for (pair in list(c(stratifier, vehicle), c(vehicle, stratifier))) {
      pooled <- stratified_summary(line_list(s), pair[1], pair[2])
      lines <- c(lines, "", sprintf(
        "%s adjusted for %s: crude RR %.2f; Mantel-Haenszel RR %.2f (approx. 95%% CI %.2f to %.2f).", pair[1],
        pair[2], pooled$crude, pooled$adjusted[["rr"]], pooled$adjusted[["lower"]], pooled$adjusted[["upper"]]
      ))
    }
  }
  lines <- c(lines, "")
}
lines <- c(
  lines, "Random selection checks at most 100 seeds. It requires adequate comparison",
  "groups, a vehicle RR of at least 3 and the largest crude association, at least two",
  "initial ill contacts, and the scheduled number of late reporters. Scenarios with a",
  "stratifier also require an apparent association for that food that attenuates within",
  "vehicle strata, while the vehicle association remains within its strata.", "",
  "Selection deliberately favours a readable lesson. It is not a sample for estimating",
  "real-world outbreak frequencies or effects. Small cells, non-response, and recall errors still",
  "limit inference; a food association and a preparation failure do not alone establish cause."
)
writeLines(enc2utf8(lines), "docs/scenario-review.md", useBytes = TRUE)
