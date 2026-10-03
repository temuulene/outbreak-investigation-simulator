if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, encoding = "UTF-8")

lines <- c("# Scenario reference review", "",
  "Instructor reference; contains scenario spoilers. Both authored seeds are evaluated",
  "at 72 hours of game time with every attendee and all symptom, onset, and food fields.",
  "This complete follow-up reference differs from an earlier or incomplete learner collection.",
  "")
for (id in scenario_catalog()$id) {
  s <- start_session(id)
  s <- advance_time(s, 72)
  s <- collect_records(s, s$reported$id,
    domain_fields(c("symptoms", "onset"), names(food_labels(s$sc))))
  rates <- attack_rates(line_list(s), names(food_labels(s$sc)))
  lines <- c(lines, paste("##", s$sc$title), "",
    paste("Seed:", s$sc$seed, "| Collected attendees:", nrow(s$collected),
      "| Teaching signal accepted:", teaching_signal(s)), "",
    "| Exposure | Exposed cases / total | Unexposed cases / total | Crude RR |",
    "| --- | ---: | ---: | ---: |")
  for (i in seq_len(nrow(rates))) {
    r <- rates[i, ]
    lines <- c(lines, sprintf("| %s | %d / %d | %d / %d | %.2f |",
      r$food, r$exposed_cases, r$exposed_total, r$unexposed_cases, r$unexposed_total, r$relative_risk))
  }
  if (!is.null(s$sc$analysis$stratifier)) {
    strata <- stratified_rates(line_list(s), s$sc$analysis$stratifier, s$sc$truth$vehicle)
    lines <- c(lines, "", paste("Association for", s$sc$analysis$stratifier,
      "within levels of", s$sc$truth$vehicle, ":"), "",
      "| Stratum | Exposed cases / total | Unexposed cases / total | RR |",
      "| --- | ---: | ---: | ---: |")
    for (i in seq_len(nrow(strata))) {
      r <- strata[i, ]
      lines <- c(lines, sprintf("| %s | %d / %d | %d / %d | %.2f |",
        r$stratum, r$exposed_cases, r$exposed_total, r$unexposed_cases, r$unexposed_total, r$relative_risk))
    }
  }
  lines <- c(lines, "")
}
lines <- c(lines, "Random selection checks at most 100 seeds. It requires adequate comparison",
  "groups, a vehicle RR of at least 3 and the largest crude association, at least two",
  "initial ill contacts, and three late reporters. Intermediate runs also require an",
  "apparent coleslaw association that attenuates within chicken-salad strata, while",
  "the vehicle association remains within coleslaw strata.", "",
  "Selection deliberately favours a readable lesson. It is not a sample for estimating",
  "real-world outbreak frequencies or effects. Small cells and recall errors still",
  "limit inference; a food association and a cooling failure do not alone establish cause.")
writeLines(enc2utf8(lines), "docs/scenario-review.md", useBytes = TRUE)
