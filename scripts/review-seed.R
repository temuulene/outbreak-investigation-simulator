if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, encoding = "UTF-8")
s <- new_state(read_scenario())
s <- interview(s, "organizer", "menu and missing walk-ins")
s <- advance_time(request_task(s, "guest_list"), 3)
s <- request_task(s, "team_interviews", "all", c("demographics", "symptoms", "onset"), names(food_labels(s$sc)))
s <- advance_time(s, 25)
rates <- attack_rates(line_list(s), names(food_labels(s$sc)))
lines <- c("# Pilot seed review", "", "Seed 4172; full attendee plan, all foods, starter clinical definition, reported recall data.",
  "", paste("Collected records:", nrow(s$collected)), paste("Classifications:", paste(names(table(line_list(s)$case_status)), table(line_list(s)$case_status), collapse = "; ")),
  "", "| Exposure | Exposed cases / total | Unexposed cases / total | RR | Approx. 95% CI |", "| --- | ---: | ---: | ---: | --- |")
for (i in seq_len(nrow(rates))) {
  r <- rates[i, ]
  lines <- c(lines, sprintf("| %s | %d / %d | %d / %d | %.2f | %.2f–%.2f |", r$food,
    r$exposed_cases, r$exposed_total, r$unexposed_cases, r$unexposed_total, r$relative_risk, r$rr_lower, r$rr_upper))
}
lines <- c(lines, "", "The vehicle must retain RR > 3 and the largest observed association under this reference plan; tests enforce this.",
  "Intervals are approximate log-Wald intervals for nonzero cells, not exact inference. This review does not imply every learner-selected plan is adequate.",
  "Alternative food associations may occur by chance. Cooling evidence alone is not causal proof.")
writeLines(enc2utf8(lines), "docs/seed-review.md", useBytes = TRUE)
print(rates)
