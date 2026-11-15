session_for <- function(id = "potluck-01") start_session(id, directory = file.path(root, "scenarios"))
ask_pat <- function(s, question = "menu and missing walk-ins") interview(s, "organizer", question)
finding <- function(s, label) Filter(function(e) e$source == label, s$evidence)[[1]]$finding
performance <- function(s, clinical) {
  definition <- default_definition()
  definition$clinical <- clinical
  definition$start <- -1000
  definition$end <- 1000
  case <- classify_cases(s$reported, definition) == "Case"
  c(sensitivity = mean(case[s$truth$ill]), specificity = mean(!case[!s$truth$ill]))
}

test_that("unrelated illness makes broad case definitions less specific", {
  for (id in c("potluck-01", "potluck-02", "potluck-03")) {
    s <- session_for(id)
    background <- s$truth[s$truth$background, ]
    expect_gt(nrow(background), 0)
    expect_false(any(background$ill))
    expect_true(all(rowSums(background[names(s$sc$pathogen$symptoms)]) >= 1))
    expect_false(anyNA(background$onset_hours))
    narrow <- performance(s, "diarrhea")
    broad <- performance(s, "any")
    expect_gte(broad[["sensitivity"]], narrow[["sensitivity"]])
    expect_lt(broad[["specificity"]], 1)
    expect_lte(broad[["specificity"]], narrow[["specificity"]])
  }
  s <- session_for()
  expect_gt(performance(s, "any")[["sensitivity"]], performance(s, "diarrhea")[["sensitivity"]])
  expect_lt(performance(s, "any")[["specificity"]], performance(s, "diarrhea")[["specificity"]])
  # Some unrelated illness starts before the meal, which a time criterion can exclude.
  expect_true(any(unlist(lapply(c("potluck-01", "potluck-02", "potluck-03"), function(id) {
    session_for(id)$truth$onset_hours
  })) < 0, na.rm = TRUE))
})

test_that("optional realism never changes the core records for a seed", {
  sc <- scenario()
  plain <- sc
  plain[c("background_illness", "reporting", "response", "lab")] <- NULL
  plain$recall$rumour <- NULL
  full <- generate_data(sc)
  bare <- generate_data(plain)
  core <- c("id", "age", "sex", "on_list", names(food_labels(sc)), "ill", "health_care_visits")
  expect_identical(full$truth[core], bare$truth[core])
  expect_identical(full$reported[names(food_labels(sc))], bare$reported[names(food_labels(sc))])
  expect_identical(full$truth$onset_hours[full$truth$ill], bare$truth$onset_hours[bare$truth$ill])
  expect_false(any(bare$truth$background))
  expect_true(all(bare$truth$responds))
  expect_identical(bare$truth$stool_detectable, bare$truth$ill)
  expect_false(anyNA(bare$truth$report_hour[bare$truth$ill]))
})

test_that("not every case phones in, and late reporters arrive on schedule", {
  s <- session_for()
  ill <- s$truth$ill
  expect_gt(sum(ill & is.na(s$truth$report_hour)), 0)
  event <- Filter(function(e) !is.null(e$late_reporters), s$sc$events)[[1]]
  expect_equal(sum(s$truth$report_hour[ill] == event$at_hour, na.rm = TRUE), event$late_reporters$count)
  s <- advance_time(request_task(ask_pat(s), "guest_list"), 3)
  reporters <- plan_people(s, "ill")
  expect_true(all(has_reported(s)[match(reporters, s$reported$id)]))
  expect_lt(length(reporters), sum(ill))
  # A complete plan still finds the cases who never phoned.
  s <- request_task(s, "team_interviews", "all", c("symptoms", "onset"), names(food_labels(s$sc)))
  s <- advance_time(s, 25)
  silent <- s$reported$id[ill & is.na(s$reported$report_hour) & s$reported$responds]
  expect_true(all(silent %in% s$collected$id))
})

test_that("only guests who respond enter the line list, consistently across visits", {
  s <- advance_time(request_task(ask_pat(session_for()), "guest_list"), 3)
  plan <- plan_people(s, "all")
  expect_length(plan, nrow(s$reported))
  s <- advance_time(request_task(s, "team_interviews", "all", "symptoms"), 25)
  answered <- s$reported$id[s$reported$responds]
  expect_setequal(s$collected$id, answered)
  expect_lt(length(answered), length(plan))
  expect_match(finding(s, "Send the collection team"), sprintf("%d of %d guests on the plan answered", length(answered), length(plan)))
  expect_match(finding(s, "Send the collection team"), "did not respond")
  again <- advance_time(request_task(s, "team_interviews", "all", "demographics"), 25)
  expect_setequal(again$collected$id, answered)
  expect_false(any(c("responds", "background", "stool_detectable", "ill") %in% names(again$collected)))
})

test_that("a rumour changes what ill guests recall, not what they ate", {
  s <- session_for("potluck-03")
  food <- s$sc$recall$rumour$food
  ill <- s$truth$ill
  expect_identical(s$sc$pathogen$name, "Clostridium perfringens")
  expect_true(teaching_signal(s))
  expect_gt(mean(s$reported[[food]][ill]), mean(s$truth[[food]][ill]))
  reference <- truth_summary(s)
  expect_identical(reference$rumour$food, food)
  expect_gt(reference$rumour$recalled$relative_risk, reference$rumour$actual$relative_risk)
  expect_gt(reference$rumour$recalled$relative_risk, 2)
  expect_null(truth_summary(session_for())$rumour)
  s <- advance_time(s, 4)
  expect_match(finding(s, "Community group post"), "garden salad")
})

test_that("laboratory tests can miss true positives and say so", {
  s <- advance_time(request_task(session_for(), "stool_samples"), 49)
  sampled <- s$collected
  expect_identical(sampled$stool_positive, s$truth$stool_detectable[match(sampled$id, s$truth$id)])
  missed <- session_for()
  missed$reported$stool_detectable[] <- FALSE
  missed <- advance_time(request_task(missed, "stool_samples"), 49)
  expect_false(any(missed$collected$stool_positive))
  expect_match(finding(missed, "Request stool samples"), "No pathogen was isolated")
  expect_match(finding(missed, "Request stool samples"), "does not rule out infection")
  definition <- default_definition()
  definition$lab <- TRUE
  expect_false(any(classify_cases(missed$collected, definition) == "Case"))
  negative <- session_for()
  negative$lab$food_detected <- FALSE
  negative <- request_task(request_task(negative, "hold_leftovers"), "stool_samples")
  negative <- advance_time(request_task(negative, "leftover_testing"), 73)
  expect_match(finding(negative, "Test preserved leftovers"), "No pathogen was isolated")
  expect_false(grepl("match", finding(negative, "Test preserved leftovers")))
  expect_false(grepl("match", finding(negative, "Request stool samples")))
})

test_that("exports carry onset and report as date-times that recover the hours", {
  s <- advance_time(session_for(), 72)
  s <- collect_records(s, s$reported$id, domain_fields(c("symptoms", "onset"), names(food_labels(s$sc))))
  data <- line_list(s)
  exported <- export_line_list(data, s$sc)
  expect_false(any(c("onset_hours", "report_hour") %in% names(exported)))
  expect_identical(which(names(exported) == "onset_time"), which(names(data) == "onset_hours"))
  expect_identical(is.na(exported$onset_time), is.na(data$onset_hours))
  expect_identical(format(meal_time(s$sc), "%Y-%m-%d %H:%M"), "2026-06-06 18:00")
  onset <- as.POSIXct(exported$onset_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  hours <- as.numeric(difftime(onset, meal_time(s$sc), units = "hours"))
  expect_lt(max(abs(hours - data$onset_hours), na.rm = TRUE), 1 / 60 + 1e-9)
  first_report <- min(exported$report_time, na.rm = TRUE)
  expect_identical(first_report, "2026-06-08 09:00")
  expect_identical(export_line_list(data[c("id", "on_list")], s$sc), data[c("id", "on_list")])
  path <- tempfile(fileext = ".xlsx")
  write_workbook(data, path, s$sc)
  expect_true("onset_time" %in% names(openxlsx::read.xlsx(path, "Line list")))
})

test_that("guests recall onset as a day and time", {
  s <- session_for()
  guest <- unname(available_characters(s)[3])
  reply <- interview(s, guest, "When did your symptoms start?")$chats[[guest]][[1]]$reply
  expect_match(reply, "started on (Saturday|Sunday|Monday) at about [0-2][0-9]:[0-5]0\\.")
  expect_false(grepl("hours after", reply))
  expect_identical(onset_phrase(s$sc, 23.7), "Sunday at about 17:40")
  expect_identical(onset_phrase(s$sc, -20), "Friday at about 22:00")
  expect_match(interview(s, "organizer", "Tell me about the potluck")$chats$organizer[[1]]$reply, "Saturday 6 June")
})

test_that("the cook's account and food words come from the scenario file", {
  s <- session_for("potluck-03")
  cook <- interview(s, "cook", "Did you reheat it?")
  expect_true(cook$process_known)
  expect_identical(cook$chats$cook[[1]]$reply, s$sc$cook$preparation)
  expect_match(interview(s, "organizer", "Who made the stew?")$chats$organizer[[1]]$reply, "brought the beef stew")
  expect_true("food_sources" %in% classify_topics("Who brought the lemonade?"))
  expect_true("food_sources" %in% classify_topics("Who made the sandwich?"))
  expect_setequal(dialogue_anchors(s$sc$cook$preparation), c("Friday", "Saturday", s$sc$cook$locked_terms))
  expect_true("Beef stew" %in% dialogue_anchors("I had the beef stew."))
  # Terms are matched literally, whatever characters a scenario author uses.
  expect_true(dialogue_has_term("5 C (41 F)", "kept at 5 C (41 F) overnight"))
  expect_false(dialogue_has_term("5 C (41 F)", "kept at 5 C 41 F overnight"))
  source_files <- list.files(file.path(root, "R"), full.names = TRUE)
  code <- unlist(lapply(source_files, readLines, warn = FALSE))
  expect_false(any(grepl("chicken|coleslaw|stock pot|beef|stew", code, ignore.case = TRUE)))
})

test_that("action conditions follow the evidence on record", {
  s <- session_for()
  expect_false(take_action(s, "discard_leftovers", "Precaution")$actions[[1]]$justified)
  told <- interview(s, "organizer", "What happened to the leftovers?")
  expect_true(told$leftovers_home)
  expect_match(told$chats$organizer[[1]]$reply, "took plates of leftovers home")
  expect_true(take_action(told, "discard_leftovers", "Guests have food at home")$actions[[1]]$justified)
  expect_true(take_action(s, "advise_guests", "Several reports")$actions[[1]]$justified)
  quiet <- s
  quiet$reported$report_hour[] <- NA_real_
  expect_false(take_action(quiet, "advise_guests", "No reports yet")$actions[[1]]$justified)
  expect_false(take_action(told, "close_hall", "Concern")$actions[[1]]$justified)
  for (condition in action_conditions()) expect_true(nzchar(action_condition_note(condition)))
})

test_that("a guest interviewed without the menu yields only the foods they name", {
  s <- session_for()
  guest <- unname(available_characters(s)[3])
  eaten <- names(food_labels(s$sc))[unlist(s$reported[s$reported$id == sub("guest_", "", guest), names(food_labels(s$sc))])]
  open <- interview(s, guest, "What did you eat?")
  expect_setequal(intersect(names(open$collected), names(food_labels(s$sc))), eaten)
  expect_true(all(unlist(open$collected[eaten])))
  expect_match(open$chats[[guest]][[1]]$reply, "Without a list of what was served")
  prompted <- interview(ask_pat(s, "What was on the menu?"), guest, "What did you eat?")
  expect_true(all(names(food_labels(s$sc)) %in% names(prompted$collected)))
  expect_false(anyNA(prompted$collected[names(food_labels(s$sc))]))
})

test_that("the line list keeps one column order however fields were gathered", {
  s <- session_for()
  guest <- unname(available_characters(s)[3])
  s <- interview(ask_pat(s), guest, "What did you eat, and when did your symptoms start?")
  s <- interview(s, guest, "How old are you, and what symptoms did you have?")
  expected <- c(
    "id", "on_list", "age", "sex", names(s$sc$pathogen$symptoms), "onset_hours", "report_hour",
    names(food_labels(s$sc)), "case_status"
  )
  expect_identical(names(line_list(s)), expected)
  expect_identical(line_list(s)[names(s$collected)], s$collected)
})

test_that("small and empty cells get an exact p-value and a plain caution", {
  d <- data.frame(
    food = rep(c(TRUE, FALSE), c(12, 10)),
    case_status = c(rep("Case", 9), rep("Non-case", 3), rep("Non-case", 10))
  )
  r <- attack_rates(d, "food")
  expect_identical(r$relative_risk, Inf)
  expect_true(is.na(r$rr_lower))
  expect_equal(r$fisher_p, stats::fisher.test(matrix(c(9, 3, 0, 10), 2))$p.value)
  expect_match(r$caution, "Empty cell")
  d$case_status[13:14] <- "Case"
  expect_match(attack_rates(d, "food")$caution, "Small cell")
  expect_identical(attack_rates(d[d$food, ], "food")$caution, "No comparison group")
  big <- data.frame(food = rep(c(TRUE, FALSE), each = 40), case_status = rep(c("Case", "Non-case"), 40))
  expect_identical(attack_rates(big, "food")$caution, "")
  shown <- format_rates(rbind(r, attack_rates(d, "missing")), c(food = "Stew"))
  expect_identical(shown[[1]], c("Stew", "missing"))
  expect_identical(shown[["Relative risk (approx. 95% CI)"]], c("Infinite", "Not estimable"))
  expect_identical(shown[["Ate it: cases / total"]][1], "9 / 12")
  expect_false(any(grepl("_", names(shown))))
})

test_that("the curve axis extends before the meal when illness began earlier", {
  d <- data.frame(case_status = c("Non-case", "Case", "Case"), onset_hours = c(-20, 10, 30))
  curve <- onset_counts(d, 6)
  expect_identical(curve$breaks[1], -24)
  expect_equal(sum(curve$counts), 3)
  expect_equal(unname(curve$counts[2, 1]), 1)
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  expect_silent(plot_epi_curve(curve))
})

test_that("text the app accepts can always be saved and resumed", {
  s <- session_for()
  long <- strrep("x", max_text_length() + 1L)
  expect_error(save_checkpoint(s, "After analysis", long, "Low", "a", "b", "c", "d"), "within 20,000")
  expect_error(take_action(s, "advise_guests", long), "within 20,000")
  expect_error(check_text("  ", "Write something."), "Write something.")
  kept <- save_checkpoint(s, "After analysis", strrep("x", max_text_length()), "Low", "a", "b", "c", "d")
  path <- tempfile(fileext = ".json")
  save_session(kept, path)
  expect_identical(restore_session(path, directory = file.path(root, "scenarios"))$state, kept)
})

test_that("saved files hold no simulated truth and older files still restore", {
  s <- interview(session_for(), "organizer", "What happened to the leftovers?")
  path <- tempfile(fileext = ".json")
  save_session(s, path)
  record <- jsonlite::read_json(path)
  expect_false(any(private_fields() %in% unlist(record$state$names)))
  # A file written before a field existed restores with that field's starting value.
  drop <- which(unlist(record$state$names) == "leftovers_home")
  record$state$names <- record$state$names[-drop]
  record$state$values <- record$state$values[-drop]
  jsonlite::write_json(record, path, auto_unbox = TRUE, null = "null")
  expect_false(restore_session(path, directory = file.path(root, "scenarios"))$state$leftovers_home)
  bad <- s
  bad$collected <- data.frame(id = 1, on_list = TRUE, responds = TRUE)
  save_session(bad, path)
  expect_error(restore_session(path, directory = file.path(root, "scenarios")), "Invalid")
})

test_that("guide steps are addressed by id", {
  steps <- guide_steps()
  expect_false(anyDuplicated(steps$id) > 0)
  expect_identical(steps$page[step_index("weigh")], "decision")
  expect_identical(steps$checkpoint[step_index("interview_review")], "After interviews")
  expect_identical(step_index("debrief"), nrow(steps))
  expect_error(step_index("missing"), "Unknown guide step")
  decisions <- as.character(which(steps$page == "decision"))
  expect_silent(validate_guide(list(drafts = setNames(list(list(suspect = "x")), decisions[1]))))
  expect_error(validate_guide(list(drafts = list(`3` = list(suspect = "x")))), "draft")
  expect_error(validate_guide(list(unknown = 1)), "guide")
})
