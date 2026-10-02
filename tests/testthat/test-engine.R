test_that("seed is repeatable and preserves the calling RNG", {
  set.seed(123)
  old <- .Random.seed
  first <- generate_data(scenario())
  expect_identical(.Random.seed, old)
  expect_identical(first, generate_data(scenario()))
  expect_equal(nrow(first$truth), 60)
  expect_true(any(first$truth$chicken_salad != first$reported$chicken_salad))
})

test_that("collection never leaks truth or unasked exposures", {
  s <- new_state(scenario())
  s <- collect_records(s, c(1, 2), c("age", "ill", "on_list"))
  expect_setequal(names(s$collected), c("id", "on_list", "age"))
  s <- collect_records(s, 2, "diarrhea")
  expect_true(is.na(s$collected$diarrhea[1]))
  expect_equal(nrow(s$collected), 2)
  expect_true(all(classify_cases(s$collected) == "Unknown"))
})

test_that("collection does not reveal future onset or reports", {
  s <- new_state(scenario())
  future <- s$reported$id[which(s$reported$onset_hours > 39)]
  expect_gt(length(future), 0)
  s <- collect_records(s, future, c("diarrhea", "onset_hours", "report_hour"))
  expect_true(all(is.na(s$collected$onset_hours)))
  expect_true(all(is.na(s$collected$report_hour)))
  expect_true(all(!s$collected$diarrhea))
})

test_that("classification applies clinical, time, population, and lab criteria", {
  d <- data.frame(diarrhea = c(TRUE, FALSE, TRUE, TRUE), cramps = FALSE, fever = FALSE,
    vomiting = FALSE, onset_hours = c(24, NA, 90, 24), on_list = c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(classify_cases(d), c("Case", "Non-case", "Non-case", "Case"))
  def <- default_definition(); def$person <- "rsvp"
  expect_equal(classify_cases(d, def)[4], "Non-case")
  def$lab <- TRUE
  expect_equal(classify_cases(d, def)[1], "Unknown")
})

test_that("task effort, turnaround and leftover window have consequences", {
  s <- new_state(scenario())
  expect_error(request_task(s, "leftover_testing"), "Preserve")
  s <- request_task(s, "guest_list")
  expect_equal(s$clock, 10 / 60)
  expect_false("guest_list" %in% s$completed)
  s <- advance_time(s, 2)
  expect_true("guest_list" %in% s$completed)
  expect_error(request_task(s, "guest_list"), "already")
  expect_error(request_task(advance_time(s, 24), "hold_leftovers"), "cleared")
  held <- request_task(new_state(scenario()), "hold_leftovers")
  expect_true("hold_leftovers" %in% held$completed)
  expect_silent(request_task(held, "leftover_testing"))
})

test_that("time processes results at their due time, not the end of a jump", {
  s <- request_task(new_state(scenario()), "food_prep_review")
  due <- s$queue[[1]]$due
  s <- advance_time(s, 100)
  expect_equal(s$evidence[[2]]$time, due)
  expect_equal(sum(vapply(s$evidence, function(e) e$source == "Guest reports", logical(1))), 1)
  expect_equal(nrow(s$truth), 60)
  expect_true(s$process_known)
})

test_that("lab results work before collection and only classify sampled guests", {
  s <- new_state(scenario())
  s <- request_task(s, "stool_samples")
  s <- advance_time(s, 49)
  expect_equal(nrow(s$collected), 3)
  expect_true(all(s$collected$stool_positive))
  expect_true(all(classify_cases(s$collected) == "Unknown"))
  expect_true(any(vapply(s$evidence, function(e) grepl(s$sc$pathogen$name, e$finding, fixed = TRUE), logical(1))))
  s <- request_task(new_state(scenario()), "hold_leftovers")
  s <- request_task(s, "leftover_testing")
  s <- advance_time(s, 73)
  expect_true("leftover_testing" %in% s$completed)
})

test_that("plan choices restrict who and what gets collected", {
  s <- new_state(scenario())
  expect_error(request_task(s, "team_interviews"), "guest list")
  s <- advance_time(request_task(s, "guest_list"), 3)
  expect_lt(length(plan_people(s, "all")), 60)
  expect_lt(length(plan_people(s, "ill")), length(plan_people(s, "all")))
  s <- request_task(s, "team_interviews", "ill", c("symptoms", "onset"), "chicken_salad")
  s <- advance_time(s, 25)
  expect_false("chicken_salad" %in% names(s$collected))
  expect_true(all(s$reported$ill[match(s$collected$id, s$reported$id)]))
  complete <- full_plan()
  expect_equal(nrow(complete$collected), 60)
  expect_true(all(names(food_labels(complete$sc)) %in% names(complete$collected)))
})

test_that("interviews are stable, topic charged, separate and bounded", {
  s <- new_state(scenario())
  s <- interview(s, "cook", "Walk me through how you made it")
  expect_true(s$process_known)
  old_time <- s$clock
  s <- interview(s, "cook", "How did you prepare it?")
  expect_equal(s$clock, old_time)
  s <- interview(s, "organizer", "What pathogen caused this?", "auto")
  expect_false(grepl(s$sc$pathogen$name, s$chats$organizer[[1]]$reply, fixed = TRUE))
  expect_equal(length(s$chats$cook), 2)
  s$interviewed <- paste0("contact", 1:6)
  expect_error(interview(s, "organizer", "menu"), "limit")
})

test_that("action conditions are captured at the moment of action", {
  s <- take_action(new_state(scenario()), "food_handler_guidance", "Precaution")
  expect_false(s$actions[[1]]$justified)
  s <- interview(s, "cook", "preparation")
  s <- take_action(s, "food_handler_guidance", "Cooling failure")
  expect_true(s$actions[[2]]$justified)
  expect_false(s$actions[[1]]$justified)
  s <- take_action(s, "close_hall", "Concern")
  expect_false(s$actions[[3]]$justified)
})

test_that("risk denominators exclude missing exposures and unknown cases", {
  d <- data.frame(food = c(TRUE, TRUE, FALSE, FALSE, NA, TRUE), case_status = c("Case", "Non-case", "Non-case", "Non-case", "Case", "Unknown"))
  a <- attack_rates(d, "food")
  expect_equal(a$exposed_total, 2)
  expect_equal(a$unexposed_total, 2)
  expect_equal(a$attack_rate_exposed, .5)
  expect_equal(a$excluded_unknown, 2)
  expect_equal(a$relative_risk, Inf)
  expect_true(check_calculation(d, "food", .5, 0, Inf)$ok)
  expect_false(check_calculation(d, "food", 1, 0, Inf)$ok)
  expect_true(is.na(attack_rates(d, "unasked")$relative_risk))
})

test_that("checkpoint evidence and analysis data remain fixed after later work", {
  s <- full_plan()
  s <- save_checkpoint(s, "After analysis", "Open", "Low", "Cluster", "Recall", "Lab", "Advice")
  saved <- s$checkpoints[[1]]$evidence
  snapshot <- line_list(s)
  s <- add_evidence(s, "Later", "New evidence")
  s$definition$end <- 1
  expect_identical(s$checkpoints[[1]]$evidence, saved)
  expect_false(identical(snapshot, line_list(s)))
  file <- tempfile(fileext = ".xlsx")
  write_workbook(snapshot, file, s$sc)
  expect_true(file.exists(file))
  expect_equal(openxlsx::getSheetNames(file), c("Read me", "Line list", "Attack rates"))
})

test_that("pilot seed retains a teachable learner-visible association", {
  s <- full_plan()
  rates <- attack_rates(line_list(s), names(food_labels(s$sc)))
  vehicle <- rates[rates$food == s$sc$truth$vehicle, ]
  expect_gt(vehicle$relative_risk, 3)
  expect_equal(rates$food[which.max(rates$relative_risk)], s$sc$truth$vehicle)
  expect_gt(vehicle$exposed_total, 10)
  expect_gt(vehicle$unexposed_total, 10)
})
