test_that("catalog validates scenarios and stable reference seeds", {
  catalog <- scenario_catalog(file.path(root, "scenarios"))
  expect_setequal(catalog$id, c("potluck-01", "potluck-02"))
  for (id in catalog$id) {
    s <- start_session(id, directory = file.path(root, "scenarios"))
    expect_true(teaching_signal(s))
    expect_identical(s, start_session(id, seed = s$sc$seed, directory = file.path(root, "scenarios")))
  }
  expect_error(start_session("../potluck-01", directory = file.path(root, "scenarios")), "Unknown")
  expect_error(start_session(seed = Inf, directory = file.path(root, "scenarios")), "seed")
})

test_that("intermediate scenario illustrates confounding using reported follow-up data", {
  s <- start_session("potluck-02", directory = file.path(root, "scenarios"))
  s <- advance_time(s, 72)
  s <- collect_records(s, s$reported$id, domain_fields(c("symptoms", "onset"), names(food_labels(s$sc))))
  d <- line_list(s)
  crude <- attack_rates(d, "coleslaw")
  adjusted <- stratified_rates(d, "coleslaw", "chicken_salad")
  expect_gte(crude$relative_risk, 1.5)
  expect_true(all(adjusted$relative_risk < crude$relative_risk))
  expect_true(all(adjusted$relative_risk <= 1.5))
  expect_true(all(stratified_rates(d, "chicken_salad", "coleslaw")$relative_risk > 2))
  expect_error(stratified_rates(d, "cake", "cake"), "different")
  expect_error(stratified_rates(d, "cake", "unknown"), "Collect")
  d$chicken_salad[1] <- NA
  expect_true(all(stratified_rates(d, "coleslaw", "chicken_salad")$excluded_stratifier_unknown == 1))
})

test_that("random scenarios pass bounded teaching selection", {
  set.seed(91)
  for (id in c("potluck-01", "potluck-02")) {
    s <- start_session(id, randomize = TRUE, directory = file.path(root, "scenarios"))
    expect_true(teaching_signal(s))
  }
})

test_that("scheduled media and response evidence are time bounded", {
  s <- new_state(scenario())
  expect_error(submit_media_response(s, "press-call", "We are investigating"), "not available")
  s <- advance_time(s, 36)
  expect_length(s$media_events, 1)
  s <- submit_media_response(s, "press-call", "A cluster is reported; source remains uncertain.", 1)
  expect_identical(s$media_responses[[1]]$evidence[[1]], s$evidence[[1]])
  expect_error(submit_media_response(s, "press-call", "Update", 99), "references")
  expect_length(advance_time(s, 100)$media_events, 1)
})

test_that("repeat collection adds fields without duplicate people", {
  s <- full_plan()
  old <- s$collected$id
  s <- request_task(s, "team_interviews", "all", "health_care_visits")
  expect_error(request_task(s, "team_interviews", "all", "health_care_visits"), "already")
  s <- advance_time(s, 25)
  expect_identical(s$collected$id, old)
  expect_true("health_care_visits" %in% names(s$collected))
})

test_that("assessment anchors reasoning and supports auditable instructor notes", {
  s <- full_plan()
  s <- save_checkpoint(s, "After analysis", "Open", "Low", "Reported rates", "Recall error", "New samples", "Preserve food")
  s <- take_action(s, "close_hall", "Concern")
  a <- assessment_profile(s)
  expect_match(a$criteria$feedback[4], "Recall error")
  expect_match(a$criteria$feedback[5], "1 did not meet")
  s <- instructor_override(s, "synthesis", "Evidence recorded", "Reviewed alternate explanation against snapshot 1")
  expect_equal(assessment_profile(s)$criteria$status[4], "Evidence recorded")
  expect_length(s$instructor_reviews, 1)
  expect_error(instructor_override(s, "vehicle", "Pass", "Correct source"), "Invalid")
})

test_that("JSON resume roundtrips learner state without exporting hidden tables", {
  s <- full_plan()
  s <- advance_time(s, 10)
  s <- submit_media_response(s, "press-call", "Source uncertain", 1)
  s <- request_task(s, "team_interviews", "all", "health_care_visits")
  f <- tempfile(fileext = ".json")
  guide <- list(step = 6L, furthest = 8L, drafts = list(`2` = list(suspect = "Unsure")))
  save_session(s, f, guide)
  parsed <- jsonlite::read_json(f)
  expect_false(any(c("truth", "reported", "sc") %in% unlist(parsed$state$names)))
  restored <- restore_session(f, directory = file.path(root, "scenarios"))
  expect_identical(restored$state, s)
  expect_identical(restored$guide, guide)
  expect_identical(advance_time(restored$state, 25), advance_time(s, 25))
})

test_that("JSON resume rejects corrupt and tampered state shapes", {
  s <- full_plan()
  f <- tempfile(fileext = ".json")
  bad <- s; bad$clock <- Inf; save_session(bad, f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "Invalid")
  for (field in c("checks", "chats", "downloads")) {
    bad <- s; bad[[field]] <- list(list(unexpected = "value")); save_session(bad, f)
    expect_error(restore_session(f, directory = file.path(root, "scenarios")), "Invalid")
  }
  bad <- s; bad$queue <- list(list(id = "guest_list", label = "list", requested = 0, due = 0)); save_session(bad, f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "timeline")
  save_session(s, f, list(step = 999))
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "guide")
  writeLines("not json", f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "JSON")
})

test_that("resume rejects invalid contact identities and malformed queued questionnaire fields", {
  f <- tempfile(fileext = ".json")
  s <- full_plan()
  bad <- s; bad$interviewed <- "guest_9999"; save_session(bad, f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "Invalid")
  bad <- s; names(bad$chats) <- "unknown"; save_session(bad, f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "Invalid")
  bad <- request_task(s, "team_interviews", "all", "symptoms")
  bad$queue[[1]]$fields <- list("diarrhea"); save_session(bad, f)
  expect_error(restore_session(f, directory = file.path(root, "scenarios")), "Invalid")
})
