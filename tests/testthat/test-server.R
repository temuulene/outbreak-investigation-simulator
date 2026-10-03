test_that("guided journey completes checkpoints in order and reaches the debrief", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    checkpoint_inputs <- function(n) session$setInputs(suspect = "Still open", confidence = "Low",
      supporting = "Illness after event", against = "Recall errors", change = "Lab evidence", action_now = "Advise guests", checkpoint = n)
    expect_equal(guide_step(), 1L)
    session$setInputs(begin = 1)
    expect_equal(guide_step(), 2L)
    session$setInputs(suspect = "", supporting = "", checkpoint = 1)
    expect_equal(guide_step(), 2L)
    expect_length(s()$checkpoints, 0)
    checkpoint_inputs(2)
    expect_equal(guide_step(), 3L)
    expect_equal(s()$checkpoints[[1]]$stage, "After initial assessment")
    session$setInputs(character = "organizer", topic = "auto", question = "menu and walk-ins", ask = 1)
    expect_true(s()$menu)
    session$setInputs(interviews_done = 1)
    expect_equal(guide_step(), 4L)
    checkpoint_inputs(3)
    expect_equal(guide_step(), 5L)
    session$setInputs(person = "all", clinical = "standard", start = 0, end = 72, lab = FALSE,
      definition_reason = "Initial working definition", save_definition = 1)
    expect_equal(guide_step(), 6L)
    # The learner can request and receive the list without opening task tools.
    session$setInputs(prepare_list = 1)
    expect_false("guest_list" %in% s()$completed)
    session$setInputs(prepare_list = 2)
    expect_true("guest_list" %in% s()$completed)
    session$setInputs(audience = "all", domains = c("symptoms", "onset"), foods = names(food_labels(s()$sc)), send_team = 1)
    expect_equal(guide_step(), 6L)
    expect_equal(nrow(s()$collected), 0)
    session$setInputs(collection_done = 1)
    expect_equal(guide_step(), 7L)
    expect_equal(nrow(s()$collected), 60)
    session$setInputs(freeze = 1)
    expect_equal(nrow(snapshot()$data), 60)
    session$setInputs(analysis_done = 1)
    expect_equal(guide_step(), 8L)
    checkpoint_inputs(4)
    expect_equal(guide_step(), 9L)
    expect_equal(vapply(s()$checkpoints, `[[`, character(1), "stage"), c("After initial assessment", "After interviews", "After analysis"))
    session$setInputs(recommendation = "Suspected food exposure; advise guests while awaiting corroboration.", finish = 1)
    expect_equal(guide_step(), 10L)
    expect_match(output$debrief$html, "A profile, not a score", fixed = TRUE)
    session$setInputs(retry_checkpoint = "1", retry_reason = "I was unsure", retry_text = "Preserve samples now", save_retry = 1)
    expect_equal(s()$retry$revision, "Preserve samples now")
    session$setInputs(reveal = 1)
    expect_match(output$truth_reveal$html, "Simulated vehicle", fixed = TRUE)
  })
})

test_that("navigation preserves checkpoint drafts and never advances time", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    session$setInputs(begin = 1, suspect = "Something at the event", supporting = "Several callers")
    session$setInputs(back_step = 1)
    expect_equal(guide_step(), 1L)
    expect_equal(checkpoint_drafts()[["2"]]$suspect, "Something at the event")
    session$setInputs(begin = 2)
    expect_equal(guide_step(), 2L)
    expect_equal(s()$clock, 0)
    expect_length(s()$checkpoints, 0)
    session$setInputs(revisit = "10", go_revisit = 1)
    expect_equal(guide_step(), 2L)
    session$setInputs(collection_done = 1)
    expect_equal(guide_step(), 2L)
    session$setInputs(revisit = "1", go_revisit = 2)
    expect_equal(guide_step(), 1L)
  })
})


test_that("optional workflows preserve session evidence and navigation", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    session$setInputs(scenario_id = "potluck-02", randomize = FALSE, begin = 1)
    expect_equal(s()$sc$id, "potluck-02")
    session$setInputs(character = "organizer", topic = "auto", question = strrep("x", 1201), ask = 1)
    expect_length(s()$interviewed, 0)
    session$setInputs(question = "menu", ask = 2)
    expect_true(s()$menu)
    session$setInputs(wait = 1)
    session$setInputs(wait = 2)
    expect_length(s()$media_events, 1)
    session$setInputs(media_id = "press-call", media_text = "Several reports are being investigated. The source remains uncertain. We are collecting comparable histories.", media_evidence = "1", respond_media = 1)
    expect_length(s()$media_responses, 1)
    session$setInputs(review_criterion = "communication", review_status = "Instructor review", review_reason = "Review uncertainty and supporting evidence", review_save = 1)
    expect_length(s()$instructor_reviews, 1)
    path <- tempfile(fileext = ".json")
    save_session(s(), path, list(step = 3L, furthest = 3L))
    restored_clock <- s()$clock
    s(start_session())
    restore_upload(list(datapath = path))
    expect_equal(s()$clock, restored_clock)
    expect_equal(s()$sc$id, "potluck-02")
    expect_equal(guide_step(), 3L)
    before <- s()
    save_session(start_session(), path, list(step = list("bad")))
    restore_upload(list(datapath = path))
    expect_identical(s(), before)
  })
})

test_that("resume navigation rejects malformed fields before state changes", {
  expect_error(validate_guide_restore(list(step = NaN)), "step")
  expect_error(validate_guide_restore(list(step = 3, furthest = 2)), "order")
  expect_error(validate_guide_restore(list(current_fields = list(suspect = list("bad")))), "draft")
  expect_equal(validate_guide_restore(list(step = 2))$furthest, 2L)
})

test_that("both conversation interfaces build without credentials", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  library(bslib)
  options(sass.cache = FALSE)
  withr::local_envvar(FIELDNOTES_CHAT_UI = "shinychat")
  html <- as.character(app_ui())
  expect_match(html, "conversation")
  expect_match(html, "Choose a challenge")
  expect_match(html, "Resume a saved session")
})


test_that("repeat collection requires a reason and keeps the frozen analysis", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    s(full_plan())
    freeze_snapshot()
    first <- snapshot()
    session$setInputs(repeat_reason = "", repeat_collection = 1)
    expect_false(repeat_plan())
    session$setInputs(repeat_reason = "Add health care histories", repeat_collection = 2)
    expect_true(repeat_plan())
    session$setInputs(audience = "all", domains = c("symptoms", "onset", "health_care_visits"), foods = names(food_labels(s()$sc)), send_team = 1)
    expect_false(repeat_plan())
    expect_true(any(vapply(s()$queue, function(job) job$id == "team_interviews", logical(1))))
    session$setInputs(collection_done = 1)
    expect_true("health_care_visits" %in% names(s()$collected))
    expect_identical(snapshot(), first)
  })
})

test_that("restoring preserves the latest frozen snapshot and rejects zero-width definitions", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    s(full_plan())
    freeze_snapshot()
    frozen <- snapshot()
    path <- tempfile(fileext = ".json")
    save_session(s(), path, list(step = 7L, furthest = 7L))
    restore_upload(list(datapath = path))
    expect_identical(snapshot(), frozen)
    session$setInputs(person = "all", clinical = "standard", start = 12, end = 12,
      lab = FALSE, definition_reason = "No interval", save_definition = 1)
    expect_equal(s()$definition$start, 0)
    expect_equal(guide_step(), 7L)
  })
})

test_that("stratification uses its own food selection and clears when the snapshot changes", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  shiny::testServer(app_server, {
    s(full_plan())
    session$setInputs(analysis_food = "cake", strata_exposure = "coleslaw",
      strata_food = "chicken_salad", show_strata = 1)
    expect_true(all(strata_result()$food == "coleslaw"))
    expect_equal(strata_snapshot(), snapshot()$id)
    freeze_snapshot()
    expect_null(strata_result())
  })
})

test_that("widget text submission follows the same interview boundary", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  library(shiny)
  withr::local_envvar(FIELDNOTES_CHAT_UI = "shinychat")
  shiny::testServer(app_server, {
    session$setInputs(character = "organizer", topic = "auto", conversation_user_input = list("menu"))
    expect_true(s()$menu)
    expect_equal(s()$chats$organizer[[1]]$question, "menu")
    before <- s()
    session$setInputs(conversation_user_input = list("storage", list(type = "image")))
    expect_identical(s(), before)
  })
})

test_that("the placeholder question works through both conversation interfaces", {
  withr::local_dir(root)
  library(shiny)
  for (interface in c("scripted", "shinychat")) {
    withr::with_envvar(c(FIELDNOTES_CHAT_UI = interface), shiny::testServer(app_server, {
      session$setInputs(character = "organizer", topic = "auto")
      if (interface == "shinychat") {
        session$setInputs(conversation_user_input = list("Tell me about the potluck"))
      } else {
        session$setInputs(question = "Tell me about the potluck", ask = 1)
      }
      expect_identical(s()$chats$organizer[[1]]$topics, "event")
      expect_match(s()$chats$organizer[[1]]$reply, "Saturday")
    }))
  }
})

test_that("successful questions reset the topic and text while rejected submissions keep them", {
  withr::local_dir(root)
  library(shiny)
  shiny::testServer(app_server, {
    updates <- list()
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    session$setInputs(character = "organizer", topic = "auto")
    updates <- list()
    session$setInputs(topic = "menu", question = "Were there walk-ins?", ask = 1)
    expect_true(s()$menu && s()$walk_ins)
    expect_identical(updates$topic$value, "auto")
    expect_identical(updates$question$value, "")
    updates <- list()
    session$setInputs(question = strrep("x", 1201), ask = 2)
    expect_length(updates, 0)
  })
})

test_that("assisted submissions reset controls only after an authored reply is applied", {
  withr::local_dir(root)
  withr::local_envvar(FIELDNOTES_DIALOGUE_PROVIDER = "gemini")
  library(shiny)
  shiny::testServer(app_server, {
    updates <- list()
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    session$setInputs(character = "organizer", topic = "auto")
    updates <- list()
    # An explicit topic uses the asynchronous path without a service request.
    session$setInputs(topic = "menu", question = "Were there walk-ins?", ask = 1)
    deadline <- Sys.time() + 2
    while (isolate(dialogue_busy()) && Sys.time() < deadline) later::run_now(0.01)
    expect_false(dialogue_busy())
    expect_true(s()$menu && s()$walk_ins)
    expect_identical(updates$topic$value, "auto")
    expect_identical(updates$question$value, "")
  })
})

test_that("a delayed reply preserves a new draft or a different contact's controls", {
  withr::local_dir(root)
  withr::local_envvar(FIELDNOTES_DIALOGUE_PROVIDER = "gemini")
  library(shiny)
  release <- NULL
  request_environment <- environment(app_server)
  original_request <- get("dialogue_request_async", envir = request_environment)
  withr::defer(assign("dialogue_request_async", original_request, envir = request_environment))
  assign("dialogue_request_async", function(session, character, question, ...) {
    # Match the real boundary, which validates and forces inputs before waiting.
    force(character)
    force(question)
    session$token <- session$token + 1L
    promises::promise(function(resolve, reject) {
      release <<- function() resolve(list(topics = "menu", intro = "plain", status = "assisted",
        token = session$token, character = character, question = question))
    })
  }, envir = request_environment)
  for (switch_contact in c(FALSE, TRUE)) shiny::testServer(app_server, {
    updates <- list()
    session$sendInputMessage <- function(inputId, message) updates[[inputId]] <<- message
    session$setInputs(character = "organizer", topic = "auto")
    session$setInputs(topic = "menu", question = "What did you serve?", ask = 1)
    expect_true(dialogue_busy())
    if (switch_contact) {
      session$setInputs(character = "cook")
    } else {
      session$setInputs(topic = "guest_list", question = "Who came?")
    }
    updates <- list()
    release()
    deadline <- Sys.time() + 2
    while (isolate(dialogue_busy()) && Sys.time() < deadline) later::run_now(0.01)
    expect_true(s()$menu)
    expect_false(dialogue_busy())
    expect_null(updates$question)
    expect_null(updates$topic)
  })
})

test_that("the assisted UI passes scoped context and applies reviewed wording", {
  withr::local_dir(root)
  withr::local_envvar(FIELDNOTES_DIALOGUE_PROVIDER = "gemini", FIELDNOTES_DIALOGUE_MODEL = "test")
  request_environment <- environment(app_server)
  original_classifier <- get("dialogue_provider_async", envir = request_environment)
  original_writer <- get("dialogue_rewrite_async", envir = request_environment)
  withr::defer(assign("dialogue_provider_async", original_classifier, envir = request_environment))
  withr::defer(assign("dialogue_rewrite_async", original_writer, envir = request_environment))
  histories <- list()
  assign("dialogue_provider_async", function(question, character, provider, model, history = list()) {
    histories[[length(histories) + 1L]] <<- history
    promises::promise_resolve(list(topics = "menu", intro = "plain"))
  }, envir = request_environment)
  assign("dialogue_rewrite_async", function(..., answer) promises::promise_resolve(list(
    reply = paste("Here's what I remember:", answer),
    review = list(supported = TRUE, complete = TRUE, in_character = TRUE))), envir = request_environment)
  shiny::testServer(app_server, {
    initial_evidence <- length(s()$evidence)
    session$setInputs(character = "organizer", topic = "auto", question = "menu", ask = 1)
    deadline <- Sys.time() + 2
    while (isolate(dialogue_busy()) && Sys.time() < deadline) later::run_now(0.01)
    expect_match(s()$chats$organizer[[1]]$reply, "Here's what I remember:", fixed = TRUE)
    expect_match(dialogue_note(), "Gemini-assisted reply", fixed = TRUE)
    dialogue$last <- -Inf
    session$setInputs(question = "Tell me more", ask = 2)
    deadline <- Sys.time() + 2
    while (isolate(dialogue_busy()) && Sys.time() < deadline) later::run_now(0.01)
    expect_length(histories[[1]], 0)
    expect_length(histories[[2]], 1)
    expect_identical(histories[[2]][[1]]$reply, s()$chats$organizer[[1]]$reply)
    expect_length(s()$evidence, initial_evidence + 2L)
  })
})
