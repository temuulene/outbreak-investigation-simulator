await_dialogue <- function(p) {
  ready <- FALSE; value <- NULL; failure <- NULL
  promises::then(p, function(x) {value <<- x; ready <<- TRUE}, function(e) {failure <<- e; ready <<- TRUE})
  deadline <- Sys.time() + 2
  while (!ready && Sys.time() < deadline) later::run_now(0.01)
  if (!is.null(failure)) stop(failure)
  if (!ready) stop("Promise did not settle")
  value
}

testthat::test_that("structured dialogue accepts only allowed topic and connective identifiers", {
  testthat::expect_true(dialogue_validate(list(topics = c("menu", "storage"), intro = "plain"), "cook"))
  testthat::expect_false(dialogue_validate(list(topics = "diagnosis", intro = "plain"), "cook"))
  testthat::expect_false(dialogue_validate(list(topics = "menu", intro = "12 guests died"), "cook"))
  testthat::expect_false(dialogue_validate(list(topics = "menu", intro = "plain", facts = "secret"), "cook"))
  testthat::expect_false(dialogue_validate(list(topics = c("menu", "menu"), intro = "plain"), "cook"))
  testthat::expect_identical(dialogue_connectives("guest_1"), dialogue_connectives("guest_2"))
})

testthat::test_that("scripted default and explicit topics make no provider calls", {
  session <- dialogue_session()
  fail <- function(...) stop("Should never run")
  result <- await_dialogue(dialogue_request_async(session, "organizer", "menu", transport = fail, provider = "scripted"))
  testthat::expect_identical(result$status, "scripted")
  testthat::expect_identical(session$calls, 0L)
  result <- await_dialogue(dialogue_request_async(session, "cook", "", "storage", provider = "gemini", transport = fail))
  testthat::expect_identical(result$topics, "storage")
})

testthat::test_that("ellmer enum-array factors are normalized before strict validation", {
  request <- function(topics) {
    transport <- function(...) promises::promise_resolve(list(topics = topics, intro = "plain"))
    await_dialogue(dialogue_request_async(dialogue_session(), "organizer", "menu",
      provider = "gemini", model = "test", transport = transport))
  }
  # ellmer 0.5 converts arrays of enums to factors, including empty arrays.
  result <- request(factor("menu", levels = dialogue_topics("organizer")))
  testthat::expect_identical(result$status, "assisted")
  testthat::expect_identical(result$topics, "menu")
  empty <- request(factor(character(), levels = dialogue_topics("organizer")))
  testthat::expect_identical(empty$status, "assisted")
  testthat::expect_identical(empty$topics, character())
  testthat::expect_identical(request(factor("diagnosis"))$status, "fallback")
  testthat::expect_identical(request(factor(NA_character_, levels = "menu"))$status, "fallback")
  testthat::expect_identical(request(factor(c("menu", "menu")))$status, "fallback")
})

testthat::test_that("provider result only selects engine facts and does not receive state", {
  session <- dialogue_session()
  captured <- NULL
  transport <- function(question, character, provider, model) {
    captured <<- list(question = question, character = character, provider = provider, model = model)
    promises::promise_resolve(list(topics = c("menu", "walk_ins"), intro = "warm"))
  }
  result <- await_dialogue(dialogue_request_async(session, "organizer", "Tell me the foods and walk-ins", provider = "gemini", transport = transport))
  before <- new_state(scenario())
  after <- dialogue_apply(before, result)
  authored <- interview(before, "organizer", result$question, c("menu", "walk_ins"))
  testthat::expect_true(after$menu && after$walk_ins)
  testthat::expect_identical(after$evidence, authored$evidence)
  testthat::expect_identical(after$truth, before$truth)
  testthat::expect_identical(names(captured), c("question", "character", "provider", "model"))
  testthat::expect_match(after$chats$organizer[[1]]$reply, "Thanks for checking", fixed = TRUE)
  testthat::expect_identical(result$status, "assisted")
})

testthat::test_that("errors and malformed outputs use authored fallback without leaking diagnostics", {
  for (transport in list(function(...) stop("SECRET api token abc"),
      function(...) promises::promise_reject(simpleError("SECRET request body")),
      function(...) promises::promise_resolve(list(topics = "menu", intro = "<script>secret</script>")))) {
    result <- await_dialogue(dialogue_request_async(dialogue_session(), "organizer", "menu", provider = "gemini", transport = transport))
    testthat::expect_identical(result$status, "fallback")
    testthat::expect_identical(result$topics, "menu")
    testthat::expect_false(any(grepl("SECRET|script", unlist(result))))
  }
})

testthat::test_that("requests are bounded, session isolated, cancellable and deadline limited", {
  transport <- function(...) promises::promise_resolve(list(topics = "menu", intro = "plain"))
  first <- dialogue_session(max_calls = 1L)
  second <- dialogue_session()
  result <- await_dialogue(dialogue_request_async(first, "organizer", "menu", provider = "gemini", transport = transport))
  testthat::expect_identical(second$calls, 0L)
  testthat::expect_true(dialogue_result_current(first, result))
  dialogue_cancel(first)
  testthat::expect_false(dialogue_result_current(first, result))
  limited <- await_dialogue(dialogue_request_async(first, "organizer", "menu", provider = "gemini", transport = transport))
  testthat::expect_identical(limited$status, "limited")
  testthat::expect_error(dialogue_request_async(first, "cook", strrep("x", 1201)), "1,200")
  hanging <- dialogue_session(timeout = 0.01)
  p <- dialogue_request_async(hanging, "cook", "menu", provider = "gemini", transport = function(...) promises::promise(function(resolve, reject) NULL))
  testthat::expect_error(dialogue_request_async(hanging, "cook", "menu"), "wait")
  testthat::expect_identical(await_dialogue(p)$status, "timeout")
  testthat::expect_false(hanging$pending)
  rate <- dialogue_session()
  await_dialogue(dialogue_request_async(rate, "cook", "menu", provider = "gemini", transport = transport, now = 100))
  testthat::expect_identical(await_dialogue(dialogue_request_async(rate, "cook", "menu", provider = "gemini", transport = transport, now = 101))$status, "limited")
})

testthat::test_that("cancelled and late replies cannot become a current request", {
  release <- NULL
  session <- dialogue_session(timeout = 0.01)
  pending <- dialogue_request_async(session, "cook", "menu", provider = "gemini",
    transport = function(...) promises::promise(function(resolve, reject) release <<- resolve))
  timed <- await_dialogue(pending)
  testthat::expect_identical(timed$status, "timeout")
  current <- await_dialogue(dialogue_request_async(session, "cook", "storage", provider = "scripted"))
  release(list(topics = "menu", intro = "warm"))
  later::run_now(0.05)
  testthat::expect_true(dialogue_result_current(session, current))
  testthat::expect_false(dialogue_result_current(session, timed))
  testthat::expect_false(session$pending)
})

testthat::test_that("unrelated questions disclose no new topic facts", {
  result <- await_dialogue(dialogue_request_async(dialogue_session(), "cook", "Tell me a joke", provider = "gemini",
    transport = function(...) promises::promise_resolve(list(topics = character(), intro = "plain"))))
  s <- new_state(scenario())
  next_state <- dialogue_apply(s, result)
  testthat::expect_identical(next_state$evidence, s$evidence)
  testthat::expect_identical(next_state$clock, s$clock)
  testthat::expect_false(next_state$process_known)
})

testthat::test_that("chat widget input accepts text only", {
  testthat::expect_identical(dialogue_input_text("menu"), "menu")
  testthat::expect_identical(dialogue_input_text(list("menu")), "menu")
  testthat::expect_null(dialogue_input_text(list("menu", list(type = "image"))))
  testthat::expect_null(dialogue_input_text(list(text = list("menu"))))
})
