await_experience <- function(p) {
  ready <- FALSE; value <- NULL
  promises::then(p, function(x) {value <<- x; ready <<- TRUE})
  deadline <- Sys.time() + 2
  while (!ready && Sys.time() < deadline) later::run_now(0.01)
  if (!ready) stop("Promise did not settle")
  value
}

test_that("recent context includes only the selected contact's bounded exchanges", {
  s <- new_state(scenario())
  s <- interview(s, "organizer", "menu")
  s <- interview(s, "cook", "storage")
  for (i in seq_len(6)) s <- interview(s, "organizer", paste("menu", i))
  context <- dialogue_context(s, "organizer")
  expect_length(context, 4)
  expect_identical(names(context[[1]]), c("question", "reply", "topics"))
  expect_false(any(grepl("stock pot", unlist(context), fixed = TRUE)))
  expect_null(context[[1]]$truth)
  expect_length(dialogue_context(new_state(scenario()), "organizer"), 0)
  s$chats$organizer[[7]]$question <- strrep("x", 2000)
  s$chats$organizer[[7]]$reply <- strrep("y", 3000)
  context <- dialogue_context(s, "organizer")
  expect_lte(nchar(context[[4]]$question), 600)
  expect_lte(nchar(context[[4]]$reply), 1600)
})

test_that("follow-up context is used for classification and an outage fallback", {
  history <- list(list(question = "How did you store it?", reply = "Cooling overnight.", topics = "storage"))
  captured <- NULL
  result <- await_experience(dialogue_request_async(dialogue_session(), "cook", "Can you tell me more?",
    provider = "gemini", history = history, transport = function(..., history) {
      captured <<- history
      promises::promise_resolve(list(topics = "storage", intro = "plain"))
    }))
  expect_identical(captured, history)
  expect_identical(result$topics, "storage")
  fallback <- await_experience(dialogue_request_async(dialogue_session(), "cook", "Tell me more",
    provider = "gemini", history = history, transport = function(...) stop("Unavailable")))
  expect_identical(fallback$topics, "storage")
  unrelated <- await_experience(dialogue_request_async(dialogue_session(), "cook", "What is the capital of France?",
    provider = "scripted", history = history))
  expect_length(unrelated$topics, 0)
})

test_that("reviewed wording changes the chat but preserves authored evidence and state", {
  before <- new_state(scenario())
  builder <- function(topics) tail(interview(before, "organizer", "menu", topics)$chats$organizer, 1)[[1]]$reply
  captured <- NULL
  rewritten <- paste("Here's the menu:", sub("We served ", "", builder("menu"), fixed = TRUE))
  result <- await_experience(dialogue_request_async(dialogue_session(), "organizer", "menu",
    provider = "gemini", reply_builder = builder,
    transport = function(...) promises::promise_resolve(list(topics = "menu", intro = "plain")),
    rewrite_transport = function(question, character, provider, model, answer, history, ...) {
      captured <<- answer
      promises::promise_resolve(list(reply = rewritten,
        review = list(supported = TRUE, complete = TRUE, in_character = TRUE)))
    }))
  after <- dialogue_apply(before, result)
  authored <- interview(before, "organizer", "menu", "menu")
  expect_identical(captured, authored$chats$organizer[[1]]$reply)
  expect_identical(after$chats$organizer[[1]]$reply, rewritten)
  expect_identical(after$chats$organizer[[1]]$authored_reply, captured)
  expect_identical(after$chats$organizer[[1]]$dialogue_status, "assisted")
  expect_identical(after$evidence, authored$evidence)
  expect_identical(after$collected, authored$collected)
  expect_identical(after$clock, authored$clock)
  expect_identical(after$truth, before$truth)
  expect_identical(dialogue_context(after, "organizer")[[1]]$reply, rewritten)
  path <- tempfile(fileext = ".json")
  save_session(after, path)
  expect_identical(restore_session(path, file.path(root, "scenarios"))$state$chats, after$chats)
})

test_that("changed numbers, unsafe text and rejected reviews cannot become replies", {
  good <- list(supported = TRUE, complete = TRUE, in_character = TRUE)
  expect_true(dialogue_reply_validate("Saturday's meal was at 18:00.", "The meal was Saturday at 18:00.", good))
  expect_false(dialogue_reply_validate("The meal was at 19:00.", "The meal was at 18:00.", good))
  expect_false(dialogue_reply_validate("The meal was at 18:00, with 12 guests.", "The meal was at 18:00.", good))
  expect_false(dialogue_reply_validate("<script>alert('x')</script>", "A reply", good))
  expect_false(dialogue_reply_validate("Visit https://example.com", "A reply", good))
  expect_false(dialogue_reply_validate(strrep("x", 3201), "A reply", good))
  expect_false(dialogue_reply_validate(NA_character_, "A reply", good))
  for (field in names(good)) {
    bad <- good; bad[[field]] <- FALSE
    expect_false(dialogue_reply_validate("A reply", "A reply", bad))
  }
  expect_false(dialogue_reply_validate("A reply", "A reply", c(good, list(extra = TRUE))))
})

test_that("rewriting failures retain successfully recognized topics and canonical facts", {
  before <- new_state(scenario())
  builder <- function(topics) tail(interview(before, "cook", "How did you keep it?", topics)$chats$cook, 1)[[1]]$reply
  for (rewrite in list(function(...) stop("SECRET key"),
      function(...) promises::promise_reject(simpleError("SECRET key")),
      function(...) promises::promise_resolve(list(reply = "The chicken was contaminated.",
        review = list(supported = FALSE, complete = FALSE, in_character = TRUE))))) {
    result <- await_experience(dialogue_request_async(dialogue_session(), "cook", "How did you keep it?",
      provider = "gemini", reply_builder = builder,
      transport = function(...) promises::promise_resolve(list(topics = "storage", intro = "plain")),
      rewrite_transport = rewrite))
    expect_identical(result$topics, "storage")
    expect_identical(result$status, "authored")
    after <- dialogue_apply(before, result)
    expect_true(after$process_known)
    expect_identical(after$chats$cook[[1]]$reply, builder("storage"))
    expect_false(any(grepl("SECRET|contaminated", unlist(result))))
  }
})

test_that("explicit topics receive assisted wording when a provider is configured", {
  before <- new_state(scenario())
  classify <- FALSE; rewrite <- FALSE
  result <- await_experience(dialogue_request_async(dialogue_session(), "cook", "", "menu",
    provider = "gemini", reply_builder = function(topics) tail(interview(before, "cook", "", topics)$chats$cook, 1)[[1]]$reply,
    transport = function(...) {classify <<- TRUE; stop("Should not classify an explicit topic")},
    rewrite_transport = function(..., answer) {
      rewrite <<- TRUE
      promises::promise_resolve(list(reply = answer,
        review = list(supported = TRUE, complete = TRUE, in_character = TRUE)))
    }))
  expect_false(classify)
  expect_true(rewrite)
  expect_identical(result$status, "assisted")
  expect_identical(result$topics, "menu")
})

test_that("a rewrite deadline falls back without losing recognized follow-up topics", {
  result <- await_experience(dialogue_request_async(dialogue_session(timeout = 0.01), "cook", "How did you keep it?",
    provider = "gemini", reply_builder = function(topics) "Cooling overnight.",
    transport = function(...) promises::promise_resolve(list(topics = "storage", intro = "plain")),
    rewrite_transport = function(...) promises::promise(function(resolve, reject) NULL)))
  expect_identical(result$status, "timeout")
  expect_identical(result$topics, "storage")
  expect_null(result$reply)
})

test_that("writing and review use separate scoped chats", {
  calls <- list(); payloads <- list()
  factory <- function(provider, model, prompt, ...) {
    index <- length(calls) + 1L
    calls[[index]] <<- list(prompt = prompt, ...)
    list(chat_structured_async = function(payload, type) {
      payloads[[index]] <<- jsonlite::fromJSON(payload, simplifyVector = FALSE)
      promises::promise_resolve(if (index == 1L) list(reply = "I brought chicken salad sandwiches.") else
        list(supported = TRUE, complete = TRUE, in_character = TRUE))
    })
  }
  history <- list(list(question = "Who brought it?", reply = "Lou.", topics = "food_sources"))
  result <- await_experience(dialogue_rewrite_async("What did you bring?", "cook", "gemini", "test",
    "I brought chicken salad sandwiches.", history, chat_factory = factory))
  expect_length(calls, 2)
  expect_identical(payloads[[1]]$history, history)
  expect_identical(names(payloads[[2]]), c("character", "authored_answer", "proposed_reply"))
  expect_identical(result$review$supported, TRUE)
  expect_match(calls[[1]]$prompt, "untrusted data", fixed = TRUE)
  expect_match(calls[[2]]$prompt, "without contradiction or omission", fixed = TRUE)
})

test_that("cancelled and numerically invalid drafts do not spend a review request", {
  for (cancelled in c(FALSE, TRUE)) {
    calls <- 0L
    factory <- function(...) {
      calls <<- calls + 1L
      list(chat_structured_async = function(...) promises::promise_resolve(list(reply = "The meal was at 19:00.")))
    }
    rejected <- await_experience(promises::then(dialogue_rewrite_async("When?", "organizer", "gemini", "test", "The meal was at 18:00.",
      list(), current = function() !cancelled, chat_factory = factory),
      onRejected = function(error) TRUE))
    expect_true(rejected)
    expect_identical(calls, 1L)
  }
})

test_that("rewritten replies cannot be applied to changed canonical facts", {
  before <- new_state(scenario())
  result <- list(topics = "walk_ins", intro = "plain", character = "organizer", question = "Were there walk-ins?",
    status = "assisted", reply = "I found 99 walk-ins.", authored_reply = "I found 99 walk-ins.",
    review = list(supported = TRUE, complete = TRUE, in_character = TRUE))
  after <- dialogue_apply(before, result)
  expect_false(grepl("99", after$chats$organizer[[1]]$reply, fixed = TRUE))
})
