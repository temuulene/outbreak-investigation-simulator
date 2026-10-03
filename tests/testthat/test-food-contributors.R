test_that("Pat answers the contributor follow-up without repeating the menu", {
  question <- "don't understand why Lou is being mentioned here. Who else brought which food?"
  for (id in c("potluck-01", "potluck-02")) {
    before <- new_state(read_scenario(file.path(root, "scenarios", id, "scenario.yaml")))
    menu <- interview(before, "organizer", "what was on the menu")
    first <- menu$chats$organizer[[1]]
    expect_match(first$reply, "We served Chocolate cake, Fruit punch, Fried rice, Coleslaw, Chicken salad sandwiches.", fixed = TRUE)
    expect_false(grepl("Lou", first$reply, fixed = TRUE))
    after <- interview(menu, "organizer", question)
    follow_up <- after$chats$organizer[[2]]
    expect_identical(follow_up$topics, "food_sources")
    expect_match(follow_up$reply, "Lou is the cook who brought the chicken salad sandwiches", fixed = TRUE)
    expect_match(follow_up$reply, "don't have a record of who brought", fixed = TRUE)
    for (dish in c("Chocolate cake", "Fruit punch", "Fried rice", "Coleslaw")) {
      expect_match(follow_up$reply, dish, fixed = TRUE)
    }
    expect_false(grepl("We served", follow_up$reply, fixed = TRUE))
    expect_false(after$process_known)
    expect_identical(after$truth, before$truth)
  }
})

test_that("contributor and identity questions do not request food histories or preparation", {
  for (question in c("Who brought the food?", "Who else brought which food?",
      "Who made the chicken salad?", "Who prepared the coleslaw?",
      "Who cooked the fried rice?", "Which guests supplied the dishes?",
      "Who provided the fruit punch?", "Who brought the drinks?", "Who supplied the beverages?",
      "Who brought the sandwiches?", "Who brought what?", "Who made it?",
      "Tell me about the food contributors", "Who is Lou?",
      "Why was Lou mentioned?", "What did Lou bring?", "What did you bring?")) {
    expect_identical(classify_topics(question), "food_sources", info = question)
  }
  expect_identical(classify_topics("Who brought their dog?"), character())
  expect_true("menu" %in% classify_topics("What did you eat?"))
  expect_true("preparation" %in% classify_topics("How did you prepare the chicken?"))
  expect_setequal(classify_topics("Who brought the food, and how was it prepared?"),
    c("food_sources", "preparation"))
  expect_setequal(classify_topics("Who brought the food, and what did you eat?"),
    c("food_sources", "menu"))
})

test_that("each contact reports only their authored knowledge of contributors", {
  before <- new_state(scenario())
  cook <- interview(before, "cook", "Who made the chicken salad?")
  expect_match(cook$chats$cook[[1]]$reply, "I brought the chicken salad sandwiches", fixed = TRUE)
  expect_match(cook$chats$cook[[1]]$reply, "don't know who brought the other dishes", fixed = TRUE)
  expect_false(cook$process_known || cook$menu)
  guest <- unname(available_characters(before)[3])
  after <- interview(before, guest, "Who brought the food?")
  expect_match(after$chats[[guest]][[1]]$reply, "don't know who brought", fixed = TRUE)
  expect_false(after$process_known || after$menu)
  expect_identical(after$collected, before$collected)
  selected <- interview(before, "organizer", "", "food_sources")
  expect_match(selected$chats$organizer[[1]]$reply, "Lou is the cook", fixed = TRUE)
  expect_false(selected$menu || selected$process_known)
  mixed <- interview(before, "cook", "Who brought the food, and how was it prepared?")
  expect_true(mixed$process_known)
  expect_match(mixed$chats$cook[[1]]$reply, "stock pot", fixed = TRUE)
})

test_that("contributor follow-ups work through both conversation interfaces", {
  withr::local_dir(root)
  library(shiny)
  for (interface in c("scripted", "shinychat")) {
    withr::with_envvar(c(FIELDNOTES_CHAT_UI = interface), shiny::testServer(app_server, {
      session$setInputs(character = "organizer", topic = "auto")
      question <- "Who else brought which food?"
      if (interface == "shinychat") {
        session$setInputs(conversation_user_input = list("what was on the menu"))
        session$setInputs(conversation_user_input = list(question))
      } else {
        session$setInputs(question = "what was on the menu", ask = 1)
        session$setInputs(question = question, ask = 2)
      }
      expect_length(s()$chats$organizer, 2)
      expect_identical(s()$chats$organizer[[2]]$topics, "food_sources")
      expect_match(s()$chats$organizer[[2]]$reply, "don't have a record", fixed = TRUE)
    }))
  }
})
