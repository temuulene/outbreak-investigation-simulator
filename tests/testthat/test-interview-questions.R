test_that("the suggested opening question receives an event overview in both scenarios", {
  for (id in c("potluck-01", "potluck-02")) {
    before <- new_state(read_scenario(file.path(root, "scenarios", id, "scenario.yaml")))
    after <- interview(before, "organizer", "Tell me about the potluck")
    entry <- after$chats$organizer[[1]]
    expect_identical(entry$topics, "event")
    expect_match(entry$reply, "Saturday")
    expect_match(entry$reply, "18:00", fixed = TRUE)
    expect_match(entry$reply, "Several guests")
    expect_false(grepl("don't know or don't remember", entry$reply, fixed = TRUE))
    expect_false(after$menu || after$walk_ins || after$process_known)
    expect_identical(after$truth, before$truth)
    expect_equal(after$clock, before$sc$interviews$minutes_per_topic / 60)
    expect_identical(interview(after, "organizer", "Describe the event")$clock, after$clock)
  }
})

test_that("ordinary opening and focused questions recognize their intended topics", {
  questions <- list(
    event = c("Tell me about the potluck", "What happened at the event?",
      "Describe Saturday's potluck", "When and where was the potluck?",
      "Give me an overview of the event"),
    menu = c("What did you serve?", "What was for dinner?", "What did you bring?",
      "Which dishes were there?", "What beverages were available?"),
    guest_list = c("Who came?", "Who was there?", "Who attended?", "Can I have the attendees' names?"),
    walk_ins = c("Did anyone come without an RSVP?", "Was anyone left off the list?"),
    leftovers = c("Is anything left over?", "Will the leftovers be thrown away?"),
    preparation = c("How did you handle the chicken?", "How was it prepared?"),
    storage = c("Was it refrigerated?", "Was it kept cold?"),
    symptoms = c("How are you feeling?", "Did you have diarrhoea?", "Were you unwell?"),
    onset = c("When did you become sick?", "How long after the meal did symptoms begin?"),
    demographics = c("How old are you?", "What is your gender?"),
    health_care_visits = c("Did you seek medical help?", "Were you seen at a clinic?")
  )
  for (topic in names(questions)) {
    for (question in questions[[topic]]) {
      expect_true(topic %in% classify_topics(question), info = question)
    }
  }
})

test_that("whole words prevent incidental topic matches", {
  for (question in c("Will you help?", "What is your average?", "Tell me a story",
      "Who else likes music?", "What else can you say?")) {
    expect_identical(classify_topics(question), character(), info = question)
  }
  expect_identical(classify_topics("Was it kept cold?"), "storage")
  expect_identical(classify_topics("How old are you?"), "demographics")
  expect_false("onset" %in% classify_topics("When and where was the potluck?"))
  expect_false("event" %in% classify_topics("When did symptoms start after the potluck?"))
})

test_that("focused questions do not collect incidental guest lists or miss everyday symptom wording", {
  expect_identical(classify_topics("Can I have a list of dishes?"), "menu")
  expect_identical(classify_topics("What did people eat?"), "menu")
  expect_identical(classify_topics("Did you throw up?"), "symptoms")
  expect_identical(classify_topics("Did you have bloody stools?"), "symptoms")
  expect_identical(classify_topics("How was the food kept?"), c("menu", "storage"))
  expect_true("walk_ins" %in% classify_topics("Who didn't RSVP?"))
})

test_that("basic follow-ups produce useful answers within each character's knowledge", {
  cases <- list(
    list(character = "organizer", question = "What did you serve?", topic = "menu", reply = "We served"),
    list(character = "organizer", question = "Who came?", topic = "guest_list", reply = "RSVP list"),
    list(character = "organizer", question = "Is anything left over?", topic = "leftovers", reply = "Tuesday"),
    list(character = "cook", question = "Was it refrigerated?", topic = "storage", reply = "overnight"),
    list(character = "cook", question = "How did you handle the chicken?", topic = "preparation", reply = "stock pot")
  )
  for (case in cases) {
    after <- interview(new_state(scenario()), case$character, case$question)
    entry <- after$chats[[case$character]][[1]]
    expect_true(case$topic %in% entry$topics, info = case$question)
    expect_match(entry$reply, case$reply, fixed = TRUE)
  }
})

test_that("a topic selection supplements a written question", {
  after <- interview(new_state(scenario()), "organizer", "Were there walk-ins?", "menu")
  expect_true(after$menu && after$walk_ins)
  expect_setequal(after$chats$organizer[[1]]$topics, c("menu", "walk_ins"))
  blank <- interview(new_state(scenario()), "organizer", "", "guest_list")
  expect_identical(blank$chats$organizer[[1]]$topics, "guest_list")
})

test_that("event overviews keep preparation and personal histories for follow-up questions", {
  before <- new_state(scenario())
  contacts <- c("cook", unname(available_characters(before)[3:4]))
  for (contact in contacts) {
    after <- interview(before, contact, "Tell me about the potluck")
    entry <- after$chats[[contact]][[1]]
    expect_identical(entry$topics, "event")
    expect_match(entry$reply, "Saturday")
    expect_false(after$process_known || after$menu || after$walk_ins)
    expect_false(any(c("diarrhea", "onset_hours", names(food_labels(before$sc))) %in% names(after$collected)))
    expect_false(grepl(before$sc$pathogen$name, entry$reply, fixed = TRUE))
  }
})

test_that("unmatched questions offer a concrete follow-up without adding evidence", {
  before <- new_state(scenario())
  for (contact in c("organizer", "cook", unname(available_characters(before)[3]))) {
    after <- interview(before, contact, "Tell me a joke")
    entry <- after$chats[[contact]][[1]]
    expect_match(entry$reply, "For example", fixed = TRUE)
    expect_false(grepl("don't know or don't remember", entry$reply, fixed = TRUE))
    expect_identical(after$evidence, before$evidence)
    expect_identical(after$clock, before$clock)
    expect_false(after$menu || after$walk_ins || after$process_known)
  }
})
