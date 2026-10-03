available_characters <- function(s) {
  ill <- s$reported$id[which(s$reported$ill & s$reported$report_hour <= s$clock)]
  well <- s$reported$id[which(!s$reported$ill)]
  c("Pat · organizer" = "organizer", "Lou · cook" = "cook",
    setNames(paste0("guest_", head(ill, 2)), paste("Guest", head(ill, 2), "· reported ill")),
    setNames(paste0("guest_", head(well, 2)), paste("Guest", head(well, 2), "· well")))
}

classify_topics <- function(question) {
  patterns <- c(
    event = paste0(
      "\\b(?:tell me about|describe)\\s+(?:(?:the|this|that|saturday['’]s)\\s+)?",
      "(?:community\\s+)?(?:potluck|event|meal|gathering)\\b|",
      "\\bwhat happened(?:\\s+at (?:the\\s+)?(?:potluck|event|meal|gathering))?\\s*\\??$|",
      "\\b(?:when(?: and where)?|where)\\s+(?:was|did)\\s+(?:the\\s+)?(?:potluck|event|meal|gathering)\\b|",
      "\\b(?:overview|background)\\s+(?:of|on|about)\\s+(?:the\\s+)?(?:potluck|event|meal|gathering)\\b"
    ),
    menu = "\\b(?:menus?|foods?|eat(?:en|ing)?|ate|drinks?|beverages?|serv(?:e|ed|ing)|dishes|dinner|lunch|breakfast)\\b",
    food_sources = paste0(
      "\\b(?:who|which (?:guests?|people|attendees?))\\b[^?!.]{0,60}\\b(?:brought|made|prepared|cooked|provided|supplied|contributed)\\b",
      "[^?!.]{0,50}\\b(?:foods?|dish(?:es)?|meals?|drinks?|beverages?|cake|punch|rice|coleslaw|chicken|salad|sandwich(?:es)?)\\b|",
      "\\bwho(?: else)? (?:brought|made|provided|supplied) what\\b|",
      "\\bwho(?: else)? (?:brought|made|prepared|cooked|provided|supplied) (?:it|this|that|them)\\s*[?.!]?\\s*$|",
      "\\bfood (?:contributors?|sources?)\\b|\\bwho is lou\\b|",
      "\\bwhy\\b[^?!.]{0,20}\\blou\\b[^?!.]{0,30}\\bmention(?:ed|ing)?\\b|",
      "\\bwhat did (?:lou|you) bring\\b"
    ),
    guest_list = paste0(
      "\\b(?:rsvp|attend(?:ed|ing|ance|ees?)?|contacts?)\\b|\\b(?:guest|rsvp|attendee|contact) (?:list|names)\\b|",
      "\\bwho (?:came|was there|were there)\\b|\\blist (?:of )?(?:guests?|people|attendees?|names)\\b|^\\s*list\\s*\\??$"
    ),
    walk_ins = paste0(
      "\\bwalk[ -]?ins?\\b|\\b(?:missing|unlisted|uninvited)\\s+(?:guests?|people|attendees?|names)\\b|",
      "\\b(?:without|didn['’]t|did not) (?:an?\\s+)?rsvp\\b|\\b(?:not on|off|missing from|left off) (?:the\\s+)?(?:guest\\s+|rsvp\\s+)?list\\b|",
      "\\b(?:anyone|who) else (?:came|attended|was there)\\b|\\b(?:is|was) (?:the )?(?:guest )?list complete\\b"
    ),
    leftovers = "\\bleft[ -]?overs?\\b|\\bleft over\\b|\\b(?:fridge|preserv(?:e|ed|ing|ation)|discard(?:ed|ing)?|throw(?:n|ing)? away)\\b",
    preparation = "\\b(?:prepar(?:e|ed|ing|ation)|made|make|making|cook(?:ed|ing)?|process|recipes?|handl(?:e|ed|ing))\\b|\\bwalk me through\\b",
    storage = "\\b(?:stor(?:e|ed|ing|age)|cool(?:ed|ing)?|cold|overnight|temperatures?|refrigerat(?:e|ed|ing|ion))\\b|\\bhow (?:was|were) .{0,40}\\bkept\\b",
    symptoms = "\\b(?:symptoms?|sick|ill(?:ness)?|unwell|feel(?:ing)?|felt|diarrh(?:ea|oea)|fevers?|vomit(?:ed|ing)?|cramps?|nausea|nauseous)\\b|\\b(?:throw(?:ing)?|threw) up\\b|\\bbloody stools?\\b",
    onset = "\\b(?:when|onset|start(?:ed|ing)?|times?|began|begin)\\b|\\bhow long after\\b",
    demographics = "\\b(?:ages?|old|sex|gender)\\b",
    health_care_visits = "\\b(?:doctors?|hospitals?|care|clinics?|medical|treatment|treated)\\b"
  )
  topics <- names(patterns)[vapply(patterns, function(pattern) {
    grepl(pattern, question, ignore.case = TRUE, perl = TRUE)
  }, logical(1))]
  # Event timing is distinct from the guest's illness onset.
  if ("event" %in% topics && !"symptoms" %in% topics) topics <- setdiff(topics, "onset")
  if ("food_sources" %in% topics) {
    # Asking who made food does not ask what guests ate or how it was prepared.
    if (!grepl("\\bmenus?\\b|\\b(?:eat(?:en|ing)?|ate)\\b|\\bwhat\\b[^?!.]{0,40}\\bserv(?:e|ed|ing)\\b", question,
        ignore.case = TRUE, perl = TRUE)) topics <- setdiff(topics, "menu")
    if (!grepl("\\bhow\\b[^?!.]{0,70}\\b(?:prepar(?:e|ed|ing|ation)|made|make|cooked|cook|handled)\\b", question,
        ignore.case = TRUE, perl = TRUE)) topics <- setdiff(topics, "preparation")
  }
  topics
}

interview_topics <- function(question, character, selected_topic = "auto") {
  intersect(unique(c(classify_topics(question), setdiff(selected_topic, "auto"))),
    dialogue_topics(character))
}

interview_clarification <- function(character) {
  if (character == "organizer") {
    return('I can tell you about the event, menu, who brought the food, guest list, walk-ins, leftovers, or reports of illness. For example, "What was on the menu?"')
  }
  if (character == "cook") {
    return('I can tell you what I brought and how I prepared or stored it. For example, "How did you prepare the food?"')
  }
  'I can tell you what I ate, my symptoms, when they started, my age and sex, or any health care visits. For example, "How have you been feeling?"'
}

interview <- function(s, character, question, selected_topic = "auto") {
  if (!character %in% unname(available_characters(s))) stop("Choose an available interview contact.")
  if (!nzchar(trimws(question)) && identical(selected_topic, "auto")) stop("Enter a question or choose a topic.")
  if (!character %in% s$interviewed && length(s$interviewed) >= s$sc$interviews$max) stop("The six-interview limit has been reached.")
  topics <- interview_topics(question, character, selected_topic)
  replies <- character()
  if (character == "organizer") {
    if ("event" %in% topics) replies <- c(replies, paste(
      "We held the community potluck at the hall on Saturday, with the meal at 18:00.",
      "Several guests have since called about stomach illness.",
      "I can tell you about the menu, who attended, and the leftovers."))
    if ("menu" %in% topics) {
      s$menu <- TRUE
      replies <- c(replies, paste0("We served ", paste(food_labels(s$sc)[s$sc$menu_order], collapse = ", "), "."))
    }
    if ("food_sources" %in% topics) {
      other_dishes <- food_labels(s$sc)[setdiff(s$sc$menu_order, "chicken_salad")]
      replies <- c(replies, paste0("Lou is the cook who brought the chicken salad sandwiches. ",
        "I don't have a record of who brought these other dishes: ", paste(other_dishes, collapse = ", "),
        ". You can ask Lou directly about their own dish."))
    }
    if ("guest_list" %in% topics) replies <- c(replies, "I can send the RSVP list. Request it through the task desk; it doesn't include everyone who walked in.")
    if ("walk_ins" %in% topics) {
      s$walk_ins <- TRUE
      replies <- c(replies, paste("I found", sum(!s$reported$on_list), "walk-ins. I'll add their names to your team's contact plan."))
    }
    if ("leftovers" %in% topics) replies <- c(replies, "The committee plans to clear the fridge Tuesday at 09:00. Use the preserve-leftovers task if you need samples kept.")
    if ("symptoms" %in% topics) replies <- c(replies, "Several guests have called about stomach illness after the potluck. Please ask them directly about their symptoms.")
  } else if (character == "cook") {
    if ("event" %in% topics) replies <- c(replies, paste(
      "I brought the chicken salad sandwiches to the hall for Saturday's 18:00 potluck.",
      "I can tell you how I prepared and stored them."))
    if (any(c("preparation", "storage") %in% topics)) {
      s$process_known <- TRUE
      replies <- c(replies, paste("I cooked the chicken, then left it cooling in a deep stock pot overnight. I mixed the salad the next day and brought it to the hall."))
    }
    if ("menu" %in% topics) replies <- c(replies, "I brought the chicken salad sandwiches. I don't know what everyone ate.")
    if ("food_sources" %in% topics) replies <- c(replies, "I brought the chicken salad sandwiches. I don't know who brought the other dishes.")
  } else {
    id <- as.integer(sub("guest_", "", character))
    records <- reported_as_of(s)
    r <- records[records$id == id, ]
    if ("event" %in% topics) replies <- c(replies, paste(
      "I attended the community potluck at the hall on Saturday. The meal was at 18:00.",
      "You can ask me what I ate and how I've felt since then."))
    foods <- if ("menu" %in% topics) names(food_labels(s$sc)) else character()
    fields <- domain_fields(setdiff(topics, "menu"), foods)
    if (length(fields)) s <- collect_records(s, id, fields)
    if ("food_sources" %in% topics) replies <- c(replies, "I don't know who brought the food. You could ask Pat, the organizer, about the contributors.")
    if ("menu" %in% topics) {
      # Individual recall does not substitute for obtaining the complete event menu.
      eaten <- names(food_labels(s$sc))[vapply(names(food_labels(s$sc)), function(f) isTRUE(r[[f]]), logical(1))]
      replies <- c(replies, if (length(eaten)) paste("I remember having", paste(food_labels(s$sc)[eaten], collapse = ", "), ".") else "I don't remember eating any of those foods.")
    }
    if ("symptoms" %in% topics) {
      symptoms <- names(s$sc$pathogen$symptoms)[vapply(names(s$sc$pathogen$symptoms), function(f) isTRUE(r[[f]]), logical(1))]
      replies <- c(replies, if (length(symptoms)) paste("I had", paste(gsub("_", " ", symptoms), collapse = ", "), ".") else "I haven't had any of those symptoms.")
    }
    if ("onset" %in% topics) replies <- c(replies, if (is.na(r$onset_hours)) "I haven't been ill, so there's no onset time." else paste("My symptoms started", round(r$onset_hours, 1), "hours after Saturday's 18:00 meal."))
    if ("demographics" %in% topics) replies <- c(replies, paste("My age is", r$age, "and my recorded sex is", r$sex, "."))
    if ("health_care_visits" %in% topics) replies <- c(replies, if (r$health_care_visits) "I visited a health care service." else "I haven't visited a health care service.")
  }
  if (!length(replies)) replies <- interview_clarification(character)
  s$interviewed <- unique(c(s$interviewed, character))
  # Charge for newly covered topics; rephrasing doesn't change the record or cost time.
  previous <- unique(unlist(lapply(s$chats[[character]], `[[`, "topics")))
  s <- advance_time(s, length(setdiff(topics, previous)) * s$sc$interviews$minutes_per_topic / 60)
  entry <- list(time = s$clock, question = if (nzchar(question)) question else selected_topic,
    topics = topics, reply = paste(replies, collapse = " "))
  s$chats[[character]] <- append(s$chats[[character]], list(entry))
  s <- record_event(s, "interview", c(list(character = character), entry))
  if (length(topics)) s <- add_evidence(s, names(available_characters(s))[match(character, available_characters(s))], entry$reply)
  s
}
