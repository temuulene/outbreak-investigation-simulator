# The roster does not move with the clock: it holds the first guests who had
# reported illness by Monday 09:00, plus well guests. A roster that followed
# the clock would drop earlier contacts and their conversations as new reports arrived.
available_characters <- function(s) {
  r <- s$reported
  ill <- r$id[which(r$ill & !is.na(r$report_hour) & r$report_hour <= 0)]
  well <- r$id[which(!r$ill & !r$background)]
  c(
    "Pat · organizer" = "organizer", "Lou · cook" = "cook",
    setNames(paste0("guest_", head(ill, 2)), paste("Guest", head(ill, 2), "· reported ill")),
    setNames(paste0("guest_", head(well, 2)), paste("Guest", head(well, 2), "· well"))
  )
}

# Generic food nouns plus every word used in a scenario's food labels.
food_word_pattern <- function() {
  generic <- c("foods?", "dish(?:es)?", "meals?", "drinks?", "beverages?")
  paste(c(generic, scenario_vocabulary$food_words), collapse = "|")
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
    menu = paste0(
      "\\b(?:menus?|foods?|eat(?:en|ing)?|ate|drinks?|beverages?|serv(?:e|ed|ing)|dishes|dinner|lunch|",
      "breakfast)\\b"
    ),
    food_sources = paste0(
      "\\b(?:who|which (?:guests?|people|attendees?))\\b[^?!.]{0,60}",
      "\\b(?:brought|made|prepared|cooked|provided|supplied|contributed)\\b",
      "[^?!.]{0,50}\\b(?:", food_word_pattern(), ")\\b|",
      "\\bwho(?: else)? (?:brought|made|provided|supplied) what\\b|",
      "\\bwho(?: else)? (?:brought|made|prepared|cooked|provided|supplied) (?:it|this|that|them)\\s*[?.!]?\\s*$|",
      "\\bfood (?:contributors?|sources?)\\b|\\bwho is lou\\b|",
      "\\bwhy\\b[^?!.]{0,20}\\blou\\b[^?!.]{0,30}\\bmention(?:ed|ing)?\\b|",
      "\\bwhat did (?:lou|you) bring\\b"
    ),
    guest_list = paste0(
      "\\b(?:rsvp|attend(?:ed|ing|ance|ees?)?|contacts?)\\b|\\b(?:guest|rsvp|attendee|contact) (?:list|names)\\b|",
      "\\bwho (?:came|was there|were there)\\b|\\blist (?:of )?(?:guests?|people|attendees?|names)\\b|",
      "^\\s*list\\s*\\??$"
    ),
    walk_ins = paste0(
      "\\bwalk[ -]?ins?\\b|\\b(?:missing|unlisted|uninvited)\\s+(?:guests?|people|attendees?|names)\\b|",
      "\\b(?:without|didn['’]t|did not) (?:an?\\s+)?rsvp\\b|",
      "\\b(?:not on|off|missing from|left off) (?:the\\s+)?(?:guest\\s+|rsvp\\s+)?list\\b|",
      "\\b(?:anyone|who) else (?:came|attended|was there)\\b|\\b(?:is|was) (?:the )?(?:guest )?list complete\\b"
    ),
    leftovers = paste0(
      "\\bleft[ -]?overs?\\b|\\bleft over\\b|",
      "\\b(?:fridge|preserv(?:e|ed|ing|ation)|discard(?:ed|ing)?|throw(?:n|ing)? away)\\b"
    ),
    preparation = paste0(
      "\\b(?:prepar(?:e|ed|ing|ation)|made|make|making|cook(?:ed|ing)?|process|recipes?|handl(?:e|ed|ing)|",
      "re-?heat(?:ed|ing)?|warm(?:ed|ing)?)\\b|\\bwalk me through\\b"
    ),
    storage = paste0(
      "\\b(?:stor(?:e|ed|ing|age)|cool(?:ed|ing)?|cold|overnight|temperatures?|refrigerat(?:e|ed|ing|ion))\\b|",
      "\\bhow (?:was|were) .{0,40}\\bkept\\b"
    ),
    symptoms = paste0(
      "\\b(?:symptoms?|sick|ill(?:ness)?|unwell|feel(?:ing)?|felt|diarrh(?:ea|oea)|fevers?|vomit(?:ed|ing)?|",
      "cramps?|nausea|nauseous)\\b|\\b(?:throw(?:ing)?|threw) up\\b|\\bbloody stools?\\b"
    ),
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
  intersect(unique(c(classify_topics(question), setdiff(selected_topic, "auto"))), dialogue_topics(character))
}

interview_clarification <- function(character) {
  if (character == "organizer") {
    return(paste(
      "I can tell you about the event, menu, who brought the food, guest list, walk-ins, leftovers,",
      'or reports of illness. For example, "What was on the menu?"'
    ))
  }
  if (character == "cook") {
    return(paste(
      "I can tell you what I brought and how I prepared or stored it.",
      'For example, "How did you prepare the food?"'
    ))
  }
  paste(
    "I can tell you what I ate, my symptoms, when they started, my age and sex, or any health care visits.",
    'For example, "How have you been feeling?"'
  )
}

# "Saturday 6 June": the meal date as people say it.
meal_day_phrase <- function(sc) {
  day <- as.POSIXlt(meal_time(sc))
  paste("Saturday", day$mday, month.name[day$mon + 1L])
}

# An onset as a guest would recall it: a day and a time to the nearest ten minutes.
onset_phrase <- function(sc, hours) {
  at <- as.POSIXlt(meal_time(sc) + round(hours * 6) * 600, tz = "UTC")
  days <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
  paste0(days[at$wday + 1L], " at about ", sprintf("%02d:%02d", at$hour, at$min))
}

organizer_replies <- function(s, topics) {
  sc <- s$sc
  replies <- character()
  if ("event" %in% topics) {
    replies <- c(replies, paste0(
      "We held the community potluck at the hall on ", meal_day_phrase(sc), ", with the meal at 18:00. ",
      "Several guests have since called about stomach illness. ",
      "I can tell you about the menu, who attended, and the leftovers."
    ))
  }
  if ("menu" %in% topics) {
    s$menu <- TRUE
    replies <- c(replies, paste0("We served ", paste(food_labels(sc), collapse = ", "), "."))
  }
  if ("food_sources" %in% topics) {
    others <- food_labels(sc)[setdiff(names(food_labels(sc)), sc$cook$dish)]
    replies <- c(replies, paste0(
      "Lou is the cook who brought the ", dish_phrase(sc, sc$cook$dish), ". ",
      "I don't have a record of who brought these other dishes: ", paste(others, collapse = ", "),
      ". You can ask Lou directly about their own dish."
    ))
  }
  if ("guest_list" %in% topics) {
    replies <- c(replies, paste(
      "I can send the RSVP list. Request it through the task desk;",
      "it doesn't include everyone who walked in."
    ))
  }
  if ("walk_ins" %in% topics) {
    s$walk_ins <- TRUE
    replies <- c(replies, paste(
      "I found", sum(!s$reported$on_list), "walk-ins. I'll add their names to your team's contact plan."
    ))
  }
  if ("leftovers" %in% topics) {
    took_home <- isTRUE(sc$setting$leftovers_taken_home)
    if (took_home) s$leftovers_home <- TRUE
    replies <- c(replies, paste0(
      "The committee plans to clear the fridge Tuesday at 09:00. ",
      "Use the preserve-leftovers task if you need samples kept.",
      if (took_home) " A few guests also took plates of leftovers home on Saturday night." else ""
    ))
  }
  if ("symptoms" %in% topics) {
    replies <- c(replies, paste(
      "Several guests have called about stomach illness after the potluck.",
      "Please ask them directly about their symptoms."
    ))
  }
  list(state = s, replies = replies)
}

cook_replies <- function(s, topics) {
  dish <- dish_phrase(s$sc, s$sc$cook$dish)
  replies <- character()
  if ("event" %in% topics) {
    replies <- c(replies, paste0(
      "I brought the ", dish, " to the hall for Saturday's 18:00 potluck. ",
      "I can tell you how I prepared and stored the food."
    ))
  }
  if (any(c("preparation", "storage") %in% topics)) {
    s$process_known <- TRUE
    replies <- c(replies, s$sc$cook$preparation)
  }
  if ("menu" %in% topics) replies <- c(replies, paste0("I brought the ", dish, ". I don't know what everyone ate."))
  if ("food_sources" %in% topics) {
    replies <- c(replies, paste0("I brought the ", dish, ". I don't know who brought the other dishes."))
  }
  list(state = s, replies = replies)
}

guest_replies <- function(s, character, topics) {
  sc <- s$sc
  id <- as.integer(sub("guest_", "", character))
  records <- reported_as_of(s)
  r <- records[records$id == id, ]
  labels <- food_labels(sc)
  replies <- character()
  if ("event" %in% topics) {
    replies <- c(replies, paste(
      "I attended the community potluck at the hall on Saturday. The meal was at 18:00.",
      "You can ask me what I ate and how I've felt since then."
    ))
  }
  eaten <- names(labels)[vapply(names(labels), function(f) isTRUE(r[[f]]), logical(1))]
  # With the menu in hand the interviewer goes through every item, so "no" is
  # recorded too. Without it, only the foods a guest names are known.
  foods <- if (!"menu" %in% topics) character() else if (s$menu) names(labels) else eaten
  fields <- domain_fields(setdiff(topics, "menu"), foods)
  if (length(fields)) s <- collect_records(s, id, fields)
  if ("food_sources" %in% topics) {
    replies <- c(
      replies, "I don't know who brought the food. You could ask Pat, the organizer, about the contributors."
    )
  }
  if ("menu" %in% topics) {
    had <- paste(labels[eaten], collapse = ", ")
    replies <- c(replies, if (s$menu) {
      if (length(eaten)) paste0("I remember having ", had, ".") else "I don't remember eating any of those foods."
    } else if (length(eaten)) {
      paste0("From memory, I had ", had, ". Without a list of what was served, I may have left something out.")
    } else {
      "I can't recall what I ate without a list of what was served."
    })
  }
  if ("symptoms" %in% topics) {
    symptoms <- names(sc$pathogen$symptoms)
    present <- symptoms[vapply(symptoms, function(f) isTRUE(r[[f]]), logical(1))]
    replies <- c(replies, if (length(present)) {
      paste0("I had ", paste(gsub("_", " ", present), collapse = ", "), ".")
    } else {
      "I haven't had any of those symptoms."
    })
  }
  if ("onset" %in% topics) {
    replies <- c(replies, if (is.na(r$onset_hours)) {
      "I haven't been ill, so there's no onset time."
    } else {
      paste0("My symptoms started on ", onset_phrase(sc, r$onset_hours), ".")
    })
  }
  if ("demographics" %in% topics) {
    replies <- c(replies, paste0("My age is ", r$age, " and my recorded sex is ", r$sex, "."))
  }
  if ("health_care_visits" %in% topics) {
    replies <- c(replies, if (r$health_care_visits) {
      "I visited a health care service."
    } else {
      "I haven't visited a health care service."
    })
  }
  list(state = s, replies = replies)
}

interview <- function(s, character, question, selected_topic = "auto") {
  roster <- available_characters(s)
  if (!character %in% unname(roster)) stop("Choose an available interview contact.")
  if (!nzchar(trimws(question)) && identical(selected_topic, "auto")) stop("Enter a question or choose a topic.")
  if (!character %in% s$interviewed && length(s$interviewed) >= s$sc$interviews$max) {
    stop("The ", s$sc$interviews$max, "-interview limit has been reached.")
  }
  topics <- interview_topics(question, character, selected_topic)
  answer <- switch(character,
    organizer = organizer_replies(s, topics),
    cook = cook_replies(s, topics),
    guest_replies(s, character, topics)
  )
  s <- answer$state
  replies <- if (length(answer$replies)) answer$replies else interview_clarification(character)
  s$interviewed <- unique(c(s$interviewed, character))
  # Charge for newly covered topics; rephrasing doesn't change the record or cost time.
  previous <- unique(unlist(lapply(s$chats[[character]], `[[`, "topics")))
  s <- advance_time(s, length(setdiff(topics, previous)) * s$sc$interviews$minutes_per_topic / 60)
  entry <- list(
    time = s$clock, question = if (nzchar(question)) question else selected_topic,
    topics = topics, reply = paste(replies, collapse = " ")
  )
  s$chats[[character]] <- append(s$chats[[character]], list(entry))
  s <- record_event(s, "interview", c(list(character = character), entry))
  if (length(topics)) s <- add_evidence(s, names(roster)[match(character, roster)], entry$reply)
  s
}
