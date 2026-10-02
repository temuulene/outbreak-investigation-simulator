available_characters <- function(s) {
  ill <- s$reported$id[which(s$reported$ill & s$reported$report_hour <= s$clock)]
  well <- s$reported$id[which(!s$reported$ill)]
  c("Pat · organizer" = "organizer", "Lou · cook" = "cook",
    setNames(paste0("guest_", head(ill, 2)), paste("Guest", head(ill, 2), "· reported ill")),
    setNames(paste0("guest_", head(well, 2)), paste("Guest", head(well, 2), "· well")))
}

classify_topics <- function(question) {
  patterns <- c(menu = "menu|food|eat|ate|drink|served", guest_list = "list|rsvp|attend|contact|people|guest",
    walk_ins = "walk.?in|missing|else|without|complete", leftovers = "leftover|fridge|preserv|discard|keep",
    preparation = "prepar|made|make|cook|walk me|process|recipe", storage = "stor|cool|overnight|temperature",
    symptoms = "symptom|sick|ill|feel|diarr|fever|vomit|cramp", onset = "when|onset|start|time",
    demographics = "age|old|sex", health_care_visits = "doctor|hospital|care|clinic")
  names(patterns)[vapply(patterns, function(pattern) grepl(pattern, question, ignore.case = TRUE), logical(1))]
}

interview <- function(s, character, question, selected_topic = "auto") {
  if (!character %in% unname(available_characters(s))) stop("Choose an available interview contact.")
  if (!nzchar(trimws(question)) && selected_topic == "auto") stop("Enter a question or choose a topic.")
  if (!character %in% s$interviewed && length(s$interviewed) >= s$sc$interviews$max) stop("The six-interview limit has been reached.")
  topics <- if (selected_topic == "auto") classify_topics(question) else selected_topic
  allowed <- if (character == "organizer") c("menu", "guest_list", "walk_ins", "leftovers", "symptoms") else if (character == "cook") c("preparation", "storage", "menu") else c("menu", "symptoms", "onset", "demographics", "health_care_visits")
  topics <- intersect(topics, allowed)
  replies <- character()
  if (character == "organizer") {
    if ("menu" %in% topics) {
      s$menu <- TRUE
      replies <- c(replies, paste("We served", paste(food_labels(s$sc)[s$sc$menu_order], collapse = ", "), ". Lou brought the chicken salad."))
    }
    if ("guest_list" %in% topics) replies <- c(replies, "I can send the RSVP list. Request it through the task desk; it doesn't include everyone who walked in.")
    if ("walk_ins" %in% topics) {
      s$walk_ins <- TRUE
      replies <- c(replies, paste("I found", sum(!s$reported$on_list), "walk-ins. I'll add their names to your team's contact plan."))
    }
    if ("leftovers" %in% topics) replies <- c(replies, "The committee plans to clear the fridge Tuesday at 09:00. Use the preserve-leftovers task if you need samples kept.")
    if ("symptoms" %in% topics) replies <- c(replies, "Several guests have called about stomach illness after the potluck. Please ask them directly about their symptoms.")
  } else if (character == "cook") {
    if (any(c("preparation", "storage") %in% topics)) {
      s$process_known <- TRUE
      replies <- c(replies, paste("I cooked the chicken, then left it cooling in a deep stock pot overnight. I mixed the salad the next day and brought it to the hall."))
    }
    if ("menu" %in% topics) replies <- c(replies, "I brought the chicken salad sandwiches. I don't know what everyone ate.")
  } else {
    id <- as.integer(sub("guest_", "", character))
    records <- reported_as_of(s)
    r <- records[records$id == id, ]
    foods <- if ("menu" %in% topics) names(food_labels(s$sc)) else character()
    s <- collect_records(s, id, domain_fields(setdiff(topics, "menu"), foods))
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
  if (!length(replies)) replies <- "I don't know or don't remember. Try the topic menu to ask about something I can speak to."
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
