app_server <- function(input, output, session) {
  s <- reactiveVal(start_session())
  dialogue <- dialogue_session()
  dialogue_busy <- reactiveVal(FALSE)
  dialogue_note <- reactiveVal(NULL)
  repeat_plan <- reactiveVal(FALSE)
  debrief_generation <- reactiveVal(0L)
  session$onSessionEnded(function() dialogue_cancel(dialogue))
  snapshot <- reactiveVal(NULL)
  feedback <- reactiveVal(NULL)
  strata_result <- reactiveVal(NULL)
  strata_snapshot <- reactiveVal(NULL)
  steps <- guide_steps()
  guide_step <- reactiveVal(1L)
  furthest_step <- reactiveVal(1L)
  checkpoint_drafts <- reactiveVal(list())
  run <- function(operation, message = "Saved.") {
    tryCatch({
      s(operation(isolate(s())))
      if (!is.null(message)) showNotification(message, type = "message", duration = 3)
      TRUE
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 6)
      FALSE
    })
  }
  focus_step <- function() session$sendCustomMessage("guide-focus", list())
  go_step <- function(index) {
    current <- isolate(guide_step())
    drafts <- isolate(checkpoint_drafts())
    if (steps$page[current] == "decision") {
      drafts[[as.character(current)]] <- setNames(lapply(checkpoint_fields(), function(field) isolate(input[[field]])), checkpoint_fields())
      checkpoint_drafts(drafts)
    }
    guide_step(index)
    furthest_step(max(isolate(furthest_step()), index))
    updateTabsetPanel(session, "stage", selected = steps$page[index])
    if (steps$page[index] == "decision") {
      draft <- drafts[[as.character(index)]]
      for (field in setdiff(checkpoint_fields(), "confidence")) {
        value <- if (is.null(draft[[field]])) "" else draft[[field]]
        if (field == "suspect") updateTextInput(session, field, value = value) else updateTextAreaInput(session, field, value = value)
      }
      updateRadioButtons(session, "confidence", selected = if (is.null(draft$confidence)) "Low" else draft$confidence)
      updateTabsetPanel(session, "reflection", selected = "suspect")
    }
    focus_step()
  }
  observe({
    visited <- seq_len(furthest_step())
    updateSelectInput(session, "revisit", choices = setNames(visited, steps$label[visited]), selected = guide_step())
  })
  observeEvent(input$go_revisit, {
    index <- suppressWarnings(as.integer(input$revisit))
    if (length(index) == 1 && !is.na(index) && index >= 1 && index <= furthest_step()) go_step(index)
  })
  observeEvent(input$back_step, go_step(max(1L, guide_step() - 1L)))
  output$guide_progress <- renderUI({
    index <- guide_step()
    div(div(class = "step-label", span(steps$label[index]), span(paste("Step", max(1, index - 1), "of 9"))),
      div(class = "progress-track", role = "progressbar", `aria-label` = "Investigation steps", `aria-valuemin` = 1, `aria-valuemax` = 9, `aria-valuenow` = max(1, index - 1),
        div(class = "progress-fill", style = paste0("width:", max(1, index - 1) / 9 * 100, "%"))))
  })
  output$pending_badge <- renderUI(if (length(s()$queue)) span(class = "pending-badge", length(s()$queue)))
  output$media_badge <- renderUI({
    pending <- setdiff(vapply(s()$media_events, `[[`, character(1), "id"), vapply(s()$media_responses, `[[`, character(1), "id"))
    if (length(pending)) span(class = "pending-badge", `aria-label` = "Media requests awaiting a response", length(pending))
  })
  output$checkpoint_prompt <- renderUI({
    prompt <- switch(as.character(guide_step()),
      `2` = "Before asking questions, capture your first thoughts. You aren’t expected to know the source yet.",
      `4` = "Think back over the interviews. What has changed, and what still needs checking?",
      "Bring together the data and what people told you. How strong is the evidence now?")
    p(class = "step-intro", prompt)
  })
  observeEvent(input$reflection_next, {
    if (any(!nzchar(trimws(c(input$suspect, input$supporting))))) {
      showNotification("Add your working idea and a short reason. ‘Still open’ is fine.", type = "warning"); return()
    }
    updateTabsetPanel(session, "reflection", selected = "uncertainty"); focus_step()
  })
  observeEvent(input$reflection_action, {
    if (any(!nzchar(trimws(c(input$against, input$change))))) {
      showNotification("Note what is missing and what might change your mind.", type = "warning"); return()
    }
    updateTabsetPanel(session, "reflection", selected = "action"); focus_step()
  })
  observeEvent(input$reflection_back, {updateTabsetPanel(session, "reflection", selected = "suspect"); focus_step()})
  observeEvent(input$reflection_back_again, {updateTabsetPanel(session, "reflection", selected = "uncertainty"); focus_step()})
  observeEvent(input$interviews_done, go_step(4L))
  observeEvent(input$analysis_done, go_step(8L))
  observe({
    state <- s()
    choices <- available_characters(state)
    selected <- isolate(input$character)
    updateSelectInput(session, "character", choices = choices,
      selected = if (!is.null(selected) && selected %in% choices) selected else "organizer")
  })
  observe({
    sc <- s()$sc
    tasks <- Filter(function(x) x$id != "team_interviews", sc$tasks)
    updateSelectInput(session, "task", choices = setNames(vapply(tasks, `[[`, character(1), "id"),
      vapply(tasks, function(t) paste0(t$label, " · ", t$effort_min, " min + ", t$turnaround_hours, " h"), character(1))))
    updateSelectInput(session, "action", choices = setNames(vapply(sc$actions, `[[`, character(1), "id"), vapply(sc$actions, `[[`, character(1), "label")))
  })
  observe({
    state <- s()
    labels <- food_labels(state$sc)
    if (!state$menu) labels <- labels[intersect(names(labels), names(state$collected))]
    exposure <- input$strata_exposure
    if (is.null(exposure) || !exposure %in% names(labels)) exposure <- head(names(labels), 1L)
    groups <- setdiff(names(labels), exposure)
    selected_group <- intersect(isolate(input$strata_food), groups)
    updateSelectInput(session, "strata_food", choices = setNames(groups, labels[groups]), selected = if (length(selected_group)) selected_group else head(groups, 1L))
    updateSelectInput(session, "strata_exposure", choices = setNames(names(labels), labels), selected = exposure)
    updateSelectInput(session, "analysis_food", choices = setNames(names(labels), labels), selected = isolate(input$analysis_food))
  })
  output$clock <- renderText(game_time(s()$clock))
  output$evidence <- renderUI(tagList(lapply(rev(s()$evidence), function(e) div(class = "evidence-entry",
    div(class = "evidence-meta", game_time(e$time), " / ", e$source), p(e$finding)))))
  output$metrics <- renderUI({
    state <- s()
    div(class = "metrics", div(strong(nrow(state$collected)), "guest records"),
      div(strong(length(state$interviewed)), "of 6 contacts"), div(strong(length(state$checkpoints)), "checkpoints saved"))
  })
  observeEvent(input$begin, {
    if (furthest_step() == 1L) {
      ok <- run(function(state) start_session(if (is.null(input$scenario_id)) "potluck-01" else input$scenario_id, isTRUE(input$randomize)), NULL)
      if (!ok) return()
    }
    go_step(2L)
  })
  observeEvent(input$hint, showModal(modalDialog(title = "A useful next step",
    switch(steps$page[guide_step()],
      decision = "Write what you know now. ‘Still open’ and ‘not enough evidence yet’ are useful answers when you explain why. You can read the notebook without losing this draft.",
      interview = "Start with Pat: ask about the menu and who attended. Then compare an ill guest’s account with a well guest’s. The topic menu can help if a question is misunderstood.",
      define = "Use criteria your team can apply consistently. Think about people, onset time, and symptoms. Requiring lab confirmation may leave many people unclassified.",
      collect = "Ask the same exposure questions of ill and well guests. The preview shows who your team can reach. Ask Pat about walk-ins if the list seems incomplete.",
      analysis = "Start with the workbook or R script. For each food, divide cases by people with known exposure and case status. Compare attack rates, and discuss uncertainty.",
      recommend = "State the suspected source, your supporting evidence, your suggested actions, and what remains uncertain. A well-supported cautious conclusion is enough.",
      "Choose one earlier decision. Explain why you made it, then say what you would do differently."), easyClose = TRUE)))
  output$chat <- renderUI({
    req(input$character)
    history <- s()$chats[[input$character]]
    if (!length(history)) return(div(class = "chat-empty", p("Start with an open question, then follow up on what you hear.")))
    div(class = "chat-history", role = "log", `aria-live` = "polite", lapply(history, function(x) tagList(
      div(class = "chat-question", eyebrow(paste("YOU /", game_time(x$time))), x$question),
      div(class = "chat-answer", x$reply))))
  })
  observeEvent(input$character, {
    topics <- dialogue_topics(input$character)
    updateSelectInput(session, "topic", choices = c("Use my written question" = "auto", setNames(topics, gsub("_", " ", topics))), selected = "auto")
  })
  chat_notice <- function(text) {
    if (identical(Sys.getenv("FIELDNOTES_CHAT_UI"), "shinychat")) shinychat::chat_append("conversation", tags$p(text), session = session)
  }
  ask_question <- function(question) {
    if (dialogue_busy()) {dialogue_note("Please wait for the current reply."); return(FALSE)}
    if (!is.character(question) || length(question) != 1L || is.na(question) || nchar(question) > 1200L) {showNotification("Enter a question of up to 1200 characters.", type = "warning"); return(FALSE)}
    if (is.null(input$character) || !input$character %in% available_characters(s())) return(FALSE)
    topic <- input$topic
    if (is.null(topic) || !topic %in% c("auto", dialogue_topics(input$character))) topic <- "auto"
    if (Sys.getenv("FIELDNOTES_DIALOGUE_PROVIDER", "scripted") == "scripted") {
      return(run(function(state) interview(state, input$character, question, topic), "Reply added to your notebook."))
    }
    before <- isolate(s())
    dialogue_busy(TRUE)
    dialogue_note("Preparing a reply… You can keep reading your notebook.")
    promise <- tryCatch(dialogue_request_async(dialogue, input$character, question, topic), error = function(e) {
      dialogue_busy(FALSE); dialogue_note(conditionMessage(e)); NULL
    })
    if (is.null(promise)) return(FALSE)
    request_token <- dialogue$token
    promises::then(promise, onFulfilled = function(result) {
      if (!dialogue_result_current(dialogue, result)) return(NULL)
      dialogue_busy(FALSE)
      if (!identical(isolate(s()), before)) {
        dialogue_note("Your investigation changed while the reply was preparing. Ask again to use the current evidence.")
        chat_notice("Your investigation changed while the reply was preparing. Ask again to use the current evidence.")
        return(NULL)
      }
      run(function(state) dialogue_apply(state, result), NULL)
      dialogue_note(if (result$status == "scripted") "Reply added to your notebook." else paste("Reply added ·", result$status))
      NULL
    }, onRejected = function(error) {
      if (!identical(dialogue$token, request_token)) return(NULL)
      dialogue_busy(FALSE); dialogue_note("Reply unavailable. Please try again.")
      chat_notice("Reply unavailable. Please try again."); NULL
    })
  }
  observeEvent(input$ask, ask_question(if (identical(Sys.getenv("FIELDNOTES_CHAT_UI"), "shinychat")) "" else input$question))
  observeEvent(input$conversation_user_input, {
    handled <- ask_question(dialogue_input_text(input$conversation_user_input))
    if (identical(handled, FALSE)) chat_notice(if (dialogue_busy()) "Please wait for the current reply." else "Choose a contact and enter a text question of up to 1,200 characters, or use the topic menu.")
  })
  observeEvent(list(input$character, s()$chats), {
    if (!identical(Sys.getenv("FIELDNOTES_CHAT_UI"), "shinychat")) return()
    shinychat::chat_clear("conversation", session = session)
    for (entry in s()$chats[[input$character]]) {
      shinychat::chat_append("conversation", tags$p(entry$question), role = "user", session = session)
      shinychat::chat_append("conversation", tags$p(entry$reply), session = session)
    }
  }, ignoreInit = TRUE)
  output$dialogue_status <- renderUI(if (!is.null(dialogue_note())) p(class = "small-note", role = "status", dialogue_note()))
  output$interview_progress <- renderUI(p(class = "interview-count", paste(length(s()$interviewed), "of 6 contacts used · Aim for 3–4, then continue when ready.")))
  menu_known <- reactiveVal(FALSE)
  observe(menu_known(s()$menu))
  output$food_questions <- renderUI({
    if (!menu_known()) return(div(class = "note", "You don’t have the menu yet. Go back to ask Pat, or send your plan without food questions."))
    sc <- isolate(s())$sc
    labels <- food_labels(sc)[sc$menu_order]
    checkboxGroupInput("foods", "Foods to ask everyone about", choices = setNames(names(labels), labels), selected = names(labels))
  })
  draft_definition <- reactive({
    req(input$person, input$clinical, !is.null(input$start), !is.null(input$end))
    list(person = input$person, place = "Community hall", start = input$start, end = input$end,
      clinical = input$clinical, lab = isTRUE(input$lab), reason = input$definition_reason)
  })
  output$definition_summary <- renderUI({
    d <- draft_definition()
    symptoms <- switch(d$clinical, standard = "diarrhea or at least two of cramps, fever, and vomiting", diarrhea = "diarrhea", any = "any of diarrhea, cramps, fever, or vomiting")
    div(class = "definition-card", eyebrow("YOUR WORKING DEFINITION"),
      p(paste(if (d$person == "all") "Any community potluck attendee" else "An RSVP guest at the community potluck",
        "whose illness began", d$start, "to", d$end, "hours after the meal, with", paste0(symptoms, if (d$lab) " and a positive stool result." else "."))))
  })
  output$case_preview <- renderUI({
    d <- draft_definition()
    if (d$start >= d$end) return(div(class = "note", "The end of the window must follow the start."))
    counts <- table(factor(classify_cases(s()$collected, d), levels = c("Case", "Non-case", "Unknown")))
    div(class = "preview", eyebrow("LIVE PREVIEW · CURRENTLY COLLECTED RECORDS"),
      paste(paste(names(counts), counts, sep = ": "), collapse = " · "))
  })
  observeEvent(input$save_definition, {
    saved <- run(function(state) {
    d <- draft_definition()
    if (!is.finite(d$start) || !is.finite(d$end) || d$start < 0 || d$start >= d$end) stop("Enter a valid onset window with an end after its start.")
    if (!nzchar(trimws(d$reason))) stop("Explain the reason for this definition.")
    state$definition <- d
    state$case_defs <- append(state$case_defs, list(c(d, list(time = state$clock))))
    record_event(state, "case_definition", d)
    }, NULL)
    if (saved) go_step(6L)
  })
  output$list_ready <- reactive("guest_list" %in% s()$completed)
  output$team_sent <- reactive(!repeat_plan() && "team_interviews" %in% c(s()$completed, vapply(s()$queue, `[[`, character(1), "id")))
  outputOptions(output, "list_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "team_sent", suspendWhenHidden = FALSE)
  output$list_help <- renderUI({
    queued <- any(vapply(s()$queue, function(job) job$id == "guest_list", logical(1)))
    tagList(p(if (queued) "Pat is preparing the list. Receive it before choosing who to contact." else "First, get Pat’s RSVP list so your team knows who to contact."),
      next_button("prepare_list", if (queued) "Wait for Pat’s list →" else "Request Pat’s list →"))
  })
  observeEvent(input$prepare_list, run(function(state) {
    queued <- Filter(function(job) job$id == "guest_list", state$queue)
    if (length(queued)) advance_time(state, queued[[1]]$due - state$clock) else request_task(state, "guest_list")
  }, "Guest-list task updated."))
  output$collection_status <- renderUI({
    job <- Filter(function(job) job$id == "team_interviews", s()$queue)
    if (length(job)) div(class = "note", strong("Your team is on it."), p(paste("Results are due", game_time(job[[1]]$due), ". Continue when you’re ready to move the game clock forward. You can still interview or take action first.")))
    else div(class = "note", strong("Your results are ready."), p(paste(nrow(s()$collected), "guest records are ready to explore.")))
  })
  observeEvent(input$collection_done, {
    ready <- run(function(state) {
      job <- Filter(function(job) job$id == "team_interviews", state$queue)
      if (length(job)) state <- advance_time(state, job[[1]]$due - state$clock)
      if (!"team_interviews" %in% state$completed) stop("Send your collection plan first.")
      state
    }, NULL)
    if (ready) go_step(7L)
  })
  output$analysis_summary <- renderUI(div(class = "note", paste(nrow(s()$collected), "guest records collected."),
    " The download includes only what you or your team asked about."))
  output$plan_preview <- renderUI({
    req(input$audience)
    n <- length(plan_people(s(), input$audience))
    div(class = "preview", eyebrow("PLAN PREVIEW"), strong(paste(n, "guests reachable")),
      p(if (input$audience == "ill") "Only guests who have reported illness. This selection cannot provide a valid full-cohort comparison." else "All available contacts, whether ill or well. Ask about walk-ins to complete coverage."),
      p(paste(length(input$foods), "food exposures selected. Unasked foods will remain unknown.")))
  })
  observeEvent(input$repeat_collection, {
    if (!nzchar(trimws(input$repeat_reason))) {showNotification("Explain what you need to collect next.", type = "warning"); return()}
    if (any(vapply(s()$queue, function(x) x$id == "team_interviews", logical(1)))) {showNotification("Wait for the current collection first.", type = "warning"); return()}
    repeat_plan(TRUE)
  })
  observeEvent(input$send_team, {
    saved <- run(function(state) {
      if (repeat_plan()) state <- record_event(state, "repeat_collection_reason", input$repeat_reason)
      request_task(state, "team_interviews", input$audience, input$domains, input$foods)
    })
    if (saved) repeat_plan(FALSE)
  })
  observeEvent(input$request_task, run(function(state) request_task(state, input$task)))
  output$queue <- renderUI({
    state <- s()
    tagList(if (!length(state$queue)) p(class = "empty-state", "No pending results."), lapply(state$queue, function(job)
      div(class = "queue-entry", strong(job$label), span(paste("Due", game_time(job$due))))),
      if (length(state$completed)) p(class = "small-note", paste(length(state$completed), "tasks completed")))
  })
  observeEvent(input$wait, run(function(state) {
    pending <- Filter(function(e) !e$id %in% state$fired_events, state$sc$events)
    times <- c(vapply(state$queue, `[[`, numeric(1), "due"), vapply(pending, `[[`, numeric(1), "at_hour"))
    if (!length(times)) stop("No pending results or scheduled reports.")
    advance_time(state, min(times) - state$clock)
  }))
  observeEvent(input$take_action, run(function(state) take_action(state, input$action, input$action_reason)))
  output$action_feedback <- renderUI({
    actions <- s()$actions
    if (!length(actions)) return(NULL)
    last <- tail(actions, 1)[[1]]
    div(class = "note", if (last$justified) "Recorded. This action meets the scenario's current evidence condition; discuss your reasoning in the debrief." else "Recorded for reflection. This action does not meet the current evidence condition. Exposure ended Saturday; consider a more proportionate response.")
  })
  output$line_list <- renderTable(head(line_list(s()), 12), na = "—", striped = TRUE, digits = 1)
  freeze_snapshot <- function() {
    state <- isolate(s())
    if (!nrow(state$collected)) stop("Collect at least one guest record first.")
    snap <- list(id = length(state$downloads) + 1L, time = state$clock, definition = state$definition, data = line_list(state))
    state$downloads <- append(state$downloads, list(snap))
    s(record_event(state, "analysis_snapshot", list(id = snap$id, time = snap$time)))
    snapshot(snap)
    feedback(NULL)
    strata_result(NULL); strata_snapshot(NULL)
    snap
  }
  observeEvent(input$freeze, tryCatch({freeze_snapshot(); showNotification("Analysis snapshot frozen. Downloads and calculation checks use this exact version.")}, error = function(e) showNotification(conditionMessage(e), type = "warning")))
  get_snapshot <- function() {
    if (is.null(isolate(snapshot()))) freeze_snapshot() else isolate(snapshot())
  }
  output$snapshot_info <- renderUI({
    snap <- snapshot()
    if (is.null(snap)) return(p(class = "small-note", "Downloads freeze your first snapshot automatically. The preview below shows up to 12 current records."))
    div(class = "note", paste("Downloads use snapshot", snap$id, "·", nrow(snap$data), "records ·", game_time(snap$time), ". Freeze again to include later collection or definition changes. Preview below is current."))
  })
  output$csv <- downloadHandler("potluck-line-list.csv", function(file) write.csv(get_snapshot()$data, file, row.names = FALSE, na = ""))
  output$xlsx <- downloadHandler("potluck-analysis.xlsx", function(file) write_workbook(get_snapshot()$data, file, isolate(s())$sc))
  output$r_script <- downloadHandler("starter-analysis.R", function(file) file.copy("starter/analysis.R", file))
  observeEvent(input$check, {
    tryCatch({
      if (is.null(input$analysis_food) || !nzchar(input$analysis_food)) stop("Collect exposure histories or obtain the menu first.")
      snap <- get_snapshot()
      rr <- suppressWarnings(as.numeric(input$rr))
      if (is.na(rr) || rr < 0) stop("Enter a non-negative relative risk or Inf.")
      result <- check_calculation(snap$data, input$analysis_food, input$ar_e, input$ar_u, rr)
      feedback(result$message)
      state <- s()
      state$checks <- append(state$checks, list(list(snapshot = snap$id, food = input$analysis_food, result = result)))
      s(state)
    }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
  })
  output$calculation_feedback <- renderUI(if (!is.null(feedback())) div(class = "note", role = "status", feedback()))
  output$rates <- renderTable({
    req(input$show_rates)
    snap <- snapshot()
    data <- if (is.null(snap)) line_list(s()) else snap$data
    foods <- names(food_labels(s()$sc))
    if (!s()$menu) foods <- intersect(foods, names(data))
    req(length(foods))
    attack_rates(data, foods)
  }, digits = 3, na = "Not estimable")
  output$epi_curve <- renderPlot({
    data <- line_list(s())
    if (!"onset_hours" %in% names(data)) return(invisible(NULL))
    onset <- data$onset_hours[data$case_status == "Case" & !is.na(data$onset_hours)]
    if (!length(onset)) return(invisible(NULL))
    par(mar = c(4, 4, 2, 1), bg = "#ffffff", col.axis = "#203d38")
    hist(onset, breaks = seq(0, max(96, ceiling(max(onset) / 12) * 12 + 12), 12),
      col = "#418675", border = "white", main = "Onset, not report time", xlab = "Hours after Saturday's meal", ylab = "Collected cases")
  })
  observeEvent(input$checkpoint, {
    index <- guide_step()
    if (steps$page[index] != "decision") return()
    saved <- run(function(state) save_checkpoint(state, steps$checkpoint[index],
      input$suspect, input$confidence, input$supporting, input$against, input$change, input$action_now), NULL)
    if (saved) go_step(index + 1L)
  })
  output$checkpoint_history <- renderUI(tagList(lapply(s()$checkpoints, function(cp) div(class = "queue-entry", strong(cp$stage), span(paste(game_time(cp$time), "·", cp$suspect, "·", cp$confidence))))))
  observeEvent(input$finish, {
    if (!nzchar(trimws(input$recommendation))) {showNotification("Write a recommendation including uncertainty first.", type = "warning"); return()}
    state <- s()
    if (length(unique(vapply(state$checkpoints, `[[`, character(1), "stage"))) < 3) {
      showNotification("Save all three checkpoint stages before the debrief.", type = "warning"); return()
    }
    state$recommendation <- input$recommendation
    state <- record_event(state, "recommendation", input$recommendation)
    s(state)
    go_step(10L)
  })
  debrief_ready <- reactiveVal(FALSE)
  observe(debrief_ready(!is.null(s()$recommendation)))
  output$debrief <- renderUI({
    debrief_generation()
    if (!debrief_ready()) return(panel("Pause, then reflect", p("Complete your investigation and recommendation first.")))
    # Keep the form mounted when optional actions or the reveal update state.
    state <- isolate(s())
    tagList(panel("Try one decision again", p(class = "step-intro", "Look back at one choice. What would you do differently now? Take about five minutes."),
        selectInput("retry_checkpoint", "Decision to revisit", choices = setNames(seq_along(state$checkpoints), vapply(state$checkpoints, function(cp) paste(cp$stage, game_time(cp$time)), character(1)))),
        uiOutput("decision_review"), textAreaInput("retry_reason", "Why did you choose that approach then?", rows = 2),
        textAreaInput("retry_text", "What would you do next, and what is still uncertain?", rows = 3), next_button("save_retry", "Save my reflection"), uiOutput("retry_saved"),
      disclosure("Review my learning profile", p("A profile, not a score. Your reasoning matters more than naming the simulated source."), uiOutput("learning_profile")),
      disclosure("Read my recommendation", textOutput("final_recommendation")),
      disclosure("Reveal what happened", uiOutput("truth_reveal"))))
  })
  output$final_recommendation <- renderText(s()$recommendation)
  output$learning_profile <- renderUI({
    profile <- assessment_profile(s())
    tagList(p(profile$caveat), lapply(seq_len(nrow(profile$criteria)), function(i) {
      row <- profile$criteria[i, ]
      div(class = "profile-row", h4(row$label), strong(row$status), p(row$feedback), p(class = "small-note", row$evidence))
    }), disclosure("Facilitator review", p("A local review note, not an authenticated grade. Overrides remain in the session audit record."), selectInput("review_criterion", "Criterion", choices = setNames(profile$criteria$id, profile$criteria$label)), selectInput("review_status", "Judgment", c("Evidence recorded", "Needs attention", "Instructor review")), textAreaInput("review_reason", "Reason for this judgment"), actionButton("review_save", "Save facilitator judgment")))
  })
  output$truth_reveal <- renderUI({
    state <- s()
    if (!state$revealed) return(tagList(p("Compare your reasoning with the simulated facts. This is not a pass or fail."), actionButton("reveal", "Reveal scenario facts")))
    tagList(p(paste("Simulated vehicle:", food_labels(state$sc)[state$sc$truth$vehicle])), p(paste("Pathogen:", state$sc$pathogen$name)),
      p(state$sc$truth$process_failure), p("Recall errors and incomplete collection can change the observed association. Slow cooling alone does not prove cause. Later reports concern earlier onsets; the scenario does not model ongoing exposure or cases prevented."))
  })
  output$decision_review <- renderUI({
    req(input$retry_checkpoint)
    cp <- s()$checkpoints[[as.integer(input$retry_checkpoint)]]
    div(class = "preview", h4(paste("Then:", cp$suspect, "·", cp$confidence)), p(cp$action),
      disclosure("See the evidence you had then", tags$ul(lapply(cp$evidence, function(e) tags$li(paste(e$source, "—", e$finding))))),
      disclosure("Consider another approach", p("Keep alternative sources open, seek comparable histories from well guests, and take low-burden protective steps while uncertainty remains. Which of these fits the evidence you had?")))
  })
  observeEvent(input$save_retry, {
    if (!nzchar(trimws(input$retry_text)) || !nzchar(trimws(input$retry_reason))) {showNotification("Explain your original reasoning and the revision.", type = "warning"); return()}
    state <- s()
    state$retry <- list(checkpoint = as.integer(input$retry_checkpoint), reason = input$retry_reason, revision = input$retry_text)
    s(record_event(state, "retry", state$retry))
  })
  output$retry_saved <- renderUI(if (!is.null(s()$retry)) div(class = "note", "Retry saved. Export your session record for facilitator review."))
  observeEvent(input$reveal, {state <- s(); state$revealed <- TRUE; s(state)})
  observeEvent(input$review_save, run(function(state) instructor_override(state, input$review_criterion, input$review_status, input$review_reason)))
  observe({
    state <- s()
    events <- state$media_events
    ids <- vapply(events, `[[`, character(1), "id")
    updateSelectInput(session, "media_id", choices = setNames(ids, vapply(events, `[[`, character(1), "source")), selected = intersect(isolate(input$media_id), ids))
    updateCheckboxGroupInput(session, "media_evidence", choices = setNames(seq_along(state$evidence), vapply(state$evidence, `[[`, character(1), "finding")), selected = intersect(isolate(input$media_evidence), as.character(seq_along(state$evidence))))
  })
  output$media_events <- renderUI({
    events <- s()$media_events
    if (!length(events)) return(p("No media requests yet. Requests arrive as game time advances."))
    tagList(lapply(events, function(e) div(class = "note", strong(e$source), p(e$text))), lapply(s()$media_responses, function(e) div(class = "evidence-entry", strong("Your response"), p(e$text))))
  })
  observeEvent(input$respond_media, run(function(state) submit_media_response(state, input$media_id, input$media_text, as.integer(input$media_evidence))))
  observeEvent(input$show_strata, {
    tryCatch({
      snap <- get_snapshot()
      strata_result(stratified_rates(snap$data, input$strata_exposure, input$strata_food))
      strata_snapshot(snap$id)
    }, error = function(e) {strata_result(NULL); strata_snapshot(NULL); showNotification(conditionMessage(e), type = "warning")})
  })
  output$strata_info <- renderUI(if (!is.null(strata_snapshot())) p(class = "small-note", paste("Comparison uses frozen snapshot", strata_snapshot(), ".")))
  output$strata_table <- renderTable(strata_result(), digits = 3, na = "Not estimable")
  export_guide <- function() list(step = isolate(guide_step()), furthest = isolate(furthest_step()), drafts = isolate(checkpoint_drafts()), current_fields = setNames(lapply(checkpoint_fields(), function(field) isolate(input[[field]])), checkpoint_fields()), recommendation_draft = isolate(input$recommendation))
  output$resume_record <- downloadHandler("fieldnotes-resume.json", function(file) save_session(isolate(s()), file, export_guide()))
  restore_upload <- function(upload) {
    req(upload$datapath)
    tryCatch({
      restored <- restore_session(upload$datapath)
      g <- validate_guide_restore(restored$guide, nrow(steps))
      dialogue_cancel(dialogue); dialogue_busy(FALSE)
      s(restored$state)
      snapshot(if (length(restored$state$downloads)) tail(restored$state$downloads, 1L)[[1]] else NULL)
      feedback(NULL); repeat_plan(FALSE)
      strata_result(NULL); strata_snapshot(NULL)
      debrief_generation(isolate(debrief_generation()) + 1L)
      index <- if (is.null(g$step)) 1L else as.integer(g$step)
      if (length(index) != 1L || is.na(index) || index < 1L || index > nrow(steps)) index <- 1L
      furthest <- if (is.null(g$furthest)) index else max(index, min(nrow(steps), as.integer(g$furthest)))
      guide_step(index); furthest_step(furthest); checkpoint_drafts(if (is.null(g$drafts)) list() else g$drafts)
      updateTabsetPanel(session, "stage", selected = steps$page[index])
      for (field in checkpoint_fields()) if (!is.null(g$current_fields[[field]])) {
        if (field == "confidence") updateRadioButtons(session, field, selected = g$current_fields[[field]]) else if (field == "suspect") updateTextInput(session, field, value = g$current_fields[[field]]) else updateTextAreaInput(session, field, value = g$current_fields[[field]])
      }
      if (!is.null(g$recommendation_draft)) updateTextAreaInput(session, "recommendation", value = g$recommendation_draft)
      d <- restored$state$definition
      updateSelectInput(session, "person", selected = d$person); updateSelectInput(session, "clinical", selected = d$clinical)
      updateNumericInput(session, "start", value = d$start); updateNumericInput(session, "end", value = d$end)
      updateCheckboxInput(session, "lab", value = d$lab); updateTextInput(session, "definition_reason", value = d$reason)
      showNotification("Session restored. Continue where you left off.")
    }, error = function(e) showNotification(conditionMessage(e), type = "warning"))
  }
  observeEvent(input$resume_upload, restore_upload(input$resume_upload))
  observeEvent(input$welcome_resume, restore_upload(input$welcome_resume))
  output$session_record <- downloadHandler("investigation-record.json", function(file) {
    state <- isolate(s())
    # Exclude all hidden records, even after reveal. This is a learner evidence export.
    record <- state[c("clock", "evidence", "log", "chats", "checkpoints", "actions", "case_defs", "downloads", "checks", "recommendation", "retry", "media_events", "media_responses", "instructor_reviews")]
    record$guide <- list(step = isolate(guide_step()), drafts = isolate(checkpoint_drafts()),
      current_fields = setNames(lapply(checkpoint_fields(), function(field) isolate(input[[field]])), checkpoint_fields()),
      recommendation_draft = isolate(input$recommendation), retry_draft = isolate(input$retry_text))
    jsonlite::write_json(record, file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  })
}
