# The case definition and the team's collection plan.
server_plan <- function(input, output, session, app) {
  s <- app$s
  repeat_plan <- app$repeat_plan
  queued <- function(state, id) Filter(function(job) job$id == id, state$queue)

  draft_definition <- reactive({
    req(input$person, input$clinical, !is.null(input$start), !is.null(input$end))
    list(
      person = input$person, place = "Community hall", start = input$start, end = input$end,
      clinical = input$clinical, lab = isTRUE(input$lab), reason = input$definition_reason
    )
  })
  output$definition_summary <- renderUI({
    d <- draft_definition()
    validate(need(
      isTRUE(is.finite(d$start)) && isTRUE(is.finite(d$end)), "Enter the earliest and latest onset in hours."
    ))
    symptoms <- switch(d$clinical,
      standard = "diarrhea or at least two of cramps, fever, and vomiting",
      diarrhea = "diarrhea",
      any = "any of diarrhea, cramps, fever, or vomiting"
    )
    who <- if (d$person == "all") "Any community potluck attendee" else "An RSVP guest at the community potluck"
    div(class = "definition-card", eyebrow("YOUR WORKING DEFINITION"), p(paste0(
      who, " whose illness began ", d$start, " to ", d$end, " hours after the meal, with ", symptoms,
      if (d$lab) " and a positive stool result." else "."
    )))
  })
  output$case_preview <- renderUI({
    d <- draft_definition()
    if (!isTRUE(d$start < d$end)) return(div(class = "note", "The end of the window must follow the start."))
    counts <- table(factor(classify_cases(s()$collected, d), levels = case_status_levels()))
    if (d$person == "all") counts <- counts[names(counts) != "Excluded"]
    div(
      class = "preview", eyebrow("LIVE PREVIEW · CURRENTLY COLLECTED RECORDS"),
      paste(paste(names(counts), counts, sep = ": "), collapse = " · "),
      if (d$person == "rsvp") {
        p(class = "small-note", paste(
          "Excluded guests are outside this cohort.", "They count in neither attack-rate denominator."
        ))
      }
    )
  })
  observeEvent(input$save_definition, {
    saved <- app$run(function(state) {
      d <- draft_definition()
      if (!is.finite(d$start) || !is.finite(d$end) || d$start < 0 || d$start >= d$end) {
        stop("Enter a valid onset window with an end after its start.")
      }
      check_text(d$reason, "Explain the reason for this definition.")
      state$definition <- d
      state$case_defs <- append(state$case_defs, list(c(d, list(time = state$clock))))
      record_event(state, "case_definition", d)
    }, NULL)
    if (saved) app$go_step(step_index("collect", app$steps))
  })

  output$list_ready <- reactive("guest_list" %in% s()$completed)
  output$team_sent <- reactive({
    !repeat_plan() && "team_interviews" %in% c(s()$completed, vapply(s()$queue, `[[`, character(1), "id"))
  })
  outputOptions(output, "list_ready", suspendWhenHidden = FALSE)
  outputOptions(output, "team_sent", suspendWhenHidden = FALSE)
  output$list_help <- renderUI({
    waiting <- length(queued(s(), "guest_list")) > 0
    tagList(
      p(if (waiting) {
        "Pat is preparing the list. Receive it before choosing who to contact."
      } else {
        "First, get Pat’s RSVP list so your team knows who to contact."
      }),
      next_button("prepare_list", if (waiting) "Wait for Pat’s list →" else "Request Pat’s list →")
    )
  })
  observeEvent(input$prepare_list, app$run(function(state) {
    job <- queued(state, "guest_list")
    if (length(job)) advance_time(state, job[[1]]$due - state$clock) else request_task(state, "guest_list")
  }, "Guest-list task updated."))

  menu_known <- reactiveVal(FALSE)
  observe(menu_known(s()$menu))
  output$food_questions <- renderUI({
    if (!menu_known()) {
      return(div(
        class = "note", "You don’t have the menu yet. Go back to ask Pat, or send your plan without food questions."
      ))
    }
    labels <- food_labels(isolate(s())$sc)
    choices <- setNames(names(labels), labels)
    checkboxGroupInput("foods", "Foods to ask everyone about", choices = choices, selected = names(labels))
  })
  output$plan_preview <- renderUI({
    req(input$audience)
    n <- length(plan_people(s(), input$audience))
    div(
      class = "preview", eyebrow("PLAN PREVIEW"), strong(paste(n, "guests on the contact plan")),
      p(if (input$audience == "ill") {
        "Only guests who have reported illness. This selection cannot provide a valid full-cohort comparison."
      } else {
        "All available contacts, whether ill or well. Ask about walk-ins to complete coverage."
      }),
      p("Some guests will not answer. Check who responded before you interpret the results."),
      p(paste(length(input$foods), "food exposures selected. Unasked foods will remain unknown."))
    )
  })
  output$collection_status <- renderUI({
    job <- queued(s(), "team_interviews")
    if (length(job)) {
      div(class = "note", strong("Your team is on it."), p(paste0(
        "Results are due ", game_time(job[[1]]$due), ". Continue when you’re ready to move the game clock forward. ",
        "You can still interview or take action first."
      )))
    } else {
      div(
        class = "note", strong("Your results are ready."),
        p(paste(nrow(s()$collected), "guest records are ready to explore."))
      )
    }
  })
  observeEvent(input$collection_done, {
    ready <- app$run(function(state) {
      job <- queued(state, "team_interviews")
      if (length(job)) state <- advance_time(state, job[[1]]$due - state$clock)
      if (!"team_interviews" %in% state$completed) stop("Send your collection plan first.")
      state
    }, NULL)
    if (ready) app$go_step(step_index("analysis", app$steps))
  })
  observeEvent(input$repeat_collection, {
    if (!nzchar(trimws(input$repeat_reason))) return(app$notify("Explain what you need to collect next."))
    if (nchar(input$repeat_reason) > max_text_length()) return(app$notify("Keep the reason shorter."))
    if (length(queued(s(), "team_interviews"))) return(app$notify("Wait for the current collection first."))
    repeat_plan(TRUE)
  })
  observeEvent(input$send_team, {
    saved <- app$run(function(state) {
      if (repeat_plan()) state <- record_event(state, "repeat_collection_reason", input$repeat_reason)
      request_task(state, "team_interviews", input$audience, input$domains, input$foods)
    })
    if (saved) repeat_plan(FALSE)
  })
}
