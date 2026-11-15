# Always-available tools: the clock, notebook, delegated tasks, actions, and media.
server_tools <- function(input, output, session, app) {
  s <- app$s
  ids <- function(items) vapply(items, `[[`, character(1), "id")

  output$clock <- renderText(game_time(s()$clock))
  output$evidence <- renderUI(tagList(lapply(rev(s()$evidence), function(e) {
    div(class = "evidence-entry", div(class = "evidence-meta", game_time(e$time), " / ", e$source), p(e$finding))
  })))
  output$pending_badge <- renderUI(if (length(s()$queue)) span(class = "pending-badge", length(s()$queue)))
  output$media_badge <- renderUI({
    pending <- setdiff(ids(s()$media_events), ids(s()$media_responses))
    if (length(pending)) {
      span(class = "pending-badge", `aria-label` = "Media requests awaiting a response", length(pending))
    }
  })

  observe({
    sc <- s()$sc
    tasks <- Filter(function(x) x$id != "team_interviews", sc$tasks)
    task_labels <- vapply(tasks, function(t) {
      paste0(t$label, " · ", t$effort_min, " min + ", t$turnaround_hours, " h")
    }, character(1))
    updateSelectInput(session, "task", choices = setNames(ids(tasks), task_labels))
    action_labels <- vapply(sc$actions, `[[`, character(1), "label")
    updateSelectInput(session, "action", choices = setNames(ids(sc$actions), action_labels))
  })
  observeEvent(input$request_task, app$run(function(state) request_task(state, input$task)))
  output$queue <- renderUI({
    state <- s()
    tagList(
      if (!length(state$queue)) p(class = "empty-state", "No pending results."),
      lapply(state$queue, function(job) {
        div(class = "queue-entry", strong(job$label), span(paste("Due", game_time(job$due))))
      }),
      if (length(state$completed)) p(class = "small-note", paste(length(state$completed), "tasks completed"))
    )
  })
  observeEvent(input$wait, app$run(function(state) {
    pending <- Filter(function(e) !e$id %in% state$fired_events, state$sc$events)
    times <- c(vapply(state$queue, `[[`, numeric(1), "due"), vapply(pending, `[[`, numeric(1), "at_hour"))
    if (!length(times)) stop("No pending results or scheduled reports.")
    advance_time(state, min(times) - state$clock)
  }))

  observeEvent(input$take_action, app$run(function(state) take_action(state, input$action, input$action_reason)))
  output$action_feedback <- renderUI({
    actions <- s()$actions
    if (!length(actions)) return(NULL)
    last <- tail(actions, 1)[[1]]
    if (last$justified) {
      return(div(class = "note", paste(
        "Recorded. This action meets the scenario's current evidence condition;",
        "discuss your reasoning in the debrief."
      )))
    }
    condition <- Filter(function(a) identical(a$id, last$id), s()$sc$actions)[[1]]$justified_if
    div(class = "note", paste(
      "Recorded for reflection. This action does not meet its evidence condition yet.",
      action_condition_note(condition), "Consider what would justify it, or a more proportionate response."
    ))
  })

  observe({
    state <- s()
    events <- state$media_events
    sources <- vapply(events, `[[`, character(1), "source")
    updateSelectInput(
      session, "media_id", choices = setNames(ids(events), sources),
      selected = intersect(isolate(input$media_id), ids(events))
    )
    findings <- vapply(state$evidence, `[[`, character(1), "finding")
    updateCheckboxGroupInput(
      session, "media_evidence", choices = setNames(seq_along(state$evidence), findings),
      selected = intersect(isolate(input$media_evidence), as.character(seq_along(state$evidence)))
    )
  })
  output$media_events <- renderUI({
    events <- s()$media_events
    if (!length(events)) return(p("No media requests yet. Requests arrive as game time advances."))
    tagList(
      lapply(events, function(e) div(class = "note", strong(e$source), p(e$text))),
      lapply(s()$media_responses, function(e) div(class = "evidence-entry", strong("Your response"), p(e$text)))
    )
  })
  observeEvent(input$respond_media, app$run(function(state) {
    submit_media_response(state, input$media_id, input$media_text, as.integer(input$media_evidence))
  }))
}
