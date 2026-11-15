# Saving, resuming, and exporting a session.
server_session <- function(input, output, session, app) {
  s <- app$s
  steps <- app$steps
  export_guide <- function() {
    list(
      step = isolate(app$guide_step()), furthest = isolate(app$furthest_step()),
      drafts = isolate(app$checkpoint_drafts()), current_fields = app$current_fields(input),
      recommendation_draft = isolate(input$recommendation)
    )
  }
  output$resume_record <- downloadHandler("fieldnotes-resume.json", function(file) {
    save_session(isolate(s()), file, export_guide())
  })

  restore_inputs <- function(g, definition) {
    for (field in checkpoint_fields()) {
      value <- g$current_fields[[field]]
      if (is.null(value)) next
      switch(field,
        confidence = updateRadioButtons(session, field, selected = value),
        suspect = updateTextInput(session, field, value = value),
        updateTextAreaInput(session, field, value = value)
      )
    }
    if (!is.null(g$recommendation_draft)) updateTextAreaInput(session, "recommendation", value = g$recommendation_draft)
    updateSelectInput(session, "person", selected = definition$person)
    updateSelectInput(session, "clinical", selected = definition$clinical)
    updateNumericInput(session, "start", value = definition$start)
    updateNumericInput(session, "end", value = definition$end)
    updateCheckboxInput(session, "lab", value = definition$lab)
    updateTextInput(session, "definition_reason", value = definition$reason)
  }
  app$restore_upload <- function(upload) {
    req(upload$datapath)
    tryCatch({
      restored <- restore_session(upload$datapath)
      g <- validate_guide(restored$guide, steps)
      dialogue_cancel(app$dialogue)
      app$dialogue_busy(FALSE)
      s(restored$state)
      downloads <- restored$state$downloads
      app$snapshot(if (length(downloads)) tail(downloads, 1L)[[1]] else NULL)
      app$feedback(NULL)
      app$repeat_plan(FALSE)
      app$strata_result(NULL)
      app$strata_pooled(NULL)
      app$strata_snapshot(NULL)
      app$debrief_generation(isolate(app$debrief_generation()) + 1L)
      app$guide_step(g$step)
      app$furthest_step(g$furthest)
      app$checkpoint_drafts(g$drafts)
      updateTabsetPanel(session, "stage", selected = steps$page[g$step])
      restore_inputs(g, restored$state$definition)
      showNotification("Session restored. Continue where you left off.")
    }, error = function(e) app$notify(conditionMessage(e)))
  }
  observeEvent(input$resume_upload, app$restore_upload(input$resume_upload))
  observeEvent(input$welcome_resume, app$restore_upload(input$welcome_resume))

  output$session_record <- downloadHandler("investigation-record.json", function(file) {
    state <- isolate(s())
    # A learner evidence export: only what the learner saw or wrote, never the hidden records.
    fields <- c(
      "clock", "evidence", "log", "chats", "checkpoints", "actions", "case_defs", "downloads", "checks",
      "recommendation", "retry", "media_events", "media_responses", "instructor_reviews"
    )
    record <- state[fields]
    record$guide <- list(
      step = isolate(app$guide_step()), drafts = isolate(app$checkpoint_drafts()),
      current_fields = app$current_fields(input), recommendation_draft = isolate(input$recommendation),
      retry_draft = isolate(input$retry_text)
    )
    jsonlite::write_json(record, file, pretty = TRUE, auto_unbox = TRUE, na = "null")
  })
}
