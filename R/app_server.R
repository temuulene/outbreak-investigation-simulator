# The server is assembled from one function per part of the journey. They share
# one context: the investigation state, navigation, and a few helpers. Input
# ids are not namespaced, so saved sessions and the page stay unchanged.
new_app_context <- function(session) {
  app <- new.env(parent = emptyenv())
  app$steps <- guide_steps()
  app$s <- reactiveVal(start_session())
  app$dialogue <- dialogue_session()
  app$dialogue_busy <- reactiveVal(FALSE)
  app$dialogue_note <- reactiveVal(NULL)
  app$repeat_plan <- reactiveVal(FALSE)
  app$debrief_generation <- reactiveVal(0L)
  app$snapshot <- reactiveVal(NULL)
  app$feedback <- reactiveVal(NULL)
  app$strata_result <- reactiveVal(NULL)
  app$strata_pooled <- reactiveVal(NULL)
  app$strata_snapshot <- reactiveVal(NULL)
  app$guide_step <- reactiveVal(1L)
  app$furthest_step <- reactiveVal(1L)
  app$checkpoint_drafts <- reactiveVal(list())
  # Apply a state change; show its error as a notice instead of stopping the session.
  app$run <- function(operation, message = "Saved.") {
    tryCatch({
      app$s(operation(isolate(app$s())))
      if (!is.null(message)) showNotification(message, type = "message", duration = 3)
      TRUE
    }, error = function(e) {
      showNotification(conditionMessage(e), type = "warning", duration = 6)
      FALSE
    })
  }
  app$notify <- function(text) showNotification(text, type = "warning")
  app$focus_step <- function() session$sendCustomMessage("guide-focus", list())
  app$current_fields <- function(input) {
    setNames(lapply(checkpoint_fields(), function(field) isolate(input[[field]])), checkpoint_fields())
  }
  app
}

app_server <- function(input, output, session) {
  app <- new_app_context(session)
  server_guide(input, output, session, app)
  server_interview(input, output, session, app)
  server_plan(input, output, session, app)
  server_tools(input, output, session, app)
  server_analysis(input, output, session, app)
  server_debrief(input, output, session, app)
  server_session(input, output, session, app)

  # Short names for the shared state, used by the server tests.
  s <- app$s
  guide_step <- app$guide_step
  furthest_step <- app$furthest_step
  checkpoint_drafts <- app$checkpoint_drafts
  snapshot <- app$snapshot
  strata_result <- app$strata_result
  strata_pooled <- app$strata_pooled
  strata_snapshot <- app$strata_snapshot
  repeat_plan <- app$repeat_plan
  dialogue <- app$dialogue
  dialogue_busy <- app$dialogue_busy
  dialogue_note <- app$dialogue_note
  freeze_snapshot <- app$freeze_snapshot
  restore_upload <- app$restore_upload
}
