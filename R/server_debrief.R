# The recommendation, the retry exercise, the learning profile, and the reveal.
server_debrief <- function(input, output, session, app) {
  s <- app$s

  observeEvent(input$finish, {
    ok <- tryCatch({
      check_text(input$recommendation, "Write a recommendation including uncertainty first.")
      TRUE
    }, error = function(e) {
      app$notify(conditionMessage(e))
      FALSE
    })
    if (!ok) return()
    state <- s()
    stages <- unique(vapply(state$checkpoints, `[[`, character(1), "stage"))
    if (length(stages) < sum(!is.na(app$steps$checkpoint))) {
      return(app$notify("Save all three checkpoint stages before the debrief."))
    }
    state$recommendation <- input$recommendation
    s(record_event(state, "recommendation", input$recommendation))
    app$go_step(step_index("debrief", app$steps))
  })

  debrief_ready <- reactiveVal(FALSE)
  observe(debrief_ready(!is.null(s()$recommendation)))
  output$debrief <- renderUI({
    app$debrief_generation()
    if (!debrief_ready()) {
      return(panel("Pause, then reflect", p("Complete your investigation and recommendation first.")))
    }
    # Keep the form mounted when optional actions or the reveal update state.
    state <- isolate(s())
    decisions <- setNames(seq_along(state$checkpoints), vapply(state$checkpoints, function(cp) {
      paste(cp$stage, game_time(cp$time))
    }, character(1)))
    tagList(panel(
      "Try one decision again",
      p(
        class = "step-intro",
        "Look back at one choice. What would you do differently now? Take about five minutes."
      ),
      selectInput("retry_checkpoint", "Decision to revisit", choices = decisions),
      uiOutput("decision_review"),
      textAreaInput("retry_reason", "Why did you choose that approach then?", rows = 2),
      textAreaInput("retry_text", "What would you do next, and what is still uncertain?", rows = 3),
      next_button("save_retry", "Save my reflection"), uiOutput("retry_saved"),
      disclosure(
        "Review my learning profile",
        p("A profile, not a score. Your reasoning matters more than naming the simulated source."),
        uiOutput("learning_profile")
      ),
      disclosure("Read my recommendation", textOutput("final_recommendation")),
      disclosure("Reveal what happened", uiOutput("truth_reveal"))
    ))
  })
  output$final_recommendation <- renderText(s()$recommendation)

  output$learning_profile <- renderUI({
    profile <- assessment_profile(s())
    criteria <- profile$criteria
    tagList(
      p(profile$caveat),
      lapply(seq_len(nrow(criteria)), function(i) {
        row <- criteria[i, ]
        div(
          class = "profile-row", h4(row$label), strong(row$status), p(row$feedback),
          p(class = "small-note", row$evidence)
        )
      }),
      disclosure(
        "Facilitator review",
        p("A local review note, not an authenticated grade. Overrides remain in the session audit record."),
        selectInput("review_criterion", "Criterion", choices = setNames(criteria$id, criteria$label)),
        selectInput("review_status", "Judgment", c("Evidence recorded", "Needs attention", "Instructor review")),
        textAreaInput("review_reason", "Reason for this judgment"),
        actionButton("review_save", "Save facilitator judgment")
      )
    )
  })
  observeEvent(input$review_save, app$run(function(state) {
    instructor_override(state, input$review_criterion, input$review_status, input$review_reason)
  }))

  output$truth_reveal <- renderUI({
    state <- s()
    if (!state$revealed) {
      return(tagList(
        p("Compare your reasoning with the simulated facts. This is not a pass or fail."),
        actionButton("reveal", "Reveal scenario facts")
      ))
    }
    reference <- truth_summary(state)
    labels <- food_labels(state$sc)
    describe <- function(r) {
      sprintf(
        "%d of %d who ate it became ill and %d of %d who did not (relative risk %s)", r$exposed_cases,
        r$exposed_total, r$unexposed_cases, r$unexposed_total,
        if (is.finite(r$relative_risk)) sprintf("%.1f", r$relative_risk) else "not finite"
      )
    }
    tagList(
      p(paste("Simulated vehicle:", labels[state$sc$truth$vehicle])),
      p(paste("Pathogen:", state$sc$pathogen$name)),
      p(state$sc$truth$process_failure),
      p(paste0(
        "With every attendee and perfect recall: ", describe(reference$actual),
        ". With every attendee as guests remembered it: ", describe(reference$recalled),
        ". Compare both with your own estimate. On average, recall error unrelated to illness pulls an ",
        "association toward 1; in one small dataset, chance can move it either way."
      )),
      if (!is.null(reference$rumour)) {
        p(paste0(
          "The rumour about ", labels[reference$rumour$food], " changed what ill guests remembered. Truly: ",
          describe(reference$rumour$actual), ". As recalled: ", describe(reference$rumour$recalled),
          ". Recall that differs between ill and well guests can create an association from nothing."
        ))
      },
      p(paste(
        "A few guests had unrelated illness, some ill guests never phoned in, and some guests did not answer the",
        "team. A preparation failure alone does not prove cause. Later reports concern earlier onsets; the",
        "scenario does not model continuing exposure at the hall or cases prevented."
      ))
    )
  })
  observeEvent(input$reveal, {
    state <- s()
    state$revealed <- TRUE
    s(state)
  })

  output$decision_review <- renderUI({
    req(input$retry_checkpoint)
    index <- suppressWarnings(as.integer(input$retry_checkpoint))
    req(length(index) == 1L, !is.na(index), index >= 1L, index <= length(s()$checkpoints))
    cp <- s()$checkpoints[[index]]
    div(
      class = "preview", h4(paste("Then:", cp$suspect, "·", cp$confidence)), p(cp$action),
      disclosure("See the evidence you had then", tags$ul(lapply(cp$evidence, function(e) {
        tags$li(paste(e$source, "—", e$finding))
      }))),
      disclosure("Consider another approach", p(paste(
        "Keep alternative sources open, seek comparable histories from well guests, and take low-burden",
        "protective steps while uncertainty remains. Which of these fits the evidence you had?"
      )))
    )
  })
  observeEvent(input$save_retry, {
    ok <- tryCatch({
      check_text(input$retry_text, "Explain your original reasoning and the revision.")
      check_text(input$retry_reason, "Explain your original reasoning and the revision.")
      TRUE
    }, error = function(e) {
      app$notify(conditionMessage(e))
      FALSE
    })
    if (!ok) return()
    state <- s()
    state$retry <- list(
      checkpoint = as.integer(input$retry_checkpoint), reason = input$retry_reason, revision = input$retry_text
    )
    s(record_event(state, "retry", state$retry))
  })
  output$retry_saved <- renderUI({
    if (!is.null(s()$retry)) div(class = "note", "Retry saved. Export your session record for facilitator review.")
  })
}
