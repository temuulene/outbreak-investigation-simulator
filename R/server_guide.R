# Navigation between steps, the three reasoning checkpoints, and step help.
server_guide <- function(input, output, session, app) {
  steps <- app$steps
  s <- app$s
  guide_step <- app$guide_step
  furthest_step <- app$furthest_step
  checkpoint_drafts <- app$checkpoint_drafts

  app$go_step <- function(index) {
    current <- isolate(guide_step())
    drafts <- isolate(checkpoint_drafts())
    if (steps$page[current] == "decision") {
      drafts[[as.character(current)]] <- app$current_fields(input)
      checkpoint_drafts(drafts)
    }
    guide_step(index)
    furthest_step(max(isolate(furthest_step()), index))
    updateTabsetPanel(session, "stage", selected = steps$page[index])
    if (steps$page[index] == "decision") {
      draft <- drafts[[as.character(index)]]
      for (field in setdiff(checkpoint_fields(), "confidence")) {
        value <- if (is.null(draft[[field]])) "" else draft[[field]]
        if (field == "suspect") {
          updateTextInput(session, field, value = value)
        } else {
          updateTextAreaInput(session, field, value = value)
        }
      }
      updateRadioButtons(session, "confidence", selected = if (is.null(draft$confidence)) "Low" else draft$confidence)
      updateTabsetPanel(session, "reflection", selected = "suspect")
    }
    app$focus_step()
  }
  go_to <- function(id) app$go_step(step_index(id, steps))

  observe({
    visited <- seq_len(furthest_step())
    updateSelectInput(session, "revisit", choices = setNames(visited, steps$label[visited]), selected = guide_step())
  })
  observeEvent(input$go_revisit, {
    index <- suppressWarnings(as.integer(input$revisit))
    if (length(index) == 1 && !is.na(index) && index >= 1 && index <= furthest_step()) app$go_step(index)
  })
  observeEvent(input$back_step, app$go_step(max(1L, guide_step() - 1L)))

  output$guide_progress <- renderUI({
    # The welcome page is not counted; the first working step reads "Step 1".
    total <- nrow(steps) - 1L
    position <- max(1L, guide_step() - 1L)
    div(
      div(class = "step-label", span(steps$label[guide_step()]), span(paste("Step", position, "of", total))),
      div(
        class = "progress-track", role = "progressbar", `aria-label` = "Investigation steps",
        `aria-valuemin` = 1, `aria-valuemax` = total, `aria-valuenow` = position,
        div(class = "progress-fill", style = paste0("width:", position / total * 100, "%"))
      )
    )
  })

  output$checkpoint_prompt <- renderUI({
    prompt <- switch(steps$id[guide_step()],
      first_thoughts = paste(
        "Before asking questions, capture your first thoughts.",
        "You aren’t expected to know the source yet."
      ),
      interview_review = "Think back over the interviews. What has changed, and what still needs checking?",
      "Bring together the data and what people told you. How strong is the evidence now?"
    )
    p(class = "step-intro", prompt)
  })
  show_reflection <- function(tab) {
    updateTabsetPanel(session, "reflection", selected = tab)
    app$focus_step()
  }
  observeEvent(input$reflection_next, {
    if (any(!nzchar(trimws(c(input$suspect, input$supporting))))) {
      return(app$notify("Add your working idea and a short reason. ‘Still open’ is fine."))
    }
    show_reflection("uncertainty")
  })
  observeEvent(input$reflection_action, {
    if (any(!nzchar(trimws(c(input$against, input$change))))) {
      return(app$notify("Note what is missing and what might change your mind."))
    }
    show_reflection("action")
  })
  observeEvent(input$reflection_back, show_reflection("suspect"))
  observeEvent(input$reflection_back_again, show_reflection("uncertainty"))
  observeEvent(input$interviews_done, go_to("interview_review"))
  observeEvent(input$analysis_done, go_to("weigh"))

  observeEvent(input$begin, {
    if (furthest_step() == 1L) {
      id <- if (is.null(input$scenario_id)) "potluck-01" else input$scenario_id
      if (!app$run(function(state) start_session(id, isTRUE(input$randomize)), NULL)) return()
    }
    go_to("first_thoughts")
  })

  observeEvent(input$checkpoint, {
    index <- guide_step()
    if (steps$page[index] != "decision") return()
    saved <- app$run(function(state) {
      save_checkpoint(
        state, steps$checkpoint[index], input$suspect, input$confidence, input$supporting, input$against,
        input$change, input$action_now
      )
    }, NULL)
    if (saved) app$go_step(index + 1L)
  })
  output$checkpoint_history <- renderUI(tagList(lapply(s()$checkpoints, function(cp) {
    div(class = "queue-entry", strong(cp$stage), span(paste(game_time(cp$time), "·", cp$suspect, "·", cp$confidence)))
  })))

  observeEvent(input$hint, {
    advice <- switch(steps$page[guide_step()],
      decision = paste(
        "Write what you know now. ‘Still open’ and ‘not enough evidence yet’ are useful answers when you",
        "explain why. You can read the notebook without losing this draft."
      ),
      interview = paste(
        "Start with Pat: ask about the menu and who attended. Then compare an ill guest’s account with a well",
        "guest’s. The topic menu can help if a question is misunderstood."
      ),
      define = paste(
        "Use criteria your team can apply consistently. Think about people, onset time, and symptoms.",
        "A broad definition catches more true cases and more unrelated illness.",
        "Requiring lab confirmation may leave many people unclassified."
      ),
      collect = paste(
        "Ask the same exposure questions of ill and well guests. The preview shows who your team can reach.",
        "Ask Pat about walk-ins if the list seems incomplete. Not everyone will answer."
      ),
      analysis = paste(
        "Describe the cases first: symptoms, onset times, and the curve. Then, for each food, divide cases by",
        "people with known exposure and case status. Compare attack rates, and discuss uncertainty."
      ),
      recommend = paste(
        "State the suspected source, your supporting evidence, your suggested actions, and what remains",
        "uncertain. A well-supported cautious conclusion is enough."
      ),
      "Choose one earlier decision. Explain why you made it, then say what you would do differently."
    )
    showModal(modalDialog(title = "A useful next step", advice, easyClose = TRUE))
  })
}
