# Interviews: the contact list, the conversation, and optional assisted wording.
server_interview <- function(input, output, session, app) {
  s <- app$s
  dialogue <- app$dialogue
  dialogue_busy <- app$dialogue_busy
  dialogue_note <- app$dialogue_note
  widget <- chat_widget_enabled()
  provider <- function() Sys.getenv("FIELDNOTES_DIALOGUE_PROVIDER", "scripted")

  observe(session$sendCustomMessage("dialogue-busy", list(busy = dialogue_busy())))
  session$onSessionEnded(function() dialogue_cancel(dialogue))

  observe({
    choices <- available_characters(s())
    selected <- isolate(input$character)
    updateSelectInput(
      session, "character", choices = choices,
      selected = if (!is.null(selected) && selected %in% choices) selected else "organizer"
    )
  })
  output$chat <- renderUI({
    req(input$character)
    history <- s()$chats[[input$character]]
    if (!length(history)) {
      return(div(class = "chat-empty", p("Start with an open question, then follow up on what you hear.")))
    }
    div(class = "chat-history", role = "log", `aria-live` = "polite", lapply(history, function(x) {
      tagList(
        div(class = "chat-question", eyebrow(paste("YOU /", game_time(x$time))), x$question),
        div(class = "chat-answer", x$reply)
      )
    }))
  })
  observeEvent(input$character, {
    updateSelectInput(session, "topic", choices = topic_choices(dialogue_topics(input$character)), selected = "auto")
  })

  chat_notice <- function(text) {
    if (widget) shinychat::chat_append("conversation", tags$p(text), session = session)
  }
  reset_question <- function(question, topic, character) {
    if (!identical(isolate(input$character), character)) return(invisible(NULL))
    if (identical(isolate(input$topic), topic)) updateSelectInput(session, "topic", selected = "auto")
    if (!widget && identical(isolate(input$question), question)) updateTextAreaInput(session, "question", value = "")
  }
  ask_question <- function(question) {
    if (dialogue_busy()) {
      dialogue_note("Please wait for the current reply.")
      return(FALSE)
    }
    if (!is.character(question) || length(question) != 1L || is.na(question) || nchar(question) > 1200L) {
      app$notify("Enter a question of up to 1200 characters.")
      return(FALSE)
    }
    if (is.null(input$character) || !input$character %in% available_characters(s())) return(FALSE)
    topic <- input$topic
    if (is.null(topic) || !topic %in% c("auto", dialogue_topics(input$character))) topic <- "auto"
    if (provider() == "scripted") {
      ok <- app$run(function(state) interview(state, input$character, question, topic), "Reply added to your notebook.")
      if (ok) reset_question(question, topic, input$character)
      return(ok)
    }
    ask_assisted(question, topic, input$character)
  }
  ask_assisted <- function(question, topic, contact) {
    before <- isolate(s())
    dialogue_busy(TRUE)
    dialogue_note("Preparing a reply… You can keep reading your notebook.")
    history <- dialogue_context(before, contact)
    reply_builder <- function(topics) {
      candidate <- interview(before, contact, question, topics)
      tail(candidate$chats[[contact]], 1L)[[1L]]$reply
    }
    promise <- tryCatch(
      dialogue_request_async(dialogue, contact, question, topic, history = history, reply_builder = reply_builder),
      error = function(e) {
        dialogue_busy(FALSE)
        dialogue_note(conditionMessage(e))
        NULL
      }
    )
    if (is.null(promise)) return(FALSE)
    request_token <- dialogue$token
    changed <- "Your investigation changed while the reply was preparing. Ask again to use the current evidence."
    promises::then(promise, onFulfilled = function(result) {
      if (!dialogue_result_current(dialogue, result)) return(NULL)
      dialogue_busy(FALSE)
      if (!identical(isolate(s()), before)) {
        dialogue_note(changed)
        chat_notice(changed)
        return(NULL)
      }
      if (!app$run(function(state) dialogue_apply(state, result), NULL)) return(NULL)
      reset_question(result$question, topic, result$character)
      provider_name <- if (identical(provider(), "gemini")) "Gemini" else "Provider"
      dialogue_note(switch(result$status,
        assisted = paste0("Reply added · ", provider_name, "-assisted reply"),
        limited = "Reply added · scenario answer (assistance limit reached)",
        timeout = "Reply added · scenario answer (assistance took too long)",
        scripted = "Reply added to your notebook.",
        "Reply added · scenario answer"
      ))
      NULL
    }, onRejected = function(error) {
      if (!identical(dialogue$token, request_token)) return(NULL)
      dialogue_busy(FALSE)
      dialogue_note("Reply unavailable. Please try again.")
      chat_notice("Reply unavailable. Please try again.")
      NULL
    })
  }
  observeEvent(input$ask, ask_question(if (widget) "" else input$question))
  observeEvent(input$conversation_user_input, {
    handled <- ask_question(dialogue_input_text(input$conversation_user_input))
    if (identical(handled, FALSE)) {
      chat_notice(if (dialogue_busy()) {
        "Please wait for the current reply."
      } else {
        "Choose a contact and enter a text question of up to 1,200 characters, or use the topic menu."
      })
    }
  })
  observeEvent(list(input$character, s()$chats), {
    if (!widget) return()
    shinychat::chat_clear("conversation", session = session)
    for (entry in s()$chats[[input$character]]) {
      shinychat::chat_append("conversation", tags$p(entry$question), role = "user", session = session)
      shinychat::chat_append("conversation", tags$p(entry$reply), session = session)
    }
  }, ignoreInit = TRUE)

  output$dialogue_status <- renderUI({
    if (!is.null(dialogue_note())) p(class = "small-note", role = "status", dialogue_note())
  })
  output$dialogue_help <- renderUI({
    if (provider() == "scripted") {
      return(p(class = "small-note", "Ask naturally, or choose a topic below for a focused question."))
    }
    name <- if (provider() == "gemini") "Gemini" else "Dialogue assistance"
    p(class = "small-note", paste0(
      name, " helps with natural replies and follow-up questions. Each person answers from their own knowledge."
    ))
  })
  output$interview_progress <- renderUI({
    plan <- s()$sc$interviews
    advice <- if (is.null(plan$suggested)) {
      "Continue when ready."
    } else {
      paste0("Aim for about ", plan$suggested, ", then continue when ready.")
    }
    p(class = "interview-count", paste0(length(s()$interviewed), " of ", plan$max, " contacts used · ", advice))
  })
}
