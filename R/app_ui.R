panel <- function(title, ..., class = "") div(class = paste("paper-panel", class), h3(title), ...)
eyebrow <- function(text) div(class = "eyebrow", text)
disclosure <- function(label, ...) {
  tags$details(class = "disclosure", tags$summary(label), div(class = "disclosure-body", ...))
}
next_button <- function(id, label) div(class = "step-end", actionButton(id, label, class = "btn-primary"))
tool <- function(label, ..., id = NULL, badge = NULL) {
  tags$details(class = "tool", id = id, tags$summary(label, badge), div(class = "tool-body", ...))
}
chat_widget_enabled <- function() {
  identical(Sys.getenv("FIELDNOTES_CHAT_UI"), "shinychat") && requireNamespace("shinychat", quietly = TRUE)
}

topic_choices <- function(topics = NULL) {
  labels <- c(
    event = "About the event", menu = "Menu / foods eaten", food_sources = "Who brought the food",
    guest_list = "Guest list", walk_ins = "Walk-ins", leftovers = "Leftovers", preparation = "Preparation",
    storage = "Storage", symptoms = "Symptoms", onset = "When illness started", demographics = "Age and sex",
    health_care_visits = "Health care"
  )
  if (!is.null(topics)) labels <- labels[topics]
  c("Use my written question" = "auto", setNames(names(labels), labels))
}

ui_toolbox <- function() {
  div(
    class = "toolbox",
    tool(
      "Notebook", id = "notebook-tool", h3("What you know"), uiOutput("evidence"),
      disclosure("Your earlier decisions", uiOutput("checkpoint_history"))
    ),
    tool(
      "Tasks & actions", id = "actions-tool", badge = uiOutput("pending_badge", inline = TRUE),
      p("You can act at any point. Time moves only when you interview, delegate, or wait."),
      div(
        class = "tool-grid",
        div(
          h3("Ask your team"), selectInput("task", "Choose a task", choices = NULL),
          actionButton("request_task", "Request task"), uiOutput("queue"),
          actionButton("wait", "Wait for the next result", class = "quiet-button")
        ),
        div(
          h3("Recommend an action"), selectInput("action", "Choose an action", choices = NULL),
          textAreaInput("action_reason", "Why is this justified now?", rows = 2),
          actionButton("take_action", "Record action"), uiOutput("action_feedback")
        )
      )
    ),
    tool(
      "Media", badge = uiOutput("media_badge", inline = TRUE), uiOutput("media_events"),
      selectInput("media_id", "Message to respond to", choices = NULL),
      textAreaInput("media_text", "Your public response", rows = 3),
      checkboxGroupInput("media_evidence", "Evidence supporting your response", choices = NULL),
      actionButton("respond_media", "Record response")
    ),
    tool(
      "Help & session", actionButton("hint", "Help with this step"),
      p(class = "small-note", "Your work lasts for this session. Export it before closing or refreshing."),
      downloadButton("session_record", "Export my work"),
      downloadButton("resume_record", "Save resumable session"),
      fileInput("resume_upload", "Resume a saved session", accept = ".json"),
      disclosure(
        "Revisit a step", selectInput("revisit", "Where would you like to go?", choices = NULL),
        actionButton("go_revisit", "Go to this step")
      )
    )
  )
}

ui_welcome <- function() {
  catalog <- scenario_catalog()
  challenges <- setNames(catalog$id, paste(catalog$title, catalog$difficulty, sep = " · "))
  div(
    class = "welcome", eyebrow("AN OUTBREAK INVESTIGATION · ABOUT 80 MINUTES"),
    h1("A potluck. A few calls.\nWhere do you start?"),
    p(
      class = "welcome-intro",
      "Step into the role of an epidemiologist. Talk to guests, follow the evidence, and decide what to do next."
    ),
    div(
      class = "call-card",
      div(
        class = "caller", span(class = "avatar", "P"),
        div(strong("Pat, the organizer"), span("Monday morning · incoming call"))
      ),
      tags$blockquote(paste0(
        "“Several people have been sick since Saturday’s potluck. ",
        "The committee is clearing out the fridge tomorrow morning.”"
      ))
    ),
    disclosure(
      "Choose a challenge or resume", selectInput("scenario_id", "Challenge", choices = challenges),
      checkboxInput("randomize", "Use a reviewed variation", FALSE),
      p(class = "small-note", "Variations are checked for a useful teaching signal."),
      fileInput("welcome_resume", "Resume a saved session", accept = ".json")
    ),
    next_button("begin", "Start investigating →"),
    p(class = "reassurance", "We’ll guide you one step at a time. Mistakes are part of learning.")
  )
}

ui_decision <- function() {
  panel(
    "What do you think so far?", uiOutput("checkpoint_prompt"),
    tabsetPanel(
      id = "reflection", type = "hidden",
      tabPanel(
        "Suspect", value = "suspect", p(class = "question-number", "1 of 3 · Your working idea"),
        textInput("suspect", "What might be causing the illness?", placeholder = "It’s okay to say ‘still open’"),
        radioButtons("confidence", "How sure are you?", c("Low", "Medium", "High"), inline = TRUE),
        textAreaInput("supporting", "What points you in that direction?", placeholder = "A short note is enough.", rows = 2),
        next_button("reflection_next", "Continue →")
      ),
      tabPanel(
        "Uncertainty", value = "uncertainty", p(class = "question-number", "2 of 3 · Leave room to reconsider"),
        textAreaInput("against", "What is missing or doesn’t fit?", rows = 2),
        textAreaInput("change", "What would change your mind?", rows = 2),
        div(
          class = "step-end", actionButton("reflection_back", "← Previous", class = "text-button"),
          actionButton("reflection_action", "Continue →", class = "btn-primary")
        )
      ),
      tabPanel(
        "Action", value = "action", p(class = "question-number", "3 of 3 · A proportionate next step"),
        textAreaInput(
          "action_now", "What action is justified now, and why?", rows = 3,
          placeholder = "Explain a reasonable next step, even while you’re uncertain."
        ),
        p(
          class = "small-note",
          "This saves your reasoning. Use Tasks & actions above to record an action or delegate work."
        ),
        div(
          class = "step-end", actionButton("reflection_back_again", "← Previous", class = "text-button"),
          actionButton("checkpoint", "Save & continue →", class = "btn-primary")
        )
      )
    )
  )
}

ui_interview <- function(widget) {
  conversation <- if (widget) {
    shinychat::chat_ui(
      "conversation", greeting = "Ask an open question, then follow up.", show_history = FALSE, drawer = FALSE,
      allow_attachments = FALSE, enable_cancel = FALSE, icon_assistant = FALSE, fill = FALSE, height = "380px",
      width = "100%"
    )
  } else {
    tagList(
      uiOutput("chat"),
      textAreaInput("question", "What would you like to ask?", placeholder = "Tell me about the potluck…", rows = 2),
      actionButton("ask", "Ask question", class = "ask-button")
    )
  }
  panel(
    "Start with a conversation",
    p(class = "step-intro", "Ask Pat about the event. Then hear from an ill guest, a well guest, and the cook."),
    selectInput("character", "Who would you like to talk to?", choices = NULL), uiOutput("dialogue_help"),
    conversation,
    disclosure(
      "Need help asking a question?",
      p("Choose a topic for a focused question. You can also write your own question; both will be answered."),
      p(class = "small-note", paste(
        "Use fictional investigation questions. When assistance is enabled, your question, up to four recent",
        "exchanges with this person, and the facts needed for the answer go to the configured provider."
      )),
      selectInput("topic", "Topic", choices = topic_choices()),
      if (widget) actionButton("ask", "Ask about this topic", class = "ask-button")
    ),
    uiOutput("dialogue_status"), uiOutput("interview_progress"),
    div(
      class = "gentle-cue", "Pat mentioned tomorrow’s fridge clean-out. You can preserve samples through ",
      tags$a(href = "#actions-tool", class = "open-tool", "Tasks & actions"), "."
    ),
    next_button("interviews_done", "Pause & record my thinking →")
  )
}

ui_define <- function() {
  panel(
    "Who will you count as a case?",
    p(class = "step-intro", "Start with a working definition. You can revise it as you learn more."),
    uiOutput("definition_summary"),
    disclosure(
      "Review or change the criteria",
      selectInput(
        "person", "Who is included?",
        c("All attendees, including walk-ins" = "all", "RSVP guests only" = "rsvp")
      ),
      fluidRow(
        column(6, numericInput("start", "Earliest onset · hours after meal", 0, min = 0)),
        column(6, numericInput("end", "Latest onset · hours after meal", 72, min = 0))
      ),
      selectInput("clinical", "Symptoms", c(
        "Diarrhea OR two of cramps, fever, vomiting" = "standard", "Diarrhea only" = "diarrhea",
        "Any of diarrhea, cramps, fever, vomiting" = "any"
      )),
      checkboxInput("lab", "Also require a positive stool result", FALSE),
      textInput("definition_reason", "Why use these criteria?", "Initial working definition"),
      uiOutput("case_preview")
    ),
    next_button("save_definition", "Save definition & continue →")
  )
}

ui_collect <- function() {
  panel(
    "Give your team a clear plan",
    p(
      class = "step-intro",
      "Choose who to contact and what to ask. Your choices determine the data you’ll have to analyse."
    ),
    conditionalPanel("!output.list_ready", uiOutput("list_help")),
    conditionalPanel(
      "output.list_ready && !output.team_sent",
      radioButtons("audience", "Who should the team contact?", c(
        "Everyone on the available list" = "all", "Only people who have reported illness" = "ill"
      )),
      uiOutput("food_questions"),
      disclosure("Review the other questions", checkboxGroupInput(
        "domains", "Ask about",
        choices = c(
          "Age and sex" = "demographics", "Symptoms" = "symptoms", "Onset and report time" = "onset",
          "Health care visits" = "health_care_visits"
        ),
        selected = c("demographics", "symptoms", "onset")
      )),
      uiOutput("plan_preview"), next_button("send_team", "Send this plan to the team →")
    ),
    conditionalPanel(
      "output.team_sent", uiOutput("collection_status"),
      disclosure(
        "Collect more information", p("Return to your plan to add missing questions or reach more guests."),
        textInput("repeat_reason", "Why collect again?"),
        actionButton("repeat_collection", "Revise collection plan")
      ),
      next_button("collection_done", "Continue to the results →")
    )
  )
}

ui_analysis <- function() {
  intervals <- c("3 hours" = 3, "6 hours" = 6, "12 hours" = 12, "24 hours" = 24)
  panel(
    "What do the data suggest?",
    p(class = "step-intro", paste(
      "Compare illness among people who ate each food with those who didn’t.",
      "Keep missing information in mind."
    )),
    uiOutput("analysis_summary"),
    div(
      class = "download-choice", downloadButton("xlsx", "Download Excel workbook", class = "btn-primary"),
      p("Your collected records and a ready-to-use analysis worksheet.")
    ),
    disclosure(
      "Use R, or see the collected records",
      div(class = "button-row", downloadButton("csv", "Download CSV"), downloadButton("r_script", "Download starter R")),
      p(class = "small-note", paste(
        "Onset and report times are local date-times.",
        "Work out each incubation period from the meal time in your notebook."
      )),
      div(class = "table-scroll", tableOutput("line_list"))
    ),
    disclosure(
      "Check my calculations",
      p("Enter attack rates as proportions (0–1). Use Inf for an infinite relative risk."),
      selectInput("analysis_food", "Food", choices = NULL),
      fluidRow(
        column(4, numericInput("ar_e", "Ate it · attack rate", 0, min = 0, max = 1, step = .01)),
        column(4, numericInput("ar_u", "Didn’t eat it · attack rate", 0, min = 0, max = 1, step = .01)),
        column(4, textInput("rr", "Relative risk", "1"))
      ),
      actionButton("check", "Check my numbers"), uiOutput("calculation_feedback")
    ),
    disclosure(
      "Describe the cases: symptoms and onset curve", uiOutput("case_description"),
      selectInput("curve_interval", "Curve interval", intervals, selected = 6),
      p(class = "small-note", paste(
        "An interval of about a quarter to a third of the suspected incubation period",
        "usually shows the shape best."
      )),
      plotOutput("epi_curve", height = 230)
    ),
    disclosure(
      "Show a worked example", checkboxInput("show_rates", "Show the attack-rate table", FALSE),
      div(class = "table-scroll", tableOutput("rates")), uiOutput("rates_note")
    ),
    disclosure(
      "Compare within groups",
      p("A crude association can reflect foods eaten together. Compare one exposure within levels of another."),
      selectInput("strata_exposure", "Food to compare", choices = NULL),
      selectInput("strata_food", "Group by another food", choices = NULL),
      actionButton("show_strata", "Compare groups"), uiOutput("strata_info"),
      div(class = "table-scroll", tableOutput("strata_table"))
    ),
    disclosure(
      "About this version of the data", uiOutput("snapshot_info"),
      actionButton("freeze", "Use the latest collected data")
    ),
    next_button("analysis_done", "Record what I found →")
  )
}

ui_recommend <- function() {
  panel(
    "Bring your findings together",
    p(class = "step-intro", "Make a recommendation someone else could act on. A cautious conclusion is fine."),
    textAreaInput(
      "recommendation", "What do you recommend?", rows = 7,
      placeholder = "The suspected source is…\nThe evidence suggests…\nWe should…\nWe still don’t know…"
    ),
    p(class = "small-note", "Your notebook holds your evidence and earlier decisions."),
    next_button("finish", "Save & reflect →")
  )
}

app_ui <- function() {
  widget <- chat_widget_enabled()
  guide_script <- paste0("guide.js?v=", unname(tools::md5sum("www/guide.js")))
  theme <- bs_theme(
    version = 5, bg = "#f6f5f0", fg = "#203d38", primary = "#176956", base_font = "Segoe UI",
    heading_font = "Georgia"
  )
  fluidPage(
    title = "Fieldnotes · The community potluck", lang = "en", theme = theme,
    tags$head(tags$link(rel = "stylesheet", href = "style.css"), tags$script(src = guide_script)),
    tags$header(
      class = "topbar", div(class = "brand", span(class = "brand-mark", "+"), "FIELDNOTES"),
      div(class = "clock", textOutput("clock", inline = TRUE), span("game time"))
    ),
    div(
      class = "guided-shell",
      conditionalPanel(
        "input.stage && input.stage !== 'briefing'",
        div(
          class = "guide-header", actionButton("back_step", "← Back", class = "text-button"),
          uiOutput("guide_progress")
        ),
        ui_toolbox()
      ),
      tags$main(id = "guided-main", tabindex = "-1", tabsetPanel(
        id = "stage", type = "hidden",
        tabPanel("Welcome", value = "briefing", ui_welcome()),
        tabPanel("Your view", value = "decision", ui_decision()),
        tabPanel("Talk to people", value = "interview", ui_interview(widget)),
        tabPanel("Define a case", value = "define", ui_define()),
        tabPanel("Collect histories", value = "collect", ui_collect()),
        tabPanel("Make sense of the data", value = "analysis", ui_analysis()),
        tabPanel("Recommend", value = "recommend", ui_recommend()),
        tabPanel("Reflect", value = "debrief", uiOutput("debrief"))
      )),
      tags$footer(class = "site-footer", "A fictional investigation. A safe place to practise.")
    )
  )
}
