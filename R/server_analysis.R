# Frozen snapshots, downloads, calculation checks, description, and comparisons.
server_analysis <- function(input, output, session, app) {
  s <- app$s
  snapshot <- app$snapshot
  feedback <- app$feedback
  clear_strata <- function() {
    app$strata_result(NULL)
    app$strata_pooled(NULL)
    app$strata_snapshot(NULL)
  }

  # Foods the learner can analyse: the menu once Pat gives it, otherwise only foods guests have named.
  known_foods <- function(state, data = state$collected) {
    labels <- food_labels(state$sc)
    if (state$menu) labels else labels[intersect(names(labels), names(data))]
  }
  observe({
    labels <- known_foods(s())
    exposure <- input$strata_exposure
    if (is.null(exposure) || !exposure %in% names(labels)) exposure <- head(names(labels), 1L)
    groups <- setdiff(names(labels), exposure)
    selected_group <- intersect(isolate(input$strata_food), groups)
    updateSelectInput(
      session, "strata_food", choices = setNames(groups, labels[groups]),
      selected = if (length(selected_group)) selected_group else head(groups, 1L)
    )
    choices <- setNames(names(labels), labels)
    updateSelectInput(session, "strata_exposure", choices = choices, selected = exposure)
    updateSelectInput(session, "analysis_food", choices = choices, selected = isolate(input$analysis_food))
  })

  output$analysis_summary <- renderUI(div(
    class = "note", paste(nrow(s()$collected), "guest records collected."),
    " The download includes only what you or your team asked about."
  ))
  heading <- function(name) {
    name <- gsub("_", " ", name)
    paste0(toupper(substr(name, 1, 1)), substring(name, 2))
  }
  output$line_list <- renderTable({
    preview <- export_line_list(head(line_list(s()), 12), s()$sc)
    names(preview) <- vapply(names(preview), heading, character(1))
    preview
  }, na = "—", striped = TRUE, digits = 1)

  app$freeze_snapshot <- function() {
    state <- isolate(s())
    if (!nrow(state$collected)) stop("Collect at least one guest record first.")
    snap <- list(
      id = length(state$downloads) + 1L, time = state$clock, definition = state$definition, data = line_list(state)
    )
    state$downloads <- append(state$downloads, list(snap))
    s(record_event(state, "analysis_snapshot", list(id = snap$id, time = snap$time)))
    snapshot(snap)
    feedback(NULL)
    clear_strata()
    snap
  }
  get_snapshot <- function() if (is.null(isolate(snapshot()))) app$freeze_snapshot() else isolate(snapshot())
  observeEvent(input$freeze, tryCatch({
    app$freeze_snapshot()
    showNotification("Analysis snapshot frozen. Downloads and calculation checks use this exact version.")
  }, error = function(e) app$notify(conditionMessage(e))))
  output$snapshot_info <- renderUI({
    snap <- snapshot()
    if (is.null(snap)) {
      return(p(class = "small-note", paste(
        "Downloads freeze your first snapshot automatically.",
        "The preview shows up to 12 current records."
      )))
    }
    div(class = "note", paste0(
      "Downloads, checks, the case description, and the curve use snapshot ", snap$id, " · ", nrow(snap$data),
      " records · ", game_time(snap$time), ". Freeze again to include later collection or definition changes. ",
      "The record preview is current."
    ))
  })
  output$csv <- downloadHandler("potluck-line-list.csv", function(file) {
    write.csv(export_line_list(get_snapshot()$data, isolate(s())$sc), file, row.names = FALSE, na = "")
  })
  output$xlsx <- downloadHandler("potluck-analysis.xlsx", function(file) {
    write_workbook(get_snapshot()$data, file, isolate(s())$sc)
  })
  output$r_script <- downloadHandler("starter-analysis.R", function(file) file.copy("starter/analysis.R", file))

  observeEvent(input$check, {
    tryCatch({
      if (is.null(input$analysis_food) || !nzchar(input$analysis_food)) {
        stop("Collect exposure histories or obtain the menu first.")
      }
      snap <- get_snapshot()
      rr <- suppressWarnings(as.numeric(input$rr))
      if (is.na(rr) || rr < 0) stop("Enter a non-negative relative risk or Inf.")
      result <- check_calculation(snap$data, input$analysis_food, input$ar_e, input$ar_u, rr)
      feedback(result$message)
      state <- s()
      entry <- list(snapshot = snap$id, food = input$analysis_food, result = result)
      state$checks <- append(state$checks, list(entry))
      s(state)
    }, error = function(e) app$notify(conditionMessage(e)))
  })
  output$calculation_feedback <- renderUI(if (!is.null(feedback())) div(class = "note", role = "status", feedback()))

  # Tables, the case description, and the curve all read the same records:
  # the frozen snapshot once one exists, otherwise the current line list.
  analysis_data <- reactive({
    snap <- snapshot()
    if (is.null(snap)) line_list(s()) else snap$data
  })
  output$rates <- renderTable({
    req(input$show_rates)
    data <- analysis_data()
    labels <- known_foods(s(), data)
    req(length(labels))
    format_rates(attack_rates(data, names(labels)), labels)
  })
  output$rates_note <- renderUI({
    req(input$show_rates)
    p(class = "small-note", paste(
      "Left out: records with an unknown exposure or case status, or outside your cohort.",
      "The interval is approximate; with a small or empty cell, rely on the exact p-value and say so."
    ))
  })
  output$case_description <- renderUI({
    summary <- describe_cases(analysis_data())
    if (!summary$cases) {
      return(p(class = "small-note", paste(
        "No classified cases yet. Collect symptoms and onset times,", "then check your case definition."
      )))
    }
    spread <- function(x, unit) {
      sprintf(
        "median %s %s (range %s to %s)", format(round(x[["median"]], 1)), unit, format(round(x[["min"]], 1)),
        format(round(x[["max"]], 1))
      )
    }
    asked <- summary$symptoms[summary$symptoms$asked > 0, ]
    tagList(
      p(strong(summary$cases), paste0(if (summary$cases == 1) " case" else " cases", " under your definition.")),
      if (!is.null(summary$incubation)) {
        p(paste0(
          "Time from meal to onset: ", spread(summary$incubation, "hours"),
          ". Compare this, and the symptoms below, with the agents you suspect."
        ))
      },
      if (nrow(asked)) {
        tags$ul(lapply(seq_len(nrow(asked)), function(i) {
          tags$li(sprintf(
            "%s: %d of %d cases (%d%%)", asked$symptom[i], asked$cases[i], asked$asked[i],
            round(100 * asked$cases[i] / asked$asked[i])
          ))
        }))
      },
      if (!is.null(summary$age)) p(paste0("Age: ", spread(summary$age, "years"), ".")),
      if (!is.null(summary$sex)) {
        p(paste0("Sex: ", paste(names(summary$sex), summary$sex, sep = " ", collapse = ", "), "."))
      }
    )
  })
  epi_curve <- reactive({
    width <- suppressWarnings(as.numeric(input$curve_interval))
    onset_counts(analysis_data(), if (length(width) == 1L && is.finite(width)) width else 6)
  })
  output$epi_curve <- renderPlot({
    curve <- epi_curve()
    validate(need(!is.null(curve), "Collect onset times to draw the epidemic curve."))
    plot_epi_curve(curve)
  }, alt = reactive(epi_curve_alt(epi_curve())))

  observeEvent(input$show_strata, {
    tryCatch({
      snap <- get_snapshot()
      summary <- stratified_summary(snap$data, input$strata_exposure, input$strata_food)
      app$strata_result(summary$strata)
      app$strata_pooled(summary[c("crude", "adjusted")])
      app$strata_snapshot(snap$id)
    }, error = function(e) {
      clear_strata()
      app$notify(conditionMessage(e))
    })
  })
  output$strata_info <- renderUI({
    if (is.null(app$strata_snapshot())) return(NULL)
    pooled <- app$strata_pooled()
    show <- function(x) if (is.na(x)) "not estimable" else if (is.infinite(x)) "infinite" else sprintf("%.2f", x)
    interval <- if (!is.null(pooled) && is.finite(pooled$adjusted[["lower"]])) {
      sprintf(" (approx. 95%% CI %.2f to %.2f)", pooled$adjusted[["lower"]], pooled$adjusted[["upper"]])
    } else {
      ""
    }
    tagList(
      p(class = "small-note", paste0("Comparison uses frozen snapshot ", app$strata_snapshot(), ".")),
      if (!is.null(pooled)) {
        div(
          class = "note", role = "status",
          p(paste0(
            "Crude relative risk: ", show(pooled$crude),
            ". Mantel-Haenszel relative risk, adjusted for the grouping food: ", show(pooled$adjusted[["rr"]]),
            interval, "."
          )),
          p(paste(
            "An adjusted value that moves away from the crude value suggests confounding by the grouping food.",
            "If the two group-specific values differ widely from each other, report them separately instead of pooling."
          ))
        )
      }
    )
  })
  output$strata_table <- renderTable({
    result <- app$strata_result()
    if (is.null(result)) return(NULL)
    labels <- food_labels(s()$sc)
    group <- sub(" (un)?exposed$", "", result$stratum)
    ate <- ifelse(grepl(" unexposed$", result$stratum), "did not eat", "ate")
    result$food <- paste("Guests who", ate, labels[group])
    format_rates(result, first = "Group")
  })
}
