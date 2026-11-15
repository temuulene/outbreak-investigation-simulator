# Fields that exist only in the simulation. They never reach the learner.
hidden_fields <- function() c("ill", "background", "responds", "stool_detectable")

# Hours between the Saturday 18:00 meal and the Monday 09:00 call that starts the clock.
meal_to_clock_hours <- function() 39

generate_data <- function(sc) {
  # Preserve the caller's RNG; sessions must not affect each other's data.
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit(if (is.null(old)) {
    if (exists(".Random.seed", .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  } else {
    assign(".Random.seed", old, .GlobalEnv)
  })
  # Pin the generators so a seed gives the same records whatever the caller's RNGkind().
  # Restoring .Random.seed above also restores the caller's generator kinds.
  set.seed(sc$seed, kind = "Mersenne-Twister", normal.kind = "Inversion", sample.kind = "Rejection")
  n <- sc$setting$attendees
  truth <- data.frame(
    id = seq_len(n), age = sample(5:80, n, TRUE), sex = sample(c("F", "M"), n, TRUE),
    on_list = runif(n) < sc$setting$guest_list_completeness
  )
  for (f in sc$foods) {
    probability <- if (is.null(f$conditional_on)) {
      f$p_eaten
    } else {
      ifelse(truth[[f$conditional_on]], f$p_if_exposed, f$p_if_unexposed)
    }
    truth[[f$id]] <- runif(n) < probability
  }
  risk <- ifelse(truth[[sc$truth$vehicle]], sc$truth$attack_rate_exposed, sc$truth$attack_rate_unexposed)
  truth$ill <- runif(n) < risk
  inc <- sc$pathogen$incubation_hours
  sdlog <- (log(inc$p95) - log(inc$median)) / qnorm(.95)
  truth$onset_hours <- ifelse(truth$ill, rlnorm(n, log(inc$median), sdlog), NA_real_)
  symptoms <- names(sc$pathogen$symptoms)
  for (s in symptoms) truth[[s]] <- truth$ill & runif(n) < sc$pathogen$symptoms[[s]]
  truth$health_care_visits <- truth$ill & runif(n) < .3
  report_delay <- 6
  truth$report_hour <- ifelse(truth$ill, pmax(0, truth$onset_hours - meal_to_clock_hours() + report_delay), NA_real_)
  # Scheduled "more guests call" events name how many earlier cases report late.
  protected <- integer()
  for (event in sc$events) {
    rule <- event$late_reporters
    if (is.null(rule)) next
    eligible <- truth$ill & truth$onset_hours >= rule$onset_from & truth$onset_hours < rule$onset_to & truth$on_list
    late <- head(setdiff(which(eligible), protected), rule$count)
    truth$report_hour[late] <- event$at_hour
    protected <- c(protected, late)
  }
  reported <- truth
  for (f in sc$foods) {
    ate <- truth[[f$id]]
    forget <- ate & runif(n) < sc$recall$p_forget_item
    false <- !ate & runif(n) < sc$recall$p_false_item
    reported[[f$id]] <- (ate & !forget) | false
  }

  # Everything below draws after the core records, so the attendees, exposures,
  # illness, and recall above stay the same for a seed whichever options are set.

  # Unrelated mild illness in guests the outbreak spared. It makes a broad case
  # definition less specific and gives the time criterion work to do.
  bg <- sc$background_illness
  background <- !truth$ill & runif(n) < (if (is.null(bg)) 0 else bg$p_symptomatic)
  background_onset <- runif(n)
  background_reports <- runif(n)
  background_symptoms <- lapply(symptoms, function(s) runif(n))
  if (!is.null(bg)) {
    names(background_symptoms) <- symptoms
    has <- lapply(symptoms, function(s) background & background_symptoms[[s]] < bg$symptoms[[s]])
    names(has) <- symptoms
    background <- background & Reduce(`|`, has)
    for (s in symptoms) truth[[s]] <- truth[[s]] | (has[[s]] & background)
    onset <- bg$onset_hours$from + background_onset * (bg$onset_hours$to - bg$onset_hours$from)
    truth$onset_hours[background] <- onset[background]
    calls <- background & background_reports < bg$p_self_report
    truth$report_hour[calls] <- pmax(0, truth$onset_hours[calls] - meal_to_clock_hours() + report_delay)
  }
  truth$background <- background

  # Not every ill guest phones in. Late reporters named by an event always do.
  phones <- runif(n) < (if (is.null(sc$reporting$p_self_report)) 1 else sc$reporting$p_self_report)
  silent <- truth$ill & !phones
  silent[protected] <- FALSE
  truth$report_hour[silent] <- NA_real_

  # Response to the team's questionnaire is fixed per guest, so repeat visits agree.
  respond <- runif(n)
  p_ill <- if (is.null(sc$response$p_respond_ill)) 1 else sc$response$p_respond_ill
  p_well <- if (is.null(sc$response$p_respond_well)) 1 else sc$response$p_respond_well
  truth$responds <- respond < ifelse(truth$ill | truth$background, p_ill, p_well)

  # Differential recall: ill guests who did not eat a rumoured food may say they did.
  rumour <- runif(n)
  carry <- c(symptoms, "onset_hours", "report_hour", "background", "responds")
  reported[carry] <- truth[carry]
  if (!is.null(sc$recall$rumour)) {
    food <- sc$recall$rumour$food
    reported[[food]] <- reported[[food]] | (truth$ill & !truth[[food]] & rumour < sc$recall$rumour$p_false_if_ill)
  }

  # Laboratory tests miss some true positives.
  stool <- runif(n)
  food_test <- runif(1)
  truth$stool_detectable <- truth$ill & stool < (if (is.null(sc$lab$stool_sensitivity)) 1 else sc$lab$stool_sensitivity)
  reported$stool_detectable <- truth$stool_detectable
  lab <- list(food_detected = food_test < (if (is.null(sc$lab$food_sensitivity)) 1 else sc$lab$food_sensitivity))
  list(truth = truth, reported = reported, lab = lab)
}
