generate_data <- function(sc) {
  # Preserve the caller's RNG; sessions must not affect each other's data.
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv) else NULL
  on.exit(if (is.null(old)) {
    if (exists(".Random.seed", .GlobalEnv)) rm(".Random.seed", envir = .GlobalEnv)
  } else assign(".Random.seed", old, .GlobalEnv))
  set.seed(sc$seed)
  n <- sc$setting$attendees
  truth <- data.frame(id = seq_len(n), age = sample(5:80, n, TRUE),
    sex = sample(c("F", "M"), n, TRUE), on_list = runif(n) < sc$setting$guest_list_completeness)
  for (f in sc$foods) truth[[f$id]] <- runif(n) < f$p_eaten
  risk <- ifelse(truth[[sc$truth$vehicle]], sc$truth$attack_rate_exposed, sc$truth$attack_rate_unexposed)
  truth$ill <- runif(n) < risk
  inc <- sc$pathogen$incubation_hours
  truth$onset_hours <- ifelse(truth$ill, rlnorm(n, log(inc$median),
    (log(inc$p95) - log(inc$median)) / qnorm(.95)), NA_real_)
  for (s in names(sc$pathogen$symptoms)) truth[[s]] <- truth$ill & runif(n) < sc$pathogen$symptoms[[s]]
  truth$health_care_visits <- truth$ill & runif(n) < .3
  truth$report_hour <- ifelse(truth$ill, pmax(0, truth$onset_hours - 39 + 6), NA_real_)
  late <- head(which(truth$ill & truth$onset_hours >= 6 & truth$onset_hours < 30 & truth$on_list), 3)
  truth$report_hour[late] <- 28
  reported <- truth
  for (f in sc$foods) {
    ate <- truth[[f$id]]
    forget <- ate & runif(n) < sc$recall$p_forget_item
    false <- !ate & runif(n) < sc$recall$p_false_item
    reported[[f$id]] <- (ate & !forget) | false
  }
  list(truth = truth, reported = reported)
}
