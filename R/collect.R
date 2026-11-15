domain_fields <- function(domains, foods = character()) {
  mapping <- list(
    demographics = c("age", "sex"), symptoms = c("diarrhea", "cramps", "fever", "vomiting", "bloody_stool"),
    onset = c("onset_hours", "report_hour"), health_care_visits = "health_care_visits"
  )
  unique(c(unlist(mapping[intersect(domains, names(mapping))]), foods))
}

collect_records <- function(state, people, fields) {
  fields <- intersect(fields, setdiff(names(state$reported), c(hidden_fields(), "on_list")))
  incoming <- reported_as_of(state) |>
    dplyr::filter(.data$id %in% people) |>
    dplyr::select(dplyr::all_of(c("id", fields)))
  for (id in incoming$id) {
    if (!id %in% state$collected$id) state$collected <- dplyr::bind_rows(state$collected, data.frame(id = id))
    row <- match(id, state$collected$id)
    for (f in fields) {
      if (!f %in% names(state$collected)) state$collected[[f]] <- NA
      state$collected[[f]][row] <- incoming[[f]][match(id, incoming$id)]
    }
    # RSVP status is administrative evidence, never an unasked health/exposure field.
    state$collected$on_list[row] <- state$reported$on_list[match(id, state$reported$id)]
  }
  state
}

# Guests who have phoned in by now, whatever the cause of their illness.
has_reported <- function(state, at = state$clock) {
  !is.na(state$reported$report_hour) & state$reported$report_hour <= at
}

plan_people <- function(state, audience) {
  if (!"guest_list" %in% state$completed) return(integer())
  r <- state$reported
  reachable <- r$on_list | state$walk_ins
  if (audience == "ill") reachable <- reachable & has_reported(state)
  r$id[which(reachable)]
}

# Guests on a contact plan who answer the team's questionnaire.
responders <- function(state, people) intersect(people, state$reported$id[state$reported$responds])

reported_as_of <- function(state) {
  records <- state$reported
  future <- which(!is.na(records$onset_hours) & records$onset_hours > state$clock + meal_to_clock_hours())
  for (field in c(names(state$sc$pathogen$symptoms), "health_care_visits")) records[[field]][future] <- FALSE
  records$onset_hours[future] <- NA_real_
  records$report_hour[which(records$report_hour > state$clock)] <- NA_real_
  records
}

# A line list as a field team would hold it: onset and report as local
# date-times. Learners work out incubation from the meal time themselves.
export_line_list <- function(data, sc) {
  stamp <- function(hours, origin) ifelse(is.na(hours), NA_character_, format(origin + hours * 3600, "%Y-%m-%d %H:%M"))
  rename <- c(onset_hours = "onset_time", report_hour = "report_time")
  origin <- list(onset_hours = meal_time(sc), report_hour = meal_time(sc) + meal_to_clock_hours() * 3600)
  for (field in intersect(names(rename), names(data))) {
    data[[field]] <- stamp(data[[field]], origin[[field]])
    names(data)[names(data) == field] <- rename[[field]]
  }
  data
}
