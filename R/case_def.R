default_definition <- function() list(person = "all", place = "Community hall", start = 0,
  end = 72, clinical = "standard", lab = FALSE, reason = "Initial working definition")

classify_cases <- function(data, definition = default_definition()) {
  n <- nrow(data)
  field <- function(name) if (name %in% names(data)) data[[name]] else rep(NA, n)
  clinical <- switch(definition$clinical,
    diarrhea = field("diarrhea"),
    any = field("diarrhea") | field("cramps") | field("fever") | field("vomiting"),
    standard = field("diarrhea") | (field("cramps") + field("fever") + field("vomiting") >= 2))
  time <- field("onset_hours") >= definition$start & field("onset_hours") <= definition$end
  # Known absence of symptoms establishes a non-case even without an onset.
  result <- clinical & time
  if (definition$person == "rsvp") result <- result & field("on_list")
  if (definition$lab) result <- result & field("stool_positive")
  ifelse(is.na(result), "Unknown", ifelse(result, "Case", "Non-case"))
}
