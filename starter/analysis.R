# Download potluck-line-list.csv into your working directory.
# TRUE = exposed, FALSE = unexposed, NA = not asked or unknown.
library(dplyr)
guests <- read.csv("potluck-line-list.csv", na.strings = "")

# Every column that is not a guest detail, symptom, or status is a food exposure.
other_columns <- c("id", "on_list", "age", "sex", "diarrhea", "cramps", "fever",
  "vomiting", "bloody_stool", "onset_time", "report_time", "health_care_visits",
  "stool_positive", "case_status")
foods <- setdiff(names(guests), other_columns)
stopifnot(length(foods) > 0)
print(foods)

food <- foods[1] # Repeat for every food, or wrap the steps below in a function.
# "Unknown" and "Excluded" records belong in neither denominator.
analysis <- guests |>
  filter(case_status %in% c("Case", "Non-case"), !is.na(.data[[food]])) |>
  mutate(exposed = .data[[food]], case = case_status == "Case")
counts <- analysis |>
  summarise(cases = sum(case), total = n(), .by = exposed)
print(counts)
# TODO: add attack_rate = cases / total to the counts.
# TODO: divide exposed attack rate by unexposed attack rate for relative risk.
# Keep full precision until the last step; round only what you report.
# A zero denominator is not estimable. A zero unexposed attack rate with a
# positive exposed attack rate gives an infinite RR, not proof of causation.
# Compare unasked fields, non-response, and exclusions before interpreting.

# TODO: work out each incubation period. onset_time is a local date-time, so
# subtract the meal time from your notebook, for example:
# meal <- as.POSIXct("YYYY-MM-DD 18:00", tz = "UTC")
# onset <- as.POSIXct(guests$onset_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
# incubation_hours <- as.numeric(difftime(onset, meal, units = "hours"))
# Describe its median and range among cases before you compare foods.
