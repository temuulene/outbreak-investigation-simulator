# Download potluck-line-list.csv into your working directory.
# TRUE = exposed, FALSE = unexposed, NA = not asked or unknown.
library(dplyr)
guests <- read.csv("potluck-line-list.csv", na.strings = "")
food <- "chicken_salad" # Repeat for each collected exposure.
stopifnot(food %in% names(guests))
analysis <- guests |>
  filter(case_status != "Unknown", !is.na(.data[[food]])) |>
  mutate(exposed = .data[[food]], case = case_status == "Case")
table <- analysis |>
  summarise(cases = sum(case), total = n(), .by = exposed)
print(table)
# TODO: add attack_rate = cases / total to the table.
# TODO: divide exposed attack rate by unexposed attack rate for relative risk.
# A zero denominator is not estimable. A zero unexposed attack rate with a
# positive exposed attack rate gives an infinite RR, not proof of causation.
# Compare unasked fields and exclusions before interpreting the association.
# Onset is hours since Saturday 18:00; report_hour is since Monday 09:00.
