line_list <- function(s) {
  data <- s$collected
  data$case_status <- classify_cases(data, s$definition)
  data
}

# Cells below this count make a ratio and its interval unstable.
small_cell <- function() 5

attack_rates <- function(data, foods) {
  dplyr::bind_rows(lapply(foods, function(food) {
    exposure <- if (food %in% names(data)) data[[food]] else rep(NA, nrow(data))
    # Unknown classifications and people outside the defined cohort enter neither denominator.
    known <- !is.na(exposure) & data$case_status %in% c("Case", "Non-case")
    e <- exposure[known]
    case <- data$case_status[known] == "Case"
    ne <- sum(e)
    nu <- sum(!e)
    ce <- sum(case & e)
    cu <- sum(case & !e)
    are <- if (ne > 0) ce / ne else NA_real_
    aru <- if (nu > 0) cu / nu else NA_real_
    rr <- if (is.na(aru) || is.na(are) || (aru == 0 && are == 0)) NA_real_ else are / aru
    se <- if (ce > 0 && cu > 0) sqrt(1 / ce - 1 / ne + 1 / cu - 1 / nu) else NA_real_
    cells <- c(ce, ne - ce, cu, nu - cu)
    estimable <- ne > 0 && nu > 0
    # Fisher's exact test stays valid where the log-Wald interval fails: small or empty cells.
    fisher <- if (estimable) stats::fisher.test(matrix(cells, 2))$p.value else NA_real_
    caution <- if (!estimable) {
      "No comparison group"
    } else if (any(cells == 0)) {
      "Empty cell: no interval; use the exact p-value"
    } else if (any(cells < small_cell())) {
      "Small cell: wide interval"
    } else {
      ""
    }
    data.frame(
      food = food, exposed_cases = ce, exposed_total = ne, unexposed_cases = cu,
      unexposed_total = nu, attack_rate_exposed = are, attack_rate_unexposed = aru,
      relative_risk = rr, rr_lower = exp(log(rr) - 1.96 * se), rr_upper = exp(log(rr) + 1.96 * se),
      fisher_p = fisher, caution = caution, excluded_unknown = sum(!known)
    )
  }))
}

# A table for reading on screen: plain headings and one cell per idea.
format_rates <- function(rates, labels = NULL, first = "Food") {
  ratio <- function(x) ifelse(is.na(x), "Not estimable", ifelse(is.infinite(x), "Infinite", sprintf("%.2f", x)))
  percent <- function(x) ifelse(is.na(x), "Not estimable", sprintf("%.0f%%", 100 * x))
  interval <- ifelse(is.na(rates$rr_lower), "", sprintf(" (%.2f to %.2f)", rates$rr_lower, rates$rr_upper))
  name <- if (is.null(labels)) rates$food else unname(ifelse(rates$food %in% names(labels), labels[rates$food], rates$food))
  table <- data.frame(
    name,
    paste0(rates$exposed_cases, " / ", rates$exposed_total), percent(rates$attack_rate_exposed),
    paste0(rates$unexposed_cases, " / ", rates$unexposed_total), percent(rates$attack_rate_unexposed),
    paste0(ratio(rates$relative_risk), interval),
    ifelse(is.na(rates$fisher_p), "", ifelse(rates$fisher_p < .001, "< 0.001", sprintf("%.3f", rates$fisher_p))),
    rates$excluded_unknown, rates$caution,
    check.names = FALSE, stringsAsFactors = FALSE
  )
  names(table) <- c(
    first, "Ate it: cases / total", "Attack rate", "Did not: cases / total", "Attack rate ",
    "Relative risk (approx. 95% CI)", "Exact p", "Left out", "Note"
  )
  table
}

# Formative check of a learner's attack rates and relative risk against a frozen
# snapshot. Each value gets its own feedback, and common slips are named.
check_calculation <- function(snapshot, food, exposed, unexposed, rr, tolerance = .015, rr_tolerance = .05) {
  correct <- attack_rates(snapshot, food)
  expected <- unlist(correct[c("attack_rate_exposed", "attack_rate_unexposed", "relative_risk")], use.names = FALSE)
  if (any(is.na(expected))) return(list(ok = FALSE, message = "Not estimable from this snapshot: check missing fields and denominators."))
  number <- function(x) if (is.numeric(x) && length(x) == 1L) x else NA_real_
  supplied <- c(number(exposed), number(unexposed), number(rr))
  near <- function(x, target, tol) (is.infinite(target) && is.infinite(x) && sign(target) == sign(x)) ||
    (is.finite(target) && is.finite(x) && abs(x - target) <= tol)
  ok <- c(near(supplied[1], expected[1], tolerance), near(supplied[2], expected[2], tolerance), near(supplied[3], expected[3], rr_tolerance))
  rounded <- FALSE
  if (!ok[3] && all(ok[1:2]) && is.finite(supplied[3]) && supplied[2] > 0) {
    # A ratio of correctly rounded attack rates is the right method, with less precision.
    rounded <- abs(supplied[3] - supplied[1] / supplied[2]) <= rr_tolerance
    ok[3] <- rounded
  }
  if (all(ok)) {
    return(list(ok = TRUE, message = if (rounded) sprintf(
      "Your method is right. Dividing rounded attack rates gave %.2f; the unrounded counts give %.2f. Round only at the end.",
      supplied[3], expected[3]) else "All three calculations agree with your frozen line list."))
  }
  slip <- function(value, cases, total) {
    all_cases <- correct$exposed_cases + correct$unexposed_cases
    if (is.finite(value) && value > 1 && near(value / 100, cases / total, tolerance)) return("Enter a proportion from 0 to 1, not a percentage.")
    if (all_cases > 0 && near(value, cases / all_cases, tolerance)) return("That looks like this group's share of all cases. An attack rate divides the group's cases by everyone in the group, ill or well.")
    "Count cases and all classified guests in this group; leave out unknown and excluded records."
  }
  notes <- c(
    if (!ok[1]) paste("Attack rate among those who ate it:", slip(supplied[1], correct$exposed_cases, correct$exposed_total)),
    if (!ok[2]) paste("Attack rate among those who did not:", slip(supplied[2], correct$unexposed_cases, correct$unexposed_total)),
    if (!ok[3]) "Relative risk: divide the exposed attack rate by the unexposed attack rate. Use Inf when no unexposed guest is a case.")
  list(ok = FALSE, message = paste(c(sprintf("%d of 3 values agree with your frozen line list.", sum(ok)), notes), collapse = " "))
}

write_workbook <- function(data, path, sc) {
  wb <- openxlsx::createWorkbook()
  for (sheet in c("Read me", "Line list", "Attack rates")) openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, "Read me", data.frame(Instructions = c(
    "Use the frozen Line list tab. TRUE = exposed; FALSE = unexposed; blank = not collected.",
    "Case status follows the saved definition. Unknown and Excluded records must not enter either denominator.",
    paste0(
      "Onset and report times are local date-times. The meal was served at ",
      format(meal_time(sc), "%Y-%m-%d %H:%M"), "; incubation is onset time minus meal time."
    ),
    "Fill columns B:E with counts from the line list, separately for each exposure.",
    "F:H calculate attack rates and relative risk. NE means not estimable; Inf means zero unexposed cases.",
    "Discuss missing data, non-response, recall error, and small numbers. An association alone does not establish cause."
  )))
  openxlsx::writeData(wb, "Line list", export_line_list(data, sc))
  foods <- intersect(names(food_labels(sc)), names(data))
  # Only list exposures present in this learner's snapshot.
  if (!length(foods)) foods <- "No food exposures collected"
  table <- data.frame(
    Food = foods, Exposed_cases = NA_real_, Exposed_total = NA_real_, Unexposed_cases = NA_real_,
    Unexposed_total = NA_real_, AR_exposed = NA_real_, AR_unexposed = NA_real_, RR = NA_real_
  )
  openxlsx::writeData(wb, "Attack rates", table)
  formula <- function(text, row, column) {
    openxlsx::writeFormula(wb, "Attack rates", gsub("#", row, text, fixed = TRUE), startCol = column, startRow = row)
  }
  for (row in seq_len(nrow(table)) + 1L) {
    formula('IF(OR(B#="",C#="",C#=0),"NE",B#/C#)', row, 6)
    formula('IF(OR(D#="",E#="",E#=0),"NE",D#/E#)', row, 7)
    formula('IF(OR(F#="NE",G#="NE"),"NE",IF(G#=0,IF(F#=0,"NE","Inf"),F#/G#))', row, 8)
  }
  for (sheet in names(wb)) {
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
    openxlsx::setColWidths(wb, sheet, cols = 1:12, widths = 22)
  }
  openxlsx::setColWidths(wb, "Read me", cols = 1, widths = 115)
  openxlsx::saveWorkbook(wb, path, overwrite = TRUE)
}

stratified_rates <- function(data, exposure, stratifier) {
  if (length(exposure) != 1 || length(stratifier) != 1 || exposure == stratifier) stop("Choose two different exposures.")
  if (!stratifier %in% names(data)) stop("Collect the stratifying exposure first.")
  dplyr::bind_rows(lapply(c(FALSE, TRUE), function(level) {
    subset <- data[which(!is.na(data[[stratifier]]) & data[[stratifier]] == level), , drop = FALSE]
    result <- attack_rates(subset, exposure)
    result$stratum <- paste(stratifier, if (level) "exposed" else "unexposed")
    result$excluded_stratifier_unknown <- sum(is.na(data[[stratifier]]))
    result
  }))
}

# Mantel-Haenszel pooled risk ratio with a Greenland-Robins interval. `strata`
# holds one attack_rates() row per stratum.
mantel_haenszel_rr <- function(strata) {
  a <- strata$exposed_cases; n1 <- strata$exposed_total
  c <- strata$unexposed_cases; n0 <- strata$unexposed_total
  keep <- n1 + n0 > 0
  a <- a[keep]; n1 <- n1[keep]; c <- c[keep]; n0 <- n0[keep]; n <- n1 + n0
  numerator <- sum(a * n0 / n); denominator <- sum(c * n1 / n)
  if (!length(n) || numerator == 0 || denominator == 0) return(c(rr = NA_real_, lower = NA_real_, upper = NA_real_))
  rr <- numerator / denominator
  se <- sqrt(sum((n1 * n0 * (a + c) - a * c * n) / n^2) / (numerator * denominator))
  c(rr = rr, lower = exp(log(rr) - 1.96 * se), upper = exp(log(rr) + 1.96 * se))
}

# Crude, stratum-specific, and pooled estimates from the same records, so the
# three can be compared. Records with an unknown stratifier leave all three.
stratified_summary <- function(data, exposure, stratifier) {
  strata <- stratified_rates(data, exposure, stratifier)
  crude <- attack_rates(data[!is.na(data[[stratifier]]), , drop = FALSE], exposure)
  list(strata = strata, crude = crude$relative_risk, adjusted = mantel_haenszel_rr(strata))
}

# Descriptive summary of the classified cases: who, what symptoms, and when.
describe_cases <- function(data) {
  cases <- data[data$case_status %in% "Case", , drop = FALSE]
  symptoms <- intersect(domain_fields("symptoms"), names(cases))
  result <- list(cases = nrow(cases), symptoms = data.frame(symptom = character(), cases = integer(), asked = integer()))
  if (!nrow(cases)) return(result)
  result$symptoms <- data.frame(symptom = gsub("_", " ", symptoms),
    cases = vapply(symptoms, function(f) sum(cases[[f]], na.rm = TRUE), numeric(1)),
    asked = vapply(symptoms, function(f) sum(!is.na(cases[[f]])), numeric(1)), row.names = NULL)
  spread <- function(x) if (all(is.na(x))) NULL else c(min = min(x, na.rm = TRUE), median = stats::median(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
  if ("onset_hours" %in% names(cases)) result$incubation <- spread(cases$onset_hours)
  if ("age" %in% names(cases)) result$age <- spread(cases$age)
  if ("sex" %in% names(cases) && any(!is.na(cases$sex))) result$sex <- table(cases$sex)
  result
}

# Onset counts per interval for the epidemic curve. A common rule of thumb sets
# the interval to a quarter to a third of the suspected incubation period.
onset_counts <- function(data, width = 6) {
  if (!"onset_hours" %in% names(data) || !length(width) || !is.finite(width) || width <= 0) return(NULL)
  known <- !is.na(data$onset_hours)
  if (!any(known)) return(NULL)
  onset <- data$onset_hours[known]
  # Background illness can begin before the meal, so the axis may start below zero.
  first <- min(0, floor(min(onset) / width) * width)
  breaks <- seq(first, max(72, ceiling((max(onset) + 1e-9) / width) * width), width)
  bin <- cut(onset, breaks, right = FALSE)
  case <- data$case_status[known] == "Case"
  counts <- rbind(Cases = as.vector(table(bin[case])), `Other reported illness` = as.vector(table(bin[!case])))
  list(width = width, breaks = breaks, counts = counts)
}

plot_epi_curve <- function(curve) {
  counts <- curve$counts
  old <- graphics::par(mar = c(4, 4, 1, 1), bg = "#ffffff", col.axis = "#203d38", col.lab = "#203d38")
  on.exit(graphics::par(old))
  graphics::barplot(counts, space = 0, col = c("#176956", "#c9d6cd"), border = "white", axes = FALSE,
    ylim = c(0, max(1, colSums(counts)) * 1.15), xlab = "Onset, hours after Saturday's 18:00 meal", ylab = "People")
  at <- which(curve$breaks %% max(12, curve$width) == 0) - 1
  graphics::axis(1, at = at, labels = curve$breaks[at + 1])
  if (curve$breaks[1] < 0) graphics::abline(v = which(curve$breaks == 0) - 1, lty = 2, col = "#62736b")
  graphics::axis(2, at = unique(round(pretty(c(0, max(1, colSums(counts)))))), las = 1)
  if (any(counts[2, ] > 0)) graphics::legend("topright", legend = rownames(counts), fill = c("#176956", "#c9d6cd"), border = "white", bty = "n", cex = .85)
}

epi_curve_alt <- function(curve) {
  if (is.null(curve)) return("No onset times have been collected yet.")
  totals <- colSums(curve$counts)
  peak <- which.max(totals)
  sprintf(paste(
      "Epidemic curve in %g-hour intervals: %d cases and %d other people with a reported onset.",
      "The tallest bar is %d people at %g to %g hours after the meal; onsets span %g to %g hours."
    ),
    curve$width, sum(curve$counts[1, ]), sum(curve$counts[2, ]), totals[peak], curve$breaks[peak], curve$breaks[peak + 1],
    curve$breaks[min(which(totals > 0))], curve$breaks[max(which(totals > 0)) + 1])
}
