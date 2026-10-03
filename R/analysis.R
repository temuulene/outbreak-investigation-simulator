line_list <- function(s) {
  data <- s$collected
  data$case_status <- classify_cases(data, s$definition)
  data
}

attack_rates <- function(data, foods) {
  dplyr::bind_rows(lapply(foods, function(food) {
    exposure <- if (food %in% names(data)) data[[food]] else rep(NA, nrow(data))
    known <- !is.na(exposure) & data$case_status != "Unknown"
    e <- exposure[known]
    case <- data$case_status[known] == "Case"
    ne <- sum(e); nu <- sum(!e)
    ce <- sum(case & e); cu <- sum(case & !e)
    are <- if (ne > 0) ce / ne else NA_real_
    aru <- if (nu > 0) cu / nu else NA_real_
    rr <- if (is.na(aru) || is.na(are) || (aru == 0 && are == 0)) NA_real_ else are / aru
    se <- if (ce > 0 && cu > 0) sqrt(1 / ce - 1 / ne + 1 / cu - 1 / nu) else NA_real_
    data.frame(food = food, exposed_cases = ce, exposed_total = ne, unexposed_cases = cu,
      unexposed_total = nu, attack_rate_exposed = are, attack_rate_unexposed = aru,
      relative_risk = rr, rr_lower = exp(log(rr) - 1.96 * se), rr_upper = exp(log(rr) + 1.96 * se),
      excluded_unknown = sum(!known))
  }))
}

check_calculation <- function(snapshot, food, exposed, unexposed, rr, tolerance = .015) {
  correct <- attack_rates(snapshot, food)
  supplied <- c(exposed, unexposed, rr)
  expected <- unlist(correct[c("attack_rate_exposed", "attack_rate_unexposed", "relative_risk")], use.names = FALSE)
  if (any(is.na(expected))) return(list(ok = FALSE, message = "Not estimable from this snapshot: check missing fields and denominators."))
  ok <- all((is.infinite(expected) & is.infinite(supplied) & sign(expected) == sign(supplied)) |
    (is.finite(expected) & is.finite(supplied) & abs(supplied - expected) <= tolerance))
  list(ok = isTRUE(ok), message = if (isTRUE(ok)) "All three calculations agree with your frozen line list." else "At least one value differs. Use proportions (0–1), exclude unknowns, and check both denominators.")
}

write_workbook <- function(data, path, sc) {
  wb <- openxlsx::createWorkbook()
  for (sheet in c("Read me", "Line list", "Attack rates")) openxlsx::addWorksheet(wb, sheet)
  openxlsx::writeData(wb, "Read me", data.frame(Instructions = c(
    "Use the frozen Line list tab. TRUE = exposed; FALSE = unexposed; blank = not collected.",
    "Case status follows the saved definition. Unknown classifications must not enter either denominator.",
    "Fill columns B:E with counts from the line list, separately for each exposure.",
    "F:H calculate attack rates and relative risk. NE means not estimable; Inf means zero unexposed cases.",
    "Discuss missing data, recall error, and small numbers. An association alone does not establish cause.")))
  openxlsx::writeData(wb, "Line list", data)
  foods <- intersect(names(food_labels(sc)), names(data))
  # Only list exposures present in this learner's snapshot.
  if (!length(foods)) foods <- "No food exposures collected"
  table <- data.frame(Food = foods, Exposed_cases = NA_real_, Exposed_total = NA_real_,
    Unexposed_cases = NA_real_, Unexposed_total = NA_real_, AR_exposed = NA_real_, AR_unexposed = NA_real_, RR = NA_real_)
  openxlsx::writeData(wb, "Attack rates", table)
  for (row in seq_len(nrow(table)) + 1L) {
    openxlsx::writeFormula(wb, "Attack rates", sprintf('IF(OR(B%d="",C%d="",C%d=0),"NE",B%d/C%d)', row,row,row,row,row), startCol = 6, startRow = row)
    openxlsx::writeFormula(wb, "Attack rates", sprintf('IF(OR(D%d="",E%d="",E%d=0),"NE",D%d/E%d)', row,row,row,row,row), startCol = 7, startRow = row)
    openxlsx::writeFormula(wb, "Attack rates", sprintf('IF(OR(F%d="NE",G%d="NE"),"NE",IF(G%d=0,IF(F%d=0,"NE","Inf"),F%d/G%d))', row,row,row,row,row,row), startCol = 8, startRow = row)
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
