complete_follow_up <- function(id = "potluck-01") {
  s <- start_session(id, directory = file.path(root, "scenarios"))
  s <- advance_time(s, 72)
  collect_records(s, s$reported$id, domain_fields(c("demographics", "symptoms", "onset"), names(food_labels(s$sc))))
}

test_that("a person restriction removes guests from both numerator and denominator", {
  s <- complete_follow_up()
  everyone <- line_list(s)
  s$definition$person <- "rsvp"
  restricted <- line_list(s)
  expect_true(all(restricted$case_status[!restricted$on_list] == "Excluded"))
  expect_false(any(restricted$case_status[restricted$on_list] == "Excluded"))
  vehicle <- s$sc$truth$vehicle
  cohort <- attack_rates(everyone[everyone$on_list, ], vehicle)
  rates <- attack_rates(restricted, vehicle)
  expect_identical(rates[c("exposed_cases", "exposed_total", "unexposed_cases", "unexposed_total")],
    cohort[c("exposed_cases", "exposed_total", "unexposed_cases", "unexposed_total")])
  expect_equal(rates$excluded_unknown, sum(!everyone$on_list))
})

test_that("the interview roster and its conversations survive later reports", {
  s <- start_session("potluck-01", directory = file.path(root, "scenarios"))
  roster <- available_characters(s)
  first_ill <- unname(roster[3])
  s <- interview(s, first_ill, "How have you been feeling?")
  for (hours in c(6, 22, 44)) {
    s <- advance_time(s, hours)
    expect_identical(available_characters(s), roster)
  }
  expect_silent(s <- interview(s, first_ill, "When did it start?"))
  expect_length(s$chats[[first_ill]], 2)
  expect_lte(length(roster), s$sc$interviews$max)
})

test_that("foods follow the menu order so the vehicle is never listed first", {
  for (id in c("potluck-01", "potluck-02")) {
    s <- complete_follow_up(id)
    expect_identical(names(food_labels(s$sc)), s$sc$menu_order)
    expect_false(names(food_labels(s$sc))[1] == s$sc$truth$vehicle)
    foods <- intersect(names(s$collected), names(food_labels(s$sc)))
    expect_identical(foods, s$sc$menu_order)
    expect_identical(attack_rates(line_list(s), names(food_labels(s$sc)))$food, s$sc$menu_order)
  }
})

test_that("calculation checks accept sound methods and name common slips", {
  d <- line_list(complete_follow_up())
  for (food in c("chicken_salad", "coleslaw", "fried_rice", "cake", "punch")) {
    r <- attack_rates(d, food)
    exact <- check_calculation(d, food, r$attack_rate_exposed, r$attack_rate_unexposed, r$relative_risk)
    expect_true(exact$ok)
    expect_match(exact$message, "All three")
    e <- round(r$attack_rate_exposed, 2); u <- round(r$attack_rate_unexposed, 2)
    expect_true(check_calculation(d, food, e, u, round(e / u, 2))$ok, info = food)
  }
  r <- attack_rates(d, "chicken_salad")
  rounded <- check_calculation(d, "chicken_salad", .51, .09, 5.67)
  expect_true(rounded$ok)
  expect_match(rounded$message, "Round only at the end")
  expect_false(check_calculation(d, "chicken_salad", .51, .09, 4.5)$ok)
  cases <- r$exposed_cases + r$unexposed_cases
  share <- check_calculation(d, "chicken_salad", r$exposed_cases / cases, r$unexposed_cases / cases, 9.5)
  expect_false(share$ok)
  expect_match(share$message, "share of all cases")
  expect_match(share$message, "of 3 values agree")
  percent <- check_calculation(d, "chicken_salad", 51.4, r$attack_rate_unexposed, r$relative_risk)
  expect_false(percent$ok)
  expect_match(percent$message, "not a percentage")
  expect_match(percent$message, "2 of 3")
  expect_false(check_calculation(d, "chicken_salad", NA, NULL, "x")$ok)
  expect_identical(names(share), c("ok", "message"))
})

test_that("the pooled relative risk matches hand calculation and exposes confounding", {
  one <- data.frame(exposed_cases = 18, exposed_total = 37, unexposed_cases = 2, unexposed_total = 23)
  single <- mantel_haenszel_rr(one)
  expect_equal(single[["rr"]], (18 / 37) / (2 / 23))
  se <- sqrt(1 / 18 - 1 / 37 + 1 / 2 - 1 / 23)
  expect_equal(single[["lower"]], exp(log(single[["rr"]]) - 1.96 * se))
  # Two strata: (a, n1, c, n0) = (8, 20, 2, 20) and (30, 40, 6, 20).
  two <- data.frame(exposed_cases = c(8, 30), exposed_total = c(20, 40), unexposed_cases = c(2, 6), unexposed_total = c(20, 20))
  expect_equal(mantel_haenszel_rr(two)[["rr"]], (8 * 20 / 40 + 30 * 20 / 60) / (2 * 20 / 40 + 6 * 40 / 60))
  empty <- data.frame(exposed_cases = 0, exposed_total = 0, unexposed_cases = 0, unexposed_total = 0)
  expect_true(all(is.na(mantel_haenszel_rr(empty))))
  d <- line_list(complete_follow_up("potluck-02"))
  coleslaw <- stratified_summary(d, "coleslaw", "chicken_salad")
  expect_gte(coleslaw$crude, 1.5)
  expect_lt(coleslaw$adjusted[["rr"]], 1.2)
  expect_lt(coleslaw$adjusted[["lower"]], 1)
  expect_gt(coleslaw$adjusted[["upper"]], 1)
  vehicle <- stratified_summary(d, "chicken_salad", "coleslaw")
  expect_gt(vehicle$adjusted[["lower"]], 2)
  expect_identical(coleslaw$strata, stratified_rates(d, "coleslaw", "chicken_salad"))
})

test_that("case description and onset curve summarize classified cases only", {
  d <- data.frame(case_status = c("Case", "Case", "Non-case", "Unknown", "Case"), diarrhea = c(TRUE, TRUE, TRUE, NA, FALSE),
    fever = c(TRUE, FALSE, FALSE, NA, NA), onset_hours = c(10, 20, 100, NA, 30), age = c(20, 40, 60, 30, 50), sex = c("F", "M", "F", "M", "F"))
  summary <- describe_cases(d)
  expect_equal(summary$cases, 3)
  expect_equal(summary$symptoms$cases, c(2, 1))
  expect_equal(summary$symptoms$asked, c(3, 2))
  expect_equal(unname(summary$incubation), c(10, 20, 30))
  expect_equal(unname(summary$age[["median"]]), 40)
  expect_equal(describe_cases(d[0, ])$cases, 0)
  curve <- onset_counts(d, 6)
  expect_equal(sum(curve$counts["Cases", ]), 3)
  expect_equal(sum(curve$counts[2, ]), 1)
  expect_equal(curve$counts["Cases", 2], c(Cases = 1), ignore_attr = TRUE)
  expect_equal(ncol(curve$counts), length(curve$breaks) - 1)
  expect_gte(max(curve$breaks), 100)
  expect_null(onset_counts(d[c("case_status", "age")], 6))
  expect_match(epi_curve_alt(curve), "3 cases and 1 other")
  expect_match(epi_curve_alt(NULL), "No onset")
  grDevices::pdf(NULL); on.exit(grDevices::dev.off())
  expect_silent(plot_epi_curve(curve))
  full <- onset_counts(line_list(complete_follow_up()), 6)
  expect_equal(sum(full$counts), sum(!is.na(complete_follow_up()$collected$onset_hours)))
})

test_that("laboratory messages claim only what the evidence supports", {
  finding <- function(s, label) Filter(function(e) e$source == label, s$evidence)[[1]]$finding
  s <- request_task(new_state(scenario()), "hold_leftovers")
  s <- advance_time(request_task(s, "leftover_testing"), 73)
  food <- finding(s, "Test preserved leftovers")
  expect_match(food, "No stool isolates are available", fixed = TRUE)
  expect_false(grepl("matches the stool", food, fixed = TRUE))
  expect_match(food, "does not rule that food out", fixed = TRUE)
  s <- advance_time(request_task(s, "stool_samples"), 49)
  expect_match(finding(s, "Request stool samples"), "match the one from the preserved leftovers", fixed = TRUE)
  s <- request_task(new_state(scenario()), "hold_leftovers")
  s <- advance_time(request_task(request_task(s, "stool_samples"), "leftover_testing"), 73)
  expect_match(finding(s, "Test preserved leftovers"), "matches the stool isolates", fixed = TRUE)
  expect_error(take_action(s, "not_an_action", "Reason"), "available action")
})

test_that("the reveal compares complete accurate histories with recalled ones", {
  s <- start_session("potluck-01", directory = file.path(root, "scenarios"))
  reference <- truth_summary(s)
  v <- s$truth[[s$sc$truth$vehicle]]
  expect_equal(reference$actual$exposed_total, sum(v))
  expect_equal(reference$actual$exposed_cases, sum(s$truth$ill & v))
  expect_equal(reference$actual$exposed_total + reference$actual$unexposed_total, nrow(s$truth))
  # Holds for this authored seed; recall error can move a single dataset either way.
  expect_gt(reference$actual$relative_risk, reference$recalled$relative_risk)
})

test_that("scenario files fail with a message that names the problem", {
  write_scenario <- function(change) {
    sc <- yaml::read_yaml(file.path(root, "scenarios", "potluck-01", "scenario.yaml"))
    path <- file.path(tempfile(), "scenario.yaml")
    dir.create(dirname(path))
    yaml::write_yaml(change(sc), path)
    path
  }
  expect_silent(read_scenario(write_scenario(identity)))
  expect_error(read_scenario(write_scenario(function(sc) {sc$interviews <- NULL; sc})), "interviews")
  expect_error(read_scenario(write_scenario(function(sc) {sc$menu_order <- c("cake", "punch"); sc})), "menu_order")
  expect_error(read_scenario(write_scenario(function(sc) {sc$recall$p_forget_item <- 2; sc})), "recall")
  expect_error(read_scenario(write_scenario(function(sc) {sc$truth$vehicle <- "soup"; sc})), "vehicle")
  expect_error(read_scenario(write_scenario(function(sc) {sc$pathogen$symptoms$rash <- .2; sc})), "symptoms")
  expect_error(read_scenario(write_scenario(function(sc) {sc$foods[[1]]$label <- NULL; sc})), "food")
  expect_error(read_scenario(write_scenario(function(sc) {sc$actions[[1]]$justified_if <- "always"; sc})), "justified_if")
  expect_error(read_scenario(write_scenario(function(sc) {sc$pathogen$incubation_hours$p95 <- 12; sc})), "incubation")
  no_order <- read_scenario(write_scenario(function(sc) {sc$menu_order <- NULL; sc}))
  expect_identical(no_order$menu_order, vapply(no_order$foods, `[[`, character(1), "id"))
})

test_that("a seed gives the same records whatever generator the caller uses", {
  reference <- generate_data(scenario())
  old <- RNGkind()
  on.exit(do.call(RNGkind, as.list(old)))
  suppressWarnings(RNGkind("Wichmann-Hill", "Box-Muller", "Rounding"))
  set.seed(1)
  before <- .Random.seed
  expect_identical(generate_data(scenario()), reference)
  expect_identical(.Random.seed, before)
  expect_identical(RNGkind()[1], "Wichmann-Hill")
})

test_that("the starter script runs on a frozen export without naming a food", {
  script <- readLines(file.path(root, "starter", "analysis.R"))
  expect_false(any(grepl("chicken|coleslaw|rice|cake|punch", script)))
  s <- complete_follow_up()
  s$definition$person <- "rsvp"
  dir <- tempfile(); dir.create(dir)
  write.csv(line_list(s), file.path(dir, "potluck-line-list.csv"), row.names = FALSE, na = "")
  env <- new.env()
  withr::with_dir(dir, suppressMessages(utils::capture.output(source(file.path(root, "starter", "analysis.R"), local = env))))
  expect_identical(env$foods, s$sc$menu_order)
  reference <- attack_rates(line_list(s), env$food)
  expect_equal(env$counts$total[env$counts$exposed], reference$exposed_total)
  expect_equal(env$counts$cases[!env$counts$exposed], reference$unexposed_cases)
})
