root <- normalizePath(file.path("..", ".."), winslash = "/")
for (file in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) source(file, encoding = "UTF-8")
scenario <- function() read_scenario(file.path(root, "scenarios", "potluck-01", "scenario.yaml"))
full_plan <- function() {
  s <- new_state(scenario())
  s <- interview(s, "organizer", "menu and missing walk-ins")
  s <- request_task(s, "guest_list")
  s <- advance_time(s, 3)
  s <- request_task(s, "team_interviews", "all", c("demographics", "symptoms", "onset"), names(food_labels(s$sc)))
  advance_time(s, 25)
}
