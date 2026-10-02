# Run from the project directory: Rscript run.R
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) {
  Sys.setlocale("LC_CTYPE", "English_United States.utf8")
}
shiny::runApp(".", host = "127.0.0.1", port = 3874, launch.browser = interactive())
