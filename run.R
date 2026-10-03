# Run from the project directory: Rscript run.R
if (.Platform$OS.type == "windows" && !l10n_info()[["UTF-8"]]) Sys.setlocale("LC_CTYPE", "English_United States.utf8")
if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
host <- Sys.getenv("FIELDNOTES_HOST", "127.0.0.1")
port <- suppressWarnings(as.integer(Sys.getenv("PORT", "3874")))
if (is.na(port) || port < 1L || port > 65535L) stop("PORT must be between 1 and 65535.")
shiny::runApp(".", host = host, port = port, launch.browser = interactive())
