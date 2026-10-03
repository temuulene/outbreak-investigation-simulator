# Authenticate rsconnect locally first; never put tokens in this repository.
if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
if (!requireNamespace("rsconnect", quietly = TRUE)) stop("Install rsconnect with pak::pak('rsconnect').")
account <- Sys.getenv("FIELDNOTES_SHINYAPPS_ACCOUNT")
if (!nzchar(account)) stop("Set FIELDNOTES_SHINYAPPS_ACCOUNT to an authenticated account.")
files <- c("app.R", "DESCRIPTION", "renv.lock", unlist(lapply(c("R", "www", "scenarios", "starter"), list.files, recursive = TRUE, full.names = TRUE)))
stopifnot(!any(grepl("(^|/)(artifacts|\\.git|rsconnect)(/|$)|\\.Renviron|\\.env", files)))
rsconnect::deployApp(appDir = ".", appFiles = files, appName = "fieldnotes", account = account, launch.browser = FALSE)
