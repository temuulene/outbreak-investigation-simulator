# Generate with the R version supported by the reviewed Connect Cloud target.
if (getRversion() != "4.6.0") {
  stop("Generate this manifest under R 4.6.0; see docs/deployment.md.")
}
if (dir.exists("artifacts/library")) {
  .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
}
files <- c(
  "app.R", "DESCRIPTION", "renv.lock",
  list.files("R", recursive = TRUE, full.names = TRUE),
  list.files("www", recursive = TRUE, full.names = TRUE),
  list.files("scenarios", recursive = TRUE, full.names = TRUE),
  list.files("starter", recursive = TRUE, full.names = TRUE)
)
stopifnot(!any(grepl("(^|/)(artifacts|\\.git|rsconnect)(/|$)|\\.Renviron|\\.env", files)))
rsconnect::writeManifest(
  appDir = ".",
  appFiles = sort(files),
  appPrimaryDoc = "app.R",
  appMode = "shiny"
)
