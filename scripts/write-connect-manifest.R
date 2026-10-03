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

manifest <- jsonlite::fromJSON("manifest.json", simplifyVector = FALSE)
lock <- jsonlite::fromJSON("renv.lock", simplifyVector = FALSE)
stopifnot(
  identical(manifest$platform, "4.6.0"),
  identical(manifest$metadata$appmode, "shiny"),
  setequal(names(manifest$files), files)
)
for (file in names(manifest$files)) {
  stopifnot(identical(unname(tools::md5sum(file)), manifest$files[[file]]$checksum))
}
for (package in names(manifest$packages)) {
  stopifnot(identical(
    manifest$packages[[package]]$description$Version,
    lock$Packages[[package]]$Version
  ))
  # Omit installation timestamps so locked packages regenerate consistently.
  manifest$packages[[package]]$description$Built <- NULL
}
jsonlite::write_json(manifest, "manifest.json", auto_unbox = TRUE,
                     pretty = TRUE, null = "null")
cat("Verified manifest:", length(manifest$files), "files and",
    length(manifest$packages), "locked packages under R", manifest$platform, "\n")
