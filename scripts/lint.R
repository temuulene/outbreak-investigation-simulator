# Static checks for the app, tests, and scripts. Rules are in .lintr.
paths <- c("app.R", "run.R", "setup.R", list.files(c("R", "scripts", "starter", "tests"),
  pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
findings <- do.call(rbind, lapply(paths, function(path) as.data.frame(lintr::lint(path))))
if (nrow(findings)) {
  print(findings[c("filename", "line_number", "linter", "message")], row.names = FALSE, right = FALSE)
  stop(nrow(findings), " lint findings. Fix them or adjust .lintr.", call. = FALSE)
}
cat("No lint findings in", length(paths), "files.\n")
