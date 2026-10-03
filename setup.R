# Restore the reviewed lockfile without changing user or global libraries.
dir.create("artifacts/library", recursive = TRUE, showWarnings = FALSE)
.libPaths(c(normalizePath("artifacts/library"), .libPaths()))
Sys.setenv(RENV_PATHS_CACHE = file.path(getwd(), "artifacts/renv-cache"), RENV_PATHS_ROOT = file.path(getwd(), "artifacts/renv-root"))
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak", lib = "artifacts/library", repos = "https://cloud.r-project.org")
if (!requireNamespace("renv", quietly = TRUE)) pak::pkg_install("renv", lib = "artifacts/library", ask = FALSE)
renv::restore(library = "artifacts/library", prompt = FALSE)
