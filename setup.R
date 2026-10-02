# Install from the reviewed lockfile into an isolated project library.
if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak", repos = "https://cloud.r-project.org")
if (!requireNamespace("renv", quietly = TRUE)) pak::pak("renv")
renv::restore(prompt = FALSE)
renv::activate()
