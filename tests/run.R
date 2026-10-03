if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
withr::with_envvar(c(FIELDNOTES_DIALOGUE_PROVIDER = "scripted",
  GOOGLE_API_KEY = NA_character_, GEMINI_API_KEY = NA_character_),
  testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE))
