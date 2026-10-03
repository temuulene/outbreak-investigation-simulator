if (dir.exists("artifacts/library")) .libPaths(c(normalizePath("artifacts/library"), .libPaths()))
testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = TRUE)
