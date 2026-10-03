test_that("the guide script URL changes with its contents", {
  withr::local_dir(root)
  withr::local_envvar(FIELDNOTES_CHAT_UI = "scripted")
  library(shiny)
  library(bslib)
  html <- htmltools::renderTags(app_ui())$head
  expected <- paste0('src="guide.js?v=', unname(tools::md5sum("www/guide.js")), '"')
  expect_match(html, expected, fixed = TRUE)
})
