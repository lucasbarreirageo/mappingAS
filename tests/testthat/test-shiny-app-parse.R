test_that("bundled shiny app sources are syntactically valid", {
  shiny_dir <- system.file("shiny", package = "mappingAS")
  skip_if(!nzchar(shiny_dir), "shiny/ not installed")
  r_files <- list.files(shiny_dir, pattern = "\\.[Rr]$", full.names = TRUE)
  expect_gt(length(r_files), 0L)
  for (f in r_files) {
    issue <- tryCatch({ parse(f); NULL },
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w))
    expect_null(issue, info = paste0(basename(f), ": ", issue))
  }
})
