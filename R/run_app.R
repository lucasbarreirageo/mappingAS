#' Launch the mappingAS Shiny application
#'
#' Starts the interactive app for uploading occurrences, computing EOO/AOO and
#' MapBiomas habitat conversion, and exploring/exporting the results.
#'
#' @param launch.browser Logical; open in the default browser (default
#'   \code{TRUE}).
#' @param ... Passed to \code{shiny::runApp()}.
#' @return Invisibly \code{NULL}; called for its side effect.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  for (p in c("shiny", "bslib", "leaflet", "DT", "htmltools", "htmlwidgets",
              "ggplot2", "plotly")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required to run the app.", call. = FALSE)
    }
  }
  app_dir <- system.file("shiny", package = "mappingAS")
  if (app_dir == "" || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not locate the Shiny app. Reinstall mappingAS.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
