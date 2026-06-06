#' Launch the geoConvBR Shiny application
#'
#' Starts the interactive app for uploading occurrences, computing EOO/AOO and
#' MapBiomas habitat conversion, and exploring/exporting the results.
#'
#' @param launch.browser Logical; open in the default browser (default
#'   \code{TRUE}).
#' @param ... Passed to \code{shiny::runApp()}.
#' @return Invisibly \code{NULL}; called for its side effect.
#' @examples
#' \dontrun{ run_app() }
#' @export
run_app <- function(launch.browser = TRUE, ...) {
  for (p in c("shiny", "bslib", "leaflet", "DT")) {
    if (!requireNamespace(p, quietly = TRUE)) {
      stop("Package '", p, "' is required to run the app.", call. = FALSE)
    }
  }
  app_dir <- system.file("shiny", package = "geoConvBR")
  if (app_dir == "" || !file.exists(file.path(app_dir, "app.R"))) {
    stop("Could not locate the Shiny app. Reinstall geoConvBR.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
