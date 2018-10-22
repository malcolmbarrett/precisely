#' Launch precisely Shiny app
#'
#' `launch_precisely_app()` launches a Shiny app to calculate and plot
#' precision, sample size, and upper limit calculations.
#'
#' @export
launch_precisely_app <- function() {
  app_dir <- system.file("shiny_app", "precisely", package = "precisely")
  if (app_dir == "") {
    stop("Shiny app not found. Try re-installing `precisely`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
