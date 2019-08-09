#' launch_vivid
#' 
#' Second entry point for vivid package
#' 
#' @param launch.browser passed to \code{shiny::runApp}
#' @param ... passed to \code{shiny::runApp}
#' @export

launch_vivid <- function(launch.browser, ...){
  ui <- vivid_ui()
  server <- vivid_server()
  shiny::runApp(shiny::shinyApp(ui, server), launch.browser = launch.browser, ...)
}