#' vivid
#' 
#' Entry point for vivid package
#' 
#' @param child_process \code{TRUE} for production mode; \code{FALSE} for debug mode
#' @param launch.browser passed to \code{launch_vivid} to \code{shiny::runApp}
#' @export

vivid <- function(child_process = FALSE, launch.browser = TRUE, ...){
  if(child_process)
    return(launch_vivid_child_server(...))
  .globals$vivid_server$parent_queue <- ipc::queue()
  .globals$vivid_server$child_queue <- ipc::queue()
  .globals$remote_r <- QueueLinkedR$new(parent_queue(), child_queue())
  .globals$vivid_server$parent_queue$consumer$start(env=.GlobalEnv)
  launch_vivid(launch.browser, ...)
}