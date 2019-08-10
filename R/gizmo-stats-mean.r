
gizmo_stats_mean_ui <- function(ns){
  fluidPage(h4("Mean: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_mean_name"), "string name","John")),
              column(4,textInput(ns("stats_mean"), "string","My name is Johnny"))
            )
  )
}

gizmo_stats_mean_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "stats_mean_name", value=state$stats_mean_name)
      updateTextInput(session, "stats_mean", value=state$stats_mean)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("<!-- user may enter a string into global environment -->", "\n",
                  "```{r} \n",
                  input[["stats_mean_name"]]," <- ", "'", input[["stats_mean"]] , "'" , "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_mean=input[["stats_mean"]],
      stats_mean_name=input[["stats_mean_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_mean <- list(
  ui=gizmo_stats_mean_ui,
  server=gizmo_stats_mean_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_mean with run_standalone
#'
#' @export
run_gizmo_stats_mean <- function() run_standalone("stats_mean")
