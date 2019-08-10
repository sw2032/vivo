
gizmo_toss_coin_ui <- function(ns){
  fluidPage(h4("Input String: Enter a string into global environment"),
            fluidRow(
              column(4,textInput(ns("toss_coin_name"), "string name","John")),
              column(4,textInput(ns("toss_coin"), "string","My name is Johnny"))
            )
  )
}

gizmo_toss_coin_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "toss_coin_name", value=state$toss_coin_name)
      updateTextInput(session, "toss_coin", value=state$toss_coin)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  input[["toss_coin_name"]]," <- ", "'", input[["toss_coin"]] , "'" , "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      toss_coin=input[["toss_coin"]],
      toss_coin_name=input[["toss_coin_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$toss_coin <- list(
  ui=gizmo_toss_coin_ui,
  server=gizmo_toss_coin_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_toss_coin with run_standalone
#'
#' @export
run_gizmo_toss_coin <- function() run_standalone("toss_coin")
