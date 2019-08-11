
gizmo_toss_coin_ui <- function(ns){
  fluidPage(h4("Toss Coin: Use sample() to simulate"),
            fluidRow(
              column(4,textInput(ns("toss_coin_name"), "variable name", "coin_toss_result")),
              column(4,numericInput(ns("toss_coin"), "tossed how many times ...", value =5, min =0, max =100000, step =1))
            )
  )
}

gizmo_toss_coin_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "toss_coin_name", value=state$toss_coin_name)
      updateNumericInput(session, "toss_coin", value=state$toss_coin)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  input[["toss_coin_name"]]," <- ", "sample(c('Head', 'Tail'), ", input[["toss_coin"]] , ", replace=T)" , "\n",
				  "print(", input[["toss_coin_name"]], ")\n",
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
