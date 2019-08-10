
gizmo_input_number_ui <- function(ns){
  fluidPage(h4("Input number: Enter a number into global environment"),
            fluidRow(
              column(4,textInput(ns("input_number_name"), "number name","age")),
              column(4,textInput(ns("input_number"), "number","16"))
            )
  )
}

gizmo_input_number_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "input_number_name", value=state$input_number_name)
      updateTextInput(session, "input_number", value=state$input_number)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("<!-- user may enter a number into global environment -->", "\n",
                  "```{r} \n",
                  input[["input_number_name"]]," <- ", input[["input_number"]] , "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      input_number=input[["input_number"]],
      input_number_name=input[["input_number_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$input_number <- list(
  ui=gizmo_input_number_ui,
  server=gizmo_input_number_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_input_number with run_standalone
#'
#' @export
run_gizmo_input_number <- function() run_standalone("input_number")
