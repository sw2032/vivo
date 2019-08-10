
gizmo_input_string_ui <- function(ns){
  fluidRow(
    column(4,textInput(ns("input_string_name"), "string name","John")),
    column(4,textInput(ns("input_string"), "string","My name is John"))
  )
}

gizmo_input_string_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "input_string_name", value=state$input_string_name)
      updateTextInput(session, "input_string", value=state$input_string)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  input[["input_string_name"]]," <- ", "'", input[["input_string"]] , "'" , "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      input_string=input[["input_string"]],
      input_string_name=input[["input_string_name"]],
      `__version__` = "1.0"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$input_string <- list(
  ui=gizmo_input_string_ui,
  server=gizmo_input_string_server,
  library="vivid",
  opts=list()
)

#' run_input_string with run_standalone
#'
#' @export
run_input_string <- function() run_standalone("input_string")
