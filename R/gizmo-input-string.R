
gizmo_input_string_ui <- function(ns){
  textInput(ns("input_string"), "input")
}

gizmo_input_string_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "input_string", value=state$input_string)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("## ", input[["input_string"]])
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      input_string=input[["input_string"]],
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


run_input_string <- function() run_standalone("input_string")
