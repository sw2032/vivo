
gizmo_helloworld_ui <- function(ns){
  fluidPage(h4("HelloWorld: A Gizmo Test"),
    textInput(ns("helloworld"), "input")
  )
}

gizmo_helloworld_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "helloworld", value=state$helloworld)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("## ", input[["helloworld"]],"\n```{r}\ntest_var <- if(!exists('test_var')) 1 else test_var+1\nprint(test_var)\ndata(mtcars)\nmtcars\n```\nThe above is a test")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      helloworld=input[["helloworld"]],
      `__version__` = "0.0.1"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$helloworld <- list(
  ui=gizmo_helloworld_ui,
  server=gizmo_helloworld_server,
  library="vivid",
  opts=list()
)

#' run_gizmo_helloworld with run_standalone
#'
#' @export
run_gizmo_helloworld <- function() run_standalone("helloworld")
