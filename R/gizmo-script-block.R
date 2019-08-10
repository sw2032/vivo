gizmo_script_block_ui <- function(ns){
  tagAppendAttributes(
    textAreaInput(
      ns("markdown"),
      'Enter R Script code ...',
      width="100%",
      height="200px",
      resize="vertical"
    ),
    style = 'width: 100%;'
  )
}

gizmo_script_block_server <- function(input, output, session, state=NULL){
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  input[["markdown"]] , "\n",
                  "```\n")
    txt
  })

  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextAreaInput(session, "markdown", value=state$markdown)
    })
  }

  get_state <- function(){
    list(
      markdown=input[["markdown"]],
      `__version__`="0.0.1"
    )
  }
  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$script_block <- list(
  ui=gizmo_script_block_ui,
  server=gizmo_script_block_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_script_block with run_standalone
#'
#' @export
run_gizmo_script_block <- function() run_standalone("script_block")
