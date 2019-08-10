gizmo_markdown_block_ui <- function(ns){
  tagAppendAttributes(
    textAreaInput(
      ns("markdown"),
      NULL,
      width="100%",
      height="400px",
      resize="vertical",
    ),
    style = 'width: 100%;'
  )
}

gizmo_markdown_block_server <- function(input, output, session, state=NULL){
  txt_react <- reactive({
    txt <- input[["markdown"]]
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


.globals$gizmos$markdown_block <- list(
  ui=gizmo_markdown_block_ui,
  server=gizmo_markdown_block_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_markdown_block with run_standalone
#'
#' @export
run_gizmo_markdown_block <- function() run_standalone("markdown_block")
