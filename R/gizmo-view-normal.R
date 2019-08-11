
gizmo_view_normal_ui <- function(ns){
  fluidPage(h4("Normal Distribution: A view Tool"),
            fluidRow(
				column(4,numericInput(ns("view_normal_mean"), "mean ...", value = 0, min = -10, max = 10, step = 1)),
				column(4,numericInput(ns("view_normal_std"), "std ...", value = 1, min = 0, max = 1000, step = 1)),
				column(2,numericInput(ns("view_normal_min"), "plot with min ...", value = -5, min = -1000, max = 1000, step = 1)),
				column(2,numericInput(ns("view_normal_max"), "plot with min ...", value = 5, min = -1000, max = 1000, step = 1))
            )
  )
}

gizmo_view_normal_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
	  updateNumericInput(session, "view_normal_mean", value=state$view_normal_mean)
	  updateNumericInput(session, "view_normal_std", value=state$view_normal_std)
	  updateNumericInput(session, "view_normal_min", value=state$view_normal_min)
	  updateNumericInput(session, "view_normal_max", value=state$view_normal_max)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "view_normal_auto", value=TRUE)
    initi$initing=FALSE
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  "curve(dnorm(x, mean=",input$view_normal_mean, ", sd=", input$view_normal_std, "), ", input$view_normal_min, ", ", input$view_normal_max, ")", "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      view_normal_mean=input[["view_normal_mean"]],
	  view_normal_std=input[["view_normal_std"]],
	  view_normal_min=input[["view_normal_min"]],
	  view_normal_max=input[["view_normal_max"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$view_normal <- list(
  ui=gizmo_view_normal_ui,
  server=gizmo_view_normal_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_view_normal with run_standalone
#'
#' @export
run_gizmo_view_normal <- function() run_standalone("view_normal")
