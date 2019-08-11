
gizmo_stats_median_ui <- function(ns){
  fluidPage(h4("Median: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_median_name"), "Assigned to ...", "biostats_Age_median")),
              column(4,checkboxInput(ns("stats_median_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_median_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_median_col"), "Enter column, if data.frame or matrix", "Age"))
            )
  )
}

gizmo_stats_median_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_median_auto", value=state$stats_median_auto)
      updateTextInput(session, "stats_median_df", value=state$stats_median_df)
      updateTextInput(session, "stats_median_col", value=state$stats_median_col)
      updateTextInput(session, "stats_median_name", value=state$stats_median_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_median_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_median_df, input$stats_median_col, input$stats_median_name, input$stats_median_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_median_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_median_df"]], "_", input[["stats_median_col"]],"_stats_median")
        updateTextInput(session, "stats_median_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_median_col"]])>0) ){
                    paste0(input[["stats_median_name"]], " <- median(", input[["stats_median_df"]], "[['", input[["stats_median_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_median_name"]], " <- median(", input[["stats_median_df"]], ")\n")
                  },
                  "print(", input[["stats_median_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_median_name=input[["stats_median_name"]],
      stats_median_auto=input[["stats_median_auto"]],
      stats_median_df=input[["stats_median_df"]],
      stats_median_col=input[["stats_median_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_median <- list(
  ui=gizmo_stats_median_ui,
  server=gizmo_stats_median_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_median with run_standalone
#'
#' @export
run_gizmo_stats_median <- function() run_standalone("stats_median")

