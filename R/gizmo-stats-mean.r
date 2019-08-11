
gizmo_stats_mean_ui <- function(ns){
  fluidPage(h4("Mean: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_mean_name"), "Assigned to ...", "biostats_Age_mean")),
              column(4,checkboxInput(ns("stats_mean_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_mean_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_mean_col"), "Enter column, if data.frame or matrix", "Age"))
            )
  )
}

gizmo_stats_mean_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_mean_auto", value=state$stats_mean_auto)
      updateTextInput(session, "stats_mean_df", value=state$stats_mean_df)
      updateTextInput(session, "stats_mean_col", value=state$stats_mean_col)
      updateTextInput(session, "stats_mean_name", value=state$stats_mean_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_mean_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_mean_df, input$stats_mean_col, input$stats_mean_name, input$stats_mean_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_mean_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_mean_df"]], "_", input[["stats_mean_col"]],"_stats_mean")
        updateTextInput(session, "stats_mean_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_mean_col"]])>0) ){
                    paste0(input[["stats_mean_name"]], " <- mean(", input[["stats_mean_df"]], "[['", input[["stats_mean_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_mean_name"]], " <- mean(", input[["stats_mean_df"]], ")\n")
                  },
                  "print(", input[["stats_mean_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_mean_name=input[["stats_mean_name"]],
      stats_mean_auto=input[["stats_mean_auto"]],
      stats_mean_df=input[["stats_mean_df"]],
      stats_mean_col=input[["stats_mean_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_mean <- list(
  ui=gizmo_stats_mean_ui,
  server=gizmo_stats_mean_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_mean with run_standalone
#'
#' @export
run_gizmo_stats_mean <- function() run_standalone("stats_mean")