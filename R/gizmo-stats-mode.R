
gizmo_stats_mode_ui <- function(ns){
  fluidPage(h4("Mode: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_mode_name"), "Assigned to ...", "biostats_Age_mode")),
              column(4,checkboxInput(ns("stats_mode_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_mode_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_mode_col"), "Enter column, if data.frame or matrix", "Age"))
            )
  )
}

gizmo_stats_mode_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_mode_auto", value=state$stats_mode_auto)
      updateTextInput(session, "stats_mode_df", value=state$stats_mode_df)
      updateTextInput(session, "stats_mode_col", value=state$stats_mode_col)
      updateTextInput(session, "stats_mode_name", value=state$stats_mode_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_mode_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_mode_df, input$stats_mode_col, input$stats_mode_name, input$stats_mode_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_mode_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_mode_df"]], "_", input[["stats_mode_col"]],"_stats_mode")
        updateTextInput(session, "stats_mode_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_mode_col"]])>0) ){
                    paste0(input[["stats_mode_name"]], " <- mode(", input[["stats_mode_df"]], "[['", input[["stats_mode_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_mode_name"]], " <- mode(", input[["stats_mode_df"]], ")\n")
                  },
                  "print(", input[["stats_mode_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_mode_name=input[["stats_mode_name"]],
      stats_mode_auto=input[["stats_mode_auto"]],
      stats_mode_df=input[["stats_mode_df"]],
      stats_mode_col=input[["stats_mode_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_mode <- list(
  ui=gizmo_stats_mode_ui,
  server=gizmo_stats_mode_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_mode with run_standalone
#'
#' @export
run_gizmo_stats_mode <- function() run_standalone("stats_mode")

