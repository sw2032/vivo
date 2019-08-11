
gizmo_plot_dotchart_ui <- function(ns){
  fluidPage(h4("Dotchart: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_dotchart_name"), "Assigned to ...", "biostats_Age_mean")),
              column(4,checkboxInput(ns("plot_dotchart_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("plot_dotchart_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_dotchart_col"), "Enter column, if data.frame or matrix", "Age"))
            )
  )
}

gizmo_plot_dotchart_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_dotchart_auto", value=state$plot_dotchart_auto)
      updateTextInput(session, "plot_dotchart_df", value=state$plot_dotchart_df)
      updateTextInput(session, "plot_dotchart_col", value=state$plot_dotchart_col)
      updateTextInput(session, "plot_dotchart_name", value=state$plot_dotchart_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_dotchart_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_dotchart_df, input$plot_dotchart_col, input$plot_dotchart_name, input$plot_dotchart_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_dotchart_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_dotchart_df"]], "_", input[["plot_dotchart_col"]],"_plot_dotchart")
        updateTextInput(session, "plot_dotchart_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["plot_dotchart_col"]])>0) ){
                    paste0(input[["plot_dotchart_name"]], " <- dotchart(", input[["plot_dotchart_df"]], "[['", input[["plot_dotchart_col"]], "']]", ", labels=row.names(", input[["plot_dotchart_df"]], "))\n")
                  }else{
                    paste0(input[["plot_dotchart_name"]], " <- dotchart(", input[["plot_dotchart_df"]], ")\n")
                  },
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      plot_dotchart_name=input[["plot_dotchart_name"]],
      plot_dotchart_auto=input[["plot_dotchart_auto"]],
      plot_dotchart_df=input[["plot_dotchart_df"]],
      plot_dotchart_col=input[["plot_dotchart_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_dotchart <- list(
  ui=gizmo_plot_dotchart_ui,
  server=gizmo_plot_dotchart_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_dotchart with run_standalone
#'
#' @export
run_gizmo_plot_dotchart <- function() run_standalone("plot_dotchart")
