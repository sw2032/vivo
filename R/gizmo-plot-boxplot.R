
gizmo_plot_boxplot_ui <- function(ns){
  fluidPage(h4("Boxplot Graph: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_boxplot_name"), "Assigned to ...", "biostats_Age_hist")),
              column(4,checkboxInput(ns("plot_boxplot_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("plot_boxplot_df"), "Enter data.frame or matrix", "mtcars")),
              column(4,textInput(ns("plot_boxplot_col"), "y: Enter column", "mpg")),
              column(4,textInput(ns("plot_boxplot_col2"), "grp: Enter column", "cyl"))
            ),
            fluidRow(
              column(12, actionLink(ns("advancedmenu"), 'Advanced', icon = icon('caret-right'),
                                    onclick=paste0("
							  var dm = document.getElementById('", ns(paste0("advancedmenu", 'div')) ,"');
							  if (dm.style.display === 'none') {
							    dm.style.display = 'block';
							  } else {
							    dm.style.display = 'none';
							  }")
              )),
              column(12, tags$div(
                fluidRow(
                  column(4,textInput(ns("plot_boxplot_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_boxplot_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_boxplot_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_boxplot_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_boxplot_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_boxplot_auto", value=state$plot_boxplot_auto)
      updateTextInput(session, "plot_boxplot_df", value=state$plot_boxplot_df)
      updateTextInput(session, "plot_boxplot_col", value=state$plot_boxplot_col)
      updateTextInput(session, "plot_boxplot_col", value=state$plot_boxplot_col2)
      updateTextInput(session, "plot_boxplot_name", value=state$plot_boxplot_name)
      updateTextInput(session, "plot_boxplot_main", value=state$plot_boxplot_main)
      updateTextInput(session, "plot_boxplot_sub", value=state$plot_boxplot_sub)
      updateTextInput(session, "plot_boxplot_xlab", value=state$plot_boxplot_xlab)
      updateTextInput(session, "plot_boxplot_ylab", value=state$plot_boxplot_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_boxplot_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_boxplot_df, input$plot_boxplot_col, input$plot_boxplot_name, input$plot_boxplot_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_boxplot_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_boxplot_df"]], "_", input[["plot_boxplot_col"]],"_plot_boxplot")
        updateTextInput(session, "plot_boxplot_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_boxplot_name"]], " <- boxplot(", input[["plot_boxplot_col"]], " ~ ", input[["plot_boxplot_col2"]], ', data=', input[["plot_boxplot_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_boxplot_main"]])>0)){
                           paste0(", main='", input[["plot_boxplot_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_boxplot_sub"]])>0)){
                           paste0(", sub='", input[["plot_boxplot_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_boxplot_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_boxplot_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_boxplot_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_boxplot_ylab"]], "'" )
                         }else{
                           ""
                         },
                         ")\n"),
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      plot_boxplot_name=input[["plot_boxplot_name"]],
      plot_boxplot_auto=input[["plot_boxplot_auto"]],
      plot_boxplot_df=input[["plot_boxplot_df"]],
      plot_boxplot_col=input[["plot_boxplot_col"]],
      plot_boxplot_col2=input[["plot_boxplot_col2"]],
      plot_boxplot_main=input[["plot_boxplot_main"]],
      plot_boxplot_sub=input[["plot_boxplot_sub"]],
      plot_boxplot_xlab=input[["plot_boxplot_xlab"]],
      plot_boxplot_ylab=input[["plot_boxplot_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_boxplot <- list(
  ui=gizmo_plot_boxplot_ui,
  server=gizmo_plot_boxplot_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_boxplot with run_standalone
#'
#' @export
run_gizmo_plot_boxplot <- function() run_standalone("plot_boxplot")
