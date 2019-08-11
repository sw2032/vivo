
gizmo_plot_scatter_ui <- function(ns){
  fluidPage(h4("Scatter Graph: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_scatter_name"), "Assigned to ...", "biostats_Age_hist")),
              column(4,checkboxInput(ns("plot_scatter_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("plot_scatter_df"), "X: Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_scatter_col"), "Enter column, if data.frame or matrix", "Age"))
            ),
            fluidRow(
              column(4,textInput(ns("plot_scatter_df2"), "Y: Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_scatter_col2"), "Enter column, if data.frame or matrix", "Age"))
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
                  column(4,textInput(ns("plot_scatter_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_scatter_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_scatter_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_scatter_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_scatter_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_scatter_auto", value=state$plot_scatter_auto)
      updateTextInput(session, "plot_scatter_df", value=state$plot_scatter_df)
      updateTextInput(session, "plot_scatter_col", value=state$plot_scatter_col)
      updateTextInput(session, "plot_scatter_df", value=state$plot_scatter_df2)
      updateTextInput(session, "plot_scatter_col", value=state$plot_scatter_col2)
      updateTextInput(session, "plot_scatter_name", value=state$plot_scatter_name)
      updateTextInput(session, "plot_scatter_main", value=state$plot_scatter_main)
      updateTextInput(session, "plot_scatter_sub", value=state$plot_scatter_sub)
      updateTextInput(session, "plot_scatter_xlab", value=state$plot_scatter_xlab)
      updateTextInput(session, "plot_scatter_ylab", value=state$plot_scatter_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_scatter_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_scatter_df, input$plot_scatter_col, input$plot_scatter_name, input$plot_scatter_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_scatter_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_scatter_df"]], "_", input[["plot_scatter_col"]],"_plot_scatter")
        updateTextInput(session, "plot_scatter_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_scatter_name"]], " <- plot(x=", input[["plot_scatter_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_scatter_col"]])>0)){
                           paste0("[['", input[["plot_scatter_col"]], "']]" )
                         }else{
                           ""
                         }, 
						 ", y=", input[["plot_scatter_df2"]],
                         if(isTRUE(stringr:::str_length(input[["plot_scatter_col2"]])>0)){
                           paste0("[['", input[["plot_scatter_col2"]], "']]" )
                         }else{
                           ""
                         }, 
                         if(isTRUE(stringr:::str_length(input[["plot_scatter_main"]])>0)){
                           paste0(", main='", input[["plot_scatter_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_scatter_sub"]])>0)){
                           paste0(", sub='", input[["plot_scatter_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_scatter_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_scatter_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_scatter_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_scatter_ylab"]], "'" )
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
      plot_scatter_name=input[["plot_scatter_name"]],
      plot_scatter_auto=input[["plot_scatter_auto"]],
      plot_scatter_df=input[["plot_scatter_df"]],
      plot_scatter_col=input[["plot_scatter_col"]],
      plot_scatter_df2=input[["plot_scatter_df2"]],
      plot_scatter_col2=input[["plot_scatter_col2"]],
      plot_scatter_main=input[["plot_scatter_main"]],
      plot_scatter_sub=input[["plot_scatter_sub"]],
      plot_scatter_xlab=input[["plot_scatter_xlab"]],
      plot_scatter_ylab=input[["plot_scatter_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_scatter <- list(
  ui=gizmo_plot_scatter_ui,
  server=gizmo_plot_scatter_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_scatter with run_standalone
#'
#' @export
run_gizmo_plot_scatter <- function() run_standalone("plot_scatter")
