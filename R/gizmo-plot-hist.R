
gizmo_plot_hist_ui <- function(ns){
  fluidPage(h4("Histogram: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_hist_name"), "Assigned to ...", "biostats_Age_hist")),
              column(4,checkboxInput(ns("plot_hist_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("plot_hist_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_hist_col"), "Enter column, if data.frame or matrix", "Age"))
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
                  column(4,textInput(ns("plot_hist_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_hist_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_hist_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_hist_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_hist_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_hist_auto", value=state$plot_hist_auto)
      updateTextInput(session, "plot_hist_df", value=state$plot_hist_df)
      updateTextInput(session, "plot_hist_col", value=state$plot_hist_col)
      updateTextInput(session, "plot_hist_name", value=state$plot_hist_name)
      updateTextInput(session, "plot_hist_main", value=state$plot_hist_main)
      updateTextInput(session, "plot_hist_sub", value=state$plot_hist_sub)
      updateTextInput(session, "plot_hist_xlab", value=state$plot_hist_xlab)
      updateTextInput(session, "plot_hist_ylab", value=state$plot_hist_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_hist_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_hist_df, input$plot_hist_col, input$plot_hist_name, input$plot_hist_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_hist_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_hist_df"]], "_", input[["plot_hist_col"]],"_plot_hist")
        updateTextInput(session, "plot_hist_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_hist_name"]], " <- hist(", input[["plot_hist_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_hist_col"]])>0)){
                           paste0("[['", input[["plot_hist_col"]], "']]" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_hist_main"]])>0)){
                           paste0(", main='", input[["plot_hist_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_hist_sub"]])>0)){
                           paste0(", sub='", input[["plot_hist_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_hist_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_hist_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_hist_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_hist_ylab"]], "'" )
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
      plot_hist_name=input[["plot_hist_name"]],
      plot_hist_auto=input[["plot_hist_auto"]],
      plot_hist_df=input[["plot_hist_df"]],
      plot_hist_col=input[["plot_hist_col"]],
      plot_hist_main=input[["plot_hist_main"]],
      plot_hist_sub=input[["plot_hist_sub"]],
      plot_hist_xlab=input[["plot_hist_xlab"]],
      plot_hist_ylab=input[["plot_hist_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_hist <- list(
  ui=gizmo_plot_hist_ui,
  server=gizmo_plot_hist_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_hist with run_standalone
#'
#' @export
run_gizmo_plot_hist <- function() run_standalone("plot_hist")
