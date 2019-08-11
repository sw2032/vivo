
gizmo_plot_barplot_ui <- function(ns){
  fluidPage(h4("Barplot: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_barplot_name"), "Assigned to ...", "biostats_Age_barplot")),
              column(4,checkboxInput(ns("plot_barplot_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("plot_barplot_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_barplot_col"), "Enter column, if data.frame or matrix", "Age"))
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
                  column(4,textInput(ns("plot_barplot_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_barplot_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_barplot_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_barplot_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_barplot_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_barplot_auto", value=state$plot_barplot_auto)
      updateTextInput(session, "plot_barplot_df", value=state$plot_barplot_df)
      updateTextInput(session, "plot_barplot_col", value=state$plot_barplot_col)
      updateTextInput(session, "plot_barplot_name", value=state$plot_barplot_name)
      updateTextInput(session, "plot_barplot_main", value=state$plot_barplot_main)
      updateTextInput(session, "plot_barplot_sub", value=state$plot_barplot_sub)
      updateTextInput(session, "plot_barplot_xlab", value=state$plot_barplot_xlab)
      updateTextInput(session, "plot_barplot_ylab", value=state$plot_barplot_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_barplot_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_barplot_df, input$plot_barplot_col, input$plot_barplot_name, input$plot_barplot_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_barplot_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_barplot_df"]], "_", input[["plot_barplot_col"]],"_plot_barplot")
        updateTextInput(session, "plot_barplot_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_barplot_name"]], " <- barplot(", input[["plot_barplot_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_barplot_col"]])>0)){
                           paste0("[['", input[["plot_barplot_col"]], "']]" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_barplot_main"]])>0)){
                           paste0(", main='", input[["plot_barplot_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_barplot_sub"]])>0)){
                           paste0(", sub='", input[["plot_barplot_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_barplot_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_barplot_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_barplot_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_barplot_ylab"]], "'" )
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
      plot_barplot_name=input[["plot_barplot_name"]],
      plot_barplot_auto=input[["plot_barplot_auto"]],
      plot_barplot_df=input[["plot_barplot_df"]],
      plot_barplot_col=input[["plot_barplot_col"]],
      plot_barplot_main=input[["plot_barplot_main"]],
      plot_barplot_sub=input[["plot_barplot_sub"]],
      plot_barplot_xlab=input[["plot_barplot_xlab"]],
      plot_barplot_ylab=input[["plot_barplot_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_barplot <- list(
  ui=gizmo_plot_barplot_ui,
  server=gizmo_plot_barplot_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_barplot with run_standalone
#'
#' @export
run_gizmo_plot_barplot <- function() run_standalone("plot_barplot")
