
gizmo_plot_plot_ui <- function(ns){
  fluidPage(h4("Line Graph: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_plot_name"), "Assigned to ...", "biostats_Age_hist")),
              column(4,checkboxInput(ns("plot_plot_auto"), "auto generate name", value = FALSE, width = NULL)),
			  column(4,radioButtons(ns("plot_plot_type"), "how to present data ...",
                                    c("points" = "p",
                                      "lines" = "l",
                                      "both points and lines" = "o"), inline=TRUE ))
            ),
            fluidRow(
              column(4,textInput(ns("plot_plot_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_plot_col"), "Enter column, if data.frame or matrix", "Age"))
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
                  column(4,textInput(ns("plot_plot_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_plot_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_plot_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_plot_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_plot_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_plot_auto", value=state$plot_plot_auto)
      updateTextInput(session, "plot_plot_df", value=state$plot_plot_df)
      updateTextInput(session, "plot_plot_col", value=state$plot_plot_col)
      updateTextInput(session, "plot_plot_name", value=state$plot_plot_name)
	  updateRadioButtons(session, "plot_plot_type", selected=state$plot_plot_type)
      updateTextInput(session, "plot_plot_main", value=state$plot_plot_main)
      updateTextInput(session, "plot_plot_sub", value=state$plot_plot_sub)
      updateTextInput(session, "plot_plot_xlab", value=state$plot_plot_xlab)
      updateTextInput(session, "plot_plot_ylab", value=state$plot_plot_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_plot_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_plot_df, input$plot_plot_col, input$plot_plot_name, input$plot_plot_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_plot_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_plot_df"]], "_", input[["plot_plot_col"]],"_plot_plot")
        updateTextInput(session, "plot_plot_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_plot_name"]], " <- plot(", input[["plot_plot_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_plot_col"]])>0)){
                           paste0("[['", input[["plot_plot_col"]], "']]" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_plot_type"]])>0) && !isTRUE(input[["plot_plot_type"]]=='p')){
                           paste0(", type='", input[["plot_plot_type"]], "'" )
                         }else{
                           ""
                         },  
                         if(isTRUE(stringr:::str_length(input[["plot_plot_main"]])>0)){
                           paste0(", main='", input[["plot_plot_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_plot_sub"]])>0)){
                           paste0(", sub='", input[["plot_plot_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_plot_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_plot_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_plot_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_plot_ylab"]], "'" )
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
      plot_plot_name=input[["plot_plot_name"]],
      plot_plot_auto=input[["plot_plot_auto"]],
      plot_plot_df=input[["plot_plot_df"]],
      plot_plot_col=input[["plot_plot_col"]],
      plot_plot_main=input[["plot_plot_main"]],
	  plot_plot_type=input[["plot_plot_type"]],
      plot_plot_sub=input[["plot_plot_sub"]],
      plot_plot_xlab=input[["plot_plot_xlab"]],
      plot_plot_ylab=input[["plot_plot_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_plot <- list(
  ui=gizmo_plot_plot_ui,
  server=gizmo_plot_plot_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_plot with run_standalone
#'
#' @export
run_gizmo_plot_plot <- function() run_standalone("plot_plot")
