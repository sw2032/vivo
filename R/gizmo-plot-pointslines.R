
gizmo_plot_pointslines_ui <- function(ns){
  fluidPage(h4("Line Graph: A Plot Tool"),
            fluidRow(
              column(4,textInput(ns("plot_pointslines_name"), "Assigned to ...", "biostats_Age_hist")),
              column(4,checkboxInput(ns("plot_pointslines_auto"), "auto generate name", value = FALSE, width = NULL)),
			  column(4,radioButtons(ns("plot_pointslines_type"), "how to present data ...",
                                    c("points" = "p",
                                      "lines" = "l",
                                      "both points and lines" = "o"), inline=TRUE ))
            ),
            fluidRow(
              column(4,textInput(ns("plot_pointslines_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("plot_pointslines_col"), "Enter column, if data.frame or matrix", "Age"))
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
                  column(4,textInput(ns("plot_pointslines_main"), "overall title for the plot", "")),
                  column(4,textInput(ns("plot_pointslines_sub"), "sub title for the plot", "")),
                  column(4,textInput(ns("plot_pointslines_xlab"), "a label for the x axis", "")),
                  column(4,textInput(ns("plot_pointslines_ylab"), "a label for the y axis", ""))
                ),
                id=ns(paste0("advancedmenu", 'div')),
                style="display: none;"
              ))
            ),tags$br()
  )
}

gizmo_plot_pointslines_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "plot_pointslines_auto", value=state$plot_pointslines_auto)
      updateTextInput(session, "plot_pointslines_df", value=state$plot_pointslines_df)
      updateTextInput(session, "plot_pointslines_col", value=state$plot_pointslines_col)
      updateTextInput(session, "plot_pointslines_name", value=state$plot_pointslines_name)
	  updateRadioButtons(session, "plot_pointslines_type", selected=state$plot_pointslines_type)
      updateTextInput(session, "plot_pointslines_main", value=state$plot_pointslines_main)
      updateTextInput(session, "plot_pointslines_sub", value=state$plot_pointslines_sub)
      updateTextInput(session, "plot_pointslines_xlab", value=state$plot_pointslines_xlab)
      updateTextInput(session, "plot_pointslines_ylab", value=state$plot_pointslines_ylab)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "plot_pointslines_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$plot_pointslines_df, input$plot_pointslines_col, input$plot_pointslines_name, input$plot_pointslines_auto),{
    if(initi$initing==FALSE){
      if(input[["plot_pointslines_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["plot_pointslines_df"]], "_", input[["plot_pointslines_col"]],"_plot_pointslines")
        updateTextInput(session, "plot_pointslines_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  paste0(input[["plot_pointslines_name"]], " <- plot(", input[["plot_pointslines_df"]],
                         if(isTRUE(stringr:::str_length(input[["plot_pointslines_col"]])>0)){
                           paste0("[['", input[["plot_pointslines_col"]], "']]" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_pointslines_type"]])>0) && !isTRUE(input[["plot_pointslines_type"]]=='p')){
                           paste0(", type='", input[["plot_pointslines_type"]], "'" )
                         }else{
                           ""
                         },  
                         if(isTRUE(stringr:::str_length(input[["plot_pointslines_main"]])>0)){
                           paste0(", main='", input[["plot_pointslines_main"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_pointslines_sub"]])>0)){
                           paste0(", sub='", input[["plot_pointslines_sub"]], "'" )
                         }else{
                           ""
                         },                         
						 if(isTRUE(stringr:::str_length(input[["plot_pointslines_xlab"]])>0)){
                           paste0(", xlab='", input[["plot_pointslines_xlab"]], "'" )
                         }else{
                           ""
                         },
                         if(isTRUE(stringr:::str_length(input[["plot_pointslines_ylab"]])>0)){
                           paste0(", ylab='", input[["plot_pointslines_ylab"]], "'" )
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
      plot_pointslines_name=input[["plot_pointslines_name"]],
      plot_pointslines_auto=input[["plot_pointslines_auto"]],
      plot_pointslines_df=input[["plot_pointslines_df"]],
      plot_pointslines_col=input[["plot_pointslines_col"]],
      plot_pointslines_main=input[["plot_pointslines_main"]],
	  plot_pointslines_type=input[["plot_pointslines_type"]],
      plot_pointslines_sub=input[["plot_pointslines_sub"]],
      plot_pointslines_xlab=input[["plot_pointslines_xlab"]],
      plot_pointslines_ylab=input[["plot_pointslines_ylab"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$plot_pointslines <- list(
  ui=gizmo_plot_pointslines_ui,
  server=gizmo_plot_pointslines_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_plot_pointslines with run_standalone
#'
#' @export
run_gizmo_plot_pointslines <- function() run_standalone("plot_pointslines")
