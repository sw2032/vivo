
gizmo_stats_median_ui <- function(ns){
  fluidPage(h4("Median: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_median_name"), "Assigned to ...", "biostats_Age_median")),
              column(4,checkboxInput(ns("stats_median_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_median_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_median_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("stats_median_goButton"), "Select Tool ..."))
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
  
 # Select Tool
  observeEvent(input$stats_median_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("stats_median_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("stats_median_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("stats_median_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$stats_median_goButton_ok,{
    updateTextInput(session, "stats_median_df", value=input$stats_median_dfA)
    updateTextInput(session, "stats_median_col", value=input$stats_median_colA)
    removeModal()
  })

  observeEvent(input$stats_median_dfA,{
    if( isTRUE(stringr:::str_length(input[["stats_median_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$stats_median_dfA]])))){
      updateSelectInput(session, "stats_median_colA", choices =as.vector(names(.GlobalEnv[[input$stats_median_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "stats_median_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)

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

