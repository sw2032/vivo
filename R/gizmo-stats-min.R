
gizmo_stats_min_ui <- function(ns){
  fluidPage(h4("Min: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_min_name"), "Assigned to ...", "biostats_Age_min")),
              column(4,checkboxInput(ns("stats_min_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_min_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_min_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("stats_min_goButton"), "Select Tool ..."))
            )
  )
}

gizmo_stats_min_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_min_auto", value=state$stats_min_auto)
      updateTextInput(session, "stats_min_df", value=state$stats_min_df)
      updateTextInput(session, "stats_min_col", value=state$stats_min_col)
      updateTextInput(session, "stats_min_name", value=state$stats_min_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_min_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_min_df, input$stats_min_col, input$stats_min_name, input$stats_min_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_min_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_min_df"]], "_", input[["stats_min_col"]],"_stats_min")
        updateTextInput(session, "stats_min_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)
  
 # Select Tool
  observeEvent(input$stats_min_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("stats_min_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("stats_min_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("stats_min_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$stats_min_goButton_ok,{
    updateTextInput(session, "stats_min_df", value=input$stats_min_dfA)
    updateTextInput(session, "stats_min_col", value=input$stats_min_colA)
    removeModal()
  })

  observeEvent(input$stats_min_dfA,{
    if( isTRUE(stringr:::str_length(input[["stats_min_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$stats_min_dfA]])))){
      updateSelectInput(session, "stats_min_colA", choices =as.vector(names(.GlobalEnv[[input$stats_min_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "stats_min_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_min_col"]])>0) ){
                    paste0(input[["stats_min_name"]], " <- min(", input[["stats_min_df"]], "[['", input[["stats_min_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_min_name"]], " <- min(", input[["stats_min_df"]], ")\n")
                  },
                  "print(", input[["stats_min_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_min_name=input[["stats_min_name"]],
      stats_min_auto=input[["stats_min_auto"]],
      stats_min_df=input[["stats_min_df"]],
      stats_min_col=input[["stats_min_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_min <- list(
  ui=gizmo_stats_min_ui,
  server=gizmo_stats_min_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_min with run_standalone
#'
#' @export
run_gizmo_stats_min <- function() run_standalone("stats_min")
