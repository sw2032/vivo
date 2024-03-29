
gizmo_stats_max_ui <- function(ns){
  fluidPage(h4("Max: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_max_name"), "Assigned to ...", "biostats_Age_max")),
              column(4,checkboxInput(ns("stats_max_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_max_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_max_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("stats_max_goButton"), "Select Tool ..."))
            )
  )
}

gizmo_stats_max_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_max_auto", value=state$stats_max_auto)
      updateTextInput(session, "stats_max_df", value=state$stats_max_df)
      updateTextInput(session, "stats_max_col", value=state$stats_max_col)
      updateTextInput(session, "stats_max_name", value=state$stats_max_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_max_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_max_df, input$stats_max_col, input$stats_max_name, input$stats_max_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_max_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_max_df"]], "_", input[["stats_max_col"]],"_stats_max")
        updateTextInput(session, "stats_max_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)
  
 # Select Tool
  observeEvent(input$stats_max_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("stats_max_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("stats_max_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("stats_max_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$stats_max_goButton_ok,{
    updateTextInput(session, "stats_max_df", value=input$stats_max_dfA)
    updateTextInput(session, "stats_max_col", value=input$stats_max_colA)
    removeModal()
  })

  observeEvent(input$stats_max_dfA,{
    if( isTRUE(stringr:::str_length(input[["stats_max_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$stats_max_dfA]])))){
      updateSelectInput(session, "stats_max_colA", choices =as.vector(names(.GlobalEnv[[input$stats_max_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "stats_max_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_max_col"]])>0) ){
                    paste0(input[["stats_max_name"]], " <- max(", input[["stats_max_df"]], "[['", input[["stats_max_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_max_name"]], " <- max(", input[["stats_max_df"]], ")\n")
                  },
                  "print(", input[["stats_max_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_max_name=input[["stats_max_name"]],
      stats_max_auto=input[["stats_max_auto"]],
      stats_max_df=input[["stats_max_df"]],
      stats_max_col=input[["stats_max_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_max <- list(
  ui=gizmo_stats_max_ui,
  server=gizmo_stats_max_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_max with run_standalone
#'
#' @export
run_gizmo_stats_max <- function() run_standalone("stats_max")
