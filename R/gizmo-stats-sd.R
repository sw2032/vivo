
gizmo_stats_sd_ui <- function(ns){
  fluidPage(h4("Standard Deviation: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_sd_name"), "Assigned to ...", "biostats_Age_sd")),
              column(4,checkboxInput(ns("stats_sd_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_sd_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_sd_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("stats_sd_goButton"), "Select Tool ..."))
            )
  )
}

gizmo_stats_sd_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_sd_auto", value=state$stats_sd_auto)
      updateTextInput(session, "stats_sd_df", value=state$stats_sd_df)
      updateTextInput(session, "stats_sd_col", value=state$stats_sd_col)
      updateTextInput(session, "stats_sd_name", value=state$stats_sd_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_sd_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_sd_df, input$stats_sd_col, input$stats_sd_name, input$stats_sd_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_sd_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_sd_df"]], "_", input[["stats_sd_col"]],"_stats_sd")
        updateTextInput(session, "stats_sd_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)
  
 # Select Tool
  observeEvent(input$stats_sd_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("stats_sd_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("stats_sd_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("stats_sd_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$stats_sd_goButton_ok,{
    updateTextInput(session, "stats_sd_df", value=input$stats_sd_dfA)
    updateTextInput(session, "stats_sd_col", value=input$stats_sd_colA)
    removeModal()
  })

  observeEvent(input$stats_sd_dfA,{
    if( isTRUE(stringr:::str_length(input[["stats_sd_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$stats_sd_dfA]])))){
      updateSelectInput(session, "stats_sd_colA", choices =as.vector(names(.GlobalEnv[[input$stats_sd_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "stats_sd_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_sd_col"]])>0) ){
                    paste0(input[["stats_sd_name"]], " <- sd(", input[["stats_sd_df"]], "[['", input[["stats_sd_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_sd_name"]], " <- sd(", input[["stats_sd_df"]], ")\n")
                  },
                  "print(", input[["stats_sd_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_sd_name=input[["stats_sd_name"]],
      stats_sd_auto=input[["stats_sd_auto"]],
      stats_sd_df=input[["stats_sd_df"]],
      stats_sd_col=input[["stats_sd_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_sd <- list(
  ui=gizmo_stats_sd_ui,
  server=gizmo_stats_sd_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_sd with run_standalone
#'
#' @export
run_gizmo_stats_sd <- function() run_standalone("stats_sd")
