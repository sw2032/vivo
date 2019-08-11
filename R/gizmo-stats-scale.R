
gizmo_stats_scale_ui <- function(ns){
  fluidPage(h4("Z-Score: A Stats Tool"),
            fluidRow(
              column(4,textInput(ns("stats_scale_name"), "Assigned to ...", "biostats_Age_sd")),
              column(4,checkboxInput(ns("stats_scale_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("stats_scale_df"), "Enter data.frame or matrix, or Enter vector", "biostats")),
              column(4,textInput(ns("stats_scale_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("stats_scale_goButton"), "Select Tool ..."))
            )
  )
}

gizmo_stats_scale_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "stats_scale_auto", value=state$stats_scale_auto)
      updateTextInput(session, "stats_scale_df", value=state$stats_scale_df)
      updateTextInput(session, "stats_scale_col", value=state$stats_scale_col)
      updateTextInput(session, "stats_scale_name", value=state$stats_scale_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "stats_scale_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$stats_scale_df, input$stats_scale_col, input$stats_scale_name, input$stats_scale_auto),{
    if(initi$initing==FALSE){
      if(input[["stats_scale_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["stats_scale_df"]], "_", input[["stats_scale_col"]],"_stats_scale")
        updateTextInput(session, "stats_scale_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)
  
 # Select Tool
  observeEvent(input$stats_scale_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("stats_scale_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("stats_scale_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("stats_scale_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$stats_scale_goButton_ok,{
    updateTextInput(session, "stats_scale_df", value=input$stats_scale_dfA)
    updateTextInput(session, "stats_scale_col", value=input$stats_scale_colA)
    removeModal()
  })

  observeEvent(input$stats_scale_dfA,{
    if( isTRUE(stringr:::str_length(input[["stats_scale_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$stats_scale_dfA]])))){
      updateSelectInput(session, "stats_scale_colA", choices =as.vector(names(.GlobalEnv[[input$stats_scale_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "stats_scale_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["stats_scale_col"]])>0) ){
                    paste0(input[["stats_scale_name"]], " <- scale(", input[["stats_scale_df"]], "[['", input[["stats_scale_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["stats_scale_name"]], " <- scale(", input[["stats_scale_df"]], ")\n")
                  },
                  "print(", input[["stats_scale_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      stats_scale_name=input[["stats_scale_name"]],
      stats_scale_auto=input[["stats_scale_auto"]],
      stats_scale_df=input[["stats_scale_df"]],
      stats_scale_col=input[["stats_scale_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$stats_scale <- list(
  ui=gizmo_stats_scale_ui,
  server=gizmo_stats_scale_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_stats_scale with run_standalone
#'
#' @export
run_gizmo_stats_scale <- function() run_standalone("stats_scale")
