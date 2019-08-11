

gizmo_view_print_ui <- function(ns){
  fluidPage(h4("Print: A view Tool"),
            fluidRow(
              column(4,textInput(ns("view_print_name"), "Assigned to ...", "biostats_Age_print")),
              column(4,checkboxInput(ns("view_print_auto"), "auto generate name", value = FALSE, width = NULL))
            ),
            fluidRow(
              column(4,textInput(ns("view_print_df"), "Enter data.frame or matrix, or Enter vector, or Enter scalar", "biostats")),
              column(4,textInput(ns("view_print_col"), "Enter column, if data.frame or matrix", "Age")),
              column(4,actionButton(ns("view_print_goButton"), "Select Tool ..."))
            )
  )
}

gizmo_view_print_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateCheckboxInput(session, "view_print_auto", value=state$view_print_auto)
      updateTextInput(session, "view_print_df", value=state$view_print_df)
      updateTextInput(session, "view_print_col", value=state$view_print_col)
      updateTextInput(session, "view_print_name", value=state$view_print_name)
      initi$initing=FALSE
    })
  }else{
    updateCheckboxInput(session, "view_print_auto", value=TRUE)
    initi$initing=FALSE
  }

  observeEvent(c(input$view_print_df, input$view_print_col, input$view_print_name, input$view_print_auto),{
    if(initi$initing==FALSE){
      if(input[["view_print_auto"]]){
        if(TRUE){

        }else{

        }
        stringresult=paste0(input[["view_print_df"]], "_", input[["view_print_col"]],"_view_print")
        updateTextInput(session, "view_print_name", value=stringresult)
      }
    }
  }, ignoreInit=TRUE)

 # Select Tool
  observeEvent(input$view_print_goButton, {
    d <- modalDialog(
      title="Select ... from .GlobalEnv",
      size="l",
      tags$div(
        fluidRow(
          column(4,selectInput(session$ns("view_print_dfA"),"Enter data.frame or matrix, or Enter vector, or Enter scalar", as.vector(ls(envir=.GlobalEnv)) )),
          column(4,selectInput(session$ns("view_print_colA"), "Enter column, if data.frame or matrix", c() )),
          column(4,tags$div())
        )
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton(session$ns("view_print_goButton_ok"), "OK")
      )
    )
    showModal(d)
  })

  observeEvent(input$view_print_goButton_ok,{
    updateTextInput(session, "view_print_df", value=input$view_print_dfA)
    updateTextInput(session, "view_print_col", value=input$view_print_colA)
    removeModal()
  })

  observeEvent(input$view_print_dfA,{
    if( isTRUE(stringr:::str_length(input[["view_print_dfA"]])>0) && !isTRUE(is.null(names(.GlobalEnv[[input$view_print_dfA]])))){
      updateSelectInput(session, "view_print_colA", choices =as.vector(names(.GlobalEnv[[input$view_print_dfA]])),  selected = character(0))
    }else{
      updateSelectInput(session, "view_print_colA", choices = character(0), selected = character(0) )
    }
  }, ignoreInit=FALSE)


  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  if( isTRUE(stringr:::str_length(input[["view_print_col"]])>0) ){
                    paste0(input[["view_print_name"]], " <- print(", input[["view_print_df"]], "[['", input[["view_print_col"]], "']]", ")\n")
                  }else{
                    paste0(input[["view_print_name"]], " <- print(", input[["view_print_df"]], ")\n")
                  },
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      view_print_name=input[["view_print_name"]],
      view_print_auto=input[["view_print_auto"]],
      view_print_df=input[["view_print_df"]],
      view_print_col=input[["view_print_col"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$view_print <- list(
  ui=gizmo_view_print_ui,
  server=gizmo_view_print_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_view_print with run_standalone
#'
#' @export
run_gizmo_view_print <- function() run_standalone("view_print")
