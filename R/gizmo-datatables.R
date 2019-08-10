
gizmo_datatables_ui <- function(ns){
  fluidPage(h4("DataTables: widget to display R data objects"),
            h6("The R package DT provides an R interface to the JavaScript library DataTables. R data objects (matrices or data frames) can be displayed as tables on HTML pages, and DataTables provides filtering, pagination, sorting, and many other features in the tables."),
            fluidRow(
              column(4,textInput(ns("datatables_object"), "Object in global environment","biostats")),
              column(4,textInput(ns("datatables"), "Status","Not Found"))
            ),
            DT::DTOutput(ns("datatables_table"))
  )
}

gizmo_datatables_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "datatables_object", value=state$datatables_object)
      updateTextInput(session, "datatables", value=state$datatables)
    })
  }

  observeEvent(input$datatables_object,{
    if( isTRUE(stringr:::str_length(input[["datatables_object"]])>0) && exists( input[["datatables_object"]] ) ){
		if(length(dim(.GlobalEnv[[ input[["datatables_object"]] ]])) == 2){
			updateTextInput(session, "datatables", value="Found")
		}else{
			updateTextInput(session, "datatables", value="'data' must be 2-dimensional (e.g. data frame or matrix)")
		}
    }else{
      updateTextInput(session, "datatables", value="Not Found")
    }
  })



  output$datatables_table = DT::renderDT({
    if( isTRUE(stringr:::str_length(input[["datatables_object"]])>0) && exists( input[["datatables_object"]] ) ){
		if(length(dim(.GlobalEnv[[ input[["datatables_object"]] ]])) == 2){
			.GlobalEnv[[ input[["datatables_object"]] ]]
		}else{
			NULL
		}
    }else{
      NULL #.GlobalEnv[[ "ThisObjectDoesNotExist" ]]
    }
  }, options = list(lengthChange = FALSE, scrollX = TRUE) )

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("<!-- user may enter a string into global environment -->", "\n",
                  "```{r} \n",
                  #input[["datatables_object"]]," <- ", "'", input[["datatables"]] , "'" , "\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      datatables=input[["datatables"]],
      datatables_object=input[["datatables_object"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$datatables <- list(
  ui=gizmo_datatables_ui,
  server=gizmo_datatables_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_datatables with run_standalone
#'
#' @export
run_gizmo_datatables <- function() run_standalone("datatables")
