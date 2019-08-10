
gizmo_export_csv_ui <- function(ns){
  fluidPage(h4("Export csv: Export a csv to a file"),
            fluidRow(
              column(4,textInput(ns("export_csv_name"), "object to be exported","biostats")),
              column(8, shinyFiles::shinySaveButton(ns("export_csv_local"), "Export to ... (Local)", "Export to ...", filetype = list(csv="csv", rds="rds") ) ),
              column(12, textInput(ns("export_csv"), "file path","~/biostats.csv", width = "100%"))
            )
  )
}

gizmo_export_csv_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "export_csv_name", value=state$export_csv_name)
      updateTextInput(session, "export_csv", value=state$export_csv)
    })
  }

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

  shinyFiles::shinyFileSave(input, "export_csv_local", roots = volumes, session = session, restrictions = system.file(package = "base"))

  observeEvent(input$export_csv_local,{
    dir <- shinyFiles::parseSavePath(volumes, input$export_csv_local)
    if(nrow(dir) > 0){
      #message(dir$datapath) #debug only
      updateTextInput(session, "export_csv", value=dir$datapath)
    }

  })

  output$export_csv_local_path <- renderText({
    dir <- shinyFiles::parseFilePaths(volumes, input$export_csv_local)
    if(nrow(dir) > 0){
      file <- dir$datapath
      return(file)
    }
    NULL
  })

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
				  "rio::export(",input[["export_csv_name"]],", '",input[["export_csv"]],"')\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      export_csv=input[["export_csv"]],
      export_csv_name=input[["export_csv_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$export_csv <- list(
  ui=gizmo_export_csv_ui,
  server=gizmo_export_csv_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_export_csv with run_standalone
#'
#' @export
run_gizmo_export_csv <- function() run_standalone("export_csv")
