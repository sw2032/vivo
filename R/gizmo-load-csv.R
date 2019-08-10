
gizmo_load_csv_ui <- function(ns){
  fluidPage(h4("Load csv: Load a csv into global environment"),
            fluidRow(
              column(4,textInput(ns("load_csv_name"), "csv name","biostats")),
              column(8)
			),
			fluidRow(
			  column(8, textInput(ns("load_csv_local_path"), "csv local", "NA", width = "100%")),
              column(4, shinyFiles::shinyFilesButton(ns("load_csv_local"), "Load csv (Local)", "Select .csv file", FALSE)),
              column(12, textInput(ns("load_csv"), "csv url","https://people.sc.fsu.edu/~jburkardt/data/csv/biostats.csv", width = "100%"))
            )
  )
}

gizmo_load_csv_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "load_csv_name", value=state$load_csv_name)
      updateTextInput(session, "load_csv", value=state$load_csv)
    })
  }

  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), shinyFiles::getVolumes()())

  shinyFiles::shinyFileChoose(input, "load_csv_local", roots = volumes, session = session, filetypes=c('csv'))

  observeEvent(input$load_csv_local,{
    dir <- shinyFiles::parseFilePaths(volumes, input$load_csv_local)
    if(nrow(dir) > 0){
      message(dir$datapath)
      updateTextInput(session, "load_csv_local_path", value=dir$datapath)
    }else{

    }

  })

  output$load_csv_local_path <- renderText({
    dir <- shinyFiles::parseFilePaths(volumes, input$load_csv_local)
    if(nrow(dir) > 0){
      file <- dir$datapath
      return(file)
    }
    NULL
  })

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("<!-- user may enter a csv into global environment -->", "\n",
                  "```{r} \n",
                  "library(readr) \n",
                  input[["load_csv_name"]]," <- read_csv('", input[["load_csv"]] , "') \n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      load_csv=input[["load_csv"]],
      load_csv_name=input[["load_csv_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$load_csv <- list(
  ui=gizmo_load_csv_ui,
  server=gizmo_load_csv_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_load_csv with run_standalone
#'
#' @export
run_gizmo_load_csv <- function() run_standalone("load_csv")
