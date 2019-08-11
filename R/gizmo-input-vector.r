
gizmo_input_vector_ui <- function(ns){
  fluidPage(h4("Input Vector: Enter a vector into global environment"),
            fluidRow(
              column(4,textInput(ns("input_vector_name"), "vector name", "MyVector")),
              column(4,numericInput(ns("input_vector_length"), "how long is this vector ...", value = 4, min = 0, max = 1000, step = 1)),
              column(4,radioButtons(ns("input_vector_type"), "what type is this vector ...",
                                    c("numeric" = "numeric",
                                      "string" = "string",
                                      "bool" = "bool"), inline=TRUE ))
            ),
            fluidRow(
              column(12,uiOutput(ns("input_vector_field")))
            )
            #,fluidRow(
            #  column(4,textInput(ns("input_vector"), "Not Used", "Not Used"))
            #)

  )
}

gizmo_input_vector_server <- function(input, output, session, state=NULL){

  initi <- reactiveValues(initing=TRUE)
  
  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "input_vector_name", value=state$input_vector_name)
      updateNumericInput(session, "input_vector_length", value=state$input_vector_length)
      updateRadioButtons(session, "input_vector_type", selected=state$input_vector_type)
      updateTextInput(session, "input_vector", value=state$input_vector)
	  initi$initing=FALSE
    })
  }else{
    initi$initing=FALSE
  }

  output$input_vector_field <- renderUI({
    fluidRow(
      if(isTRUE(!is.null(input[["input_vector_length"]])) && isTRUE(input[["input_vector_length"]] > 0)){
        if(input[["input_vector_type"]]=='numeric'){
          lapply(
            X = 1:input$input_vector_length,
            FUN = function(x){
			  R=sample(-100:100, 1)
              column(2,textInput(session$ns(paste0("input_vector_element_",x)), paste0("Input Vector Element ",x), R, width="100%"))
            }
          )
        }else if(input[["input_vector_type"]]=='string'){
          lapply(
            X = 1:input$input_vector_length,
            FUN = function(x){
			  R=paste0("str",x)
              column(2,textInput(session$ns(paste0("input_vector_element_",x)), paste0("Input Vector Element ",x), R, width="100%"))
            }
          )
        }else if(input[["input_vector_type"]]=='bool'){
          lapply(
            X = 1:input$input_vector_length,
            FUN = function(x){
			  R=sample(c(TRUE, FALSE),1)
              column(2,textInput(session$ns(paste0("input_vector_element_",x)), paste0("Input Vector Element ",x), R, width="100%"))
            }
          )
        }
      }
    )
  })


  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("<!-- user may enter a vector into global environment -->", "\n",
                  "```{r} \n",
                  input[["input_vector_name"]]," <- c(",
                  if(isTRUE(!is.null(input[["input_vector_length"]])) && isTRUE(input[["input_vector_length"]] > 0)){
                    toString(
                      if(input[["input_vector_type"]]=='numeric'){
                        lapply(
                          X = 1:input$input_vector_length,
                          FUN = function(x){
                            input[[paste0("input_vector_element_",x)]]
                          }
                        )
                      }else if(input[["input_vector_type"]]=='string'){
                        lapply(
                          X = 1:input$input_vector_length,
                          FUN = function(x){
                            paste0("'", input[[paste0("input_vector_element_",x)]], "'")
                          }
                        )
                      }else if(input[["input_vector_type"]]=='bool'){
                        lapply(
                          X = 1:input$input_vector_length,
                          FUN = function(x){
                            input[[paste0("input_vector_element_",x)]]
                          }
                        )
                      }
                    )
                  }else{
                    ""
                  }
                  , ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    input_vector_element <- if(isTRUE(!is.null(input[["input_vector_length"]])) && isTRUE(input[["input_vector_length"]] > 0)){
      sapply(
        X = 1:input$input_vector_length,
        FUN = function(x){
          input[[paste0("input_vector_element_",x)]]
        }
      )}else{
        NULL
      }
      list(
        input_vector=input[["input_vector"]],
        input_vector_length=input[["input_vector_length"]],
        input_vector_type=input[["input_vector_type"]],
        input_vector_name=input[["input_vector_name"]],
		'input_vector_element'=input_vector_element,
        `__version__` = "0.0.1"
      )
    }

    list(
      code=txt_react,
      get_state=get_state
    )
  }


  .globals$gizmos$input_vector <- list(
    ui=gizmo_input_vector_ui,
    server=gizmo_input_vector_server,
    library="vivid",
    opts=list()
  )


  #' run_gizmo_input_vector with run_standalone
  #'
  #' @export
  run_gizmo_input_vector <- function() run_standalone("input_vector")
