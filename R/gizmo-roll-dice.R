
gizmo_roll_dice_ui <- function(ns){
  fluidPage(h4("Roll Dice: Use sample() to simulate"),
            fluidRow(
              column(4,textInput(ns("roll_dice_name"), "variable name", "roll_dice_result")),
              column(4,numericInput(ns("roll_dice"), "rolled how many times ...", value =10, min =0, max =100000, step =1))
            )
  )
}

gizmo_roll_dice_server <- function(input, output, session, state=NULL){

  # Restore UI state
  if (!is.null(state)) {
    session$onFlushed(function() {
      updateTextInput(session, "roll_dice_name", value=state$roll_dice_name)
      updateTextInput(session, "roll_dice", value=state$roll_dice)
    })
  }

  # RMarkdown Code
  txt_react <- reactive({
    txt <- paste0("```{r} \n",
                  input[["roll_dice_name"]]," <- ", "sample(1:6, ", input[["roll_dice"]] , ", replace=T)" , "\n",
				  "print(", input[["roll_dice_name"]], ")\n",
                  "```\n")
    txt
  })

  # Get UI state
  get_state <- function(){
    list(
      roll_dice=input[["roll_dice"]],
      roll_dice_name=input[["roll_dice_name"]],
      `__version__` = "0.0.1"
    )
  }

  list(
    code=txt_react,
    get_state=get_state
  )
}


.globals$gizmos$roll_dice <- list(
  ui=gizmo_roll_dice_ui,
  server=gizmo_roll_dice_server,
  library="vivid",
  opts=list()
)


#' run_gizmo_roll_dice with run_standalone
#'
#' @export
run_gizmo_roll_dice <- function() run_standalone("roll_dice")
