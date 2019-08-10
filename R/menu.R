add_menu_item <-function(input_id, label, menu_id="vivid-navbar-ul", active=FALSE, ...){
  active <- if(active) "active" else NULL
  insertUI(
    paste0("#", menu_id),
    "beforeEnd",
    ui = tags$li(
      class=active,
      action_link(input_id, label, ...)
    )
  )
}

add_menu <- function(input_id, label, ...){
  insertUI(
    "#vivid-navbar-ul",
    "beforeEnd",
    ui = tags$li(
      class="dropdown",
      tags$a(
        href="#",
        class="dropdown-toggle",
        `data-toggle`="dropdown",
        role="button",
        `aria-haspopup`="true",
        `aria-expanded`="false",
        label,
        tags$span(class="caret")
      ),
      tags$ul(
        id=input_id,
        class="dropdown-menu"
      )
    )
  )
}
