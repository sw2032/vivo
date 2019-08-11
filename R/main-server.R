


vivid_server <- function(){

  server <- function(input, output, session) {

    .globals$vivid_server$child_queue$consumer$start()
    .globals$remote_r$set_session(session)

    session$userData$docs <- list()

    session$userData$r_markdown <- list()

    session$userData$r_output <- list()

    server_documents(input, output, session)

    server_rstudio(input, output, session)


    observeEvent(input$interrupt_r, {
      interrupt_r()
    })


    add_gizmo_server_hook(input, output, session, "wrangle_data","wrangle_data")

    add_gizmo_server_hook(input, output, session, "markdown_block","markdown_block")
	
	add_gizmo_server_hook(input, output, session, "script_block","script_block")

    add_gizmo_server_hook(input, output, session, "input_string","input_string")

    add_gizmo_server_hook(input, output, session, "input_number","input_number")
	
	add_gizmo_server_hook(input, output, session, "input_vector","input_vector")

    add_gizmo_server_hook(input, output, session, "load_csv","load_csv")

    add_gizmo_server_hook(input, output, session, "load_package_data","load_package_data")

    add_gizmo_server_hook(input, output, session, "datatables","datatables")
	
    add_gizmo_server_hook(input, output, session, "export_csv","export_csv")
	
	add_gizmo_server_hook(input, output, session, "toss_coin","toss_coin")
	
	add_gizmo_server_hook(input, output, session, "roll_dice","roll_dice")
	
    add_gizmo_server_hook(input, output, session, "helloworld","helloworld")
	
	add_gizmo_server_hook(input, output, session, "stats_mean","stats_mean")
	
	add_gizmo_server_hook(input, output, session, "stats_median","stats_median")
	
	add_gizmo_server_hook(input, output, session, "stats_mode","stats_mode")
	
	add_gizmo_server_hook(input, output, session, "stats_max","stats_max")
	
	add_gizmo_server_hook(input, output, session, "stats_min","stats_min")	
	
    add_gizmo_server_hook(input, output, session, "scatter_3d","scatter_3d")

    add_gizmo_server_hook(input, output, session, "dynamicui","dynamicui")
	
	add_gizmo_server_hook(input, output, session, "plot_dotchart","plot_dotchart")
	
	add_gizmo_server_hook(input, output, session, "plot_pointslines","plot_pointslines")
	
	add_gizmo_server_hook(input, output, session, "plot_scatter","plot_scatter")
	
	add_gizmo_server_hook(input, output, session, "plot_hist","plot_hist")
	
	
	
	add_gizmo_server_hook(input, output, session, "view_print","view_print")

    make_menu()
    did <- add_new_document("Untitled")
    set_active_document(did)
  }
  server
}


confirmDialog <- function(..., title="Message", callback=NULL, button_labels=c("Cancel","OK"), session = getDefaultReactiveDomain()){
  uuid <- gen_uuid()
  ns <- NS(uuid)
  modal <- modalDialog(..., title=title, easyClose=FALSE, footer=tagList(
    actionButton(ns("cancel"), button_labels[1]),
    actionButton(ns("ok"), button_labels[2])
  ))
  # if(!is.null(callback)){
  #   observeEvent(session$input[[ns("cancel")]], {
  #     callback(button_labels[1])
  #     #removeModal(session=session)
  #   },
  #   domain=session)
  #   observeEvent(session$input[[ns("cancel")]], {
  #     callback(button_labels[2])
  #     #removeModal(session=session)
  #   },
  #   domain=session)
  # }
  showModal(modal, session=session)
}
