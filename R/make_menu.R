#' make_menu
#'
make_menu <- function(){
  add_menu("vivid_menu_session","Session")
  add_menu("vivid_menu_code","Code")
  add_menu("vivid_menu_data","Data")
  add_menu("vivid_menu_analysis","Analysis")
  add_menu("vivid_menu_vis","Visualize")

  add_menu_item("menu_insert_markdown_block",
                "Insert Markdown Block",
                "vivid_menu_code")

  add_menu_item("gizmo_test",
                "Gizmo Test",
                "vivid_menu_analysis")

  add_menu_item("scatter_3d",
                "3D Scatter Plot",
                "vivid_menu_vis")

  add_menu_item("dynamicui",
                "Dynamic UI",
                "vivid_menu_vis")

  ########DATA I/O########
  add_menu_item("input_string",
                "Input String",
                "vivid_menu_data")

  add_menu_item("input_number",
                "Input Number",
                "vivid_menu_data")

  add_menu_item("load_csv",
                "Load csv",
                "vivid_menu_data")

  add_menu_item("load_package_data",
                "Load Package Data",
                "vivid_menu_data")

  ########DOCS I/O########
  add_menu_item("newvdoc",
                "New Document",
                "vivid_menu_session")

  add_menu_item("savevdoc",
                "Save Document",
                "vivid_menu_session")

  add_menu_item("loadvdoc",
                "Load Document",
                "vivid_menu_session")

  add_menu_item("doc_to_markdown",
                "Convert to markdown",
                "vivid_menu_session")

  add_menu_item("doc_to_r_script",
                "Convert to R script",
                "vivid_menu_session")
}
