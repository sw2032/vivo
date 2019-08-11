#' make_menu
#'
make_menu <- function(){
  add_menu("vivid_menu_session","Session")
  add_menu("vivid_menu_code","Code")
  add_menu("vivid_menu_data","Data")
  add_menu("vivid_menu_analysis","Analysis")
  add_menu("vivid_menu_vis","View")

  add_menu_item("script_block",
                "R Script Block",
                "vivid_menu_code")
				
  add_menu_item("markdown_block",
                "R Markdown Block",
                "vivid_menu_code")

  ########DATA I/O########
  add_menu_item("input_string",
                "Input String",
                "vivid_menu_data")

  add_menu_item("input_number",
                "Input Number",
                "vivid_menu_data")
				
  add_menu_item("input_vector",
                "Input Vector",
                "vivid_menu_data")

  add_menu_item("load_csv",
                "Load csv",
                "vivid_menu_data")

  add_menu_item("load_package_data",
                "Load Package Data",
                "vivid_menu_data")
				
  add_menu_item("export_csv",
                "Export csv",
                "vivid_menu_data")
				
  add_menu_item("datatables",
                "DataTables",
                "vivid_menu_data")
				
  add_menu_item("toss_coin",
                "Toss Coin",
                "vivid_menu_data")
				
  add_menu_item("roll_dice",
                "Roll Dice",
                "vivid_menu_data")
				
  ########ANALYSIS I/O########	
				
  add_menu_item("helloworld",
                "HelloWorld",
                "vivid_menu_analysis")
				
  add_menu_item("stats_mean",
                "Mean",
                "vivid_menu_analysis")
				
  add_menu_item("stats_median",
                "Median",
                "vivid_menu_analysis")
				
  add_menu_item("stats_mode",
                "Mode",
                "vivid_menu_analysis")
				
  add_menu_item("stats_max",
                "Max",
                "vivid_menu_analysis")
				
  add_menu_item("stats_min",
                "Min",
                "vivid_menu_analysis")
				
  add_menu_item("stats_sd",
                "Standard Deviation",
                "vivid_menu_analysis")
				
  add_menu_item("stats_scale",
                "Z-Score",
                "vivid_menu_analysis")
				
  ########VIEW I/O########
				
  add_menu_item("scatter_3d",
                "3D Scatter Plot",
                "vivid_menu_vis")
				
  add_menu_item("dynamicui",
                "Dynamic UI",
                "vivid_menu_vis")	
				
  add_menu_item("plot_dotchart",
                "Dotchart",
                "vivid_menu_vis")	
				
  add_menu_item("plot_barplot",
                "Barplot",
                "vivid_menu_vis")
				
  add_menu_item("plot_hist",
                "Histogram",
                "vivid_menu_vis")
				
  add_menu_item("plot_pointslines",
                "Points/Lines Graph",
                "vivid_menu_vis")
				
  add_menu_item("plot_scatter",
                "Scatter Graph",
                "vivid_menu_vis")
				
  add_menu_item("plot_boxplot",
                "Boxplot",
                "vivid_menu_vis")
				
  add_menu_item("view_print",
                "Print",
                "vivid_menu_vis")
				
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
