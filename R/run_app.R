#' Run the Commutability Evaluation Application
#' @export
#' @import bslib
#' @import commutability
#' @import data.table
#' @import fasteqa
#' @import ggplot2
#' @import kableExtra
#' @import readxl
#' @import rmarkdown
#' @import shiny
#' @import DT
#' @import shinycssloaders
#' @import shinydashboard
#' @import shinyWidgets
#' @import smooth.commutability
#' @import tools

run_app <- function() {

  options(knitr.kable.NA = '', shiny.autoreload = TRUE)

  #### Dashbord Interface (General Options)####
  ui <- dashboardPage(
    header = dashboardHeader(title = "Commutability Evaluation", titleWidth = 300),
    #### Sidebar Menu Options ####
    sidebar = dashboardSidebar(
      sidebarMenu(
        menuItem(text = "Upload Data", tabName = "file_upload", icon = icon("upload")),
        menuItem(text = "Nonselectivity", tabName = "differences_in_non_selectivity", icon = icon("arrows-left-right-to-line")),
        menuItem(text = "Outlier Analysis", tabName = "outlier_analysis", icon = icon("ruler")),
        menuItem(text = "Model Validation", tabName = "model_validation", icon = icon("clipboard")),
        menuItem(text = "Results", tabName = "results", icon = icon("square-poll-horizontal"))
      ),
      width = 300
    ),
    body = dashboardBody(
      includeCSS("www/styles.css"),
      # The main body is composed of tabItems, each calling a specific module UI
      tabItems(
        tabItem(tabName = "file_upload", mod_file_upload_ui("file_upload_1")),
        tabItem(tabName = "differences_in_non_selectivity", mod_dins_ui("differences_in_non_selectivity_1")),
        tabItem(tabName = "outlier_analysis", mod_outlier_analysis_ui("outlier_analysis_1")),
        tabItem(tabName = "model_validation", mod_model_validation_ui("model_validation_1")),
        tabItem(tabName = "results", mod_results_ui("results_1"))
      )
    ),
    skin = "blue")



  # Back end
  server <- function(input, output) {

    # 1. Instantiate the File Upload module
    # This module returns a list of reactive expressions containing the uploaded data.
    file_upload_data <- mod_file_upload_server("file_upload_1")

    # 2. Instantiate the User Input module
    # It receives the reactive data from the file upload module.
    # It returns a reactive list of the user's selected parameters.
    mod_dins_params <- mod_dins_server("differences_in_non_selectivity_1",
                                       file_upload_data = file_upload_data)

    # 3. Instantiate the Outlier Analysis module
    # It also receives the data from the file upload module.
    outlier_data <- mod_outlier_analysis_server("outlier_analysis_1",
                                                file_upload_data = file_upload_data)

    # 4. Instantiate the Model Validation module
    # This module needs data from both the file upload and user input modules.
    model_validation_data <- mod_model_validation_server("model_validation_1",
                                                         file_upload_data = file_upload_data,
                                                         mod_dins_params = mod_dins_params)

    # 5. Instantiate the Results module
    # Pass the new data from the other modules
    mod_results_server("results_1",
                       file_upload_data = file_upload_data,
                       mod_dins_params = mod_dins_params,
                       outlier_data = outlier_data,
                       model_validation_data = model_validation_data)

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}

