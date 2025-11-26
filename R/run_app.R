#' Launch the Commutability Evaluation Shiny Application
#'
#' @description
#' This function launches a comprehensive Shiny web application for performing
#' commutability evaluations. The application is structured into a series of
#' modules, each dedicated to a specific step of the analysis pipeline:
#' data upload and validation, outlier analysis, model validation, and final
#' results interpretation.
#'
#' The user interface is built as a `shinydashboard`, providing a clean and
#' organized layout with a sidebar for easy navigation between the different
#' analysis stages.
#'
#' @details
#' The server-side logic of the application is modular, where each tab in the
#' user interface corresponds to a server module. The modules are chained
#' together to create a reactive data flow:
#'
#' 1.  `mod_file_upload_server`: Manages the initial data upload and performs
#'     critical diagnostic checks. It passes the raw data and diagnostic
#'     results to downstream modules.
#' 2.  `mod_dins_server`: Takes the uploaded data and allows the user to set
#'     key parameters for the analysis, such as data transformations and
#'     the statistical model for prediction intervals.
#' 3.  `mod_outlier_analysis_server`: Uses the clinical sample data to screen
#'     for potential outliers that could influence the analysis.
#' 4.  `mod_model_validation_server`: Provides tools for formally and
#'     visually assessing the assumptions of the chosen statistical model.
#' 5.  `mod_results_server`: The final module that consolidates data and
#'     parameters from all previous modules to calculate and display the
#'     final commutability evaluation results in tables and plots.
#'
#' This modular design ensures that the application is organized, maintainable,
#' and that data dependencies are handled efficiently and reactively.
#'
#' @return
#' This function does not return any value. It is called for its side effect of
#' launching the Shiny application.
#'
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
#' @import shinyBS
#' @import DT
#' @import shinycssloaders
#' @import shinydashboard
#' @import shinyWidgets
#' @import smooth.commutability
#' @import tinytex
#' @import tools
run_app <- function() {
  options(knitr.kable.NA = '', shiny.autoreload = TRUE)

  # --- Global User Interface (Dashbord Interface Styled) ---
  ui <- dashboardPage(
    header = dashboardHeader(title = "Commutability Evaluation", titleWidth = 300),
    # --- Sidebar Menu Options ---
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
      withMathJax(), # Load the MathJax library
      includeCSS("www/styles.css"), # Source the CSS styles
      # The main body is composed of tabItems, each calling a specific module UI
      tabItems(
        tabItem(tabName = "file_upload", mod_file_upload_ui("file_upload_1")),
        tabItem(tabName = "differences_in_non_selectivity", mod_dins_ui("differences_in_non_selectivity_1")),
        tabItem(tabName = "outlier_analysis", mod_outlier_analysis_ui("outlier_analysis_1")),
        tabItem(tabName = "model_validation", mod_model_validation_ui("model_validation_1")),
        tabItem(tabName = "results", mod_results_ui("results_1"))
      )
    ),
    skin = "blue" # Arbitrary Choice - Completely Overhauled by CSS
  )



  # Back end
  server <- function(input, output) {

    # 1. Instantiate the 'File Upload' module
    # This module returns a list of reactive expressions containing:
    # --- (1) Uploaded raw data
    # --- (2) Diagnostics of uploaded data.
    file_upload_data <- mod_file_upload_server("file_upload_1")

    # 2. Instantiate the differences in 'Nonselectivity' module
    # It receives the reactive data from the 'File Upload' module.
    # It returns a reactive list of the user's selected DINS parameters.
    # --- Note: Only requires valid uploaded clinical sample data to operate
    mod_dins_params <- mod_dins_server("differences_in_non_selectivity_1",
                                       file_upload_data = file_upload_data)

    # 3. Instantiate the 'Outlier Analysis' module
    # It receives the data from the 'File Upload' module.
    # It returns a reactive list containing:
    # --- (1) The user's selected 'Outlier Analysis' parameters
    # --- (2) The results (data.table) from the outlier analysis
    # --- --- Note: Results are only generated if button is pressed
    outlier_data <- mod_outlier_analysis_server("outlier_analysis_1",
                                                file_upload_data = file_upload_data)

    # 4. Instantiate the 'Model Validation' module
    # It receives the data from:
    # --- (1) The 'File Upload' module
    # --- (2) The differences in 'Nonselectivity' module
    # It returns a reactive list containing:
    # --- (1) The resulting assessment test table (button must be pressed)
    # --- (2) The resulting assessment plot (button must be pressed)
    model_validation_data <- mod_model_validation_server("model_validation_1",
                                                         file_upload_data = file_upload_data,
                                                         mod_dins_params = mod_dins_params)

    # 5. Instantiate the 'Results' module
    # It receives the data from all previous modules
    # It returns nothing
    mod_results_server("results_1",
                       file_upload_data = file_upload_data,
                       mod_dins_params = mod_dins_params,
                       outlier_data = outlier_data,
                       model_validation_data = model_validation_data)

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}

