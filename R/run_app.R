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
#' @import commutability
#' @import data.table
#' @import fasteqa
#' @import ggplot2
#' @import readxl
#' @import writexl
#' @import base64enc
#' @import rmarkdown
#' @import shiny
#' @import smooth.commutability
#' @import tinytex
#' @import tools
run_app <- function() {
  options(knitr.kable.NA = '', shiny.autoreload = TRUE)

  # --- GLOBAL UI ---
  ui <- glassPage(
    title = "Commutability Evaluation",

    # 1. Branding (Logo Area)
    branding = tags$div(
      # Using an icon as placeholder, replace with img(src="...") later if needed
      style = "color: #605CA8; font-size: 28px;",
      icon("dna")
    ),

    # 2. Header Extras (Right side)
    header_items = tagList(
      div(class = "app-version", "Beta v1.0"),
      glassButton("help_docs", label = "Docs", icon = icon("book"), width = "100px"),
      div(class = "user-profile-mini",
          div(class = "user-avatar", icon("user")),
          span("Guest User")
      )
    ),

    # 3. Sidebar (Navigation)
    sidebar = glassSidebar(
      inputId = "main_nav",
      glassNavItem("file_upload", icon("upload"), "Data Upload", active = TRUE),
      glassNavItem("dins", icon("arrows-left-right-to-line"), "Differences in Nonselectivity"),
      glassNavItem("outliers", icon("ruler"), "Outlier Analysis"),
      glassNavItem("model_val", icon("clipboard"), "Model Validation"),
      glassNavItem("results", icon("square-poll-horizontal"), "Results")
    ),

    # 4. Content Routes (The Pages)
    # Important: Use glassRoute here, NOT tabPanel or tabItem
    glassRoute("file_upload", mod_file_upload_ui("file_upload_1")),
    glassRoute("dins", mod_dins_ui("differences_in_non_selectivity_1")),
    glassRoute("outliers", mod_outlier_analysis_ui("outlier_analysis_1")),
    glassRoute("model_val", mod_model_validation_ui("model_validation_1")),
    glassRoute("results", mod_results_ui("results_1"))
  )

  # --- BACKEND ---
  server <- function(input, output, session) {

    # 1. File Upload Module
    file_upload_data <- mod_file_upload_server("file_upload_1")

    # 2. Nonselectivity Module
    mod_dins_params <- mod_dins_server("differences_in_non_selectivity_1",
                                       file_upload_data = file_upload_data)

    # 3. Outlier Analysis Module
    outlier_data <- mod_outlier_analysis_server("outlier_analysis_1",
                                                file_upload_data = file_upload_data)

    # 4. Model Validation Module
    model_validation_data <- mod_model_validation_server("model_validation_1",
                                                         file_upload_data = file_upload_data,
                                                         mod_dins_params = mod_dins_params)

    # 5. Results Module
    mod_results_server("results_1",
                       file_upload_data = file_upload_data,
                       mod_dins_params = mod_dins_params,
                       outlier_data = outlier_data,
                       model_validation_data = model_validation_data)

  }

  shinyApp(ui = ui, server = server)
}
