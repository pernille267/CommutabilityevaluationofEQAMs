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
      # Status Bar for Indicators
      div(
        class = "glass-header-group",
        # Referansemetode (R)
        glassHeaderIndicator(
          inputId = "global_ref_indicator",
          type = "reference",
          visible = FALSE
        ),
        # Diagnostikk (Hjerte/Status)
        glassHeaderIndicator(
          inputId = "global_diag_indicator",
          type = "diagnostics",
          visible = FALSE
        ),
        # Outlier Filter (Linjal)
        glassHeaderIndicator(
          inputId = "global_outlier_indicator",
          type = "filter",
          tooltip_text = "No active filters.",
          visible = FALSE
        )
      ),
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

    # Activate Loading-System
    useGlassLoader(),

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
                                                         mod_dins_params = mod_dins_params,
                                                         outlier_data = outlier_data)

    # 5. Results Module
    mod_results_server("results_1",
                       file_upload_data = file_upload_data,
                       mod_dins_params = mod_dins_params,
                       outlier_data = outlier_data,
                       model_validation_data = model_validation_data)

    # --- Header Indicators ---

    # --- Update Reference Method Indicator ---
    observe({
      ref <- file_upload_data$reference_method()
      if (!is.null(ref) && ref != "") {
        updateGlassHeaderIndicator(
          session, "global_ref_indicator",
          visible = TRUE,
          tooltip_text = paste("Reference Method:", ref)
        )
      } else {
        updateGlassHeaderIndicator(session, "global_ref_indicator", visible = FALSE)
      }
    })

    # --- Update Data Validity Indicator ---
    observe({
      # --- Get Status Text from 'file_upload_data' ---
      status_text <- file_upload_data$diagnostics_status_text()
      if (!is.null(status_text) && status_text != "") {
        # Bestem ikon og farge basert på teksten (enkel heuristikk)
        status_type <- "success"
        icon_name <- "heart-pulse"

        lower_txt <- tolower(status_text)
        if (grepl("fail|error|invalid", lower_txt)) {
          status_type <- "error"
          icon_name <- "triangle-exclamation"
        } else if (grepl("warning", lower_txt)) {
          status_type <- "warning"
          icon_name <- "circle-exclamation"
        }

        updateGlassHeaderIndicator(
          session, "global_diag_indicator",
          visible = TRUE,
          tooltip_text = status_text,
          status = status_type,
          icon_name = icon_name
        )
      } else {
        updateGlassHeaderIndicator(session, "global_diag_indicator", visible = FALSE)
      }
    })

    # A. Lytt på endringer i outliers som skal fjernes (fra modulen)
    observeEvent(outlier_data$outliers_to_remove(), {
      to_remove <- outlier_data$outliers_to_remove()
      if (!is.null(to_remove) && nrow(to_remove) > 0) {
        # Det finnes et aktivt filter!
        # 1. Oppdater teksten på Header Indicator (men hold den skjult enn så lenge)
        msg <- paste0("Active Filter Applied: ", nrow(to_remove),
                      " outlier(s) from Outlier Analysis are currently flagged for removal")
        updateGlassHeaderIndicator(session, "global_outlier_indicator",
                                   tooltip_text = msg,
                                   visible = FALSE) # Skjult fordi den store boksen vises først

        # 2. Vis den store notifikasjonen INNE i modulen
        # (Dette håndteres allerede internt i mod_outlier_analysis_server ved hjelp av updateGlassNotifyUser)

      } else {
        # Ingen filter (eller filter tømt)

        # Skjul Header Indicator
        updateGlassHeaderIndicator(session, "global_outlier_indicator", visible = FALSE)
      }
    }, ignoreNULL = FALSE)

    # B. Lytt på at brukeren LUKKER ("Dismiss") den store notifikasjonen
    # Notifikasjonen heter "outlier_notification" inne i modulen "outlier_analysis_1"
    # JS sender signalet: NS("outlier_notification") + "_dismissed"

    observeEvent(input[["outlier_analysis_1-outlier_notification_dismissed"]], {

      # Sjekk at vi faktisk har outliers (sikkerhetsnett)
      to_remove <- outlier_data$outliers_to_remove()

      if (!is.null(to_remove) && nrow(to_remove) > 0) {
        # Brukeren har lukket boksen, og vi har et aktivt filter.
        # Nå skal Header Indicator gli inn!

        updateGlassHeaderIndicator(session, "global_outlier_indicator", visible = TRUE)
      }
    })

  }

  shinyApp(ui = ui, server = server)
}
