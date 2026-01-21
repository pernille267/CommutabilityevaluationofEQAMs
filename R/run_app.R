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

    # Branding (Logo Area)
    branding = tags$div(
      # Using an icon as placeholder, replace with img(src="...") later if needed
      style = "color: #605CA8; font-size: 28px;",
      icon("dna") # Placeholder icon
    ),

    # Header Extras (Right side)
    header_items = tagList(
      # Status Bar for Indicators
      div(
        class = "glass-header-group",
        # Reference Method
        glassHeaderIndicator(
          inputId = "global_ref_indicator",
          type = "reference",
          visible = FALSE
        ),
        # Diagnostics (Heart/Status)
        glassHeaderIndicator(
          inputId = "global_diag_indicator",
          type = "diagnostics",
          visible = FALSE
        ),
        # Outlier Filter (Ruler)
        glassHeaderIndicator(
          inputId = "global_outlier_indicator",
          type = "filter",
          tooltip_text = "No active filters.",
          visible = FALSE
        )
      ),
      div(class = "app-version", "Beta v1.0"),
      glassButton("help_docs", label = "Docs", icon = icon("book"), width = "auto"),
      div(class = "user-profile-mini",
          div(class = "user-avatar", icon("user")),
          span("Guest User")
      )
    ),

    # Sidebar (Module Navigation Menu)
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
    
    # Activate Toast Notification System
    useGlassToast(),

    # Body (Modules)
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
    model_validation_data <- mod_model_validation_server(
      "model_validation_1",
      file_upload_data = file_upload_data,
      mod_dins_params = mod_dins_params,
      outlier_data = outlier_data
    )

    # 5. Results Module
    mod_results_server(
      "results_1",
      file_upload_data = file_upload_data,
      mod_dins_params = mod_dins_params,
      outlier_data = outlier_data,
      model_validation_data = model_validation_data
    )

    # --- Header Indicators ---
    
    # --- Help Documentation Button (Not Working Yet, Look at Later) ---
    observeEvent(input$help_docs, {
      doc_content <- get_app_documentation()
      full_page <- htmltools::tags$html(
        htmltools::tags$head(
          htmltools::tags$title("Application Documentation"),
          htmltools::tags$link(
            rel = "stylesheet",
            href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"
          ),
          htmltools::tags$style(
            "body { font-family: 'Source Sans Pro', sans-serif; background: #fdfdfd; padding: 20px; }"
          )
        ),
        htmltools::tags$body(
          htmltools::h2(
            tagList(icon("book"), " Application Documentation"),
            style = "color: #605CA8; border-bottom: 2px solid #eee; padding-bottom: 10px;"
          ),
          doc_content
        )
      )
      
      url <- session$registerDataObj(
        name = "app_docs",
        data = full_page,
        filter = function(data, req) {
          shiny::httpResponse(
            200,
            content_type = "text/html",
            content = as.character(data)
          )
        }
      )
      
      showGlassToast("This feature is under development.", type = "warning", duration = 6000)
      session$sendCustomMessage("open_url_new_tab", url)
    }, ignoreInit = TRUE)
    
    # --- Update Reference Method Indicator ---
    observe({
      ref <- file_upload_data$reference_method()
      if (!is.null(ref) && ref != "") {
        updateGlassHeaderIndicator(
          session, "global_ref_indicator",
          visible = TRUE,
          tooltip_text = paste("Reference Method:", ref)
        )
      } 
      else {
        updateGlassHeaderIndicator(session, "global_ref_indicator", visible = FALSE)
      }
    })

    # --- Update Data Validity Indicator ---
    observe({
      status_text <- file_upload_data$diagnostics_status_text()
      if (!is.null(status_text) && status_text != "") {
        status_type <- "success"
        icon_name <- "heart-pulse"
        lower_txt <- tolower(status_text)
        if (grepl("fail|error|invalid", lower_txt)) {
          status_type <- "error"
          icon_name <- "triangle-exclamation"
        } 
        else if (grepl("warning", lower_txt)) {
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
      } 
      else {
        updateGlassHeaderIndicator(
          session,
          "global_diag_indicator", 
          visible = FALSE
        )
      }
    })

    # --- Update Outlier Filter Indicator ---
    observeEvent(outlier_data$outliers_to_remove(), {
      to_remove <- outlier_data$outliers_to_remove()
      if (!is.null(to_remove) && nrow(to_remove) > 0) {
        # --- Active filter present ---
        msg <- paste0(
          "Active Filter Applied: ",
          nrow(to_remove),
          " outlier(s) from Outlier Analysis are currently flagged for removal"
        )
        updateGlassHeaderIndicator(
          session,
          "global_outlier_indicator",
          tooltip_text = msg,
          visible = FALSE
        )
      } 
      else {
        # --- No filter (or filter cleared) ---
        # Hide Header Indicator
        updateGlassHeaderIndicator(session, "global_outlier_indicator", visible = FALSE)
      }
    }, ignoreNULL = FALSE) # Trigger also when NULL

    # Show header indicator when outlier notification is dismissed
    observeEvent(input[["outlier_analysis_1-outlier_notification_dismissed"]], {
      to_remove <- outlier_data$outliers_to_remove()
      if (!is.null(to_remove) && nrow(to_remove) > 0) {
        # User closed the box, show header indicator with active filter
        updateGlassHeaderIndicator(
          session, 
          "global_outlier_indicator", 
          visible = TRUE
        )
      }
    })
  }
  shinyApp(ui = ui, server = server)
}
