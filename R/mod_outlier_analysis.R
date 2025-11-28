#' Outlier Analysis UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the outlier analysis module.
#' @noRd
mod_outlier_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "version-badge",
      icon("flask"),
      "Commutability Evaluation: Beta Version S1.0"
    ),
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("ruler"),
        "Perform Outlier Analysis"
      ),
      glassButton(
        inputId = ns("show_outlier_analysis_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question")
      )
    ),

    glassTogglePanel(
      triggerId = ns("show_outlier_analysis_explanation"),
      help_button_page_3_text()
    ),

    # --- Card 1 - Outlier Analysis Options ------------------------------------
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("sliders", class = "header-icon"),
        h3("Options")
      ),
      div(
        class = "card-body",
        fluidRow(
          # --- Outlier Test Selection ---
          column(
            width = 6,
            div(
              class = "parameter-section",
              h5("Test for Outliers"),
              radioGroupButtons(
                inputId = ns("outlier_test"),
                label = NULL,
                choiceNames = c("Between Samples", "Within Samples"),
                choiceValues = c("burnett", "qrange"),
                selected = "burnett",
                status = "primary",
                justified = TRUE
              )
            )
          ),
          column(
            width = 6,
            # --- Outlier Test Confidence Level Selection ---
            div(
              class = "parameter-section",
              h5("Confidence Level"),
              radioGroupButtons(
                inputId = ns("outlier_test_conf_level"),
                label = NULL,
                choiceNames = c("80 %", "90 %", "95 %", "99 %"),
                choiceValues = c(0.80, 0.90, 0.95, 0.99),
                selected = 0.95,
                status = "primary",
                justified = TRUE
              )
            )
          )
        )
      )
    ),

    # --- Card 2 - Outlier Analysis Results ------------------------------------
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("table", class = "header-icon"),
        h3("Results"),
        actionBttn(
          inputId = ns("get_outlier_results"),
          label = "Analyze",
          icon = icon("magnifying-glass-chart", class = "fa-solid"),
          color = "royal",
          size = "md",
          style = "gradient"
        )
      ),
      div(
        class = "card-body",
        withSpinner(
          DT::DTOutput(
            outputId = ns("outlier_results")
          ),
          type = 4,
          color = "#28A745"
        )
      )
    )
  )
}

#' Outlier Analysis Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list containing data from the file upload module.
#'        Expected elements are `raw_cs_data` (a reactive) and `diagnostics_cs` (a reactive).
#'
#' @return This module does not return any values.
#' @noRd
mod_outlier_analysis_server <- function(id, file_upload_data) {
  # --- Create the Module Server for the `Outlier Analysis` Section ---
  moduleServer(id, function(input, output, session) {

    # --- Reactive Data Preparation ---

    # --- Clinical Samples - Long-formatted - No Transformation ----------------
    raw_cs_data_long <- reactive({

      # Requires raw_cs_data and is_valid (to be TRUE) from first module
      req(
        file_upload_data$raw_cs_data(),
        file_upload_data$is_valid() == TRUE
      )

      # Try to Repair Clinical Sample Data
      cs_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = file_upload_data$raw_cs_data(),
            type = "cs",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        },
        error = function(e) "error",
        warning = function(w) "warning"
      )

      # If Repair Failed, it is due to invalid methods are too broken
      # Remove broken methods first and then attempt to repair again.
      if (is.character(cs_data_repaired)) {
        keep_these_cols <- setdiff(
          names(file_upload_data$raw_cs_data()),
          file_upload_data$remove_ivd_mds()
        )
        cs_data_repaired <- data.table::copy(
          file_upload_data$raw_cs_data()
        )[, keep_these_cols, with = FALSE]

        cs_data_repaired <- tryCatch(
          expr = {
            commutability::repair_data(
              data = cs_data_repaired,
              type = "cs",
              remove_invalid_methods = FALSE,
              include_repair_summary = FALSE
            )
          },
          error = function(e) "error",
          warning = function(w) "warning"
        )
        if (is.character(cs_data_repaired)) {
          return(NULL)
        }
      }
      else {
        keep_these_cols <- setdiff(
          names(file_upload_data$raw_cs_data()),
          file_upload_data$remove_ivd_mds()
        )
        cs_data_repaired <- subset(
          x = cs_data_repaired,
          select = keep_these_cols
        )
      }

      ref_method <- file_upload_data$reference_method()

      raw_data <- commutability::get_comparison_data(
        data = cs_data_repaired,
        reference = ref_method
      )

      return(raw_data)
    })

    # --- Create a reactiveVal to Cache Results. -------------------------------
    analysis_results_val <- reactiveVal(NULL)

    # --- Update reactiveVal when `Analyze` Button is Pressed ------------------
    observeEvent(input$get_outlier_results, {
      # Require `raw_cs_data_long()` to exist
      req(raw_cs_data_long())
      results <- commutability::do_outlier_analysis(
        data = raw_cs_data_long(),
        method = input$outlier_test,
        variable = "influence",
        level = as.numeric(input$outlier_test_conf_level),
        output = "visual"
      )
      analysis_results_val(results)
    })

    # --- Render Table Using Cached Results from `analysis_results_val` --------
    output$outlier_results <- DT::renderDT({
      # Require cache to be filled before displaying table
      req(analysis_results_val())

      # Extract results from cache
      results <- analysis_results_val()

      # Render the table
      DT::datatable(
        results,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scolllX = TRUE,
          scrollY = "400px",
          pageLength = 25,
          dom = "Bfrtip",
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
          columnDefs = list(
            list(className = 'dt-center', targets = "_all")
          ),
          initComplete = JS(
            "function(settings, json) {",
            "  $(this.api().table().container()).find('.dataTables_scrollBody').on('scroll', function() {",
            "    $(this).prev('.dataTables_scrollHead').scrollLeft($(this).scrollLeft());",
            "  });",
            "}"
          )
        )
      )
    })

    # --- Send Relevant Module Components to Other Modules ---------------------
    return(
      list(
        results = analysis_results_val,
        params = reactive({
          list(
            outlier_test = input$outlier_test,
            outlier_test_conf_level = as.numeric(input$outlier_test_conf_level)
          )
        })
      )
    )

  })
}
