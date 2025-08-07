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
      actionBttn(
        inputId = ns("show_outlier_analysis_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        style = "gradient",
        color = "success"
      )
    ),

    htmlOutput(outputId = ns("outlier_analysis_explanation")),

    # Options card
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("sliders", class = "header-icon"),
        h3("Customizable Outlier Analysis Options")
      ),
      div(
        class = "card-body",

        # Outlier test selection
        div(class = "parameter-section",
            h5("Select Outlier Test"),
            radioGroupButtons(
              inputId = ns("outlier_test"),
              label = NULL,
              choiceNames = c("Between Samples", "Within Samples"),
              choiceValues = c("burnett", "qrange"),
              selected = "burnett",
              status = "primary",
              justified = TRUE
            )
        ),

        # Confidence level selection
        div(class = "parameter-section",
            h5("Select Desired Confidence Level"),
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
    ),

    # Results card
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("table", class = "header-icon"),
        h3("Outlier Analysis Results")
      ),
      div(
        class = "card-body",
        div(
          class = "text-center mb-4",
          actionBttn(
            inputId = ns("get_outlier_results"),
            label = "Analyze",
            icon = icon("magnifying-glass-chart", class = "fa-solid"),
            color = "royal",
            size = "lg",
            style = "gradient"
          )
        ),
        withSpinner(
          DT::DTOutput(outputId = ns("outlier_results")),
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
  moduleServer(id, function(input, output, session) {

    # --- Help Text Logic ---
    hide <- reactiveValues(hide = TRUE)
    observeEvent(input$show_outlier_analysis_explanation, {
      hide$hide <- !hide$hide
    })

    output$outlier_analysis_explanation <- renderUI({
      if (!hide$hide) {
        HTML(help_button_page_3_text())
      }
    })

    # --- Reactive Data Preparation ---
    cs_data_long <- reactive({
      # This reactive depends only on the clinical sample data from the first module
      req(file_upload_data$raw_cs_data())

      cs_data_repaired <- commutability::repair_data(
        data = file_upload_data$raw_cs_data(),
        type = "cs",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )

      keep_these_cols <- setdiff(
        names(cs_data_repaired),
        file_upload_data$remove_ivd_mds()
      )

      cs_data_repaired <- subset(
        x = cs_data_repaired,
        select = keep_these_cols
      )

      ref_method <- file_upload_data$reference_method()

      commutability::get_comparison_data(
        data = cs_data_repaired,
        reference = ref_method
      )
    })

    # --- Server Logic ---

    # Create a reactiveVal to store the results. Initialize with NULL.
    analysis_results_val <- reactiveVal(NULL)

    # When the button is pressed, run the analysis and update the reactiveVal.
    observeEvent(input$get_outlier_results, {
      req(cs_data_long())
      results <- commutability::do_outlier_analysis(
        data = cs_data_long(),
        method = input$outlier_test,
        variable = "influence",
        level = as.numeric(input$outlier_test_conf_level),
        output = "visual"
      )
      analysis_results_val(results) # Update the value
    })

    # Render the results table from the reactiveVal.
    output$outlier_results <- DT::renderDT({
      # Require the value to be non-NULL before rendering.
      req(analysis_results_val())
      results <- analysis_results_val()

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
