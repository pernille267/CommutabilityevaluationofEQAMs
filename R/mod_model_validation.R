#' Model Validation UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the model validation module.
#' @noRd
mod_model_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "version-badge",
      icon("flask"),
      "Commutability Evaluation: Beta Version S1.0"
    ),
    # --- Dashboard Tab Header -------------------------------------------------
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("clipboard"),
        "Model Validation and Verification Tests"
      ),
      glassButton(
        inputId = ns("show_model_validation_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question")
      )
    ),

    # --- Help text output -----------------------------------------------------
    glassTogglePanel(
      triggerId = ns("show_model_validation_explanation"),
      help_button_page_4A_text()
    ),

    # --- Create Tabset Panels for Model Assessment ----------------------------
    glassTabsetPanel(
      inputId = ns("model_validation_tabs"),
      selected = "formal_tests",
      color = "purple",

      # --- Panel 1 - Formal Assesment ---
      glassTabPanel(
        title = "Formal Assessment",
        value = "formal_tests",
        icon = icon("clipboard"),

        # --- Panel 1 - Card 1 - Results ---
        glassResultCard(
          inputId = ns("card_formal_results"),
          title = "Formal Assessment Results",
          icon = icon("table"),
          width = "100%",
          toolbar = div(
            style = "display: flex; gap: 10px;",
            glassButton(
              inputId = ns("run_tests"),
              label = "Run tests",
              icon = icon(name = "list-check"),
              color = "purple",
              width = "160px"
            ),
            glassButton(
              inputId = ns("clear_test_results"),
              label = "Clear",
              icon = icon("trash"),
              color = "purple",
              width = "120px"
            )
          ),
          withSpinner(
            DT::DTOutput(outputId = ns("formal_assessment_tests")),
            type = 4,
            color = "#605ca8"
          )
        )
      ),

      # --- Panel 2 - Visual Assesment ---
      glassTabPanel(
        title = "Visual Assessment",
        value = "assessment_plots",
        icon = icon("chart-line"),

        # --- Options Card (Modified) ---
        glassCard(
          inputId = ns("card_plot_options"),
          title = "Customize Appearance & Advanced Options",
          icon = icon("sliders"),
          collapsible = TRUE,
          collapsed = TRUE,
          width = "100%",

          # Customization Inputs
          fluidRow(
            column(
              width = 4,
              h4("Set Labels"),
              div(
                class = "parameter-section",
                glassTextInput(
                  inputId = ns("ap_title"),
                  label = "Plot Title",
                  placeholder = "Title...",
                  label_icon = icon("heading"),
                  width = "100%"
                ),
                glassTextInput(
                  inputId = ns("ap_x_title"),
                  label = "X-Axis",
                  placeholder = "Auto",
                  label_icon = icon("arrows-left-right"),
                  width = "100%"
                ),
                glassTextInput(
                  inputId = ns("ap_y_title"),
                  label = "Y-Axis",
                  placeholder = "Auto",
                  label_icon = icon("arrows-up-down"),
                  width = "100%"
                )
              )
            ),
            column(
              width = 4,
              h4("Download Dimensions"),
              div(
                class = "parameter-section",
                glassNumericInput(
                  inputId = ns("ap_width"),
                  label = "Width",
                  value = 15,
                  min = 5,
                  max = 50,
                  label_icon = icon("ruler-horizontal"),
                  width = "100%",
                  accept = c(5, 50),
                  unit = " CM",
                  warning_unacceptable = "Width must be between 5 and 50 cm"
                ),
                glassNumericInput(
                  inputId = ns("ap_height"),
                  label = "Height",
                  value = 9.3,
                  min = 5,
                  max = 50,
                  label_icon = icon("ruler-vertical"),
                  width = "100%",
                  accept = c(5, 50),
                  unit = " CM",
                  warning_unacceptable = "Height must be between 5 and 50 cm"
                )
              )
            ),
            column(
              width = 4,
              h4("Additional Download Options"),
              div(
                class = "parameter-section",
                glassRadioButtons(
                  inputId = ns("plot_download_file_type"),
                  label = "Format",
                  choices = c("PDF" = ".pdf", "PNG" = ".png", "TIF" = ".tif"),
                  selected = ".png",
                  width = "100%"
                ),
                glassNumericInput(
                  inputId = ns("plot_download_quality"),
                  label = "Quality",
                  value = 300,
                  min = 72,
                  max = 1500,
                  label_icon = icon("image"),
                  unit = " DPI",
                  width = "100%",
                  accept = c(72, 1500),
                  warning_unacceptable = "DPI must be between 72 and 1500"
                )
              )
            )
          ),

          # Display Options (Moved here from removed column, now horizontal)
          h4("Display Options"),
          fluidRow(
            column(
              width = 4,
              glassRadioButtons(
                inputId = ns("include_se_band"),
                label = "Include Error Ribbon",
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = FALSE,
                width = "100%"
              )
            ),
            column(
              width = 4,
              glassTogglePanel(
                triggerId = ns("which_plot"),
                show_when = "sd_vs_concentration",
                glassRadioButtons(
                  inputId = ns("var_instead"),
                  label = "Use Variance Instead",
                  choices = c("No" = FALSE, "Yes" = TRUE),
                  selected = FALSE,
                  width = "100%"
                )
              )
            ),
            column(
              width = 4,
              glassTogglePanel(
                triggerId = ns("which_plot"),
                show_when = "cv_vs_concentration",
                glassRadioButtons(
                  inputId = ns("cv_percent"),
                  label = "Show as CV %",
                  choices = c("No" = FALSE, "Yes" = TRUE),
                  selected = FALSE,
                  width = "100%"
                )
              )
            )
          )
        ),

        # --- Plot Result Card (Dropdown moved here) ---
        glassResultCard(
          inputId = ns("card_assessment_plot"),
          title = "Generate Model Assessment Plot",
          icon = icon("chart-line"),
          width = "100%",
          toolbar = div(
            style = "display: flex; gap: 10px; align-items: center;",

            # --- Dropdown Moved to Toolbar ---
            div(
              style = "width: 250px;",
              glassDropdown(
                inputId = ns("which_plot"),
                label = NULL,
                help_text = "Select Plot Type",
                choices = c(
                  "Residuals vs Fitted Values" = "residual_plot",
                  "Histogram of Residuals" = "residual_histogram",
                  "QQ Normal Plot" = "qq_plot",
                  "SD vs Concentration" = "sd_vs_concentration",
                  "CV vs Concentration" = "cv_vs_concentration"
                ),
                selected = "residual_plot",
                width = "100%"
              )
            ),
            glassButton(
              inputId = ns("plot_assessment_plots"),
              label = "Plot",
              icon = icon("magnifying-glass-chart"),
              color = "purple",
              width = "auto"
            ),
            glassDownloadButton(
              outputId = ns("download_assessment_plots"),
              label = "Download",
              icon = icon("download"),
              width = "auto"
            )
          ),
          div(
            class = "plot-container",
            plotOutput(
              outputId = ns("assessment_plots"),
              width = "100%",
              height = "auto"
            )
          )
        )
      )
    )
  )
}

#' Model Validation Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list from the file upload module.
#' @param mod_dins_params A reactive list from the user input module.
#'
#' @return This module does not return any values.
#' @noRd
mod_model_validation_server <- function(id, file_upload_data, mod_dins_params) {
  moduleServer(id, function(input, output, session) {

    # --- Reactive Data Preparation ---
    cs_data_long <- reactive({
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
      raw_data <- commutability::get_comparison_data(
        data = cs_data_repaired,
        reference = ref_method
      )
      transformation <- switch(
        mod_dins_params()$transformation,
        "identity" = "identity",
        "ln" = "log#e",
        "boxcox" = "boxcox#0.5",
        "identity"
      )
      commutability::transform_data(
        data = raw_data,
        transformation = transformation
      )
    })

    # --- *DEBUGGING* look at cs_data_long() ---
    observe({
      debug_cs_data <<- cs_data_long()
    })

    # --- Server Logic for Formal Tests ---
    assessment_tests_results <- eventReactive(input$run_tests, {
      req(file_upload_data$is_valid(), cs_data_long())
      if (file_upload_data$is_valid()) {
        data_without_NAs <- na.omit(object = cs_data_long())
        commutability::perform_assessment_tests(
          data = data_without_NAs,
          B = 100,
          method = mod_dins_params()$pi_method,
          level = 0.05,
          robust = TRUE,
          adjust_p_value = FALSE,
          holm = FALSE,
          output = "visual"
        )
      }
    })

    output$formal_assessment_tests <- DT::renderDT({
      req(assessment_tests_results())
      results <- assessment_tests_results()

      DT::datatable(
        results,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scolllX = TRUE,
          scrollY = "400px",
          pageLength = 25,
          dom = "Bfrtip",
          buttons = list(
            list(extend = 'copy', className = 'glass-dt-btn'),
            list(extend = 'csv', className = 'glass-dt-btn'),
            list(extend = 'excel', className = 'glass-dt-btn'),
            list(extend = 'pdf', className = 'glass-dt-btn'),
            list(extend = 'print', className = 'glass-dt-btn')
          ),
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

    # --- Server Logic for Assessment Plots ---
    assessment_plot_object <- eventReactive(input$plot_assessment_plots, {
      req(file_upload_data$is_valid(), cs_data_long())
      if (file_upload_data$is_valid()) {

        # Helper: Convert string input from glassRadioButtons to logical
        # glassRadioButtons returns character ("TRUE"/"FALSE")
        to_logical <- function(val) {
          if (is.null(val)) return(FALSE)
          as.logical(val)
        }

        commutability::plot_assessment_plots(
          data = cs_data_long(),
          method = mod_dins_params()$pi_method,
          type = input$which_plot,
          draw_curves = TRUE,
          plot_theme = "default",
          additional_arguments = list(
            "main_title" = input$ap_title,
            "x_name" = input$ap_x_title,
            "y_name" = input$ap_y_title,
            # Convert UI string inputs to logical for internal logic
            "curve_se" = to_logical(input$include_se_band),
            "cv_percent" = to_logical(input$cv_percent),
            "var_instead" = to_logical(input$var_instead)
          )
        )
      }
    })

    output$assessment_plots <- renderPlot(
      res = 99,
      height = function() {
        req(is.numeric(input$ap_width), input$ap_width > 0, is.numeric(input$ap_height))
        plot_width_px <- session$clientData[[paste0("output_", session$ns("assessment_plots"), "_width")]]
        req(plot_width_px)
        plot_width_px * (input$ap_height / input$ap_width)
      },
      {
        plot_obj <- assessment_plot_object()
        if (!is.null(plot_obj)) {
          plot(plot_obj)
        }
      }
    )

    output$download_assessment_plots <- downloadHandler(
      filename = function() {
        paste(input$which_plot, "_", Sys.Date(), input$plot_download_file_type, sep = '')
      },
      content = function(file) {
        plot_to_save <- assessment_plot_object()
        req(plot_to_save)
        ggplot2::ggsave(
          file,
          plot = plot_to_save,
          width = as.numeric(input$ap_width),
          height = as.numeric(input$ap_height),
          units = "cm",
          dpi = as.numeric(input$plot_download_quality)
        )
      }
    )

    return(
      list(
        formal_results = assessment_tests_results,
        assessment_plot = assessment_plot_object,
        assessment_plot_type = reactive({
          input$which_plot
        })
      )
    )

  })
}
