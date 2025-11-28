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
    tabsetPanel(
      id = ns("model_validation_tabs"),
      type = "pills",
      # --- Panel 1 - Formal Assesment ---
      tabPanel(
        title = "Formal Assessment",
        value = "formal_tests",
        icon = icon("clipboard"),
        # --- Panel 1 - Card 1 - Results ---
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("table", class = "header-icon"),
            h3(
              "Formal Assessment Results"
            ),
            div(
              class = "text-center mt-4 mb-3",
              actionBttn(
                inputId = ns("run_tests"),
                label = "Run tests",
                icon = icon(name = "list-check"),
                style = "gradient",
                size = "md",
                color = "royal"
              ),
              actionBttn(
                inputId = ns("clear_test_results"),
                label = NULL,
                icon = icon("trash"),
                style = "gradient",
                color = "primary"
              )
            )
          ),
          div(
            class = "card-body",
            withSpinner(
              DT::DTOutput(outputId = ns("formal_assessment_tests")),
              type = 4,
              color = "#28A745"
            )
          )
        )
      ),

      # --- Panel 2 - Visual Assesment ---
      tabPanel(
        title = "Visual Assessment",
        value = "assessment_plots",
        icon = icon("chart-line"),
        div(
          class = "dashboard-card-drop-down",
          div(
            class = "card-header",
            icon("chart-line", class = "header-icon"),
            h3("Plotting Options")
          ),
          box(
            title = tagList(
              icon("sliders", class = "section-icon"),
              "Customize Appearance & Advanced Options"
            ),
            collapsible = TRUE,
            collapsed = TRUE,
            width = 12,
            solidHeader = TRUE,
            status = "primary",
            # Customization Inputs
            fluidRow(
              column(
                width = 4,
                h4("Set Labels"),
                div(
                  class = "parameter-section",
                  h5("Plot Title"),
                  textInputIcon(
                    inputId = ns("ap_title"),
                    label = NULL,
                    placeholder = "Title...",
                    icon = icon("heading")
                  ),
                  h5("X-Axis"),
                  textInputIcon(
                    inputId = ns("ap_x_title"),
                    label = NULL,
                    placeholder = "Auto",
                    icon = icon("arrows-left-right")
                  ),
                  h5("Y-Axis"),
                  textInputIcon(
                    inputId = ns("ap_y_title"),
                    label = NULL,
                    placeholder = "Auto",
                    icon = icon("arrows-up-down")
                  )
                )
              ),
              column(
                width = 4,
                h4("Download Dimensions"),
                div(
                  class = "parameter-section",
                  h5("Width (cm)"),
                  numericInputIcon(
                    inputId = ns("ap_width"),
                    label = NULL,
                    value = 15,
                    min = 5,
                    icon = icon("ruler-horizontal")
                  ),
                  h5("Height (cm)"),
                  numericInputIcon(
                    inputId = ns("ap_height"),
                    label = NULL,
                    value = 9.3,
                    min = 5,
                    icon = icon("ruler-vertical")
                  )
                )
              ),
              column(
                width = 4,
                h4("Additional Download Options"),
                div(
                  class = "parameter-section",
                  h5("Format"),
                  radioGroupButtons(
                    inputId = ns("plot_download_file_type"),
                    label = NULL,
                    choiceNames = c("PDF", "PNG", "TIF"),
                    choiceValues = c(".pdf", ".png", ".tif"),
                    selected = ".png",
                    status = "primary",
                    justified = TRUE,
                    size = "normal"
                  ),
                  h5("Quality (DPI)"),
                  numericInputIcon(
                    inputId = ns("plot_download_quality"),
                    label = NULL,
                    value = 300,
                    min = 72,
                    max = 1500,
                    icon = icon("image")
                  )
                )
              )
            )
          ),
          div(
            class = "card-body",
            # Plot type selection
            fluidRow(
              column(
                width = 6,
                div(
                  class = "parameter-section",
                  h5(
                    "Select Diagnostic Plot",
                    div(
                      class = "input-note",
                      # --- Force the Icon to the Right ---
                      style = "display: inline-block; margin-left: 5px;",
                      icon(name = "info-circle"),
                      id = ns("plot_explanation_tool_tip")
                    )
                  ),
                  virtualSelectInput(
                    inputId = ns("which_plot"),
                    label = NULL,
                    choices = c(
                      "Residuals vs Fitted Values" = "residual_plot",
                      "Histogram of Residuals" = "residual_histogram",
                      "QQ Normal Plot" = "qq_plot",
                      "SD vs Concentration" = "sd_vs_concentration",
                      "CV vs Concentration" = "cv_vs_concentration"
                    ),
                    selected = "residual_plot",
                    search = FALSE,
                    multiple = FALSE
                  )
                )
              ),
              column(
                width = 6,
                div(
                  class = "parameter-section",
                  h5("Display Options"),
                  div(
                    class = "switch-container",
                    materialSwitch(
                      inputId = ns("include_se_band"),
                      label = "Include Error Ribbon",
                      value = FALSE,
                      status = "success"
                    ),
                    conditionalPanel(
                      condition = sprintf(
                        "input['%s'] == 'sd_vs_concentration'",
                        ns("which_plot")
                      ),
                      materialSwitch(
                        inputId = ns("var_instead"),
                        label = "Use Variance",
                        value = FALSE,
                        status = "success"
                      )
                    ),
                    conditionalPanel(
                      condition = sprintf(
                        "input['%s'] == 'cv_vs_concentration'",
                        ns("which_plot")
                      ),
                      materialSwitch(
                        inputId = ns("cv_percent"),
                        label = "CV %",
                        value = FALSE,
                        status = "success"
                      ),
                    )
                  )
                )
              )
            ),
            shinyBS::bsTooltip(
              id = ns("plot_explanation_tool_tip"),
              title = paste0(
                "QQ, SD, and CV are abbreviations for Quantile-Quantile, ",
                "Standard Deviation, and Coefficient of Variation, Respectively."
              ),
              placement = "right"
            )
          )
        ),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("chart-line", class = "header-icon"),
            h3("Generate Model Assessment Plot"),
            actionBttn(
              inputId = ns("plot_assessment_plots"),
              label = "Plot",
              icon = icon("magnifying-glass-chart"),
              style = "gradient",
              color = "royal",
              size = "md"
            ),
            downloadBttn(
              outputId = ns("download_assessment_plots"),
              label = "Download",
              icon = icon("download"),
              style = "gradient",
              color = "royal",
              size = "md"
            )
          ),
          div(
            class = "card-body",
            div(
              # Plot output
              div(
                class = "plot-container",
                withSpinner(
                  ui_element = plotOutput(outputId = ns("assessment_plots"), width = "100%", height = "auto"),
                  type = 4,
                  color = "#28A745"
                )
              )
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
#' @param user_input_params A reactive list from the user input module.
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
      # This will create or update an object named 'debug_cs_data' in your R Global Environment
      # every time the cs_data_long() reactive updates.
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
          holm = FALSE,#(input$sim_testing == "yes"),
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
          dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
          buttons = list(
            list(extend = 'copy', className = 'btn-dt'),
            list(extend = 'csv', className = 'btn-dt'),
            list(extend = 'excel', className = 'btn-dt'),
            list(extend = 'pdf', className = 'btn-dt'),
            list(extend = 'print', className = 'btn-dt')
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
            "curve_se" = input$include_se_band,
            "cv_percent" = input$cv_percent,
            "var_instead" = input$var_instead
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
