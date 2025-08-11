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
    # Header Information
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("clipboard"),
        "Model Validation and Verification Tests"
      ),
      actionBttn(
        inputId = ns("show_model_validation_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        style = "gradient",
        color = "success"
      )
    ),

    # Help text output
    htmlOutput(outputId = ns("model_validation_explanation")),

    # Tabset Panels
    tabsetPanel(
      id = ns("model_validation_tabs"),
      type = "pills",

      # Formal tests Panel
      tabPanel(
        title = "Formal Model Assessment",
        value = "formal_tests",
        icon = icon("clipboard"),
        # Results card
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("table", class = "header-icon"),
            h3("Formal Model Assessment Analysis Results")
          ),
          div(
            class = "card-body",
            div(class = "text-center mt-4 mb-3",
                actionBttn(
                  inputId = ns("run_tests"),
                  label = "Run tests",
                  icon = icon(name = "list-check"),
                  style = "gradient",
                  size = "lg",
                  color = "royal"
                ),
                actionBttn(
                  inputId = ns("clear_test_results"),
                  label = NULL,
                  icon = icon("trash"),
                  style = "gradient",
                  color = "primary"
                )
            ),

            # Test results output
            withSpinner(
              DT::DTOutput(outputId = ns("formal_assessment_tests")),
              type = 4,
              color = "#28A745"
            )
          )
        )
      ),

      # Visual tests Panel
      tabPanel(
        title = "Model Assessment Plots",
        value = "assessment_plots",
        icon = icon("chart-line"),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("chart-line", class = "header-icon"),
            h3("Visualizing Model Assessment: Customizable Plot Options")
          ),
          div(
            class = "card-body",
            # Plot type selection
            fluidRow(
              column(
                width = 5,
                div(class = "parameter-section",
                    h5("Model Assessment Plot to Display"),
                    radioGroupButtons(
                      inputId = ns("which_plot"),
                      label = NULL,
                      choiceValues = c(
                        "residual_plot",
                        "residual_histogram",
                        "qq_plot",
                        "sd_vs_concentration",
                        "cv_vs_concentration"
                      ),
                      choiceNames = c(
                        "Residuals Versus Fitted Values",
                        "Histogram of Residuals",
                        "QQ Normal Plot",
                        "SD Versus Concentration Plot",
                        "CV Versus Concentration Plot"),
                      selected = "residual_plot",
                      status = "primary",
                      direction = "vertical",
                      justified = TRUE
                    )
                )
              ),
              column(
                width = 5,
                div(
                  class = "input-note",
                  icon("info-circle"),
                  "Abbreviations"
                ),
                div(
                  class = "input-note",
                  "QQ is short for Quantile-Quantile"
                ),
                div(
                  class = "input-note",
                  "SD is short for Standard Deviation"
                ),
                div(
                  class = "input-note",
                  "CV is short for Coefficient of Variation"
                )
              )
            ),

            # Plot customization options
            fluidRow(
              column(
                width = 5,
                div(class = "input-group-container",
                    h5("Plot Title"),
                    textInputIcon(inputId = ns("ap_title"), label = NULL, placeholder = "Some title ...", icon = icon("pen")),
                    h5("X-Axis Title"),
                    textInputIcon(inputId = ns("ap_x_title"), label = NULL, placeholder = "automatic", icon = icon("pen")),
                    h5("Y-Axis Title"),
                    textInputIcon(inputId = ns("ap_y_title"), label = NULL, placeholder = "automatic", icon = icon("pen"))
                ),
                div(class = "switch-container",
                    h5("Additional Plotting Options"),
                    materialSwitch(inputId = ns("include_se_band"), label = "Include Error Ribbon", value = FALSE, status = "success"),
                    materialSwitch(inputId = ns("cv_percent"), label = "CV in Percent (%)", value = FALSE, status = "success"),
                    materialSwitch(inputId = ns("var_instead"), label = "Use Variance Instead", value = FALSE, status = "success")
                )
              ),
              column(
                width = 5,
                div(class = "input-group-container",
                    h5("Plot Width (cm)"),
                    numericInputIcon(inputId = ns("ap_width"), label = NULL, value = 15, min = 2, max = 100, step = 0.5, icon = icon("arrows-left-right")),
                    h5("Plot Height (cm)"),
                    numericInputIcon(inputId = ns("ap_height"), label = NULL, value = 9.3, min = 2, max = 100, step = 0.5, icon = icon("arrows-up-down")),
                    h5("On-Screen Resolution (PPI)"),
                    numericInputIcon(inputId = ns("ap_res"), label = NULL, value = 96, min = 72, max = 300, step = 12, icon = icon("star"))
                ),
                div(
                  class = "switch-container",
                  h5("Download Options"),
                  radioGroupButtons(
                    inputId = ns("plot_download_file_type"),
                    choiceNames = c("PDF", "PNG", "TIF"),
                    choiceValues = c(".pdf", ".png", ".tif"),
                    status = "primary",
                    justified = TRUE
                  ),
                  radioGroupButtons(
                    inputId = ns("plot_download_quality"),
                    choiceNames = c("300 dpi", "600 dpi", "1200 dpi"),
                    choiceValues = c("300", "600", "1200"),
                    status = "primary",
                    justified = TRUE
                  ),
                  div(
                    class = "input-note",
                    icon("info-circle"),
                    "dpi (dots per inch) refers to the quality of the downloaded plot"
                  )
                )
              )
            )
          )
        ),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("chart-line", class = "header-icon"),
            h3("Generate Model Assessment Plot")
          ),
          div(
            class = "card-body",
            div(
              # Plot and download buttons
              div(class = "button-container",
                  actionBttn(
                    inputId = ns("plot_assessment_plots"),
                    label = "Plot",
                    icon = icon("magnifying-glass-chart"),
                    style = "gradient",
                    color = "royal",
                    size = "lg"
                  ),
                  downloadBttn(
                    outputId = ns("download_assessment_plots"),
                    label = "Download",
                    icon = icon("download"),
                    style = "gradient",
                    color = "royal",
                    size = "lg"
                  )
              ),

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

    # --- Help Text Logic ---
    hide <- reactiveValues(hide = TRUE)
    observeEvent(input$show_model_validation_explanation, {
      hide$hide <- !hide$hide
    })

    output$model_validation_explanation <- renderUI({
      req(input$model_validation_tabs)
      if (!hide$hide) {
        if (input$model_validation_tabs == "formal_tests") {
          wellPanel(HTML(help_button_page_4A_text()))
        } else if (input$model_validation_tabs == "assessment_plots") {
          wellPanel(HTML(help_button_page_4B_text()))
        }
      }
    })

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
