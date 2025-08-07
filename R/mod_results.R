#' Results UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the results module.
#' @noRd
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "version-badge",
      icon("flask"),
      "Commutability Evaluation: Beta Version S1.0"
    ),
    # Header
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("square-poll-horizontal"),
        "Commutability Evaluation Analysis Results"
      ),
      # The help button in the original script had the ID "show_results_tables_explanation"
      # We will create a single help button that controls context-sensitive help text.
      actionBttn(
        inputId = ns("show_results_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        style = "gradient",
        color = "success"
      )
    ),

    htmlOutput(outputId = ns("results_explanation")),

    # Select Confidence Interval
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon(name = "sliders", class = "header-icon"),
        h3("Customizable Commutability Evaluation Options")
      ),
      div(
        class = "card-body",
        fluidRow(
          column(
            width = 5,
            div(
              class = "parameter-section",
              h5("Select Desired Confidence Level"),
              radioGroupButtons(
                inputId = ns("pi_conf_level"),
                choiceValues = c(0.95, 0.99),
                choiceNames = c("95 %", "99 %"),
                selected = 0.99,
                status = "primary",
                justified = TRUE
              )
            )
          ),
          column(
            width = 5,
            div(
              class = "input-note",
              icon(name = "info-circle"),
              "Note"
            ),
            div(
              class = "input-note",
              "Confidence level here applies to the prediction intervals"
            )
          )
        )
      )
    ),

    # Tabset Panels
    tabsetPanel(
      id = ns("results_tabs"),
      type = "pills",

      # Table results Panel
      tabPanel(
        title = "Tables",
        value = "show_results_tables",
        icon = icon("table"),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon(name = "filter", class = "header-icon"),
            h3("Filtering and Table Display")
          ),
          div(
            class = "card-body",
            div(
              class = "parameter-section",
              h5("Choose Filtering for Evaluated Material Locations"),
              radioGroupButtons(
                inputId = ns("filter_eq_location"),
                choiceValues = c("n_filt", "o_inside", "o_outside"),
                choiceNames = c("No Filters", "Inside Prediction Intervals", "Outside Prediction Intervals"),
                status = "primary",
                justified = TRUE
              )
            ),
            div(
              class = "parameter-section",
              h5("Choose Filtering for IVD-MD Nonselectivity Differences"),
              radioGroupButtons(
                inputId = ns("filter_by_dins"),
                choiceValues = c("n_filt", "o_ins", "o_dins"),
                choiceNames = c("No Filters", "Acceptable", "Excessive"),
                status = "primary",
                justified = TRUE
              )
            ),
            div(
              class = "parameter-section",
              h5("Choose Format"),
              radioGroupButtons(
                inputId = ns("data_format"),
                choiceNames = c("Expanded", "Compact"),
                choiceValues = c("expanded", "compact"),
                selected = "compact",
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
            h3("Commutability Evaluation Analysis Table")
          ),
          div(
            class = "card-body",
            div(
              class = "text-center mt-4 mb-3",
              actionBttn(
                inputId = ns("calculate"),
                label = "Calculate",
                icon = icon("calculator"),
                size = "lg",
                style = "gradient",
                color = "royal"
              )
            ),
            withSpinner(
              ui_element = DT::DTOutput(outputId = ns("ce_results")),
              color = "#605ca8", # Changed color to purple to match original
              type = 4
            )
          )
        ),
        # ADDED: Report Download Card
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("file-download", class = "header-icon"),
            h3("Download Full Analysis Report")
          ),
          div(
            class = "card-body",
            p("Click the button below to download a comprehensive HTML report summarizing all data, parameters, and results from your current session."),
            div(
              class = "text-center mt-3",
              downloadBttn(
                outputId = ns("download_report"),
                label = "Download Report",
                style = "gradient",
                color = "success",
                size = "lg"
              )
            )
          )
        )
      ),

      # Plot results Panel
      tabPanel(
        title = "Plots",
        value = "show_results_plots",
        icon = icon("chart-line"),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon(name = "vial", class = "header-icon"),
            h3("Customizable Plotting Options")
          ),
          div(
            class = "card-body",
            div(
              class = "parameter-section",
              h5("Axis Tick Density"),
              radioGroupButtons(
                inputId = ns("tick_density"),
                choiceValues = c(6, 10, 15),
                choiceNames = c("Low", "Medium", "High"),
                status = "primary",
                size = "normal",
                justified = TRUE
              )
            ),
            div(
              class = "parameter-section",
              h5("Select Pattern Curve for Clinical Samples"),
              radioGroupButtons(
                inputId = ns("cs_curve"),
                choiceNames = c("None", "Diagonal", "Fitted", "Flexible"),
                choiceValues = c("none", "equivalence_curve", "fitted_curve", "smooth_curve"),
                selected = "none",
                status = "primary",
                justified = TRUE
              )
            ),
            fluidRow(
              column(
                width = 5,
                div(
                  class = "input-group-container",
                  h5("Plot Title"),
                  textInputIcon(inputId = ns("title"), label = NULL, placeholder = "Some title ...", icon = icon("t")),
                  h5("X-Axis Title"),
                  textInputIcon(inputId = ns("x_name"), label = NULL, placeholder = "Some title ...", icon = icon("pen")),
                  h5("Y-Axis Title"),
                  textInputIcon(inputId = ns("y_name"), label = NULL, placeholder = "Some title ...", icon = icon("pen")),
                  div(
                    class = "input-note",
                    icon("info-circle"),
                    "You must press the Plot button for changes to take effect"
                  )
                )
              ),
              column(
                width = 5,
                div(
                  class = "input-group-container",
                  h5("Plot Width (cm)"),
                  numericInputIcon(inputId = ns("width"), label = NULL, value = 17.6, min = 2, max = 100, step = 0.5, icon = icon("arrows-left-right")),
                  h5("Plot Height (cm)"),
                  numericInputIcon(inputId = ns("height"), label = NULL, value = 11.7, min = 2, max = 100, step = 0.5, icon = icon("arrows-up-down")),
                  h5("On-Screen Resolution (PPI)"),
                  numericInputIcon(inputId = ns("resolution"), label = NULL, value = 99, min = 99, max = 99, step = 5, icon = icon("star")),
                  div(
                    class = "input-note",
                    icon("info-circle"),
                    "Plot width is only relevant for downloaded plots"
                  )
                )
              )
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
        ),
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon(name = "chart-line", class = "header-icon"),
            h3("Generate Commutability Evaluation Plot")
          ),
          div(
            class = "card-body",
            div(
              class = "button-container",
              actionBttn(
                inputId = ns("plot"),
                label = "Plot",
                icon = icon("chart-line"),
                size = "lg",
                style = "gradient",
                color = "royal"
              ),
              downloadBttn(
                outputId = ns("download_ce_plots"),
                label = "Download",
                icon = icon("download"),
                size = "lg",
                style = "gradient",
                color = "royal"
              )
            ),
            div(
              class = "plot-container",
              style = "max-width: 100%; overflow-x: auto;", # Ensures container itself can scroll if content is too wide
              withSpinner(
                ui_element = plotOutput(outputId = ns("ce_plots"), width = "100%", height = "auto"),
                type = 4,
                color = "#605ca8"
              )
            )
          )
        )
      )
    )
  )
}

#' Results Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list from the file upload module.
#' @param mod_dins_params A reactive list from the user input module.
#' @param outlier_data A reactive list from the outlier analysis module.
#' @param model_validation_data A reactive list from the model validation module.
#'
#' @return This module does not return any values.
#' @noRd
mod_results_server <- function(id, file_upload_data, mod_dins_params, outlier_data, model_validation_data) {
  moduleServer(id, function(input, output, session) {

    # --- Help Text Logic ---
    hide <- reactiveValues(hide = TRUE)
    observeEvent(input$show_results_explanation, {
      hide$hide <- !hide$hide
    })

    output$results_explanation <- renderUI({
      if (!hide$hide) {
        # The active tab determines which help text to show
        if (input$results_tabs == "show_results_tables") {
          wellPanel(HTML(help_button_page_5A_text()))
        } else {
          wellPanel(HTML(help_button_page_5B_text()))
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

    eq_data_long <- reactive({
      req(file_upload_data$raw_eq_data())
      eq_data_repaired <- commutability::repair_data(
        data = file_upload_data$raw_eq_data(),
        type = "eqam",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
      keep_these_cols <- setdiff(
        names(eq_data_repaired),
        file_upload_data$remove_ivd_mds()
      )
      eq_data_repaired <- subset(
        x = eq_data_repaired,
        select = keep_these_cols
      )
      ref_method <- file_upload_data$reference_method()
      raw_data <- commutability::get_comparison_data(
        data = eq_data_repaired,
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

    # --- EventReactive Calculations ---

    # This reactive performs the core calculation. It will be triggered by either button.
    perform_calculation <- function() {
      if (file_upload_data$is_valid()) {
        params <- mod_dins_params()
        data_without_NA_values <- na.omit(object = cs_data_long())
        new_data_without_NA_values <- na.omit(object = eq_data_long())
        upper_zeta_val <- if (params$zeta_upper <= 1) NULL else params$zeta_upper

        commutability::do_commutability_evaluation(
          data = data_without_NA_values,
          new_data = new_data_without_NA_values,
          B = 100,
          N = 100,
          method_pi = params$pi_method,
          method_bs = "BCa",
          level_pi = as.numeric(input$pi_conf_level),
          level_bs = 0.95,
          M = params$M,
          upper_zeta = upper_zeta_val
        )
      } else {
        list(error = "Validation tests do not pass, so calculations are not possible")
      }
    }

    # Calculation for the table is triggered by the 'Calculate' button
    calculate_button_pressed <- eventReactive(input$calculate, {
      perform_calculation()
    })

    # Calculation for the plot is triggered by the 'Plot' button
    plot_data_reactive <- eventReactive(input$plot, {
      perform_calculation()
    })

    # --- Table Rendering ---
    output$ce_results <- DT::renderDT({
      results_list <- calculate_button_pressed()

      if (!is.null(results_list$error)) {
        return(DT::datatable(data.table("Error:" = results_list$error), options = list(dom = 't')))
      }

      output_tbl <- results_list$merged_ce_data

      # Apply Filters
      pi_filter <- switch(input$filter_eq_location,
                          "o_inside" = which(output_tbl$pi_inside == 1L),
                          "o_outside" = which(output_tbl$pi_inside == 0L),
                          seq_len(nrow(output_tbl))
      )

      dins_filter <- switch(input$filter_by_dins,
                            "o_ins" = which(output_tbl$dins_conclusion == 0L),
                            "o_dins" = which(output_tbl$dins_conclusion == 1L),
                            seq_len(nrow(output_tbl))
      )

      combined_filter <- intersect(pi_filter, dins_filter)

      if (length(combined_filter) > 0) {
        output_tbl <- output_tbl[combined_filter]
      }

      # Format table based on user selection
      if (input$data_format == "compact") {
        new_zeta <- paste0(
          format(output_tbl$zeta, nsmall = 2L, digits = 2L), " (",
          format(output_tbl$zeta_ci_lwr, nsmall = 2L, digits = 2L), " - ",
          format(output_tbl$zeta_ci_upr, nsmall = 2L, digits = 2L), ")"
        )
        new_dins <- ifelse(output_tbl$dins_conclusion == 1L, "deemed excessive", "deemed acceptable")
        new_meas <- paste0(
          "(", format(output_tbl$MP_B, nsmall = 2, digits = 2), ", ",
          format(output_tbl$MP_A, nsmall = 2, digits = 2), ")"
        )
        new_pred <- paste0(
          format(output_tbl$prediction, nsmall = 2L, digits = 2L), " (",
          format(output_tbl$pi_lwr, nsmall = 2L, digits = 2L), " - ",
          format(output_tbl$pi_upr, nsmall = 2L, digits = 2L), ")"
        )
        new_comm <- ifelse(output_tbl$pi_inside == 1L, "inside PI", "outside PI")
        new_conc <- sapply(seq_len(nrow(output_tbl)), function(row_id) {
          if (output_tbl$pi_inside[row_id] == 1L) output_tbl$inside_rate[row_id] * 100
          else 100 - output_tbl$inside_rate[row_id] * 100
        })

        display_tbl <- data.table(
          "IVD-MD Comparison" = output_tbl$comparison,
          "ID of evaluated material" = output_tbl$SampleID,
          "zeta (lwr - upr)" = new_zeta,
          "zeta upper" = output_tbl$zeta_upper,
          "Differences in non-selectivity is" = new_dins,
          "Measurements" = new_meas,
          "Prediction (lwr - upr)" = new_pred,
          "Evaluated material is" = new_comm,
          "Conclusion strength %" = paste0(format(new_conc, nsmall = 1L, digits = 1L), " %")
        )
      } else { # Expanded format
        display_tbl <- data.table::copy(output_tbl)
        display_tbl[, dins_conclusion := ifelse(dins_conclusion == 1L, "deemed excessive", "deemed acceptable")]
        display_tbl[, pi_inside := ifelse(pi_inside == 1L, "inside PI", "outside PI")]
        display_tbl[, inside_rate := format(inside_rate, nsmall = 3L, digits = 3L)]

        setnames(display_tbl,
                 old = c("comparison", "SampleID", "zeta", "zeta_ci_lwr", "zeta_ci_upr", "zeta_upper", "dins_conclusion", "MP_B", "MP_A", "prediction", "pi_lwr", "pi_upr", "pi_inside", "inside_rate"),
                 new = c("IVD-MD comparison", "ID of evaluated material", "zeta", "zeta CI lwr", "zeta CI upr", "upper zeta value", "Differences in non-selectivity is", "IVD-MD 1", "IVD-MD 2", "Prediction", "PI lwr", "PI upr", "Evaluated material is", "Conclusion strength")
        )
      }

      DT::datatable(
        display_tbl,
        rownames = FALSE,
        extensions = 'Buttons',
        options = list(
          scolllX = TRUE,
          scrollY = "400px",
          pageLength = 25,
          dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
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

    # --- Plot Rendering and Download ---

    plot_button_pressed <- eventReactive(input$plot, {
      req(file_upload_data$is_valid())

      results_list <- plot_data_reactive()
      req(!is.null(results_list$merged_ce_data)) # Ensure calculations have run

      if (file_upload_data$is_valid()) {
        commutability::plot_commutability_evaluation_plots(
          cs_data = cs_data_long()[, fasteqa::fun_of_replicates(.SD), by = "comparison"],
          ce_data = results_list$merged_ce_data,
          pb_data = results_list$merged_pb_data,
          exclude_rings = FALSE,
          exclude_cs = FALSE,
          plot_theme = "defailt",
          additional_arguments = list(
            "main_title" = input$title,
            "sub_title" = "",
            "x_name" = input$x_name,
            "y_name" = input$y_name,
            "n_breaks" = as.numeric(input$tick_density),
            "curve" = (input$cs_curve != "none"),
            "curve_type" = input$cs_curve
          )
        )
      }
    })

    output$ce_plots <- renderPlot(
      height = function() {
        # Ensure inputs are valid numbers before calculating
        req(is.numeric(input$width), input$width > 0, is.numeric(input$height))
        # Get the current width of the plot container in pixels
        plot_width_px <- session$clientData[[paste0("output_", session$ns("ce_plots"), "_width")]]
        req(plot_width_px)
        # Calculate the height in pixels that maintains the desired aspect ratio
        plot_width_px * (input$height / input$width)
      },
      res = 120,
      {
        plot_obj <- plot_button_pressed()
        if (!is.null(plot_obj)) {
          plot(plot_obj)
        }
      }
    )

    output$download_ce_plots <- downloadHandler(
      filename = function() {
        paste(
          toupper(mod_dins_params()$pi_method),
          "_", Sys.Date(), input$plot_download_file_type, sep = ""
        )
      },
      content = function(file) {
        plot_to_save <- plot_button_pressed()
        req(plot_to_save)
        ggplot2::ggsave(
          file,
          plot = plot_to_save,
          width = as.numeric(input$width),
          height = as.numeric(input$height),
          units = "cm",
          dpi = as.numeric(input$plot_download_quality)
        )
      }
    )

    # --- UPDATED: PDF Report Generation Logic ---
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Commutability-Evaluation-Report-", Sys.Date(), ".pdf")
      },
      content = function(file) {
        id <- showNotification(
          "Generating PDF report... This may take a moment.",
          duration = NULL,
          closeButton = FALSE,
          type = "message"
        )
        # Ensure the notification is removed when the function exits
        on.exit(removeNotification(id))

        # Correctly find the path to the Rmd template within the package.
        report_path <- system.file("report.Rmd", package = "CommutabilityevaluationofEQAMs")

        if (report_path == "") {
          stop("Could not find report.Rmd in the package. Make sure it is in the 'inst' directory.")
        }

        # Create a temporary copy to avoid writing to the package library
        temp_report <- file.path(tempdir(), "report.Rmd")
        file.copy(report_path, temp_report, overwrite = TRUE)

        # CORRECTED: Create a temporary directory for intermediate files to avoid path issues
        temp_dir <- tempfile()
        dir.create(temp_dir)

        # Gather all necessary parameters to pass to the Rmd file.
        params <- list(
          # From mod_file_upload
          cs_data = file_upload_data$raw_cs_data(),
          eq_data = file_upload_data$raw_eq_data(),
          diagnostics_cs = file_upload_data$diagnostics_cs(),
          diagnostics_eq = file_upload_data$diagnostics_eq(),
          diagnostics_both = file_upload_data$diagnostics_both(),

          # From mod_outlier_analysis
          outlier_results = outlier_data$results(),
          outlier_params = outlier_data$params(),

          # From mod_model_validation
          formal_assessment_results = model_validation_data$formal_results(),
          assessment_plot = model_validation_data$assessment_plot(),

          # From mod_dins
          dins_params = mod_dins_params(),

          # From this module (mod_results)
          ce_results = calculate_button_pressed(),
          ce_plot = plot_button_pressed()
        )

        #browser()

        # --- NEW PLOT HANDLING LOGIC ---
        # Create safe file paths for the plots
        assessment_plot_path <- tempfile(fileext = ".png")
        ce_plot_path <- tempfile(fileext = ".png")

        # Save the plots to these paths
        if (inherits(params$assessment_plot, "ggplot")) {
          ggsave(assessment_plot_path, plot = params$assessment_plot, device = "png", width = 20, height = 20, dpi = 1800, units = "cm")
          # Replace the plot object with the file path
          params$assessment_plot <- assessment_plot_path
        } else {
          params$assessment_plot <- NULL # Ensure it's NULL if not a valid plot
        }

        if (inherits(params$ce_plot, "ggplot")) {
          ggsave(ce_plot_path, plot = params$ce_plot, device = "png", width = 20, height = 20, dpi = 1800, units = "cm")
          # Replace the plot object with the file path
          params$ce_plot <- ce_plot_path
        } else {
          params$ce_plot <- NULL # Ensure it's NULL if not a valid plot
        }
        # --- END OF NEW LOGIC ---

        browser()

        # Render the report to PDF
        rmarkdown::render(
          input = temp_report,
          output_file = file,
          output_format = "pdf_document",
          params = params,
          envir = new.env(parent = globalenv()), # Use a clean environment
          intermediates_dir = temp_dir # Use the safe temporary directory
        )
      }
    )

  })
}
