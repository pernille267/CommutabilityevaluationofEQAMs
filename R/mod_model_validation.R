#' Model Validation UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the model validation module.
#' @noRd
mod_model_validation_ui <- function(id) {
  ns <- NS(id)
  tagList(
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
        icon = icon(name = "circle-question"),
        color = "green"
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
      boxed = TRUE,
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
              width = "auto",
              disabled = FALSE
            ),
            glassDownloadButton(
              outputId = ns("download_model_at_visual_results"),
              label = "Table",
              icon = icon("download"),
              width = "auto"
            ),
            glassDownloadButton(
              outputId = ns("download_model_at_exact_results"),
              label = "Raw Table",
              icon = icon("download"),
              width = "auto"
            )
          ),
          attached = TRUE,
          uiOutput(
            outputId = ns("formal_assessment_tests_table")
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
          title = "Configuration",
          icon = icon("sliders"),
          collapsible = TRUE,
          collapsed = TRUE,
          attached = TRUE,
          width = "100%",
          # Customization Inputs
          glassRow(
            glassCol(
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
            glassCol(
              width = 3,
              h4("Display Options"),
              div(
                class = "parameter-section",
                glassRadioButtons(
                  inputId = ns("include_se_band"),
                  label = "Include Error Ribbon",
                  label_icon = icon("bacon"),
                  choices = c("No" = FALSE, "Yes" = TRUE),
                  selected = FALSE,
                  width = "100%"
                ),
                glassTogglePanel(
                  triggerId = ns("which_plot"),
                  show_when = "sd_vs_concentration",
                  glassRadioButtons(
                    inputId = ns("var_instead"),
                    label = "Use Variance Instead",
                    label_icon = icon("vaadin"),
                    choices = c("No" = FALSE, "Yes" = TRUE),
                    selected = FALSE,
                    width = "100%"
                  )
                ),
                glassTogglePanel(
                  triggerId = ns("which_plot"),
                  show_when = "cv_vs_concentration",
                  glassRadioButtons(
                    inputId = ns("cv_percent"),
                    label = "Show as CV %",
                    label_icon = icon("percent"),
                    choices = c("No" = FALSE, "Yes" = TRUE),
                    selected = FALSE,
                    width = "100%"
                  )
                )
              )
            ),
            glassCol(
              width = 5,
              h4("Download Options"),
              div(
                class = "parameter-section",
                glassRow(
                  glassCol(
                    width = 6,
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
                    )
                  ),
                  glassCol(
                    width = 6,
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
                ),
                glassRadioButtons(
                  inputId = ns("plot_download_file_type"),
                  label = "Format",
                  label_icon = icon("highlighter"),
                  choices = c("PDF" = ".pdf", "PNG" = ".png", "TIF" = ".tif"),
                  selected = ".png",
                  width = "100%"
                )
              )
            )
          ),
          h4("Display Options"),
          glassRow(
            glassCol(
              width = 4,
              glassRadioButtons(
                inputId = ns("include_se_band"),
                label = "Include Error Ribbon",
                choices = c("No" = FALSE, "Yes" = TRUE),
                selected = FALSE,
                width = "100%"
              )
            ),
            glassCol(
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
                ),
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
            ),
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
mod_model_validation_server <- function(id, file_upload_data, mod_dins_params, outlier_data) {
  moduleServer(id, function(input, output, session) {

    # --- Trackers -------------------------------------------------------------

    # --- Track whether recalculation of Plot Cache is necessary ---
    plot_cache_state <- reactiveValues(
      filled = FALSE,
      need_update = TRUE
    )

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

      to_remove <- outlier_data$outliers_to_remove()
      if (!is.null(to_remove) && nrow(to_remove) > 0) {
        raw_keys <- paste(raw_data$comparison, raw_data$SampleID, sep = " |SECRET-KEY| ")
        remove_keys <- paste(to_remove$comparison, to_remove$SampleID, sep = " |SECRET-KEY| ")
        keep_idx <- !raw_keys %in% remove_keys
        raw_data <- raw_data[keep_idx, ]
      }

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

    # --- Server Logic for Formal Assessment Tests -----------------------------

    # --- Create a reactiveVal to Cache Results. ---
    current_assessment_tests_results <- reactiveVal(NULL)

    # --- Watch for input changes to re-enable buttons ---
    observeEvent(list(cs_data_long(), mod_dins_params()$pi_method), {

      # --- Plot cache needs update if input data or regr. model changes ---
      plot_cache_state$need_update <- TRUE

      # Only proceed if data is available
      if (!is.null(cs_data_long())) {
        updateGlassButton(
          session = session,
          inputId = "run_tests",
          disabled = FALSE
        )
        updateGlassButton(
          session = session,
          inputId = "plot_assessment_plots",
          disabled = FALSE
        )
      }
    }, ignoreInit = TRUE)

    # --- Update Cache when `Run Tests` Button is Pressed (Disables Button) ---
    observeEvent(input$run_tests, {
      # Require `cs_data_long()` to exist
      req(cs_data_long())


      # --- Add Loader ---

      # --- Activate Loader ---
      showGlassLoader(
        id = session$ns("loader_formal"),
        text = "Calculating formal tests...",
        selector = paste0("#", session$ns("card_formal_results"))
      )

      # --- Deactivate Loader at Any Exit ---
      on.exit(hideGlassLoader(id = session$ns("loader_formal")))

      # Disable button immediately on click
      updateGlassButton(
        session = session,
        inputId = "run_tests",
        disabled = TRUE
      )



      # Remove NA-values before performing tests
      data_without_NAs <- na.omit(object = cs_data_long())

      # Make visual table
      out_visual <- commutability::perform_assessment_tests(
        data = data_without_NAs,
        B = 100,
        method = mod_dins_params()$pi_method,
        level = 0.95,
        robust = TRUE,
        adjust_p_value = FALSE,
        holm = FALSE,
        output = "visual"
      )

      # Make exact table (for row high-lighting)
      out_exact <- commutability::perform_assessment_tests(
        data = data_without_NAs,
        B = 100,
        method = mod_dins_params()$pi_method,
        level = 0.95,
        robust = TRUE,
        adjust_p_value = FALSE,
        holm = FALSE,
        output = "exact"
      )

      # Combine into list
      results <- list(
        "visual" = out_visual,
        "exact" = out_exact
      )

      # Fill Cache
      current_assessment_tests_results(results)
    })

    # --- *DEBUGGING* ---
    observe({
      debug_cs_data <<- cs_data_long()
      debug_fmad_data <<- current_assessment_tests_results()
      debug_plot_cache_state <<- plot_cache_state
      debug_vmad_data <<- current_assessment_plots()
    })

    # --- Render Visual Assessment Table ---
    output$formal_assessment_tests_table <- renderUI({
      req(
        current_assessment_tests_results()
      )

      # Extract tests results from Cache
      results <- current_assessment_tests_results()

      # Extract individual components from 'results'
      dt_visual <- results$visual
      dt_exact <- results$exact

      # Format Data for Display
      #dt_display <- data.table::data.table(
      #  "Comparison" = dt$comparison,
      #  "Zeta" = format(dt$zeta, nsmall = 2, digits = 2),
      #  "Zeta_Crit" = format(inputs$z_crit, nsmall = 2, digits = 2),
      #  "Conclusion" = ifelse(dt$zeta > inputs$z_crit, "Excessive", "Acceptable")
      #)

      # Generate Caption
      n_total <- nrow(dt_visual)
      n_normality <- nrow(dt_exact[testing == "normality"])
      n_homoscedasticity <- nrow(dt_exact[testing == "normality"])
      n_reject_normality <- sum(
        dt_exact[testing == "normality"]$conclusion == "reject"
      )
      n_reject_homoscedasticity <- sum(
        dt_exact[testing == "homoscedasticity"]$conclusion == "reject"
      )
      n_reject <- n_reject_normality + n_reject_homoscedasticity

      caption_text <- if (n_reject == 0) {
        sprintf("All %d comparisons passed both model assessment tests. Great!", n_normality)
      }
      else if (n_reject == n_total) {
        sprintf("All %d comparisons FAILED both model assessment tests. This is where you stop and reflect over what you have done.", n_normality)
      }
      else {
        paste0(
          sprintf("<b>%d</b> out of %d comparisons failed the normality tests. ", n_reject_normality, n_normality),
          sprintf("Also, <b>%d</b> out of %d comparisons failed the homoscedasticity tests.", n_reject_homoscedasticity, n_homoscedasticity),
          sprintf(" That is <b>%d</b> out of %d failed tests in total!", n_reject, n_total)
        )
      }

      # Highlight rows with failed tests
      bad_rows <- which(dt_exact$conclusion == "reject")

      # 4. Render
      renderGlassTable(
        data = dt_visual,
        col_names = c("IVD-MD Comparison", "Testing", "Test Name", "\\(p\\)-value", "Test Conclusion", "Rate of Rejected Tests During Resampling"),
        caption = caption_text,
        highlight_rows = bad_rows,
        sortable = TRUE,
        sidebar_html = NULL
      )
    })

    # --- Download Formatted Table (Visual Assessment Table) ---
    output$download_model_at_visual_results <- downloadHandler(
      filename = function() {
        paste0(
          "ceapkfcr_formal_model_validation_tests_visual_table_",
          Sys.Date(),
          ".xlsx"
        )
      },
      content = function(file) {
        table_to_save <- current_assessment_tests_results()$visual
        req(table_to_save)
        writexl::write_xlsx(
          x = table_to_save,
          path = file,
          col_names = TRUE,
          format_headers = TRUE
        )
      }
    )

    # --- Download Exact Numeric Table (Raw) ---
    output$download_model_at_exact_results <- downloadHandler(
      filename = function() {
        paste0(
          "ceapkfcr_formal_model_validation_tests_raw_table_",
          Sys.Date(),
          ".xlsx"
        )
      },
      content = function(file) {
        table_to_save <- current_assessment_tests_results()$exact
        req(table_to_save)
        writexl::write_xlsx(
          x = table_to_save,
          path = file,
          col_names = TRUE,
          format_headers = TRUE
        )
      }
    )

    # --- Server Logic for Assessment Plots ------------------------------------

    # --- Create a reactiveVal to Cache Plots. ---
    current_assessment_plots <- reactiveVal(NULL)

    # --- Watch for input changes to re-enable Plot button ---
    observeEvent(
      eventExpr = {
        c(
          input$ap_title,
          input$ap_x_title,
          input$ap_y_title,
          input$which_plot,
          input$include_se_band,
          input$var_instead,
          input$cv_percent
        )
      },
      handlerExpr = {
        updateGlassButton(
          session = session,
          inputId = "plot_assessment_plots",
          disabled = FALSE
        )
      }
    )

    # --- Update Cache when `Plot` Button is Pressed (Disables Button) ---
    assessment_plot_object <- eventReactive(input$plot_assessment_plots, {
      req(file_upload_data$is_valid(), cs_data_long())

      # Update Selector To point to Results Card
      showGlassLoader(
        id = session$ns("loader_plot"),
        text = "Rendering plot...",
        selector = paste0("#", session$ns("card_assessment_plot"))
      )

      # Fjern loader når plottet er ferdig generert (eller feiler)
      on.exit(hideGlassLoader(id = session$ns("loader_plot")))


      if (file_upload_data$is_valid()) {

        # Disable Button
        updateGlassButton(
          session = session,
          inputId = "plot_assessment_plots",
          disabled = TRUE
        )

        # Helper: Convert string input from glassRadioButtons to logical
        # glassRadioButtons returns character ("TRUE"/"FALSE")
        to_logical <- function(val) {
          if (is.null(val)) return(FALSE)
          as.logical(val)
        }

        # Update Cache if necessary
        if (isTRUE(plot_cache_state$need_update)) {
          assessment_plots <- sapply(
            X = c(
              "residual_plot",
              "residual_histogram",
              "qq_plot",
              "sd_vs_concentration",
              "cv_vs_concentration"
            ),
            FUN = function(plot_type) {
              commutability::plot_assessment_plots(
                data = cs_data_long(),
                method = mod_dins_params()$pi_method,
                type = plot_type,
                draw_curves = TRUE,
                plot_theme = "default",
                additional_arguments = list(
                  "curve_se" = FALSE,
                  "cv_percent" = TRUE,
                  "var_instead" = FALSE
                )
              )
            },
            simplify = FALSE
          )
          # Fill Cache
          current_assessment_plots(assessment_plots)
          plot_cache_state$need_update <- FALSE
          plot_cache_state$filled <- TRUE
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
            "curve_se" = to_logical(input$include_se_band),
            "cv_percent" = to_logical(input$cv_percent),
            "var_instead" = to_logical(input$var_instead)
          )
        )
      }
    })

    # --- Render Assessment Plot ---
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

    # --- Download Assessment Plot ---
    output$download_assessment_plots <- downloadHandler(
      filename = function() {
        paste0(
          "ceapkfcr_visual_model_validation_",
          input$which_plot,
          "_",
          Sys.Date(),
          input$plot_download_file_type
        )
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

    # --- Avoid Suspension Issues ---

    # 1. Wake up the DATA GENERATORS first (The Table and Plot)
    outputOptions(output, "formal_assessment_tests_table", suspendWhenHidden = FALSE)
    outputOptions(output, "assessment_plots", suspendWhenHidden = FALSE)

    # 2. Wake up the DATA CONSUMERS second (The Download Handlers)
    outputOptions(output, "download_model_at_visual_results", suspendWhenHidden = FALSE)
    outputOptions(output, "download_model_at_exact_results", suspendWhenHidden = FALSE)
    outputOptions(output, "download_assessment_plots", suspendWhenHidden = FALSE)


    return(
      list(
        formal_results = current_assessment_tests_results,
        assessment_plot = current_assessment_plots,
        assessment_plot_type = reactive({
          input$which_plot
        })
      )
    )

  })
}
