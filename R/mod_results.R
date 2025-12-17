#' Results UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the results module.
#' @noRd
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # --- Generate Main Header: 'Commutability Evaluation Analysis Results' ---
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("square-poll-horizontal"),
        "Commutability Evaluation Analysis Results"
      ),
      # --- Button: Displays Help Text for this Module & Panel ---
      glassButton(
        inputId = ns("show_results_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question")
      )
    ),
    # --- Help Text Output for this Module (Display After Button is Pressed) ---
    glassTogglePanel(
      triggerId = ns("show_results_explanation"),
      help_button_page_5A_text()
    ),
    # --- Create Common Card for all Three Result Panels ---
    # --- Titled: 'Customizable Commutability Evaluation Options' ---
    # Includes:
    # 1) Confidence Level Selection for PI(s)
    # 2) Select Whether to Exclude Extrapolated Result(s)
    glassCard(
      inputId = ns("global_options"),
      title = "Global Analysis Options",
      icon = icon(name = "sliders"),
      collapsible = TRUE,
      collapsed = FALSE,
      disabled = FALSE,
      width = "100%",
      glassRow(
        glassCol(
          width = 4,
          div(
            class = "parameter-section",
            glassRadioButtons(
              inputId = ns("pi_conf_level"),
              label = "Select Desired Confidence Level",
              help_text = paste0(
                "Confidence level here signify the nominal confidence level ",
                "of the prediction intervals. It is generally recommended ",
                "setting this to 99 % (the default)."
              ),
              choices = c(
                "95 %" = 0.95,
                "99 %" = 0.99
              ),
              selected = 0.99,
              inline = TRUE,
              width = "100%",
              disabled = FALSE
            )
          )
        ),
        glassCol(
          width = 4,
          div(
            class = "parameter-section",
            glassRadioButtons(
              inputId = ns("exclude_extrapolations"),
              label = "Exclude Extrapolated Results",
              help_text = paste0(
                "Applies to evaluated materials within IVD-MD pairs. ",
                "Excludes evaluated material results outside the observed ",
                "measurement interval of the clinical samples for each IVD-MD ",
                "pair. It is not recommended to set this to -No-."
              ),
              choices = c(
                "Yes" = "Yes",
                "No" = "No"
              ),
              selected = "Yes",
              inline = TRUE,
              width = "100%",
              disabled = FALSE
            )
          )
        ),
        glassCol(
          width = 4,
          div(
            style = "margin: 25px; ",
            glassButton(
              inputId = ns("update_cache"),
              label = "Run Analysis",
              icon = icon("rotate"),
              width = "100%"
            )
          )
        )
      )
    ),

    # --- Create Tabset Panel Functionality ------------------------------------
    # --- Three Result Tabs (Tables - Plots - Report) ---
    glassTabsetPanel(
      inputId = ns("results_tabs"),
      selected = "show_results_tables",
      color = "purple",
      boxed = TRUE,
      # --- Panel 1 - Results Demonstrated in Table Format ---------------------
      glassTabPanel(
        title = "Tables",
        value = "show_results_tables",
        icon = icon("table"),
        glassCard(
          inputId = ns("table_filter_and_display"),
          title = "Filtering & Table Display",
          icon = icon(name = "filter"),
          collapsible = TRUE,
          collapsed = TRUE,
          attached = TRUE,
          disabled = FALSE,
          width = "100%",
          glassRow(
            glassCol(
              width = 6,
              div(
                class = "parameter-section",
                glassRadioButtons(
                  inputId = ns("filter_eq_location"),
                  label = "Choose Filtering for Evaluated Material Locations",
                  help_text = paste0(
                    "PIs is an abbreviation for prediction intervals. Only ",
                    "include evaluated materials that have results inside ",
                    "or outside the established prediction intervals."
                  ),
                  choices = c(
                    "No Filters" = "n_filt",
                    "Inside PIs" = "o_inside",
                    "Outside PIs" = "o_outside"
                  ),
                  selected = "n_filt",
                  inline = TRUE,
                  width = "100%",
                  disabled = FALSE
                )
              )
            ),
            glassCol(
              width = 6,
              div(
                class = "parameter-section",
                glassRadioButtons(
                  inputId = ns("filter_by_dins"),
                  label = "Choose Filtering for IVD-MD Nonselectivity Differences",
                  help_text = paste0(
                    "Applies to IVD-MD pairs. Only include IVD-MD pairs ",
                    "demonstrating acceptable or excessive differences in ",
                    "nonselectivity (i.e., DINS)."
                  ),
                  choices = c(
                    "No Filters" = "n_filt",
                    "Acceptable" = "o_ins",
                    "Excessive" = "o_dins"
                  ),
                  selected = "n_filt",
                  inline = TRUE,
                  width = "100%",
                  disabled = FALSE
                )
              )
            )
          ),
          glassRow(
            glassCol(
              width = 6,
              div(
                class = "parameter-section",
                glassRadioButtons(
                  inputId = ns("data_format"),
                  label = "Choose Table Format",
                  help_text = paste0(
                    "Controls the presentation of the results in the ",
                    "commutability evaluation analysis table below. If expanded ",
                    "is selected, each individual piece of information will get ",
                    "their own column. Otherwise, results that are possible (",
                    "and convenient) to group toghether, are grouped together."
                  ),
                  choices = c(
                    "Expanded" = "expanded",
                    "Compact" = "compact"
                  ),
                  selected = "compact",
                  inline = TRUE,
                  width = "100%",
                  disabled = FALSE
                )
              )
            ),
            glassCol(
              width = 6,
              # --- TO ME: Should add summary for filterings later here ---
              div()
            )
          )
        ),
        # --- Results Card 1 - Results Demonstrated in Tables ------------------
        glassTabsetPanel(
          inputId = ns("selected_table"),
          selected = "main_table",
          color = "green",
          boxed = TRUE,
          glassTabPanel(
            title = "Main",
            value = "main_table",
            icon = icon("table-list"),
            glassResultCard(
              inputId = ns("main_table_output_card"),
              title = "Commutability Evaluation Analysis Table",
              toolbar = glassButton(
                inputId = ns("calculate"),
                label = "Display",
                icon = icon("table-list"),
                width = NULL,
                disabled = TRUE
              ),
              icon = icon("table-list"),
              attached = TRUE,
              uiOutput(outputId = ns("ce_results"))
            )
          ),
          glassTabPanel(
            title = "Material-Wise",
            value = "material_wise_table",
            icon = icon("vial"),
            glassResultCard(
              inputId = ns("material_wise_table_output_card"),
              title = "Commutability Evaluation Results for Individual Materials",
              toolbar = div(
                style = "display: flex; align-items: center; gap: 10px;",
                div(
                  style = "width: 250px;",
                  glassDropdown(
                    inputId = ns("material"),
                    choices = "none",
                    selected = "none",
                    disabled = TRUE,
                    width = "100%"
                  )
                ),
                glassButton(
                  inputId = ns("calculate_grid"),
                  label = "Display",
                  icon = icon("vial"),
                  width = "auto",
                  disabled = TRUE
                )
              ),
              icon = icon("vial"),
              attached = TRUE,
              uiOutput(outputId = ns("ce_results_grid"))
            )
          )
        )
      ),

      # --- Panel 2 - Results Demonstrated in Plot Format ----------------------
      glassTabPanel(
        title = "Plots",
        value = "show_results_plots",
        icon = icon("chart-line"),
        # --- Card 1: Appearance Options (Converted from box) ---
        glassCard(
          inputId = ns("card_plot_appearance"),
          title = "Configuration",
          icon = icon("sliders"),
          collapsible = TRUE,
          collapsed = TRUE,
          attached = TRUE,
          glassRow(
            glassCol(
              width = 4,
              h4("Labels"),
              div(
                class = "parameter-section",
                glassTextInput(
                  inputId = ns("title"),
                  label = "Plot Title",
                  value = "",
                  label_icon = icon("h"),
                  placeholder = "Some random title ...",
                  tooltip_empty = "Write it now!",
                  tooltip_filled = "Cool title!",
                  width = "100%",
                  help_text = "Write the desired plot title here"
                ),
                glassTextInput(
                  inputId = ns("x_name"),
                  label = "X-Axis Title",
                  value = "",
                  label_icon = icon("x"),
                  placeholder = "Some title for the x-axis ...",
                  tooltip_empty = "Write it here",
                  tooltip_filled = "Now that is a cool x-axis title",
                  width = "100%",
                  help_text = "What values do we see on the x-axis? Write it."
                ),
                glassTextInput(
                  inputId = ns("y_name"),
                  label = "Y-Axis Title",
                  value = "",
                  label_icon = icon("y"),
                  placeholder = "Some title for the x-axis ...",
                  tooltip_empty = "Write it here",
                  tooltip_filled = "Nice y-axis title!",
                  width = "100%",
                  help_text = "What values do we see on the y-axis? Write it."
                )
              )
            ),
            glassCol(
              width = 3,
              h4("Display Options"),
              div(
                class = "parameter-section",
                glassDropdown(
                  inputId = ns("cs_curve"),
                  label = "Pattern Curve",
                  label_icon = icon("bezier-curve"),
                  choices = c(
                    "None" = "none",
                    "Diagonal" = "equivalence_curve",
                    "Fitted" = "fitted_curve",
                    "Flexible" = "smooth_curve"
                  ),
                  selected = "none",
                  width = "100%"
                ),
                glassSlider(
                  inputId = ns("tick_density"),
                  label = "Axis Ticks",
                  label_icon = icon("grip"),
                  choices = 3:20,
                  selected = 6,
                  unit = " Ticks",
                  width = "100%"
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
                      inputId = ns("width"),
                      label = "Width",
                      value = 17.6,
                      min = 2,
                      max = 42,
                      step = 0.4,
                      label_icon = icon("arrows-left-right"),
                      width = "100%",
                      unit = "cm",
                      accept = c(2, 42),
                      warning_unacceptable = "No ... please don't."
                    )
                  ),
                  glassCol(
                    width = 6,
                    glassNumericInput(
                      inputId = ns("height"),
                      label = "Height",
                      value = 11.7,
                      min = 2,
                      max = 60,
                      step = 0.4,
                      label_icon = icon("arrows-up-down"),
                      width = "100%",
                      unit = "cm",
                      accept = c(2, 60),
                      warning_unacceptable = "No ... please don't."
                    )
                  )
                ),
                glassDropdown(
                  inputId = ns("plot_download_file_type"),
                  label = "Format",
                  label_icon = icon("file"),
                  choices = c(
                    "PDF" = ".pdf",
                    "PNG" = ".png",
                    "TIF" = ".tif",
                    "TIFF" = ".tiff",
                    "JPEG" = ".jpeg"
                  ),
                  selected = NULL,
                  width = "100%"
                ),
                glassSlider(
                  inputId = ns("plot_download_quality"),
                  label = "Quality",
                  label_icon = icon("image"),
                  choices = c(
                    100, 200, 300,
                    450, 600, 750,
                    1000, 1250, 1500
                  ),
                  selected = 300,
                  width = "100%",
                  unit = " dpi"
                )
              )
            )
          )
        ),
        # --- Card 2: Plot Result (Converted to Result Card) ---
        glassResultCard(
          inputId = ns("plot_result_card"),
          title = "Commutability Evaluation Plot",
          icon = icon("chart-line"),
          toolbar = tagList(
            glassButton(
              inputId = ns("plot"),
              label = "Plot",
              icon = icon("chart-line"),
              disabled = TRUE
            ),
            glassDownloadButton(
              outputId = ns("download_ce_plots"),
              label = "Download",
              icon = icon("download")
            )
          ),
          plotOutput(
            outputId = ns("ce_plots"),
            width = "100%",
            height = "auto"
          )
        )
      ),
      glassTabPanel(
        title = "Report",
        value = "show_results_report",
        icon = icon("paper-plane"),

        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("file-download", class = "header-icon"),
            h3("Download Full Analysis Report"),
            glassDownloadButton(
              outputId = ns("download_report"),
              label = "Download",
              icon = icon("download"),
              width = NULL,
              disabled = FALSE
            )
          ),
          div(
            class = "card-body"
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

    # --- State Management for Labels & Cache ---
    results_data_cache <- reactiveVal(NULL)

    # Track the "freshness" of the analysis regarding RAW DATA
    # TRUE = Data changed, FALSE = Just updating params. Both need full rerun
    btn_state <- reactiveValues(
      is_fresh_start = TRUE,
      global_dirty = FALSE
    )

    # Track Local Button State
    local_state <- reactiveValues(
      # Have the buttons been clicked at least once since last Reset?
      main_clicked = FALSE,
      grid_clicked = FALSE,
      plot_clicked = FALSE,

      # Does the current display match the current inputs? (Controls "Enabled/Disabled")
      main_fresh = FALSE,
      grid_fresh = FALSE,
      plot_fresh = FALSE
    )

    # Raw Data Changes -> Full Reset of Labels
    observeEvent(c(file_upload_data$raw_cs_data(), file_upload_data$raw_eq_data()), {

      # Mark as a fresh start
      btn_state$is_fresh_start <- TRUE
      btn_state$global_dirty <- TRUE

      # Reset Local State
      local_state$main_clicked <- FALSE
      local_state$grid_clicked <- FALSE
      local_state$plot_clicked <- FALSE
      local_state$main_fresh <- FALSE
      local_state$grid_fresh <- FALSE
      local_state$plot_fresh <- FALSE

      # Reset Button Labels
      updateGlassButton(session, "update_cache", label = "Run Analysis")
      updateGlassButton(session, "calculate", label = "Display", disabled = TRUE)
      updateGlassButton(session, "calculate_grid", label = "Display", disabled = TRUE)
      updateGlassButton(session, "plot", label = "Plot", disabled = TRUE)

      # Invalidate Cache
      results_data_cache(NULL)

    }, ignoreInit = FALSE)

    # Parameter Changes -> Disable Local Buttons (Not Resetting Labels)
    observeEvent(
      eventExpr = {
        c(
          file_upload_data$is_valid(),
          file_upload_data$reference_method(),
          mod_dins_params(),
          input$pi_conf_level
        )
      },
      handlerExpr = {

        # Require Clicking on Local Buttons
        btn_state$global_dirty <- TRUE

        # Disable downstream buttons until updated.
        updateGlassButton(session, "calculate", disabled = TRUE)
        updateGlassButton(session, "calculate_grid", disabled = TRUE)
        updateGlassButton(session, "plot", disabled = TRUE)

        # Ensure Update button is enabled
        updateGlassButton(session, "update_cache", disabled = FALSE)
      },
      ignoreInit = TRUE
    )

    # Local Input Changes (Re-Enable Locals if Global not dirty)

    # Helper function to safely enable
    check_and_enable <- function(id, is_dirty_flag) {
      if (!btn_state$global_dirty) {
        local_state[[is_dirty_flag]] <- FALSE # It's no longer fresh (display doesn't match input)
        updateGlassButton(session, id, disabled = FALSE)
      }
    }

    observeEvent(c(input$exclude_extrapolations, input$filter_eq_location, input$filter_by_dins, input$data_format), {
      check_and_enable("calculate", "main_fresh")
    })

    observeEvent(c(input$exclude_extrapolations, input$filter_eq_location, input$filter_by_dins, input$material), {
      check_and_enable("calculate_grid", "grid_fresh")
    })

    observeEvent(
      eventExpr = {
        c(
          input$exclude_extrapolations,
          input$title,
          input$x_name,
          input$y_name,
          input$cs_curve,
          input$tick_density,
          input$width,
          input$height,
          input$plot_download_file_type,
          input$plot_download_quality
        )
      },
      handlerExpr = {
        check_and_enable("plot", "plot_fresh")
      }
    )

    # Global Update Click -> Run Analysis & Update Labels
    observeEvent(
      input$update_cache,
      handlerExpr = {
        if (isTRUE(file_upload_data$is_valid())) {
          params <- mod_dins_params()
          data_without_NA_values <- na.omit(object = cs_data_long())
          new_data_without_NA_values <- na.omit(object = eq_data_long())
          upper_zeta_val <- if (params$zeta_upper <= 1) NULL else params$zeta_upper

          result <- commutability::do_commutability_evaluation(
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

          # Populate the cache
          results_data_cache(result)

          # Reset Dirty Flags
          btn_state$global_dirty <- FALSE
          local_state$main_fresh <- FALSE
          local_state$grid_fresh <- FALSE
          local_state$plot_fresh <- FALSE


          # Enable the local buttons now that cache is fresh
          updateGlassButton(session, "calculate", disabled = FALSE)
          updateGlassButton(session, "calculate_grid", disabled = FALSE)
          updateGlassButton(session, "plot", disabled = FALSE)

          # Label Logic for Main Button
          if (btn_state$is_fresh_start) {
            updateGlassButton(session, "update_cache", label = "Update Results")
            btn_state$is_fresh_start <- FALSE
          }

          # --- For debugging (remove after finished product) ---
          debug_ce_results <<- result
        }
        else {
          results_data_cache(
            list(error = "Validation tests do not pass, so calculations are not possible")
          )
        }
      })

    observeEvent(input$calculate, {
      local_state$main_fresh <- TRUE
      local_state$main_clicked <- TRUE
      updateGlassButton(session, "calculate", label = "Refresh")
    })

    observeEvent(input$calculate_grid, {
      local_state$grid_fresh <- TRUE
      local_state$grid_clicked <- TRUE
      updateGlassButton(session, "calculate_grid", disabled = TRUE, label = "Refresh")
    })

    observeEvent(input$plot, {
      local_state$plot_fresh <- TRUE
      local_state$plot_clicked <- TRUE
      updateGlassButton(session, "plot", disabled = TRUE, label = "Refresh")
    })

    # --- Prepare to Render Main Table -----------------------------------------
    ce_results_display <- eventReactive(input$calculate, {
      # --- Ensure the calculation has run at least once ---
      req(results_data_cache())

      # --- Get all results from cache ---
      results_list <- results_data_cache()

      # --- Double ensure that the `results_list` is valid ---
      if (!is.null(results_list$error)) {
        # Using renderGlassTable for error display
        return(renderGlassTable(data = data.table("Error" = results_list$error), sortable = FALSE))
      }

      # --- Get copy of relevant object from the results list ---
      output_tbl <- data.table::copy(results_list$merged_ce_data)

      # --- Apply Filters ---

      # --- Extrapolation Filter ---
      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(output_tbl$extrapolate == 0L),
        seq_len(nrow(output_tbl))
      )

      # --- Inside / Outside PI Filter ---
      pi_filter <- switch(input$filter_eq_location,
                          "o_inside" = which(output_tbl$pi_inside == 1L),
                          "o_outside" = which(output_tbl$pi_inside == 0L),
                          seq_len(nrow(output_tbl))
      )

      # --- Acceptable / Excessive DINS Filter ---
      dins_filter <- switch(input$filter_by_dins,
                            "o_ins" = which(output_tbl$dins_conclusion == 0L),
                            "o_dins" = which(output_tbl$dins_conclusion == 1L),
                            seq_len(nrow(output_tbl))
      )

      # --- Get all rows that satisfy the three given filters ---
      combined_filter <- intersect(
        x = ep_filter,
        y = intersect(
          x = pi_filter,
          y = dins_filter
        )
      )

      # --- Keep only rows that satisfy the three given filters ---
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
      }
      else { # Expanded format
        display_tbl <- data.table::copy(output_tbl)
        display_tbl[, dins_conclusion := ifelse(dins_conclusion == 1L, "deemed excessive", "deemed acceptable")]
        display_tbl[, pi_inside := ifelse(pi_inside == 1L, "inside PI", "outside PI")]
        display_tbl[, inside_rate := format(inside_rate, nsmall = 3L, digits = 3L)]

        setnames(display_tbl,
                 old = c("comparison", "SampleID",
                         "zeta", "zeta_ci_lwr", "zeta_ci_upr",
                         "zeta_upper", "dins_conclusion",
                         "MP_B", "MP_A", "prediction",
                         "pi_lwr", "pi_upr", "pi_inside", "inside_rate"),
                 new = c("IVD-MD comparison", "ID of evaluated material",
                         "zeta", "zeta CI lwr", "zeta CI upr",
                         "upper zeta value", "Differences in non-selectivity is",
                         "IVD-MD 1", "IVD-MD 2", "Prediction",
                         "PI lwr", "PI upr", "Evaluated material is", "Conclusion strength"),
                 skip_absent = TRUE
        )
      }

      # --- Create the processed table using renderGlassTable ---
      renderGlassTable(
        data = display_tbl,
        col_names = names(display_tbl),
        sortable = TRUE
      )
    })

    # --- Display Main Table in UI ---------------------------------------------
    output$ce_results <- renderUI({
      # --- Ensure that the output is processed and prepared ---
      req(ce_results_display())
      # --- Display table in UI ---
      ce_results_display()
    })

    # --- Display Commutability Results for One EQAM ---------------------------
    ce_results_grid_display <- eventReactive(input$calculate_grid, {
      # --- Ensure cache is non-empty (hopefully correctly filled) ---
      req(results_data_cache())

      # --- Get results list from cache ---
      results_list <- results_data_cache()

      # --- Double ensure that the `results_list` is valid ---
      if (!is.null(results_list$error)) {
        # Using renderGlassTable for error display
        return(renderGlassTable(data = data.table("Error" = results_list$error), sortable = FALSE))
      }

      # --- Get copy of relevant object from the results list ---
      output_tbl <- data.table::copy(results_list$merged_ce_data)

      # --- Apply Filters ---

      # --- Extrapolation Filter ---
      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(output_tbl$extrapolate == 0L),
        seq_len(nrow(output_tbl))
      )

      # --- Inside / Outside PI Filter ---
      pi_filter <- switch(input$filter_eq_location,
                          "o_inside" = which(output_tbl$pi_inside == 1L),
                          "o_outside" = which(output_tbl$pi_inside == 0L),
                          seq_len(nrow(output_tbl))
      )

      # --- Acceptable / Excessive DINS Filter ---
      dins_filter <- switch(input$filter_by_dins,
                            "o_ins" = which(output_tbl$dins_conclusion == 0L),
                            "o_dins" = which(output_tbl$dins_conclusion == 1L),
                            seq_len(nrow(output_tbl))
      )

      # --- Get all rows that satisfy the three given filters ---
      combined_filter <- intersect(
        x = ep_filter,
        y = intersect(
          x = pi_filter,
          y = dins_filter
        )
      )

      # --- Keep only rows that satisfy the three given filters ---
      if (length(combined_filter) > 0) {
        output_tbl <- output_tbl[combined_filter]
      }

      # --- Create conclusion labels (C, NC, EI, EO) ---
      output_tbl <- output_tbl[, list(
        "conclusion" = ifelse(
          pi_inside == 1 & dins_conclusion == 0,
          "C",
          ifelse(
            pi_inside == 0 & dins_conclusion == 0,
            "NC",
            ifelse(
              pi_inside == 1 & dins_conclusion == 1,
              "EI",
              "EO"
            )
          )
        )
      ), by = c("comparison", "SampleID")]

      # --- Create EQAM commutability results table for every material ---
      output_tbl[, c("Method1", "Method2") := tstrsplit(comparison, " - ", fixed = TRUE)]
      all_methods <- sort(unique(c(output_tbl$Method1, output_tbl$Method2)))
      output_tbl_list <- lapply(
        X = split(output_tbl, by = "SampleID"),
        FUN = function(dt_subset) {
          dt_subset[, Method1 := factor(Method1, levels = all_methods)]
          dt_subset[, Method2 := factor(Method2, levels = all_methods)]
          wide_dt <- dcast.data.table(
            data = dt_subset,
            formula = Method1 ~ Method2,
            value.var = "conclusion",
            drop = FALSE
          )
          return(wide_dt)
        })

      # --- Get selected EQAM from input ---
      particular_material <- input$material

      # --- Prepare table only if an material is selected ---
      if (particular_material != "none") {
        renderGlassTable(
          data = output_tbl_list[[particular_material]],
          sortable = TRUE
        )
      }
      # --- Nothing is returned if input$material is 'none' ---
    })

    # --- Display Commutability Results for One EQAM in the UI -----------------
    output$ce_results_grid <- renderUI({
      # --- Ensure that the output is processed and prepared ---
      req(ce_results_grid_display())
      # --- Render the commutability table for the selected EQAM ---
      ce_results_grid_display()
    })

    # --- Render Commutability Evaluation Plots --------------------------------
    plot_object <- eventReactive(input$plot, {
      # --- Try to extract results from cache ---
      results_list <- results_data_cache()
      #browser()
      # --- Check if `resuls_list` is valid
      req(
        results_list,
        !is.null(results_list$merged_ce_data),
        !is.null(results_list$merged_pb_data),
        file_upload_data$is_valid()
      )

      # --- Get copy of relevant object from the results list ---
      temp_ce_data <- data.table::copy(results_list$merged_ce_data)

      # --- Apply Filters ---

      # --- Extrapolation Filter ---
      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(temp_ce_data$extrapolate == 0L),
        seq_len(nrow(temp_ce_data))
      )

      # --- Keep only rows that satisfy the extrapolation filter ---
      if (length(ep_filter) > 0) {
        temp_ce_data <- temp_ce_data[ep_filter]
      }

      # --- Draw the plot in the UI ---
      ce_plot <- commutability::plot_commutability_evaluation_plots(
        cs_data = cs_data_long()[, fasteqa::fun_of_replicates(.SD), by = "comparison"],
        ce_data = temp_ce_data,
        pb_data = results_list$merged_pb_data,
        exclude_rings = FALSE,
        exclude_cs = FALSE,
        plot_theme = "default",
        additional_arguments = list(
          "main_title" = input$title,
          "sub_title" = "",
          "x_name" = input$x_name,
          "y_name" = input$y_name,
          "n_breaks" = as.numeric(input$tick_density),
          "curve" = (input$cs_curve != "none"),
          "curve_type" = input$cs_curve,
          "hide_prediction_intervals" = TRUE
        )
      )

      ce_plot + theme(
        legend.position = "bottom"
      )

    })

    # --- DYNAMIC DIMENSION LOGIC ----------------------------------------------
    # --- (Experimental) -------------------------------------------------------

    # --- Reactive to calculate optimal download dimensions --------------------
    optimal_dims <- reactive({
      # --- Try to extract results from cache ---
      results_list <- results_data_cache()

      # --- Ensure that cache actually contained the results ---
      req(
        results_list,
        !is.null(results_list$merged_ce_data)
      )
      # --- Calculate required number of panels (# Comparisons) ---
      n_panels <- length(unique(results_list$merged_ce_data$comparison))

      # --- Return some default values if there are no panels ---
      if (n_panels == 0) {
        return(list(
          width = 17.6,
          height = 11.7
        ))
      }

      # --- Attempt to mimic the waiver() functionality in ggplot2 ---
      n_cols <- ceiling(sqrt(n_panels))
      n_rows <- ceiling(n_panels / n_cols)

      # --- Define dimensions in centimeters (assuming ~6cm per panel) ---
      # --- Reserve some space for labels and legend ---
      optimal_width <- 6 + (n_cols * 6)
      optimal_height <- 3 + (n_rows * 6)

      # --- Clamp the values to a reasonable range ---
      # --- Hopefully avoids excessive file sizes and long download times ---
      optimal_width <- max(12, min(40, optimal_width))
      optimal_height <- max(10, min(50, optimal_height))

      # --- Return 'optimal' download width and download height (in list) ---
      return(
        list(
          width = round(optimal_width, 1),
          height = round(optimal_height, 1)
        )
      )
    })

    # --- Observer to update the numeric inputs when the plot is generated -----
    observeEvent(optimal_dims(), {
      dims <- optimal_dims()
      # --- Can be manually changed after ---
      updateNumericInputIcon(
        session = session,
        inputId = "width",
        value = dims$width
      )
      # --- Can be manually changed after ---
      updateNumericInputIcon(
        session = session,
        inputId = "height",
        value = dims$height
      )
    })

    # --- Observer to update list of possible EQAMs to select ------------------
    observe({
      # --- Ensure cache is non-empty (hopefully correctly filled) ---
      req(results_data_cache())

      # --- Get results list from cache ---
      results_list <- results_data_cache()

      # --- Double ensure that the `results_list` is valid ---
      if (!is.null(results_list$error)) {
        # Using renderGlassTable for error display handling inside renderUI
        return(NULL)
      }

      # --- Get copy of relevant object from the results list ---
      output_tbl <- data.table::copy(results_list$merged_ce_data)

      # --- Apply Filters ---

      # --- Extrapolation Filter ---
      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(output_tbl$extrapolate == 0L),
        seq_len(nrow(output_tbl))
      )

      # --- Inside / Outside PI Filter ---
      pi_filter <- switch(input$filter_eq_location,
                          "o_inside" = which(output_tbl$pi_inside == 1L),
                          "o_outside" = which(output_tbl$pi_inside == 0L),
                          seq_len(nrow(output_tbl))
      )

      # --- Acceptable / Excessive DINS Filter ---
      dins_filter <- switch(input$filter_by_dins,
                            "o_ins" = which(output_tbl$dins_conclusion == 0L),
                            "o_dins" = which(output_tbl$dins_conclusion == 1L),
                            seq_len(nrow(output_tbl))
      )

      # --- Get all rows that satisfy the three given filters ---
      combined_filter <- intersect(
        x = ep_filter,
        y = intersect(
          x = pi_filter,
          y = dins_filter
        )
      )

      # --- Keep only rows that satisfy the three given filters ---
      if (length(combined_filter) > 0) {
        output_tbl <- output_tbl[combined_filter]
      }

      # --- Check which EQAM can be selected after all filters are applied ---
      choices <- unique(output_tbl$SampleID)
      if (length(choices) == 0) {
        choices <- "None Left After Filtering"
      }

      # Update list of choices for reference method
      updateGlassDropdown(
        session = session,
        inputId = "material",
        choices = choices,
        selected = choices[1],
        disabled = !file_upload_data$is_valid()
      )
      #updateVirtualSelect(
      #  session = session,
      #  inputId = "material",
      #  choices = choices,
      #  selected = choices[1],
      #  disable = !file_upload_data$is_valid()
      #)
    })

    # --- Display Commutability Evaluation Plots -------------------------------
    output$ce_plots <- renderPlot(
      res = 120,
      height = function() {
        # --- DYNAMIC HEIGHT CALCULATION ---
        results_list <- results_data_cache()
        req(
          results_list,
          !is.null(results_list$merged_ce_data)
        )

        # 1. Get the number of unique panels (comparisons)
        n_panels <- length(unique(results_list$merged_ce_data$comparison))
        if (n_panels == 0) return(400) # Default height if no data

        # 2. Determine the layout ggplot will use (it tries to be square-ish)
        n_cols <- ceiling(sqrt(n_panels))
        n_rows <- ceiling(n_panels / n_cols)

        # 3. Calculate total height in pixels
        # (Base height for title/legend + height per row)
        base_height_px <- 150
        height_per_row_px <- 280
        total_height <- base_height_px + (n_rows * height_per_row_px)

        return(total_height)
      },
      {
        plot_obj <- plot_object()
        if (!is.null(plot_obj)) {
          plot(plot_obj)
        }
      }
    )

    # --- Download Handler for Downloading Commutability Evaluation Plots ------
    output$download_ce_plots <- downloadHandler(
      filename = function() {
        paste0(
          "ce_plots_",
          toupper(mod_dins_params()$pi_method),
          "_",
          mod_dins_params()$transformation,
          "_",
          paste0(
            "M_used_",
            mod_dins_params()$M
          ),
          "generated_on_",
          Sys.Date(),
          input$plot_download_file_type
        )
      },
      content = function(file) {
        plot_to_save <- plot_object()
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

    # --- Report Generation Logic ---
    output$download_report <- downloadHandler(
      filename = function() {
        paste0("Commutability-Evaluation-Report-", Sys.Date(), ".", "pdf")
      },
      content = function(file) {
        id <- showNotification(
          "Generating report... This may take a moment.",
          duration = NULL,
          closeButton = FALSE,
          type = "message"
        )
        on.exit(removeNotification(id))

        # Create a temporary directory for intermediate files like plots
        temp_dir <- tempdir()

        # Find the path to the Rmd template within the package.
        report_path <- system.file("report.Rmd", package = "CommutabilityevaluationofEQAMs")
        if (report_path == "") {
          stop("Could not find report.Rmd. Make sure it is in the 'inst' directory.")
        }

        # Create a temporary copy of the report to avoid writing to the package library
        temp_report <- file.path(temp_dir, "report.Rmd")
        file.copy(report_path, temp_report, overwrite = TRUE)

        # Gather all necessary parameters to pass to the Rmd file.
        # Use tryCatch() to gracefully handle cases where calculations haven't been run
        params <- list(
          cs_data = tryCatch(file_upload_data$raw_cs_data(), error = function(e) NA),
          eq_data = tryCatch(file_upload_data$raw_eq_data(), error = function(e) NA),
          diagnostics_cs = tryCatch(file_upload_data$diagnostics_cs(), error = function(e) NA),
          diagnostics_eq = tryCatch(file_upload_data$diagnostics_eq(), error = function(e) NA),
          diagnostics_both = tryCatch(file_upload_data$diagnostics_both(), error = function(e) NA),
          outlier_results = tryCatch(outlier_data$results(), error = function(e) NA),
          outlier_params = tryCatch(outlier_data$params(), error = function(e) NA),
          formal_assessment_results = tryCatch(model_validation_data$formal_results(), error = function(e) NA),
          assessment_plot = tryCatch(model_validation_data$assessment_plot(), error = function(e) NA),
          assessment_plot_type = tryCatch(model_validation_data$assessment_plot_type(), error = function(e) NA),
          dins_params = tryCatch(mod_dins_params(), error = function(e) NA),
          ce_results = tryCatch(results_data_cache(), error = function(e) NA),
          ce_plot = tryCatch(plot_object(), error = function(e) NA)
        )

        # Render the report
        rendered_report <- rmarkdown::render(
          input = temp_report,
          output_format = "pdf_document",
          params = params,
          envir = new.env(parent = globalenv()),
          intermediates_dir = temp_dir
        )

        # Relocate the file
        file.copy(
          from = rendered_report,
          to = file
        )
      }
    )

    outputOptions(output, "ce_results", suspendWhenHidden = FALSE)
    outputOptions(output, "ce_results_grid", suspendWhenHidden = FALSE)
    outputOptions(output, "ce_plots", suspendWhenHidden = FALSE)
    outputOptions(output, "download_ce_plots", suspendWhenHidden = FALSE)


  })
}
