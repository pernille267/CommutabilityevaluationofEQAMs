#' Differences in Nonselectivity UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the differences in nonselectivity module.
#' @noRd
mod_dins_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("arrows-left-right-to-line"), "Evaluate Differences in Nonselectivity"
      ),
      glassButton(
        inputId = ns("show_dins_explanation"),
        label = "Show Help Text",
        icon = icon("circle-question"),
        color = "green"
      )
    ),
    glassTogglePanel(
      triggerId = ns("show_dins_explanation"),
      help_button_page_2_text()
    ),

    # --- Card 1: Main Configuration (Consolidated) ----------------------------
    glassCard(
      inputId = ns("card_config_dins"),
      title = "Configuration",
      icon = icon("sliders"),
      collapsible = TRUE,
      width = "100%",

      glassRow(
        # 1. Transformation & Setting M(%)
        glassCol(
          width = 6,
          div(
            class = "parameter-section",
            glassRadioButtons(
              inputId = ns("transformation"),
              label = "Transformation",
              label_icon = icon("right-left"),
              help_text = paste0(
                "Select None to use raw data. Select the transformation ",
                "that results in the most \\(\\zeta\\) estimates closest to 1."
              ),
              choices = c(
                "None" = "identity",
                "Log" = "ln",
                "Box-Cox" = "boxcox"
              ),
              selected = "identity",
              width = "100%"
            ),
            glassSlider(
              inputId = ns("M"),
              label = "M(%) - Maximum Tolerable DINS",
              label_icon = icon("arrow-right-to-bracket"),
              help_text = paste0(
                "DINS is short for Differences in Nonselectivity. ",
                "M(%) is the maximum average percentage increase in prediction interval ",
                "width caused by differences in nonselectivity (DINS). M is ",
                "related to the upper boundary of what can be considered ",
                "acceptable DINS between two IVD-MDs \\(\\zeta_{\\mathrm{upper}}\\)."
              ),
              choices = seq(0, 200, by = 5),
              selected = 25,
              unit = " %",
              width = "100%"
            )
          )
        ),
        # 2. Regression Model & Weigthed Smoothing Spline (Conditional)
        glassCol(
          width = 6,
          div(
            class = "parameter-section",
            glassRadioButtons(
              inputId = ns("pi_method"),
              label = "Regression Model",
              label_icon = icon("chart-line"),
              help_text = paste0(
                "Which regression model to use to model the relationship ",
                "between results produced by each pair of IVD-MDs."
              ),
              choices = c(
                "Deming" = "fg",
                "Smoothing Spline" = "ss"
              ),
              selected = "fg",
              width = "100%"
            ),
            glassTogglePanel(
              triggerId = ns("pi_method"),
              show_when = "ss", # Shows when `pi_method` == `ss`
              glassRadioButtons(
                inputId = ns("ss_weighted"),
                label = "Weighted Smoothing Spline",
                label_icon = icon("scale-unbalanced"),
                choices = c("No", "Yes"),
                selected = "No",
                width = "100%"
              )
            )
          )
        )
      )
    ),

    # --- Tabset Panels ---
    glassTabsetPanel(
      inputId = ns("dins_results_tabs"),
      selected = "dins_recommendations_and_info",
      color = "purple",
      boxed = TRUE,
      # --- Recommendations and Information ---
      glassTabPanel(
        title = "General",
        value = "dins_recommendations_and_info",
        icon = icon("circle-info"),
        glassResultCard(
          inputId = ns("card_recommendations_and_info"),
          title = "Recommendations and Information",
          icon = icon("circle-info"),
          width = "100%",
          toolbar = glassButton(
            inputId = ns("apply_recommendations"),
            label = "Use Recommendations",
            icon = icon("wand-magic-sparkles"),
            color = "green",
            width = "220px"
          ),
          attached = TRUE,
          uiOutput(
            outputId = ns("general_tab_content")
          )
        )
      ),
      # --- Zeta Estimates Table & Downloads ---
      glassTabPanel(
        title = "Differences in Nonselectivity Estimates",
        value = "dins_estimates",
        icon = icon("calculator"),
        glassResultCard(
          inputId = ns("card_result_zetas"),
          title = "Differences in Nonselectivity Estimates",
          icon = icon("calculator"),
          width = "100%",
          toolbar = div(
            style = "display: flex; gap: 10px;",
            glassDownloadButton(
              outputId = ns("download_current_zeta"),
              label = "Current Estimates",
              icon = icon("download"),
              width = "auto"
            ),
            glassDownloadButton(
              outputId = ns("download_all_zetas"),
              label = "All Estimates",
              icon = icon("download"),
              width = "auto"
            )
          ),
          attached = TRUE,
          uiOutput(
            outputId = ns("calculated_zetas_table")
          )
        )
      ),
      # --- Zeta Estimates Table & Downloads ---
      glassTabPanel(
        title = "IVD-MD Imprecision Estimates",
        value = "imprecision_estimates",
        icon = icon("bullseye"),
        glassResultCard(
          inputId = ns("card_result_imprecision"),
          title = "IVD-MD Imprecision Estimates",
          icon = icon("bullseye"),
          width = "100%",
          # Toolbar with Download Button
          toolbar = glassDownloadButton(
            outputId = ns("download_imprecision"),
            label = "Imprecision Estimates",
            icon = icon("download"),
            width = "100%"
          ),
          attached = TRUE,
          # Render Imprecision Table to UI
          uiOutput(
            outputId = ns("calculated_imprecision_table")
          )
        )
      )
    )
  )
}

#' Differences in Nonselectivity Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list containing outputs from the file upload module.
#'
#' @return A reactive list of user-selected parameters.
#' @noRd
mod_dins_server <- function(id, file_upload_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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

    # --- Clinical Samples - Long-formatted - With Transformation --------------
    cs_data_long <- reactive({
      req(raw_cs_data_long())
      transformation <- switch(
        input$transformation,
        "identity" = "identity",
        "ln" = "log#e",
        "boxcox" = "boxcox#0.5",
        "identity"
      )
      commutability::transform_data(
        data = raw_cs_data_long(),
        transformation = transformation
      )
    })

    # --- Recommended Zeta Upper Value ---
    current_recommended_zeta_upper <- reactive({
      req(
        cs_data_long(),
        input$M
      )
      # Use the cached diagnostics from upstream instead of recalculating
      diagnostics <- file_upload_data$diagnostics_cs()

      # diagnostics must exist
      req(diagnostics)

      # Get study design quality metrics
      study_design_information <- diagnostics$quality

      # To be conservative we use n_eff = max(n_1, n_2, ..., n_I)
      maximum_n <- max(study_design_information$effective_number_of_samples, na.rm = TRUE)
      maximum_R <- ceiling(max(study_design_information$average_number_of_replicates, na.rm = TRUE))

      # Calculate M as decimal number instead of percentage
      current_M <- as.numeric(input$M) / 100

      # Clamp values to table limits
      maximum_n <- max(20, min(50, maximum_n))
      maximum_R <- max(2, min(5, maximum_R))

      # Find cell-match in LU table
      matches_n <- which(abs(commutability::look_up_table$n - maximum_n) <= 0.1)
      matches_R <- which(abs(commutability::look_up_table$R - maximum_R) <= 0.1)
      matches_M <- which(abs(commutability::look_up_table$M - current_M) <= 0.01)

      which_row <- intersect(intersect(matches_n, matches_R), matches_M)

      recommended_zeta_upper <- if (length(which_row) == 0) {
        which_row_fallback <- intersect(matches_n, matches_R)
        commutability::look_up_table$zeta[which_row_fallback][1] * (1 + current_M)^2
      } else {
        commutability::look_up_table$zeta[which_row][1]
      }

      round(recommended_zeta_upper, digits = 2L)
    })

    # --- Cache the Model Fitting ---
    cached_zeta_estimates <- reactive({
      req(raw_cs_data_long())

      # Show a notification because this takes a few seconds
      id <- showNotification("Fitting regression models...", type = "message", duration = NULL)
      on.exit(removeNotification(id))

      calculate_candidate_zetas(raw_cs_data_long())
    })

    # --- Recommendation Engine (Heavy Lifter) ---
    optimization_results <- reactive({
      req(
        cached_zeta_estimates(),
        current_recommended_zeta_upper()
      )

      # Run the lightweight decision logic
      engine_output <- tryCatch(
        select_best_model(
          candidate_results = cached_zeta_estimates(),
          zeta_critical = current_recommended_zeta_upper()
        ),
        error = function(e) {
          warning("Selection Logic Error: ", e$message)
          return(NULL)
        }
      )

      if (is.null(engine_output) || length(engine_output) == 0) return(NULL)

      # --- Aggregation Logic (Unchanged) ---
      results_dt <- data.table::rbindlist(engine_output)

      ranking <- results_dt[, .(
        votes = .N,
        avg_zeta = mean(zeta, na.rm = TRUE)
      ), by = .(best_transformation, model_type)]

      data.table::setorder(ranking, -votes, avg_zeta)
      winner <- ranking[1]

      # Map to UI
      ui_tr <- switch(winner$best_transformation,
                      "identity"   = "identity",
                      "log#e"      = "ln",
                      "boxcox#0.5" = "boxcox",
                      "identity"
      )

      ui_model <- if (winner$model_type == "OLS") "fg" else "ss"

      list(
        tr = ui_tr,
        model = ui_model,
        weighted = "No", # Still hardcoded as per original logic
        is_valid = winner$avg_zeta <= current_recommended_zeta_upper(),
        zeta = winner$avg_zeta,
        reason = sprintf(
          "Global Consensus: Selected by %s%% of comparisons (%d/%d).",
          round(100 * winner$votes / nrow(results_dt), 1), winner$votes, nrow(results_dt)
        )
      )
    })

    # --- Render General Tab Content ---
    output$general_tab_content <- renderUI({
      req(
        current_recommended_zeta_upper(),
        optimization_results()
      )

      # Get Current Maximum Tolerable DINS and Optimization Results
      z_val <- current_recommended_zeta_upper()
      rec <- optimization_results()

      # Map internal codes to readable labels
      tr_label <- switch(
        rec$tr,
        "identity" = "None",
        "ln" = "Log",
        "boxcox" = "Box-Cox",
        "None"
      )

      model_label <- if (rec$model == "fg") "Deming" else "Smoothing Spline"
      if (rec$model == "ss" && rec$weighted == "Yes") {
        model_label <- "Weighted Smoothing Spline"
      }

      status_color <- if (rec$is_valid) "#28A745" else "#dc3545"
      status_icon  <- if (rec$is_valid) "check-circle" else "circle-exclamation"
      status_text  <- if (rec$is_valid) "Optimization Successful" else "Criteria Not Met (Best Effort)"

      div(
        class = "dins-general-tab-content",
        style = "padding: 10px;",
        withMathJax(),
        # --- Block 1: The Limit ---
        div(
          class = "glass-help-info-box",
          style = "border-left-color: #605CA8; background-color: #f8f9fa; padding: 20px; margin-bottom: 25px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",
          div(
            style = "display: flex; align-items: center; gap: 10px; margin-bottom: 15px; border-bottom: 1px solid #e9ecef; padding-bottom: 10px;",
            icon("calculator", style = "color: #605CA8; font-size: 1.3em;"),
            span(style = "font-size: 1.2em; font-weight: 700; color: #495057;", "Calculated Upper Limit")
          ),
          div(
            style = "font-size: 1.2em; font-weight: 600; color: #2c3e50; margin: 10px 0; text-align: center;",
            sprintf("\\(\\hat{\\zeta}_{\\mathrm{upper}} = %s\\)", format(z_val, nsmall = 2, digits = 2))
          ),
          div(
            style = "color: #6c757d; font-style: italic; font-size: 1.1em; text-align: center;",
            "Any IVD-MD pair exceeding this limit suggests excessive differences in nonselectivity."
          )
        ),

        # --- Block 2: The Recommendations ---
        div(
          class = "glass-help-info-box",
          style = "border-left-color: #28A745; background-color: #f8f9fa; padding: 20px; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.05);",

          # Header row with Status Badge
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 15px; border-bottom: 1px solid #e9ecef; padding-bottom: 10px;",
            div(
              style = "display: flex; align-items: center; gap: 10px;",
              icon("wand-magic-sparkles", style = "color: #28A745; font-size: 1.3em;"),
              span(style = "font-size: 1.2em; font-weight: 700; color: #495057;", "Auto-Recommendation")
            ),
            span(
              class = "glass-diag-badge-pill",
              style = sprintf("border: 1px solid %s; color: %s; background-color: white; font-size: 0.85em; display: flex; align-items: center; gap: 5px; padding: 5px 12px;", status_color, status_color),
              icon(status_icon), status_text
            )
          ),

          div(
            style = "margin-bottom: 20px; color: #495057; font-size: 1.15em;",
            "Based on the goodness-of-fit scoring system, the following settings are recommended:"
          ),

          # Settings Grid
          div(
            style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 20px;",

            # Transform Setting
            div(
              style = "background: white; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
              div(style = "font-size: 0.85em; font-weight: 700; color: #adb5bd; text-transform: uppercase; margin-bottom: 5px;", "Transformation"),
              div(style = "font-size: 1.2em; font-weight: 600; color: #2c3e50;", tr_label)
            ),

            # Model Setting
            div(
              style = "background: white; padding: 15px; border-radius: 8px; border: 1px solid #e9ecef;",
              div(style = "font-size: 0.85em; font-weight: 700; color: #adb5bd; text-transform: uppercase; margin-bottom: 5px;", "Regression Model"),
              div(style = "font-size: 1.2em; font-weight: 600; color: #2c3e50;", model_label)
            )
          ),

          # Reasoning Block (New)
          # This displays the sophisticated output from the expert system (e.g., "Consensus among metrics...")
          div(
            style = "background: #eef5f9; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8;",
            div(style = "font-size: 0.85em; font-weight: 700; color: #17a2b8; text-transform: uppercase; margin-bottom: 5px;", "Automated Reasoning"),
            div(style = "font-size: 1.0em; font-style: italic; color: #495057;", rec$reason)
          )
        )
      )
    })

    # ---  Handle "Use Recommendations" Click ---
    observeEvent(input$apply_recommendations, {
      rec <- optimization_results()
      req(rec)

      # Update Inputs
      updateGlassRadio(session, "transformation", selected = rec$tr)
      updateGlassRadio(session, "pi_method", selected = rec$model)

      # Weighted Spline is nested in toggle panel
      updateGlassRadio(session, "ss_weighted", selected = rec$weighted)

      # Provide Button Feedback
      updateGlassButton(
        session,
        inputId = "apply_recommendations",
        label = "Applied!",
        icon = icon("check"),
        disabled = TRUE
      )
    })

    # --- State Watcher: Auto-enable/disable based on alignment ---
    observeEvent({
      list(
        input$M,                # Changes Recommendation
        raw_cs_data_long(),     # Changes Recommendation
        input$transformation,   # User Manual Change
        input$pi_method,        # User Manual Change
        input$ss_weighted       # User Manual Change
      )
    }, {
      rec <- optimization_results()
      # If system isn't ready, default to enabled so user can try again
      if (is.null(rec)) return()

      # Check if current UI settings match the Engine's recommendations
      # Using isTRUE to safely handle potential NULLs during init
      matches_tr       <- isTRUE(input$transformation == rec$tr)
      matches_model    <- isTRUE(input$pi_method == rec$model)
      matches_weighted <- isTRUE(input$ss_weighted == rec$weighted)

      if (matches_tr && matches_model && matches_weighted) {
        # Current state == Recommended state -> Button is "Applied"
        updateGlassButton(
          session,
          "apply_recommendations",
          label = "Applied!",
          icon = icon("check"),
          disabled = TRUE
        )
      } else {
        # Current state != Recommended state -> Button is available
        updateGlassButton(
          session,
          "apply_recommendations",
          label = "Use Recommendations",
          icon = icon("wand-magic-sparkles"),
          disabled = FALSE
        )
      }
    }, ignoreInit = FALSE)

    # ==========================================================================
    # ZETA TABLE LOGIC (Glass Table + Downloads)
    # ==========================================================================

    # A. Debounce Inputs to prevent spam
    dins_inputs_debounced <- reactive({
      list(
        tr = input$transformation,
        model = input$pi_method,
        weighted = input$ss_weighted,
        z_crit = current_recommended_zeta_upper()
      )
    }) |> shiny::debounce(millis = 500)

    # B. Calculate Current Zeta Data
    current_zeta_data <- reactive({
      inputs <- dins_inputs_debounced()
      req(cs_data_long(), inputs$z_crit)

      zeta_method <- inputs$model
      if (zeta_method == "fg") zeta_method <- "ols"
      if (zeta_method == "ss" && inputs$weighted == "Yes") zeta_method <- "ssw"

      # We can use the cached candidates for OLS/SS-Unweighted to make this instant
      # But for SS-Weighted we might need to run it (or just run standard function since it's fast for point est)
      out <- commutability::estimate_zeta_data(
        data = cs_data_long(),
        B = NULL, # Point estimates only (Fast)
        method = zeta_method,
        zeta_critical = inputs$z_crit,
        M = 1, N = 1
      )

      return(out)
    })

    # C. Render Custom Glass Table
    output$calculated_zetas_table <- renderUI({
      req(
        current_zeta_data(),
        dins_inputs_debounced()
      )
      inputs <- dins_inputs_debounced()
      dt <- current_zeta_data()

      # 1. Format Data for Display
      dt_display <- data.table::data.table(
        "Comparison" = dt$comparison,
        "Zeta" = format(dt$zeta, nsmall = 2, digits = 2),
        "Zeta_Crit" = format(inputs$z_crit, nsmall = 2, digits = 2),
        "Conclusion" = ifelse(dt$zeta > inputs$z_crit, "Excessive", "Acceptable")
      )

      # 2. Generate Caption
      n_total <- nrow(dt)
      n_excessive <- sum(dt$zeta > inputs$z_crit)

      caption_text <- if (n_excessive == 0) {
        sprintf("All %d comparisons demonstrate <b>acceptable</b> differences in nonselectivity.", n_total)
      } else if (n_excessive == n_total) {
        sprintf("All %d comparisons demonstrate <b>excessive</b> differences in nonselectivity.", n_total)
      } else {
        sprintf("<b>%d</b> out of %d comparisons demonstrate excessive differences in nonselectivity.", n_excessive, n_total)
      }

      # 3. Highlight rows with Excessive DINS
      bad_rows <- which(dt$zeta > inputs$z_crit)

      # 4. Render
      renderGlassTable(
        data = dt_display,
        col_names = c("IVD-MD Comparison", "\\(\\hat{\\zeta}\\)", "\\(\\zeta_{\\mathrm{upper}}\\)", "Conclusion"),
        caption = caption_text,
        highlight_rows = bad_rows,
        sortable = TRUE,
        sidebar_html = NULL
      )
    })

    # D. Download Handler 1: Current Zeta Results
    output$download_current_zeta <- downloadHandler(
      filename = function() { paste0("ceapkfcr_dins_current_", Sys.Date(), ".xlsx") },
      content = function(file) {
        req(current_zeta_data())
        final_zeta_data_debug <<- current_zeta_data()
        writexl::write_xlsx(
          x = current_zeta_data(),
          path = file,
          col_names = TRUE
        )
      }
    )

    # E. Download Handler 2: All Possible Zeta Results (Using Cache)
    output$download_all_zetas <- downloadHandler(
      filename = function() { paste0("ceapkfcr_dins_all_transformations_and_models_", Sys.Date(), ".xlsx") },
      content = function(file) {
        req(cached_zeta_estimates())

        # Flatten the list structure from cached_zeta_estimates
        raw_list <- cached_zeta_estimates()

        # Helper to extract rows
        flat_dt <- lapply(raw_list, function(item) {
          data.table::data.table(
            comparison = item$comparison,
            ols_identity = item$ols["identity"],
            ols_log = item$ols["log"],
            ols_boxcox = item$ols["boxcox"],
            ss_identity = item$ss["identity"],
            ss_log = item$ss["log"],
            ss_boxcox = item$ss["boxcox"]
          )
        })

        final_dt <- data.table::rbindlist(flat_dt)
        final_dt_debug <<- final_dt
        writexl::write_xlsx(
          x = final_dt,
          path = file,
          col_names = TRUE
        )
      }
    )

    # ==========================================================================
    # IMPRECISION TABLE LOGIC (Glass Table + Downloads)
    # ==========================================================================

    # A. Calculate Current Imprecision Data
    current_imprecision_data <- reactive({
      req(
        raw_cs_data_long()
      )

      raw_imprecision_data <- commutability::estimate_imprecision_data(
        data = raw_cs_data_long(),
        B = 1000L,
        type = "percentile",
        level = 0.95
      )

      # To highlight interesting rows
      unformatted_imprecison_data <- MS_wise_imprecision(
        imprecision_data = raw_imprecision_data,
        mode = "exact",
        percent = TRUE,
        variance = FALSE,
        rounding = 3
      )

      # For view only
      formatted_imprecision_data <- MS_wise_imprecision(
        imprecision_data = raw_imprecision_data,
        mode = "visual",
        percent = TRUE,
        variance = FALSE,
        rounding = 3
      )

      # Hard coding. Replacing a (b, c) with a (b - c).
      formatted_imprecision_data[[2]] <- gsub(", ", " - ", formatted_imprecision_data[[2]])
      formatted_imprecision_data[[3]] <- gsub(", ", " - ", formatted_imprecision_data[[3]])

      # Fix Column Names for Output
      names(formatted_imprecision_data) <- c(
        "IVD-MD",
        "CV (%) (95 % PBCI: Lower - Upper)",
        "SD (95 % PBCI: Lower - Upper)"
      )

      return(
        list(
          "raw" = raw_imprecision_data,
          "unformatted" = unformatted_imprecison_data,
          "formatted" = formatted_imprecision_data
        )
      )
    })

    # B. Render Custom Glass Table
    output$calculated_imprecision_table <- renderUI({
      req(
        current_imprecision_data()
      )

      # Extract from cache
      unformatted_imprecison_data <- current_imprecision_data()$unformatted
      formatted_imprecision_data <- current_imprecision_data()$formatted

      # Any much larger than the rest?
      larger_than_rest <- which(
        unformatted_imprecison_data$CV > 4 * median(unformatted_imprecison_data$CV)
      )

      # Fix Column Names for HTML Output
      names(formatted_imprecision_data) <- c(
        "IVD-MD",
        "\\(\\mathrm{CV} (\\%) \\, (95 \\% \\, \\mathrm{PBCI}: \\, \\mathrm{CV}_{\\mathrm{lower}} - \\mathrm{CV}_{\\mathrm{upper}})\\)",
        "\\(\\mathrm{SD} \\, (95 \\% \\, \\mathrm{PBCI}: \\, \\mathrm{SD}_{\\mathrm{lower}} - \\mathrm{SD}_{\\mathrm{upper}})\\)"
      )

      # 2. Generate Caption
      ivd_mds_string <- NULL
      ivd_md_names <- formatted_imprecision_data$`IVD-MD`
      n_total <- length(ivd_md_names)

      if (n_total == 2) {
        ivd_mds_string <- paste(
          ivd_md_names,
          collapse = " and "
        )
      }
      else {
        ivd_md_names[n_total] <- paste0(
          "and ",
          ivd_md_names[n_total]
        )
        ivd_mds_string <- paste(
          ivd_md_names,
          collapse = ", "
        )
      }

      caption_text <- paste0(
        "IVD-MD imprecision estimates and corresponding 95% percentile ",
        "bootstrap confidence intervals (PBCIs) for ",
        ivd_mds_string,
        "."
      )

      renderGlassTable(
        data = formatted_imprecision_data,
        col_names = names(formatted_imprecision_data),
        caption = caption_text,
        sidebar_html = NULL,
        sidebar_title = NULL,
        highlight_rows = NULL,
        sortable = TRUE
      )
    })

    # C. Download Handler: Imprecison Estimates
    output$download_imprecision <- downloadHandler(
      filename = function() {
        fn <- paste0(
          "ceapkfcr_IVD_MD_wise_imprecision_estimates_",
          Sys.Date(),
          ".xlsx"
        )
        return(fn)
      },
      content = function(file) {
        req(
          current_imprecision_data()
        )
        final_imprecision_data_debug <<- current_imprecision_data()$formatted
        writexl::write_xlsx(
          x = current_imprecision_data()$formatted,
          path = file,
          col_names = TRUE
        )
      }
    )

    # --- Avoid Suspension Issues ---

    # 1. Wake up the DATA GENERATORS first (The Tables)
    outputOptions(output, "general_tab_content", suspendWhenHidden = FALSE)
    outputOptions(output, "calculated_zetas_table", suspendWhenHidden = FALSE)
    outputOptions(output, "calculated_imprecision_table", suspendWhenHidden = FALSE)

    # 2. Wake up the DATA CONSUMERS second (The Download Handlers)
    outputOptions(output, "download_current_zeta", suspendWhenHidden = FALSE)
    outputOptions(output, "download_all_zetas", suspendWhenHidden = FALSE)
    outputOptions(output, "download_imprecision", suspendWhenHidden = FALSE)


    # --- Return values for other modules ---
    return(
      reactive({
        final_pi_method <- if (input$pi_method == "ss" && input$ss_weighted == "Yes") {
          "ssw"
        }
        else {
          input$pi_method
        }
        list(
          transformation = input$transformation,
          pi_method = final_pi_method,
          M = as.numeric(input$M) / 100,
          zeta_upper = current_recommended_zeta_upper()
        )
      })
    )
  })
}


