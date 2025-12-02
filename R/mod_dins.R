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
      class = "version-badge",
      icon("flask"),
      "Commutability Evaluation: Beta Version S1.0"
    ),
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

      fluidRow(
        # 1. Transformation & Setting M(%)
        column(
          width = 6,
          div(
            class = "parameter-section",
            glassRadioButtons(
              inputId = ns("transformation"),
              label = "Transformation",
              label_icon = icon("right-left"),
              help_text = paste0(
                "Select None to use raw data. Select the transformation ",
                "that results in the most zeta estimates closest to 1."
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
        column(
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

    glassTabsetPanel(
      inputId = ns("dins_results_tabs"),
      selected = "dins_recommendations_and_info",
      color = "purple",
      glassTabPanel(
        title = "General",
        value = "dins_recommendations_and_info",
        icon = icon("circle-info"),
        glassResultCard(
          inputId = ns("card_recommendations_and_info"),
          title = "Information and Recommendations",
          icon = icon("circle-info"),
          width = "100%",
          toolbar = glassButton(
            inputId = ns("apply_recommendations"),
            label = "Use Recommendations",
            icon = icon("wand-magic-sparkles"),
            color = "green",
            width = "220px"
          ),
          uiOutput(ns("general_tab_content"))
        )
      ),
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
            glassButton(
              inputId = ns("calculate_zetas"),
              label = "Estimate",
              icon = icon("calculator"),
              width = "50%"
            ),
            glassButton(
              inputId = ns("clear_zetas"),
              label = "Clear",
              icon = icon("trash"),
              width = "50%"
            )
          ),

          withSpinner(
            ui_element = DT::DTOutput(outputId = ns("calculated_zetas")),
            type = 6,
            color = "#605CA8",
            hide.ui = TRUE
          )
        )
      ),
      glassTabPanel(
        title = "IVD-MD Imprecision Estimates",
        value = "imprecision_estimates",
        icon = icon("bullseye"),
        glassResultCard(
          inputId = ns("card_result_imprecision"),
          title = "IVD-MD Imprecision Estimates",
          icon = icon("bullseye"),
          width = "100%",

          # Toolbar with Action Buttons
          toolbar = div(
            style = "display: flex; gap: 10px;",
            glassButton(
              inputId = ns("calculate_imprecision"),
              label = "Estimate",
              icon = icon("bullseye"),
              width = "120px"
            ),
            glassButton(
              inputId = ns("clear_imprecision"),
              label = "Clear",
              icon = icon("trash"),
              width = "100px"
            )
          ),

          withSpinner(
            ui_element = DT::DTOutput(outputId = ns("im_results")),
            type = 6,
            color = "#28A745",
            hide.ui = TRUE
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

      diagnostics <- commutability::check_data(data = file_upload_data$raw_cs_data(), type = "cs")
      study_design_information <- diagnostics$quality

      maximum_n <- max(study_design_information$effective_number_of_samples, na.rm = TRUE)
      maximum_R <- ceiling(max(study_design_information$average_number_of_replicates, na.rm = TRUE))

      current_M <- as.numeric(input$M) / 100

      if (maximum_n < 20) maximum_n <- 20
      if (maximum_n > 50) maximum_n <- 50
      if (maximum_R < 2)  maximum_R <- 2
      if (maximum_R > 5)  maximum_R <- 5

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

    # --- Recommendation Engine (Heavy Lifter) ---
    optimization_results <- reactive({
      req(
        raw_cs_data_long(),
        current_recommended_zeta_upper()
      )

      #browser()

      # 1. Run the Expert System
      # This returns a list of results (one per comparison in your dataset)
      engine_output <- tryCatch(
        recommendation_engine(
          data = raw_cs_data_long(),
          zeta_critical = current_recommended_zeta_upper()
        ),
        error = function(e) {
          warning("Recommendation Engine Error: ", e$message)
          return(NULL)
        }
      )

      if (is.null(engine_output) || length(engine_output) == 0) return(NULL)



      # 2. Aggregate Results (Majority Vote)
      # We convert the list to a data.table to easily count frequencies
      results_dt <- data.table::rbindlist(engine_output)

      # We group by Transformation and Model to find the most frequent combination
      # We also calculate the average Zeta for that combo to determine overall validity
      ranking <- results_dt[, .(
        votes = .N,
        avg_zeta = mean(zeta, na.rm = TRUE)
      ), by = .(best_transformation, model_type)]

      # Sort by votes (descending) and then avg_zeta (ascending, as tie-breaker)
      data.table::setorder(ranking, -votes, avg_zeta)

      winner <- ranking[1]

      # 3. Map Winner to UI Inputs

      # Map Transformation Code -> UI Radio Value
      ui_tr <- switch(winner$best_transformation,
                      "identity"   = "identity",
                      "log#e"      = "ln",
                      "boxcox#0.5" = "boxcox",
                      "identity"   # Fallback
      )

      # Map Model Type -> UI Radio Value
      ui_model <- if (winner$model_type == "OLS") "fg" else "ss"

      # Map Weighted
      # Currently hardcoded to No as per engine logic, but ready for future expansion
      ui_weighted <- "No"

      # Map Validity
      # We define "Valid" if the average zeta of the winning strategy is below the limit
      is_valid <- winner$avg_zeta <= current_recommended_zeta_upper()

      # Create a descriptive reason string for the UI
      total_comparisons <- nrow(results_dt)
      vote_share <- round(100 * winner$votes / total_comparisons, 1)

      reason_text <- sprintf(
        "Global Consensus: Selected by %s%% of comparisons (%d/%d).",
        vote_share, winner$votes, total_comparisons
      )

      # Return structured list
      list(
        tr = ui_tr,
        model = ui_model,
        weighted = ui_weighted,
        is_valid = is_valid,
        zeta = winner$avg_zeta,
        reason = reason_text
      )
    })

    # --- 4. Render General Tab Content ---
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

    # --- 5. Handle "Use Recommendations" Click ---
    observeEvent(input$apply_recommendations, {
      rec <- optimization_results()
      req(rec)

      # Update Inputs
      updateGlassRadio(session, "transformation", selected = rec$tr)
      updateGlassRadio(session, "pi_method", selected = rec$model)

      # Weighted Spline is nested in toggle panel
      updateGlassRadio(session, "ss_weighted", selected = rec$weighted)

      showNotification("Recommendations applied successfully!", type = "message", duration = 3)
    })

    # --- DINS Estimation Logic ------------------------------------------------
    # --- Activates if Relevant Button is Pressed ---
    # --- Event Reactive Checks if Estimate DINS Button is Pressed ---
    zetas_calculated <- eventReactive(input$calculate_zetas, {
      req(cs_data_long())

      zeta_method <- if (input$pi_method == "fg") {
        "ols"
      }
      else if (input$pi_method == "ss") {
        # Check if the conditional weighted input is "Yes"
        if (input$ss_weighted == "Yes") {
          "ssw"
        }
        else {
          "ss"
        }
      }

      commutability::estimate_zeta_data(
        data = cs_data_long(),
        B = NULL,
        method = zeta_method,
        M = 1,
        N = 1
      )
    })

    # --- IVD-MD Imprecision Estimation Logic ----------------------------------
    # --- Activates if Relevant Button is Pressed ---
    # --- Event Reactive Checks if Estimate Repeatability Button is Pressed ---
    imprecision_calculated <- eventReactive(input$calculate_imprecision, {
      # Requires raw_cs_data_long() to work
      req(
        raw_cs_data_long()
      )
      commutability::estimate_imprecision_data(
        data = raw_cs_data_long(),
        B = 1000L,
        type = "percentile",
        level = 0.95
      )
    })

    # --- UI Rendering for Tables ---
    # --- Control visibility of the tables ---

    # --- Show / Hide Dins Estimates Table ----
    show_zetas <- reactiveVal(FALSE)
    observeEvent(input$calculate_zetas, { show_zetas(TRUE) })
    observeEvent(input$clear_zetas, { show_zetas(FALSE) })

    # --- Show / Hide IVD-MD Repeatability Estimates Table ---
    show_imprecision <- reactiveVal(FALSE)
    observeEvent(input$calculate_imprecision, { show_imprecision(TRUE) })
    observeEvent(input$clear_imprecision, { show_imprecision(FALSE) })

    # --- Render Table of DINS Estimates ---
    output$calculated_zetas <- DT::renderDT({

      # Check first if DINS estimates should be showed
      if (show_zetas()) {
        out <- zetas_calculated()
        out$zeta <- format(
          x = out$zeta,
          nsmall = 2,
          digits = 2
        )

        names(out) <- c("IVD-MD Comparison", "zeta")

        DT::datatable(
          out,
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
      }
    })

    # Render Imprecision Table
    output$im_results <- DT::renderDT({
      if (show_imprecision()) {
        imprecision_data <- imprecision_calculated()
        out <- MS_wise_imprecision(
          imprecision_data = imprecision_data,
          mode = "visual",
          percent = TRUE,
          rounding = 3L)

        names(out) <- c("IVD-MD", "CV % (95 % CI: lower - upper)", "SD (95 % CI: lower - upper)")

        # Hard coding. Replacing a (b, c) with a (b - c).
        out$`CV % (95 % CI: lower - upper)` <- gsub(", ", " - ", out$`CV % (95 % CI: lower - upper)`)
        out$`SD (95 % CI: lower - upper)` <- gsub(", ", " - ", out$`SD (95 % CI: lower - upper)`)

        DT::datatable(
          out,
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
      }
    })

    # ADDED: Render the recommended zeta display
    output$recommended_zeta_display <- renderUI({
      req(current_recommended_zeta_upper())
      zeta_val <- current_recommended_zeta_upper()
      withMathJax(
        div(
          class = "help-info-box",
          style = "margin-top: 0px; border-left-color: #605CA8;",
          tagList(
            div(
              class = "input-note",
              style = "font-style: normal; font-weight: 600;",
              icon("cogs"),
              sprintf("Calculated value: \\(\\hat{\\zeta}_{\\mathrm{upper}} = %s\\)", format(zeta_val, nsmall = 2, digits = 2))
            ),
            div(
              style = "margin-top: 10px; color: #6c757d; font-size: 1.25rem;",
              "Any IVD-MD pair with a \\(\\hat{\\zeta}\\) value above this limit will be deemed to have excessive differences in nonselectivity."
            )
          )
        )
      )
    })

    outputOptions(output, "recommended_zeta_display", suspendWhenHidden = FALSE)
    outputOptions(output, "calculated_zetas", suspendWhenHidden = FALSE)
    outputOptions(output, "im_results", suspendWhenHidden = FALSE)

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


