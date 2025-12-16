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
      class = "page-header",
      h1(
        class = "main-title",
        icon("ruler"),
        "Perform Outlier Analysis"
      ),
      glassButton(
        inputId = ns("show_outlier_analysis_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        color = "green"
      )
    ),

    glassTogglePanel(
      triggerId = ns("show_outlier_analysis_explanation"),
      help_button_page_3_text()
    ),

    # --- Card 1 - Outlier Analysis Options ------------------------------------
    glassCard(
      inputId = ns("outlier_options"),
      title = "Configuration",
      icon = icon("sliders"),
      collapsible = TRUE,
      collapsed = FALSE,
      disabled = FALSE,
      glassRow(
        glassCol(
          width = 4,
          glassRadioButtons(
            inputId = ns("outlier_test"),
            label = "Test for Outliers",
            label_icon = icon("people-arrows"),
            help_text = paste0(
              "Outlier test selection. Between samples means that we try to ",
              "detect particular samples with measurements being ",
              "unexpectedely different from one IVD-MD to another. Within ",
              "samples means that we try to detect particular ",
              "replicates within a sample being unexpectedely different from ",
              "other replicates within that sample."
            ),
            choices = c(
              "Between Samples" = "burnett",
              "Within Samples" = "qrange"
            ),
            selected = "burnett",
            width = "100%",
            disabled = FALSE
          )
        ),
        glassCol(
          width = 4,
          glassRadioButtons(
            inputId = ns("outlier_test_conf_level"),
            label = "Confidence Level",
            label_icon = icon("percent"),
            help_text = paste0(
              "Confidence level for the selected outlier test. Higher ",
              "confidence level signify a more conservative test, meaning ",
              "it takes more to deem a sample to be an outlier."
            ),
            choices = c(
              "80 %" = "0.80",
              "90 %" = "0.90",
              "95 %" = "0.95",
              "99 %" = "0.99"
            ),
            selected = "0.95",
            width = "100%",
            disabled = FALSE
          )
        )
      )
    ),

    # --- Card 2 - Outlier Analysis Results ------------------------------------
    glassResultCard(
      inputId = ns("outlier_test_results"),
      title = "Outlier Test Results",
      toolbar = glassButton(
        inputId = ns("get_outlier_results"),
        label = "Analyze",
        icon = icon("magnifying-glass-chart"),
        width = "50%",
        color = "purple",
        disabled = FALSE
      ),
      icon = icon("table"),
      width = "100%",
      # CHANGE 2: Using uiOutput instead of DTOutput for glassTable
      uiOutput(outputId = ns("outlier_results_ui"))
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

    # --- CHANGE 3: Enable Button Logic ----------------------------------------
    # Watch for Data Availability AND Input Changes to re-enable the button
    observeEvent(list(raw_cs_data_long(), input$outlier_test, input$outlier_test_conf_level), {
      # Only proceed if data is available
      if (!is.null(raw_cs_data_long())) {
        updateGlassButton(
          session = session,
          inputId = "get_outlier_results",
          disabled = FALSE
        )
      }
    }, ignoreInit = TRUE)

    # --- Update reactiveVal when `Analyze` Button is Pressed ------------------
    observeEvent(input$get_outlier_results, {
      # Require `raw_cs_data_long()` to exist
      req(raw_cs_data_long())

      # CHANGE 4: Disable button immediately on click
      updateGlassButton(
        session = session,
        inputId = "get_outlier_results",
        disabled = TRUE
      )

      results <- commutability::do_outlier_analysis(
        data = raw_cs_data_long(),
        method = input$outlier_test,
        variable = "influence",
        level = as.numeric(input$outlier_test_conf_level),
        output = "visual"
      )
      analysis_results_val(results)
    })

    # --- Render Table Using renderGlassTable ------------------------
    output$outlier_results_ui <- renderUI({
      # Require cache to be filled before displaying table
      req(analysis_results_val())

      # Extract results from cache
      results <- analysis_results_val()

      # --- 1. Identify Row Indices to Highlight (Logic for "." repeats) ---
      highlight_idx <- integer(0)

      # Case A: Sample-level tests (Burnett, IQR, Chauvenet)
      # Structure contains column "Outliers". Values: "none", "sample1...", or "."
      if ("Outliers" %in% names(results)) {
        vals <- results[["Outliers"]]
        last_val <- "none" # Default assumption
        for (i in seq_len(length(vals))) {
          val <- vals[i]
          if (val != ".") {
            last_val <- val
          }
          # Highlight if the effective value is not "none"
          if (last_val != "none") {
            highlight_idx <- c(highlight_idx, i)
          }
        }
      }
      # Case B: Replicate-level tests (Q-Range)
      # Structure: SampleID, Comparison columns with "yes"/"no"/".", etc.
      else {
        # Convert to matrix for easier cell access
        mat <- as.matrix(results)
        n_rows <- nrow(mat)
        n_cols <- ncol(mat)

        # Maintain state of the current row (to resolve dots)
        current_row_state <- rep(NA_character_, n_cols)

        for (i in seq_len(n_rows)) {
          row_vals <- mat[i, ]
          # Update state: if value is not ".", update the current state
          not_dot <- row_vals != "."
          current_row_state[not_dot] <- row_vals[not_dot]

          # Check for "yes" in any column of the resolved row
          if (any(current_row_state == "yes", na.rm = TRUE)) {
            highlight_idx <- c(highlight_idx, i)
          }
        }
      }

      # --- 2. Reactive Caption Logic ---
      has_outliers <- length(highlight_idx) > 0
      if (has_outliers) {
        caption_html <- "<span style='color: #B61F06; font-weight: 700;'><i class='fa fa-exclamation-triangle'></i> Potential outliers detected. Please review the highlighted items.</span>"
      } else {
        caption_html <- "<span style='color: #28A745; font-weight: 700;'><i class='fa fa-check-circle'></i> No outliers detected.</span>"
      }

      # --- 3. Generate Base HTML ---
      # We use highlight_rows here to leverage glassTable.R native functionality
      table_tag <- renderGlassTable(
        data = results,
        caption = caption_html,
        highlight_rows = highlight_idx,
        sortable = FALSE
      )

      # --- 4. HTML Injection for "Ditto" and "Yes" ---
      html_str <- as.character(table_tag)

      # A: Replace dots with Flex Ditto Icon
      # REMOVED justify-content: center as requested. Kept display: flex.
      icon_html <- "><span style='color: #ccc; font-size: 1.5em; line-height: 0.5; display: flex;'>&rdquo;</span></td>"

      modified_html_str <- gsub(
        pattern = ">\\.</td>",
        replacement = icon_html,
        x = html_str,
        fixed = FALSE
      )

      # B: Styling for "yes" cells
      # Bold and Red (#B61F06)
      modified_html_str <- gsub(
        pattern = ">yes</td>",
        replacement = "><span style='font-weight: bold; color: #B61F06;'>yes</span></td>",
        x = modified_html_str,
        fixed = TRUE
      )

      HTML(modified_html_str)
    })

    # --- Avoid Suspension Issues ---
    outputOptions(output, "outlier_results_ui", suspendWhenHidden = FALSE)

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
