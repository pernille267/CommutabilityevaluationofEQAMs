#' Generates the content for the help panel on the 'Upload data' page.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_1_text <- function() {

  glassHelpCard(
    # 1. Tip Section
    glassHelpTip(
      text = "<b>Pro Tip:</b> Click the 'Show Help Text' button again to hide this panel at any time."
    ),

    # 2. Instructions
    glassHelpSection(
      title = "How to Get Started",

      glassHelpStep(
        number = 1,
        title = "Upload Clinical Sample Data",
        description = "Click the first 'Browse...' button to select your clinical sample data file. We support <span style='color:#28A745; font-weight:600;'>.xlsx</span> and <span style='color:#28A745; font-weight:600;'>.csv</span> formats."
      ),

      glassHelpStep(
        number = 2,
        title = "Upload EQA Material Data",
        description = "Click the second 'Browse...' button to select your External Quality Assessment (EQA) data file."
      ),

      glassHelpStep(
        number = 3,
        title = "Review Diagnostics",
        description = "The app automatically runs checks. Look for the status badge next to the 'Diagnostic Overview' title."
      )
    ),

    # 3. Interpretation
    glassHelpSection(
      title = "Interpreting the Diagnostics",
      htmltools::p(style = "color:#666; margin-bottom:15px;",
                   "The diagnostic tables summarize data quality using a 0-9 score."),

      glassHelpInfoBox(
        glassHelpInfoItem("Perfect (9)", "Exceptional quality. No issues.", color = "#7851a9"),
        glassHelpInfoItem("Acceptable (4-8)", "Meets analysis requirements.", color = "#28A745"),
        glassHelpInfoItem("Questionable (1-3)", "Minor issues (auto-repaired).", color = "#FEAB3A"),
        glassHelpInfoItem("Extremely Poor (0)", "Significant issues (exclusions applied).", color = "#B61F06")
      )
    ),

    # 4. Additional Options
    glassHelpSection(
      title = "Additional Options",

      glassHelpStep(
        number = icon("list"),
        title = "Reference Method (Optional)",
        description = "Designate one IVD-MD as a reference to compare others against it. Defaults to 'All vs All'."
      ),

      glassHelpStep(
        number = icon("exclamation"),
        title = "Ignore Validation (High Risk)",
        description = "Forcing analysis on failed data is not recommended and may cause crashes.",
        color = "#dc3545"
      )
    )
  )
}

#' Generates the HTML content for the help panel on the 'Nonselectivity' page.
#'
#' @return An HTML string.
#' @noRd
help_button_page_2_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Analysis Parameter Guide",
      htmltools::p(style = "color: #555; margin-bottom: 15px;",
                   "This page allows you to set the core statistical parameters for the commutability evaluation. The choices you make here will affect the calculations in subsequent tabs."
      ),

      glassHelpStep(
        number = 1,
        title = "Data Transformation",
        description = "Select a transformation for your data. This can help stabilize variance and meet model assumptions. Try different options to see which yields zeta values closest to 1."
      ),

      glassHelpStep(
        number = 2,
        title = "Model Option",
        description = "Choose the statistical model for calculating prediction intervals. The <b>Deming</b> approach is generally recommended as it accounts for measurement errors in both methods."
      ),

      glassHelpStep(
        number = 3,
        title = "Calculate Zetas & Imprecision",
        description = "Click the 'Calculate zetas' and 'Calculate Imprecision' buttons to see estimates based on your chosen model and transformation. These tables help you assess the suitability of your choices."
      ),

      glassHelpStep(
        number = 4,
        title = "Set Non-Selectivity Tolerance (M)",
        description = "This value represents the average relative increase in the prediction interval's length you are willing to tolerate due to non-selectivity. A larger M means accepting more non-selectivity differences."
      )
    )
  )
}

#' Generates the content for the help panel on the 'Outlier Analysis' page.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_3_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Outlier Analysis Guide",
      htmltools::p(style = "color: #555;",
                   "This section is dedicated to identifying potential outliers within your <b>Clinical Sample Data</b>. Outliers are data points that deviate significantly from other observations and can unduly influence the results of your analysis."
      )
    ),

    glassHelpSection(
      title = "How to Perform the Analysis",

      glassHelpStep(
        number = 1,
        title = "Select Outlier Test",
        description = paste0(
          "Choose the statistical criterion for identifying outliers.<br>",
          "<span style='color:#6c757d; font-size:0.9em;'>&bull; <b>Between Samples:</b> Uses Burnett's criterion to identify entire samples that are outliers compared to others.</span><br>",
          "<span style='color:#6c757d; font-size:0.9em;'>&bull; <b>Within Samples:</b> Uses the Studentized Range (Q) to identify outlier replicates within a single sample.</span>"
        )
      ),

      glassHelpStep(
        number = 2,
        title = "Select Confidence Level",
        description = "Set the statistical confidence for the test. This determines how extreme a point must be to be flagged as an outlier. A higher confidence level (e.g., 99%) makes the test stricter."
      ),

      glassHelpStep(
        number = 3,
        title = "Run the Analysis",
        description = "Click the <b style='color: #605CA8;'>Analyze</b> button to perform the outlier detection."
      )
    ),

    glassHelpSection(
      title = "Interpreting the Results",
      # Using a step with a warning color to highlight the important note
      glassHelpStep(
        number = icon("exclamation-triangle"),
        title = "Important Note",
        description = "Any sample flagged as an <b style='color: #dc3545;'>outlier</b> will be highlighted in the results table. <br><br>This tool only identifies potential outliers; it does not automatically remove them. If you wish to proceed without these outliers, you must manually remove them from your source file and <b>re-upload the cleaned dataset</b> on the 'Upload Data' tab.",
        color = "#dc3545"
      )
    )
  )
}

#' Generates the content for the help panel on the 'Formal Model Assessment' tab.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_4A_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Formal Model Assessment Guide",
      htmltools::p(style = "color: #555; margin-bottom: 10px;",
                   "This section allows you to perform formal hypothesis tests to validate the assumptions of the chosen regression model. These tests provide statistical evidence to assess the model's suitability for your data."
      ),
      htmltools::p(style = "color: #555;",
                   "Simply click the <b style='color: #605CA8;'>Run tests</b> button to execute a standard set of validation tests for normality and homoscedasticity (constant variance) for each IVD-MD comparison."
      )
    )
  )
}

#' Generates the content for the help panel on the 'Model Assessment Plots' tab.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_4B_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Model Assessment Plots Guide",
      htmltools::p(style = "color: #555;",
                   "This section provides graphical tools for the visual assessment of the regression model assumptions. Plots can often reveal patterns or issues that are not obvious from formal tests alone."
      )
    ),

    glassHelpSection(
      title = "How to Generate Plots",

      glassHelpStep(
        number = 1,
        title = "Choose Plot Type",
        description = "Select one of the five available diagnostic plots to check for issues like non-linearity, non-constant variance, or non-normality of residuals."
      ),

      glassHelpStep(
        number = 2,
        title = "Customize Your Plot",
        description = "Use the options to add curves, customize titles and labels, and adjust the plot dimensions for downloaded files."
      ),

      glassHelpStep(
        number = 3,
        title = "Generate and Save",
        description = "Click the <b style='color: #605CA8;'>Plot</b> button to generate the visual. Click it again to update after changing options. Use the <b style='color: #605CA8;'>Download</b> button to save the plot to your device."
      )
    )
  )
}

#' Generates the content for the help panel on the 'Results Tables' tab.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_5A_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Results Table Guide",
      htmltools::p(style = "color: #555;",
                   "This section presents the detailed numerical results of the commutability evaluation. Use the options below to customize the analysis before generating the output table."
      )
    ),

    glassHelpSection(
      title = "How to Generate the Table",

      glassHelpStep(
        number = 1,
        title = "Select Confidence Level",
        description = "Choose the confidence level for the prediction intervals, which are used to determine commutability. A 99% level is generally recommended."
      ),

      glassHelpStep(
        number = 2,
        title = "Apply Filters (Optional)",
        description = "You can narrow down the results based on whether materials fall inside or outside the prediction intervals, or by whether the difference in non-selectivity (DINS) was acceptable."
      ),

      glassHelpStep(
        number = 3,
        title = "Choose Format",
        description = "Select 'Compact' for a concise view or 'Expanded' for a wide-format table with all data points in separate columns."
      ),

      glassHelpStep(
        number = 4,
        title = "Generate the Table",
        description = "Click the <b style='color: #605CA8;'>Calculate</b> button to perform the final analysis and display the results."
      )
    )
  )
}

#' Generates the content for the help panel on the 'Results Plots' tab.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_5B_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Results Plot Guide",
      htmltools::p(style = "color: #555;",
                   "This section provides a powerful way to visualize the commutability evaluation results. Plots offer a complementary perspective to the numerical tables, making it easier to interpret relationships and identify patterns."
      )
    ),

    glassHelpSection(
      title = "How to Customize and Generate Plots",

      glassHelpStep(
        number = 1,
        title = "Set Plotting Options",
        description = "Adjust the axis tick density for readability and choose a pattern curve to overlay on the clinical sample data to highlight trends."
      ),

      glassHelpStep(
        number = 2,
        title = "Customize Labels and Dimensions",
        description = "Provide custom titles for the plot and its axes. The width and height inputs control the dimensions of the downloaded plot."
      ),

      glassHelpStep(
        number = 3,
        title = "Prepare for Download",
        description = "Select the desired file format (PDF, PNG, TIF) and resolution (DPI) for the saved image."
      ),

      glassHelpStep(
        number = 4,
        title = "Generate and Save",
        description = "Click the <b style='color: #605CA8;'>Plot</b> button to render the graph. Click it again to refresh after changing options. Use the <b style='color: #605CA8;'>Download</b> button to save the plot."
      )
    )
  )
}

#' Generates the content for the help panel on the 'Report' tab.
#' @return A glassHelpCard tagList.
#' @noRd
help_button_page_5C_text <- function() {

  glassHelpCard(
    glassHelpTip("Click the 'Show Help Text' button again to hide this panel."),

    glassHelpSection(
      title = "Report Generation Guide",
      htmltools::p(style = "color: #555;",
                   "This tab allows you to generate a comprehensive, self-contained PDF document that summarizes your entire commutability evaluation session. This report is ideal for documentation, sharing, and archiving your analysis."
      )
    ),

    glassHelpSection(
      title = "How to Generate the Report",
      glassHelpStep(
        number = 1,
        title = "Download the Report",
        description = "Simply click the <b style='color: #605CA8;'>Download Report</b> button. The application will gather all inputs, analyses, and results from your current session and compile them into a single PDF file, which will then be downloaded to your device."
      )
    ),

    glassHelpSection(
      title = "What's Inside the Report?",
      htmltools::p(style = "color: #555; margin-bottom: 15px;",
                   "The generated report is structured to provide a complete overview of the evaluation, including:"
      ),

      glassHelpInfoBox(
        glassHelpInfoItem("Data Diagnostics", "A full summary of the initial validation and quality checks for both your clinical sample and EQA material data.", icon = "clipboard-check"),
        glassHelpInfoItem("Analysis Parameters", "A record of all the key parameters you selected, such as the data transformation, regression model, and non-selectivity tolerance (M).", icon = "sliders"),
        glassHelpInfoItem("Outlier and Model Validation", "The results from the outlier analysis and the formal and visual tests used to validate the statistical model.", icon = "ruler"),
        glassHelpInfoItem("Final Results", "The complete set of detailed numerical result tables and the final commutability evaluation plots.", icon = "table")
      )
    )
  )
}

#' Render a styled diagnostic table using GlassTable
#'
#' @param diagnostics The diagnostic list object from commutability::check_data()
#' @param type A character string, either "cs" for clinical sample or "eq" for EQA material
#' @param is_post_repair_valid Logical
#' @param post_repair_score Integer
#' @return A GlassTable UI object
#' @export
render_diagnostic_table <- function(diagnostics, type = "cs", is_post_repair_valid = TRUE, post_repair_score = NULL) {

  current_badge <- diagnostics$badge
  current_score <- diagnostics$score

  # Check Whether Current Badge and Score Exist
  if (is.null(current_badge)) {
    current_badge <- "not acceptable"
  }
  if (is.null(current_score)) {
    current_score <- 0L
  }
  if (is.na(current_score)) {
    current_score <- 0L
  }

  badge_colors <- list(
    "perfect" = "#7851a9",
    "acceptable" = "#28A745",
    "questionable" = "#FEAB3A",
    "extremely poor" = "#B61F06",
    "not acceptable" = "#000000"
  )
  score_color <- badge_colors[[current_badge]]
  if(is.null(score_color)) score_color <- "#000000"

  # --- Fallback ---
  if (!is.null(post_repair_score) && is.na(post_repair_score)) {

    # --- 1. Caption ---
    caption_text <- generate_quality_message(
      quality = "not acceptable",
      sample_type = type,
      is_post_repair_valid = FALSE,
      post_repair_score = 0
    )

    # --- 2. Sidebar ---
    badge_html <- sprintf(
      '<span class="glass-diag-badge-pill" style="color:%s; border: 1px solid %s;">%s</span>',
      score_color, score_color, tools::toTitleCase(current_badge)
    )
    circle_html <- sprintf(
      '<div class="glass-score-circle" style="background-color: %s;">%s</div>',
      score_color, current_score
    )
    sidebar_content <- paste0(badge_html, circle_html)

    generic_fall_back_data <- data.table::data.table(
      "IVD-MD" = c("Unknown"),
      "Samples" = "Unknown",
      "Replicates" = "Unknown",
      "Invalid Results (%)" = "Probably a lot"
    )

    # --- Fallback Render ---
    return(
      renderGlassTable(
        data = generic_fall_back_data,
        col_names = c("IVD-MD", "Samples", "Replicates", "Invalid Results (%)"),
        caption = caption_text,
        sidebar_html = sidebar_content,
        sidebar_title = NULL,
        highlight_rows = 1,
        sortable = FALSE
      )
    )

  }

  # --- 1. Caption ---
  caption_text <- generate_quality_message(
    quality = current_badge,
    sample_type = type,
    is_post_repair_valid = is_post_repair_valid,
    post_repair_score = post_repair_score
  )

  # --- 2. Sidebar ---
  sidebar_content <- NULL
  if (!is.na(current_score)) {
    badge_html <- sprintf(
      '<span class="glass-diag-badge-pill" style="color:%s; border: 1px solid %s;">%s</span>',
      score_color, score_color, tools::toTitleCase(current_badge)
    )

    circle_html <- sprintf(
      '<div class="glass-score-circle" style="background-color: %s;">%s</div>',
      score_color, current_score
    )
    sidebar_content <- paste0(badge_html, circle_html)
  }

  # --- 3. Data Table ---
  current_IVD_MDs <- names(diagnostics$quality$number_of_NAs)

  # Format Fraction: "0.0%"
  raw_fractions <- unlist(diagnostics$quality$fraction_of_NAs)
  # Ensure it is numeric before formatting
  formatted_fractions <- sprintf("%.1f%%", as.numeric(raw_fractions) * 100)

  output_tbl <- data.table::data.table(
    "IVD-MD" = current_IVD_MDs,
    "Number of Samples" = diagnostics$quality$effective_number_of_samples,
    "Number of Replicates" = format(diagnostics$quality$average_number_of_replicates, digits = 2),
    "Invalid %" = formatted_fractions
  )

  repaired_methods <- c(diagnostics$repair$remove_these_methods, diagnostics$repair$convert_these_methods_to_numeric)
  highlight_idx <- which(output_tbl[["IVD-MD"]] %in% repaired_methods)

  # --- 4. Render ---
  renderGlassTable(
    data = output_tbl,
    col_names = c("IVD-MD", "Samples", "Replicates", "Invalid Results (%)"),
    caption = caption_text,
    sidebar_html = sidebar_content,
    sidebar_title = NULL,
    highlight_rows = highlight_idx,
    sortable = TRUE
  )
}

#' Render diagnostic test results with Glass styling
#'
#' @param diagnostics The diagnostic list object
#' @param other_diagnostics The diagnostic list object from the other data type
#' @param type A character string, either "cs" or "eq"
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
render_diagnostic_text <- function(diagnostics, other_diagnostics, type = "cs") {

  # --- 1. Setup Labels & Icons ---
  is_cs <- (type == "cs")
  label_main <- if (is_cs) "Validation Tests for Clinical Sample Data" else "Validation Tests for Evaluated Material Data"
  label_repair <- if (is_cs) "Repairing of Clinical Sample Data" else "Repairing of Evaluated Material Data"
  icon_main <- if (is_cs) "vial" else "chart-line"

  other_label <- if (is_cs) "EQA material" else "clinical sample"

  # --- 2. Build Validation Items (Section 1) ---

  # Helper to build a single item row
  build_item <- function(title, passed) {
    status_cls <- if (is.na(passed)) "unknown" else if (passed) "success" else "fail"
    icon_cls   <- if (is.na(passed)) "question-circle" else if (passed) "check-circle" else "times-circle"
    badge_txt  <- if (is.na(passed)) "?" else if (passed) "PASS" else "FAIL"

    htmltools::tags$div(
      class = paste("glass-diag-item", status_cls),
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = paste("fa fa-", icon_cls, "glass-diag-item-icon")),
        htmltools::tags$span(class = "glass-diag-item-text", title)
      ),
      htmltools::tags$span(class = "glass-diag-badge", badge_txt)
    )
  }

  # Define the 4 tests
  tests <- list(
    list(t = "Mandatory ID Columns",    p = diagnostics$validity$valid_mandatory_id_columns),
    list(t = "Valid IVD-MD Measurements", p = diagnostics$validity$valid_numeric_columns),
    list(t = "Valid Number of Missing Values", p = diagnostics$validity$valid_number_nas),
    list(t = "Valid Number of IVD-MDs",   p = diagnostics$validity$valid_number_remaining_numeric)
  )

  # Generate HTML for items
  test_items_html <- lapply(tests, function(x) build_item(x$t, x$p))

  # --- 3. Calculate Validation Summary (Top Right) ---
  n_total <- length(tests)
  n_pass  <- sum(sapply(tests, function(x) isTRUE(x$p)))
  summary_text <- sprintf("%d/%d validation tests passed", n_pass, n_total)

  summary_cls <- if (n_pass == n_total) "success" else if (n_pass >= (n_total - 1)) "warning" else "fail"

  # --- 4. Build Repair Items (Section 3) ---

  methods_removed_local <- diagnostics$repair$remove_these_methods
  methods_removed_other <- other_diagnostics$repair$remove_these_methods
  # Only report cross-removal if it's NOT already removed locally
  cross_removed <- setdiff(methods_removed_other, methods_removed_local)

  repair_items_html <- list()
  repair_summary_text <- "No important critical repairs are required"
  repair_summary_cls <- "success"

  # A. Local Exclusions
  if (!is.null(methods_removed_local) && length(methods_removed_local) > 0) {
    bold_m <- paste(methods_removed_local, collapse = ", ")
    msg <- paste0(bold_m, " removed due to excessive invalid data.")

    repair_items_html[[length(repair_items_html) + 1]] <- htmltools::tags$div(
      class = "glass-diag-item warning",
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = "fa fa-exclamation-triangle glass-diag-item-icon"),
        htmltools::tags$span(class = "glass-diag-item-text", msg)
      ),
      htmltools::tags$span(class = "glass-diag-badge", "ACTION")
    )
    repair_summary_text <- "Repair action taken, see below"
    repair_summary_cls <- "warning"
  }

  # B. Cross Exclusions
  if (length(cross_removed) > 0) {
    bold_m <- paste(cross_removed, collapse = ", ")
    msg <- paste0(bold_m, " removed due to issues in ", other_label, " data.")

    repair_items_html[[length(repair_items_html) + 1]] <- htmltools::tags$div(
      class = "glass-diag-item warning",
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = "fa fa-link glass-diag-item-icon"),
        htmltools::tags$span(class = "glass-diag-item-text", msg)
      ),
      htmltools::tags$span(class = "glass-diag-badge", "SYNC")
    )
    repair_summary_text <- "Repair action taken, see below"
    repair_summary_cls <- "warning"
  }

  # C. Conversions
  if (!is.null(diagnostics$repair$convert_these_methods_to_numeric)) {
    bold_m <- paste(diagnostics$repair$convert_these_methods_to_numeric, collapse = ", ")
    msg <- paste0("Attempted to convert ", bold_m, " to numeric.")

    repair_items_html[[length(repair_items_html) + 1]] <- htmltools::tags$div(
      class = "glass-diag-item warning",
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = "fa fa-wrench glass-diag-item-icon"),
        htmltools::tags$span(class = "glass-diag-item-text", msg)
      ),
      htmltools::tags$span(class = "glass-diag-badge", "FIX")
    )
    # Only change summary if it wasn't already warning
    if (repair_summary_cls == "success") {
      repair_summary_text <- "Minor repairs performed"
      repair_summary_cls <- "warning"
    }
  }

  # D. Success Message (If list empty)
  if (length(repair_items_html) == 0) {
    repair_items_html[[1]] <- htmltools::tags$div(
      class = "glass-diag-item success",
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = "fa fa-check-circle glass-diag-item-icon"),
        htmltools::tags$span(class = "glass-diag-item-text", "No column exclusion performed in data repair.")
      ),
      htmltools::tags$span(class = "glass-diag-badge", "OK")
    )
  }

  # --- 5. Assemble the UI ---

  # Dependency
  dep <- htmltools::htmlDependency(
    name = "glass-diagnostics",
    version = "1.0.0",
    src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
    script = "glass_diagnostics.js",
    stylesheet = "glass_diagnostics.css"
  )

  htmltools::tagList(
    # --- Section 1: Validation Tests ---
    htmltools::tags$div(
      class = "glass-diag-container collapsed",
      # Header
      htmltools::tags$div(
        class = "glass-diag-header",
        htmltools::tags$div(
          class = "glass-diag-title-wrap",
          htmltools::tags$div(class = "glass-diag-icon", htmltools::tags$i(class = paste0("fa fa-", icon_main))),
          htmltools::tags$h4(class = "glass-diag-title", label_main)
        ),
        htmltools::tags$div(
          class = "glass-diag-right",
          htmltools::tags$span(class = paste("glass-diag-summary", summary_cls), summary_text),
          htmltools::tags$i(class = "fa fa-chevron-down glass-diag-toggle")
        )
      ),
      # Body
      htmltools::tags$div(
        class = "glass-diag-body",
        htmltools::tagList(test_items_html)
      )
    ),

    # --- Section 2: Repairing ---
    htmltools::tags$div(
      class = "glass-diag-container collapsed",
      # Header
      htmltools::tags$div(
        class = "glass-diag-header",
        htmltools::tags$div(
          class = "glass-diag-title-wrap",
          htmltools::tags$div(class = "glass-diag-icon", htmltools::tags$i(class = "fa fa-wrench")),
          htmltools::tags$h4(class = "glass-diag-title", label_repair)
        ),
        htmltools::tags$div(
          class = "glass-diag-right",
          htmltools::tags$span(class = paste("glass-diag-summary", repair_summary_cls), repair_summary_text),
          htmltools::tags$i(class = "fa fa-chevron-down glass-diag-toggle")
        )
      ),
      # Body
      htmltools::tags$div(
        class = "glass-diag-body",
        htmltools::tagList(repair_items_html)
      )
    ),
    dep
  )
}

#' Render agreement diagnostic test results with Glass styling
#'
#' @param diagnostics The diagnostic list object from check_equivalence
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
render_agreement_text <- function(diagnostics) {

  # --- 1. Helper to build a single item row ---
  build_item <- function(title, passed) {
    status_cls <- if (is.na(passed)) "unknown" else if (passed) "success" else "fail"
    icon_cls   <- if (is.na(passed)) "question-circle" else if (passed) "check-circle" else "times-circle"
    badge_txt  <- if (is.na(passed)) "?" else if (passed) "PASS" else "FAIL"

    htmltools::tags$div(
      class = paste("glass-diag-item", status_cls),
      htmltools::tags$div(
        class = "glass-diag-item-content",
        htmltools::tags$i(class = paste("fa fa-", icon_cls, "glass-diag-item-icon")),
        htmltools::tags$span(class = "glass-diag-item-text", title)
      ),
      htmltools::tags$span(class = "glass-diag-badge", badge_txt)
    )
  }

  # --- 2. Build Test Items ---
  items_html <- list()
  items_html[[1]] <- build_item("Equal IVD-MD Names", diagnostics$equal_names)

  # Logic: Only show the second test if names are equal, OR if both failed?
  # Mimicking previous logic:
  if (diagnostics$equal_names) {
    items_html[[2]] <- build_item("Equal IVD-MD Column Order", diagnostics$equal_order)
  } else {
    items_html[[2]] <- build_item("Equal IVD-MD Column Order", FALSE)
  }

  # --- 3. Summary ---
  n_pass <- sum(diagnostics$equal_names, diagnostics$equal_order)
  summary_text <- sprintf("%d/2 tests passed", n_pass)
  summary_cls <- if (n_pass == 2) "success" else "fail"

  # --- 4. Dependency ---
  dep <- htmltools::htmlDependency(
    name = "glass-diagnostics",
    version = "1.0.0",
    src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
    script = "glass_diagnostics.js",
    stylesheet = "glass_diagnostics.css"
  )

  # --- 5. Assemble UI ---
  htmltools::tagList(
    htmltools::tags$div(
      class = "glass-diag-container collapsed",
      # Header
      htmltools::tags$div(
        class = "glass-diag-header",
        htmltools::tags$div(
          class = "glass-diag-title-wrap",
          htmltools::tags$div(class = "glass-diag-icon", htmltools::tags$i(class = "fa fa-check-double")),
          htmltools::tags$h4(class = "glass-diag-title", "Structural Agreement Tests")
        ),
        htmltools::tags$div(
          class = "glass-diag-right",
          htmltools::tags$span(class = paste("glass-diag-summary", summary_cls), summary_text),
          htmltools::tags$i(class = "fa fa-chevron-down glass-diag-toggle")
        )
      ),
      # Body
      htmltools::tags$div(
        class = "glass-diag-body",
        htmltools::tagList(items_html)
      )
    ),
    dep
  )
}

#' Expert System for Optimal Regression Model and Transformation Selection
#'
#' @description
#' An automated decision-making engine that evaluates IVD-MD comparison data to recommend
#' the optimal statistical transformation (Identity, Log, or Box-Cox) and regression model
#' (Deming OLS or Smoothing Spline).
#'
#' The function employs a "gatekeeper" logic combined with a sophisticated scoring algorithm
#' to balance model simplicity (OLS) against goodness-of-fit requirements.
#'
#' @param data A \code{data.table} object containing the raw comparison data. Must include the following columns:
#' \itemize{
#'   \item \code{comparison}: Character/Factor identifier for the IVD-MD pair.
#'   \item \code{SampleID}: Identifier for specific samples.
#'   \item \code{ReplicateID}: Identifier for replicates within samples.
#'   \item \code{MP_A}: Measurement results for the first IVD-MD.
#'   \item \code{MP_B}: Measurement results for the second IVD-MD.
#' }
#' @param zeta_critical Numeric. The upper threshold for the Zeta statistic (default: 4.24).
#' If an OLS model yields a Zeta score below this threshold, it is considered a "Success" and
#' the system will prioritize OLS over Smoothing Splines to prevent overfitting.
#' @param parsimony_threshold Numeric. If both (Smoothing Splines and OLS) zeta estimates
#' exceeds \code{zeta_critical}, the percentage reduction in in the zeta estimate produced
#' by Smoothing Splines is necessary to justify recommending this model.
#'
#' @details
#' \strong{The Decision Logic:}
#' The engine follows a hierarchical decision tree for each comparison in \code{data}:
#' \enumerate{
#'   \item \strong{Transformation Evaluation:} It applies Identity, Log (natural), and Box-Cox (lambda=0.5) transformations to the data.
#'   \item \strong{Linear Model Check (Gatekeeper):} It calculates the Zeta statistic using Ordinary Least Squares (OLS) for all transformations.
#'   \item \strong{Branching:}
#'   \itemize{
#'     \item \emph{Path A (Success):} If any OLS Zeta is \eqn{\le} \code{zeta_critical}, the system restricts its selection to OLS models.
#'     \item \emph{Path B (Fallback):} If all OLS Zetas exceed the threshold, the system switches to Non-Weighted Smoothing Splines.
#'   }
#'   \item \strong{Scoring & Selection:} The surviving models are ranked using a composite scoring system:
#'   \itemize{
#'     \item \emph{Absolute Difference:} \eqn{| \zeta - 1 |}
#'     \item \emph{Relative Difference:} Penalizes deviations from the benchmark.
#'     \item \emph{Dynamic Score:} A custom continuous function \eqn{f(x, b, L)} that is convex between the cutoff \eqn{L} (0.6) and benchmark \eqn{b} (1), and decays rapidly (concave) for values > \eqn{b}.
#'   }
#' }
#'
#' \strong{Tie-Breaking:}
#' If metrics disagree, the \strong{Dynamic Score} is prioritized. If a transformation shows no meaningful
#' improvement over Identity (improvement score \eqn{\le 0}), the system reverts to the Identity transformation.
#'
#' @return A named list where each element corresponds to a unique \code{comparison} from the input \code{data}.
#' Each element contains:
#' \itemize{
#'   \item \code{comparison}: The comparison ID.
#'   \item \code{model_type}: The selected model class ("OLS" or "SmoothingSpline").
#'   \item \code{best_transformation}: The internal code for the optimal transformation (e.g., "log#e", "identity").
#'   \item \code{reason}: A descriptive string explaining the logic behind the selection (e.g., "Consensus among all score metrics").
#'   \item \code{zeta}: The Zeta statistic of the selected model/transformation.
#'   \item \code{linear_success}: Logical. \code{TRUE} if OLS was sufficient, \code{FALSE} if fallback to Spline was required.
#' }
#'
#' @export
recommendation_engine <- function(data, zeta_critical = 4.24, parsimony_threshold = 0.20) {

  # --- Local Constants ---
  transformations <- c(
    "identity" = "identity",
    "log" = "log#e",
    "boxcox" = "boxcox#0.5"
  )

  # --- Local functions ---

  # Dynamic Scoring Function
  dynamic_zeta_score <- function(zetas, benchmark = 1, zeta_bad_upper = 0.6, zeta_critical, score_at_zeta_bad_upper = 5) {

    if (zeta_bad_upper >= benchmark) {
      stop("zeta_bad_upper (cutoff) must be strictly less than the benchmark.")
    }
    # Using pmax to avoid negative inputs in log
    alpha_calc_zeta <- pmax(zeta_bad_upper, 1e-9)

    # Formula: score_at_zeta_bad_upper = 100 * (zeta_bad_upper / benchmark) ^ y
    y <- log(score_at_zeta_bad_upper / 100) / log(alpha_calc_zeta / benchmark)

    scores <- numeric(length(zetas))

    # Masking
    mask_left  <- !is.na(zetas) & (zetas <= benchmark)
    mask_right <- !is.na(zetas) & (zetas > benchmark)

    # Part 1: Left Side (Growth)
    if (any(mask_left)) {
      val_left <- pmax(zetas[mask_left], 1e-9)
      scores[mask_left] <- 100 * (val_left / benchmark) ^ y
    }

    # Part 2: Right Side (Decay)
    # Note: Using zeta_critical as the 'c' offset parameter equivalent
    if (any(mask_right)) {
      val_right <- zetas[mask_right]
      scores[mask_right] <- 100 / (1 + 9 * ((val_right - benchmark) / (zeta_critical - benchmark)) ^ 2)
    }

    scores[is.na(zetas)] <- NA
    return(scores)
  }

  # Score Aggregation Function
  zeta_scores <- function(zetas, transformations_names, benchmark = 1, zeta_critical, zeta_bad_upper = 0.6) {

    score_absolute_diff <- abs(zetas - benchmark)
    score_relative_diff <- pmax(1 / abs((zetas - benchmark) / benchmark) * 10, 100)
    score_dynamic <- dynamic_zeta_score(
      zetas = zetas,
      benchmark = benchmark,
      zeta_bad_upper = zeta_bad_upper,
      zeta_critical = zeta_critical,
      score_at_zeta_bad_upper = 5
    )

    impr_score_absolute_diff <- score_absolute_diff - score_absolute_diff[1]
    impr_score_relative_diff <- score_relative_diff - score_relative_diff[1]
    impr_score_dynamic <- score_dynamic - score_dynamic[1]

    # Best identifiers
    # Note: For absolute diff, MIN is best. For others, MAX is best.
    output_best <- c(
      "best_score_absolute_diff" = transformations_names[which.min(score_absolute_diff)],
      "best_score_relative_diff" = transformations_names[which.max(score_relative_diff)],
      "best_score_dynamic" = transformations_names[which.max(score_dynamic)],
      "best_impr_score_absolute_diff" = transformations_names[which.max(impr_score_absolute_diff)],
      "best_impr_score_relative_diff" = transformations_names[which.max(impr_score_relative_diff)],
      "best_impr_score_dynamic" = transformations_names[which.max(impr_score_dynamic)]
    )

    # Store raw values for decision logic
    raw_scores <- list(
      "dynamic" = score_dynamic,
      "impr_dynamic" = impr_score_dynamic
    )

    output_details <- sapply(seq_along(zetas), function(i) {
      list(
        "transformation" = transformations_names[i],
        "zeta" = zetas[i],
        "score_dynamic" = score_dynamic[i],
        "impr_dynamic" = impr_score_dynamic[i]
      )
    },
    simplify = FALSE)

    return(list("details" = output_details, "best" = output_best, "raw" = raw_scores))
  }

  # Decision Logic Helper (To avoid repeating code in OLS and SS blocks)
  decide_outcome <- function(scores_output, transformation_map) {

    scores_best <- scores_output$best
    raw_dynamic <- scores_output$raw$dynamic
    raw_impr <- scores_output$raw$impr_dynamic
    names(raw_dynamic) <- names(transformation_map)
    names(raw_impr) <- names(transformation_map)

    # Extract the 'best' suggestions from the different metrics
    # We focus on the indices 1:3 (Absolute, Relative, Dynamic)
    best_candidates <- unique(scores_best[1:3])

    selected_transformation <- NULL
    reason <- NULL

    # CASE 1: Consensus (All scores point to the same transformation)
    if (length(best_candidates) == 1) {
      selected_transformation <- best_candidates
      reason <- "Consensus among all score metrics."

      # CASE 2: Disagreement (Tie-breaking)
    } else {
      # Prefer Dynamic Score as per instructions
      selected_transformation <- scores_best["best_score_dynamic"]
      reason <- "Metrics disagreed; Dynamic Score prioritized."
    }

    # CHECK: Is the improvement meaningful?
    # If the selected transformation is NOT identity, we check if it actually improved things.
    if (selected_transformation != "identity") {

      # Check the dynamic improvement score for the selected transformation
      # We map the name back to the index or name in the raw vector
      imp_val <- raw_impr[selected_transformation]

      # If improvement is non-positive (or negligible), revert to identity
      if (imp_val <= 0.05) {
        reason <- paste(reason, "However, improvement score was <= 5 %, reverting to identity.")
        selected_transformation <- "identity"
      } else {
        reason <- paste(reason, sprintf("Meaningful improvement found (Score +%.2f).", imp_val))
      }
    } else {
      reason <- paste(reason, "Identity was the best fit.")
    }

    return(list(
      "selected_transformation" = unname(transformation_map[selected_transformation]),
      "selected_transformation_name" = selected_transformation,
      "reason" = reason,
      "scores" = scores_output$best
    ))
  }

  # --- Data Preparation ---

  # Create copies to avoid modifying original data.table in place by reference
  data_identity <- commutability::transform_data(data.table::copy(data), transformations["identity"])
  data_log      <- commutability::transform_data(data.table::copy(data), transformations["log"])
  data_boxcox   <- commutability::transform_data(data.table::copy(data), transformations["boxcox"])

  # Split into lists
  data_identity_list <- split(data_identity, by = "comparison")
  data_log_list      <- split(data_log, by = "comparison")
  data_boxcox_list   <- split(data_boxcox, by = "comparison")

  # --- The Expert System Loop ---
  optimal_transformations <- sapply(
    seq_along(data_identity_list),
    function(comparisonID) {

      # 1. Run OLS (Always)
      zetas_ols <- c(
        estimate_zeta_ols(data_identity_list[[comparisonID]])$zeta,
        estimate_zeta_ols(data_log_list[[comparisonID]])$zeta,
        estimate_zeta_ols(data_boxcox_list[[comparisonID]])$zeta
      )

      # Calculate OLS Scores
      scores_ols <- zeta_scores(
        zetas = zetas_ols,
        transformations_names = names(transformations),
        benchmark = 1,
        zeta_critical = zeta_critical
      )
      decision_ols <- decide_outcome(scores_ols, transformations)

      # check if OLS is valid
      ols_is_valid <- any(zetas_ols <= zeta_critical, na.rm = TRUE)

      # Extract best zeta value based on the OLS model
      best_zeta_ols <- zetas_ols[which(
        x = names(transformations) == decision_ols$selected_transformation_name
      )]

      # --- LOGIC BRANCH 1: OLS IS GOOD ENOUGH ---
      # If OLS fits the criteria, we generally stop.
      # (Usually we stick to OLS if valid).
      if (ols_is_valid) {
        return(list(
          comparison = names(data_identity_list)[comparisonID],
          model_type = "OLS",
          best_transformation = decision_ols$selected_transformation,
          reason = "OLS met criteria (Parsimony).",
          zeta = best_zeta_ols
        ))
      }

      # --- LOGIC BRANCH 2: OLS FAILED -> CHECK SS ---
      # We must calculate SS to see if it improves things
      zetas_ss <- c(
        estimate_zeta_ss(data_identity_list[[comparisonID]], df=NULL, df_max=7.5, weighted=FALSE, mor=FALSE, na_rm=TRUE)$zeta,
        estimate_zeta_ss(data_log_list[[comparisonID]],      df=NULL, df_max=7.5, weighted=FALSE, mor=FALSE, na_rm=TRUE)$zeta,
        estimate_zeta_ss(data_boxcox_list[[comparisonID]],   df=NULL, df_max=7.5, weighted=FALSE, mor=FALSE, na_rm=TRUE)$zeta
      )

      scores_ss <- zeta_scores(
        zetas = zetas_ss,
        transformations_names = names(transformations),
        benchmark = 1, zeta_critical = zeta_critical
      )

      decision_ss <- decide_outcome(scores_ss, transformations)

      # check if SS is valid
      ss_is_valid <- any(zetas_ss <= zeta_critical, na.rm = TRUE)

      # Extract best zeta value based on the OLS model
      best_zeta_ss <- zetas_ss[which(
        x = names(transformations) == decision_ss$selected_transformation_name
      )]

      # --- COMPARISON LOGIC ---

      # 1. SS Rescued the dataset (OLS Fail, SS Pass)
      if (ss_is_valid) {
        return(list(
          comparison = names(data_identity_list)[comparisonID],
          model_type = "SmoothingSpline",
          best_transformation = decision_ss$selected_transformation,
          reason = "Excessive DINS according to the ordinary least squares model; Acceptable DINS according to the Smoothing Spline model.",
          zeta = best_zeta_ss
        ))
      }

      # 2. Both Failed -> Check Relative Improvement
      # Calculate distances from 1
      dist_ols <- abs(best_zeta_ols - 1)
      dist_ss  <- abs(best_zeta_ss - 1)

      # Improvement %
      improvement <- (dist_ols - dist_ss) / dist_ols

      if (improvement >= parsimony_threshold) {
        return(list(
          comparison = names(data_identity_list)[comparisonID],
          model_type = "SmoothingSpline",
          best_transformation = decision_ss$selected_transformation,
          reason = sprintf("Excessive DINS according to both Ordinary Least Squares and Smoothing Splines. However the Smoothing Spline improved the DINS accuracy by %.1f%% (Accuracy improvement of %.0f%% is considered meaningful in this context).", improvement*100, parsimony_threshold*100),
          zeta = best_zeta_ss
        ))
      } else {
        return(list(
          comparison = names(data_identity_list)[comparisonID],
          model_type = "OLS",
          best_transformation = decision_ols$selected_transformation,
          reason = sprintf("Excessive DINS according to both Ordinary Least Squares and Smoothing Splines. The DINS estimate reduction (%.1f%%) insufficient to justify the added complexity that comes with the Smoothing Spline Model.", improvement*100),
          zeta = best_zeta_ols
        ))
      }
    },
    simplify = FALSE
  )

  return(optimal_transformations)
}

#' Step 1: Pre-calculate Zetas for all models (Expensive Operation)
#' @export
calculate_candidate_zetas <- function(data) {

  # --- Constants ---
  transformations <- c(
    "identity" = "identity",
    "log" = "log#e",
    "boxcox" = "boxcox#0.5"
  )

  # --- Data Preparation (Vectorized) ---
  data_identity <- commutability::transform_data(data.table::copy(data), transformations["identity"])
  data_log      <- commutability::transform_data(data.table::copy(data), transformations["log"])
  data_boxcox   <- commutability::transform_data(data.table::copy(data), transformations["boxcox"])

  # Split into lists
  data_identity_list <- split(data_identity, by = "comparison")
  data_log_list      <- split(data_log, by = "comparison")
  data_boxcox_list   <- split(data_boxcox, by = "comparison")

  # --- Fit Models & Extract Zetas ---
  results <- lapply(names(data_identity_list), function(comp_name) {

    # 1. OLS Zetas
    z_ols <- c(
      identity = fasteqa::estimate_zeta_ols(data_identity_list[[comp_name]])$zeta,
      log      = fasteqa::estimate_zeta_ols(data_log_list[[comp_name]])$zeta,
      boxcox   = fasteqa::estimate_zeta_ols(data_boxcox_list[[comp_name]])$zeta
    )

    # 2. Smoothing Spline Zetas (Non-weighted as per original logic)
    # Using tryCatch to be safe during batch processing
    safe_ss <- function(d) {
      tryCatch(
        smooth.commutability::estimate_zeta_ss(d, df=NULL, df_max=7.5, weighted=FALSE, mor=FALSE, na_rm=TRUE)$zeta,
        error = function(e) NA_real_
      )
    }

    z_ss <- c(
      identity = safe_ss(data_identity_list[[comp_name]]),
      log      = safe_ss(data_log_list[[comp_name]]),
      boxcox   = safe_ss(data_boxcox_list[[comp_name]])
    )

    list(comparison = comp_name, ols = z_ols, ss = z_ss)
  })

  names(results) <- names(data_identity_list)
  return(results)
}

#' Step 2: Select Best Model based on Thresholds (Fast Operation)
#' @export
select_best_model <- function(candidate_results, zeta_critical = 4.24, parsimony_threshold = 0.20) {

  # --- Helper Functions (Internal to Logic) ---
  transformations_map <- c("identity" = "identity", "log" = "log#e", "boxcox" = "boxcox#0.5")

  # Dynamic Scoring
  dynamic_zeta_score <- function(zetas, benchmark = 1, zeta_bad_upper = 0.6, z_crit, score_at_bad = 5) {
    if (zeta_bad_upper >= benchmark) stop("Invalid bad_upper")
    alpha <- pmax(zeta_bad_upper, 1e-9)
    y <- log(score_at_bad / 100) / log(alpha / benchmark)

    scores <- numeric(length(zetas))
    mask_left  <- !is.na(zetas) & (zetas <= benchmark)
    mask_right <- !is.na(zetas) & (zetas > benchmark)

    if (any(mask_left)) scores[mask_left] <- 100 * (pmax(zetas[mask_left], 1e-9) / benchmark) ^ y
    if (any(mask_right)) scores[mask_right] <- 100 / (1 + 9 * ((zetas[mask_right] - benchmark) / (z_crit - benchmark)) ^ 2)
    scores[is.na(zetas)] <- NA
    return(scores)
  }

  # Score Aggregator
  get_scores <- function(zetas) {
    score_dynamic <- dynamic_zeta_score(zetas, z_crit = zeta_critical)
    score_abs <- abs(zetas - 1)
    score_rel <- pmax(1 / abs((zetas - 1)) * 10, 100)

    # Identify winners for each metric
    best_idx <- c(
      which.min(score_abs),
      which.max(score_rel),
      which.max(score_dynamic)
    )

    names <- c("identity", "log", "boxcox")
    best_candidates <- names[unique(best_idx)]

    # Decision
    if (length(best_candidates) == 1) {
      return(list(best = best_candidates, reason = "Consensus among metrics.", score_imp = 0))
    }

    # Tie-break: Prioritize Dynamic Score
    winner <- names[which.max(score_dynamic)]

    # Check Improvement over Identity
    imp <- if(winner != "identity") score_dynamic[which(names==winner)] - score_dynamic[1] else 0

    if (winner != "identity" && imp <= 0.05) {
      return(list(best = "identity", reason = "Improvement negligible.", score_imp = imp))
    }

    return(list(best = winner, reason = "Dynamic Score prioritized.", score_imp = imp))
  }

  # --- Main Decision Loop ---
  decisions <- lapply(candidate_results, function(res) {

    # 1. Evaluate OLS
    ols_decision <- get_scores(res$ols)
    best_zeta_ols <- res$ols[[ols_decision$best]]

    # GATEKEEPER: If OLS works, take it
    if (!is.na(best_zeta_ols) && best_zeta_ols <= zeta_critical) {
      return(list(
        comparison = res$comparison,
        model_type = "OLS",
        best_transformation = transformations_map[[ols_decision$best]],
        reason = "OLS met criteria (Parsimony).",
        zeta = best_zeta_ols
      ))
    }

    # 2. Evaluate SS (Fallback)
    ss_decision <- get_scores(res$ss)
    best_zeta_ss <- res$ss[[ss_decision$best]]

    # Check SS Validity
    ss_valid <- !is.na(best_zeta_ss) && best_zeta_ss <= zeta_critical

    if (ss_valid) {
      return(list(
        comparison = res$comparison,
        model_type = "SmoothingSpline",
        best_transformation = transformations_map[[ss_decision$best]],
        reason = "OLS failed; Smoothing Spline accepted.",
        zeta = best_zeta_ss
      ))
    }

    # 3. Both Failed: Check Relative Improvement
    dist_ols <- abs(best_zeta_ols - 1)
    dist_ss <- abs(best_zeta_ss - 1)

    improvement <- (dist_ols - dist_ss) / dist_ols

    if (!is.na(improvement) && improvement >= parsimony_threshold) {
      return(list(
        comparison = res$comparison,
        model_type = "SmoothingSpline",
        best_transformation = transformations_map[[ss_decision$best]],
        reason = sprintf("Both failed, but SS improved accuracy by %.1f%%.", improvement*100),
        zeta = best_zeta_ss
      ))
    } else {
      return(list(
        comparison = res$comparison,
        model_type = "OLS",
        best_transformation = transformations_map[[ols_decision$best]],
        reason = "Both failed; SS improvement insufficient.",
        zeta = best_zeta_ols
      ))
    }
  })

  return(decisions)
}
