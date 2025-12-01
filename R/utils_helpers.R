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

  badge_colors <- list(
    "perfect" = "#7851a9",
    "acceptable" = "#28A745",
    "questionable" = "#FEAB3A",
    "extremely poor" = "#B61F06"
  )
  score_color <- badge_colors[[current_badge]]
  if(is.null(score_color)) score_color <- "#6c757d"

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
    "Number of Replicates" = format(diagnostics$quality$average_number_of_replicates, digits=3),
    "Invalid %" = formatted_fractions
  )

  repaired_methods <- c(diagnostics$repair$remove_these_methods, diagnostics$repair$convert_these_methods_to_numeric)
  highlight_idx <- which(output_tbl[["IVD-MD"]] %in% repaired_methods)

  # --- 4. Render ---
  renderGlassTable(
    data = output_tbl,
    # Soft column names (Title Case)
    col_names = c("IVD-MD", "N Samples", "N Replicates", "Invalid %"),
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
      class = "glass-diag-container",
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
      class = "glass-diag-container",
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
      class = "glass-diag-container",
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
