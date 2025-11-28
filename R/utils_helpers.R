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

#' Render a styled diagnostic table
#' @param diagnostics The diagnostic list object from commutability::check_data()
#' @param type A character string, either "cs" for clinical sample or "eq" for EQA material
#' @return A kableExtra HTML table widget
render_diagnostic_table <- function(diagnostics, type = "cs", is_post_repair_valid = TRUE, post_repair_score = NULL) {
  current_badge <- diagnostics$badge
  current_score <- diagnostics$score
  current_IVD_MDs <- names(diagnostics$quality$number_of_NAs)

  badge_colors <- list(
    "perfect" = "#7851a9",
    "acceptable" = "#28A745",
    "questionable" = "#FEAB3A",
    "extremely poor" = "#B61F06"
  )

  score_color <- badge_colors[[current_badge]]

  # Create the styled HTML for the score circle
  score_circle_html <- if (!is.na(current_score)) {
    paste0(
      "<div style='background-color: ", score_color, ";",
      " color: white; border-radius: 50%; width: 25px; height: fit-content;",
      " display: flex; align-items: center; justify-content: center;",
      " font-weight: bold; margin: auto; font-size: 16px;",
      " border: 1.5px solid #333;'>", # Added a dark border
      current_score, # Just the score
      "</div>"
    )
  } else {
    "" # Empty string if score is NA
  }

  output_tbl <- data.table(
    "Quality" = c(tools::toTitleCase(current_badge),
                  score_circle_html,
                  rep(NA_character_, length(current_IVD_MDs) - 2)),
    "IVD-MD" = names(diagnostics$quality$number_of_NAs),
    "Number of Samples" = diagnostics$quality$effective_number_of_samples,
    "Number of Replicates" = diagnostics$quality$average_number_of_replicates,
    "Invalid Data Fraction" = format(
      x = unlist(diagnostics$quality$fraction_of_NAs),
      nsmall = 2L,
      digits = 2L)
  )

  # Identify rows that correspond to repaired methods
  repaired_methods <- c(diagnostics$repair$remove_these_methods, diagnostics$repair$convert_these_methods_to_numeric)
  rows_to_highlight <- which(output_tbl[["IVD-MD"]] %in% repaired_methods)

  # Get the default background color for the first column
  default_bg <- ifelse(current_badge == "extremely poor", "#FFF3CD", "#F8F9FA")

  # Update the call to generate_quality_message
  caption_text <- generate_quality_message(
    quality = current_badge,
    sample_type = type,
    is_post_repair_valid = is_post_repair_valid,
    post_repair_score = post_repair_score # Pass the new score here
  )

  # Define styles for the caption based on the badge
  style_colors <- list(
    "perfect" = c(bg = "#f4f0f8", text = "#7851a9", border = "#7851a9"),
    "acceptable" = c(bg = "#e9f5ea", text = "#28a745", border = "#28a745"),
    "questionable" = c(bg = "#fff8e1", text = "#ffab00", border = "#ffab00"),
    "extremely poor" = c(bg = "#fdecea", text = "#B61F06", border = "#B61F06")
  )
  current_style <- style_colors[[current_badge]]

  # Create the styled HTML caption
  styled_caption <- paste0(
    "<div style='",
    "background-color: ", current_style["bg"], "; ",
    "color: ", current_style["text"], "; ",
    "border-left: 5px solid ", current_style["border"], "; ",
    "padding: 12px 15px; ",
    "text-align: left; ",
    "font-weight: 600; ",
    "font-size: 14px;",
    "'>",
    caption_text,
    "</div>"
  )

  background_color <- badge_colors[[current_badge]]

  k <- kableExtra::kbl(x = output_tbl,
                       format = "html",
                       caption = styled_caption,
                       align = "c",
                       escape = FALSE) |>
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed"),
      full_width = TRUE,
      position = "center"
    ) |>
    kableExtra::row_spec(
      row = 0,
      background = background_color,
      color = "white",
      bold = TRUE,
      font_size = "16px",
      extra_css = "box-shadow: 0 2px 4px rgba(0,0,0,0.2); border-radius: 8px 8px 0 0;"
    )

  # Conditionally apply row highlighting
  if (length(rows_to_highlight) > 0) {
    k <- k |> kableExtra::row_spec(
      rows_to_highlight,
      background = "#FFF9E6",
      color = "#555555",
      bold = TRUE
    )
  }

  k <- k |>
    kableExtra::column_spec(
      column = 1,
      bold = TRUE,
      background = default_bg, # Use the determined default background
      color = ifelse(current_badge == "extremely poor", "#856404", "#212529"),
      border_right = "1px solid #dee2e6",
      extra_css = "padding: 8px 12px;"
    ) |>
    kableExtra::scroll_box(width = "100%", height = "auto")

  return(k)
}

#' Render diagnostic test results as styled HTML text
#' @param diagnostics The diagnostic list object
#' @param other_diagnostics The diagnostic list object from the other data type
#' @param type A character string, either "cs" or "eq"
#' @return An HTML object.
render_diagnostic_text <- function(diagnostics, other_diagnostics, type = "cs") {

  data_type_label <- if (type == "cs") "Clinical Sample Data" else "Evaluated Material Data"
  other_data_type_label <- if (type == "cs") "external quality assessment" else "clinical sample"

  # Helper: CSS Unit Test Item
  css_unit_test <- function(title, fail = FALSE) {
    if (is.na(fail)) {
      icon_html <- "<i class='fa fa-question-circle' style='color: #6c757d; margin-right: 8px;'></i>"
      badge_html <- "<span class='test-badge unknown'>?</span>"
      class_name <- "test-item unknown-test"
    } else if (!fail) {
      icon_html <- "<i class='fa fa-check-circle' style='color: #28a745; margin-right: 8px;'></i>"
      badge_html <- "<span class='test-badge pass'>PASS</span>"
      class_name <- "test-item pass-test"
    } else {
      icon_html <- "<i class='fa fa-times-circle' style='color: #dc3545; margin-right: 8px;'></i>"
      badge_html <- "<span class='test-badge fail'>FAIL</span>"
      class_name <- "test-item fail-test"
    }
    paste0("<div class='", class_name, "'>", icon_html, "<span class='test-title'>", title, "</span>", badge_html, "</div>")
  }

  # --- Generate Test Items ---
  message_mandatory_id_columns_test <- css_unit_test(
    title = paste("Mandatory ID Columns"),
    fail = !diagnostics$validity$valid_mandatory_id_columns
  )
  message_numeric_columns_test <- css_unit_test(
    title = paste("Valid IVD-MD Measurements"),
    fail = !diagnostics$validity$valid_numeric_columns
  )
  message_number_of_nas_test <- css_unit_test(
    title = "Valid Number of Missing Values",
    fail = !diagnostics$validity$valid_number_nas
  )
  message_number_of_ivd_mds_test <- css_unit_test(
    title = "Valid Number of IVD-MDs",
    fail = !diagnostics$validity$valid_number_remaining_numeric
  )

  # --- Repair Messages Logic ---
  methods_removed_locally <- diagnostics$repair$remove_these_methods
  methods_removed_from_other <- other_diagnostics$repair$remove_these_methods
  cross_removed_methods <- setdiff(methods_removed_from_other, methods_removed_locally)

  exclusion_messages <- c()

  if (!is.null(methods_removed_locally)) {
    bold_methods <- paste0("<b>", methods_removed_locally, "</b>", collapse = ", ")
    msg <- paste0("<div class='repair-item warning'><i class='fa fa-exclamation-triangle' style='color: #ffc107; margin-right: 8px;'></i><span class='repair-text'>", bold_methods, " removed due to too much invalid data.</span></div>")
    exclusion_messages <- c(exclusion_messages, msg)
  }

  if (length(cross_removed_methods) > 0) {
    bold_methods <- paste0("<b>", cross_removed_methods, "</b>", collapse = ", ")
    msg <- paste0("<div class='repair-item warning'><i class='fa fa-exclamation-triangle' style='color: #ffc107; margin-right: 8px;'></i><span class='repair-text'>", bold_methods, " removed due to issues in the ", other_data_type_label, " data.</span></div>")
    exclusion_messages <- c(exclusion_messages, msg)
  }

  if (length(exclusion_messages) == 0) {
    repair_message_exclude_ivd_md <- "<div class='repair-item success'><i class='fa fa-check-circle' style='color: #28a745; margin-right: 8px;'></i><span class='repair-text'>No column exclusion performed in data repair.</span></div>"
  } else {
    repair_message_exclude_ivd_md <- paste(exclusion_messages, collapse = "")
  }

  repair_message_convert_data <- if (is.null(diagnostics$repair$convert_these_methods_to_numeric)) {
    "<div class='repair-item success'><i class='fa fa-check-circle' style='color: #28a745; margin-right: 8px;'></i><span class='repair-text'>No conversion of invalid values was necessary.</span></div>"
  } else {
    bold_methods <- paste0("<b>", diagnostics$repair$convert_these_methods_to_numeric, "</b>", collapse = ", ")
    paste0("<div class='repair-item warning'><i class='fa fa-exclamation-triangle' style='color: #ffc107; margin-right: 8px;'></i><span class='repair-text'>Attempted to convert ", bold_methods, " because they were not recognized as numeric.</span></div>")
  }

  # --- RETURN HTML (Modernized Headers) ---
  # Note: I removed inline styles for colors/margins because the CSS class .section-header now handles it
  HTML(paste(

    # Header 1: Validation Tests
    "<div class='section-header'>",
    "<i class='fa fa-clipboard-list' style='color: #a5682a;'></i>", # Keep color, let CSS handle size/spacing
    "<span>Validation Tests for ", data_type_label, "</span>",
    "</div>",

    message_mandatory_id_columns_test,
    message_numeric_columns_test,
    message_number_of_nas_test,
    message_number_of_ivd_mds_test,

    # Header 2: Repairing
    "<div class='section-header'>",
    "<i class='fa fa-wrench' style='color: #605CA8;'></i>",
    "<span>Repairing of ", data_type_label, "</span>",
    "</div>",

    repair_message_exclude_ivd_md,
    repair_message_convert_data
  ))
}

