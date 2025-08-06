#' Generates the HTML content for the help panel on the 'Upload data' page.
#'
#' @return An HTML string.
#' @noRd
help_button_page_1_text <- function() {

  # Define a modern color palette consistent with the app's theme
  colors <- list(
    primary = "#28A745",     # Main green from your theme
    heading = "#605CA8",     # Main purple from your theme
    text = "#555555",        # Soft black for readability
    accent = "#e9ecef",      # Light grey for backgrounds/borders
    success = "#28A745",     # Green for positive indicators
    warning = "#FEAB3A",     # Yellow for warnings
    danger = "#dc3545"       # Red for errors
  )

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Get Started</h5>",
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Upload Clinical Sample Data:</b> Click the first 'Browse...' button to select your clinical sample data file. This application supports both <b style='color:", colors$primary, ";'>.xlsx</b> and <b style='color:", colors$primary, ";'>.csv</b> files.</div>",
    "</div>",
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Upload EQA Material Data:</b> Click the second 'Browse...' button to select your External Quality Assessment (EQA) data file. This also supports <b style='color:", colors$primary, ";'>.xlsx</b> and <b style='color:", colors$primary, ";'>.csv</b> formats.</div>",
    "</div>",
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Review Diagnostics:</b> Once uploaded, the application will automatically run diagnostic checks. If all checks pass, you can proceed to the other sections.</div>",
    "</div>",
    "</div>"
  )

  # Additional Options Section
  additional_options_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-hammer'></i>Additional Options</h5>",
    "<p style='color:", colors$text, "; line-height: 1.6; margin-bottom: 15px;'>",
    "After uploading your data, you have two additional options to configure your analysis:",
    "</p>",
    "<div class='help-info-box' style='border-left-color: ", colors$heading, ";'>",
    "<h6 style='font-weight: 600; margin-bottom: 10px;'>Choose a Reference Method</h6>",
    "<p style='color:", colors$text, ";'>This is an <b>optional</b> step. If you designate one of your IVD-MDs as a reference method, all other methods will be compared against it. If you leave it as 'none', the application will perform all possible pairwise comparisons between your methods.</p>",
    "</div>",
    "<div class='help-info-box' style='border-left-color: ", colors$danger, "; margin-top: 15px;'>",
    "<h6 style='font-weight: 600; margin-bottom: 10px;'>Ignore Failed Validation Tests</h6>",
    "<p style='color:", colors$text, ";'>This is a high-risk option that is <b>not recommended</b>. Forcing the application to proceed with data that has failed validation may lead to unreliable results or cause the application to crash. Only use this if you are certain you understand the risks.</p>",
    "</div>",
    "</div>"
  )

  # Interpretation Section
  interpretation_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-magnifying-glass-chart'></i>Interpreting the Diagnostics</h5>",
    "<p style='color:", colors$text, "; line-height: 1.6;'>",
    "For successful validation, your datasets must contain specific identifier columns. The primary identifiers are <span class='code-text'>SampleID</span> and <span class='code-text'>ReplicateID</span>. Using these names is highly recommended. For a list of alternative names, please see the ",
    "<a href='https://github.com/pernille267/commutability/blob/master/R/typo_suggestions.R' target='_blank' style='color:", colors$heading, "; font-weight: 600;'>official documentation</a>.",
    "</p>",

    # Quality Indicators
    "<div class='help-info-box'>",
    "<h6 style='font-weight: 600; margin-bottom: 15px;'>Data Quality Indicators</h6>",
    "<div class='help-info-item'><i class='fa-solid fa-circle' style='color: #7851a9;'></i><div><b style='color: #7851a9;'>Purple:</b> Perfect quality. No issues found.</div></div>",
    "<div class='help-info-item'><i class='fa-solid fa-circle' style='color: ", colors$success, ";'></i><div><b style='color: ", colors$success, ";'>Green:</b> Acceptable quality. Suitable for analysis.</div></div>",
    "<div class='help-info-item'><i class='fa-solid fa-circle' style='color: ", colors$warning, ";'></i><div><b style='color: ", colors$warning, ";'>Yellow:</b> Questionable quality. Review data before proceeding.</div></div>",
    "<div class='help-info-item'><i class='fa-solid fa-circle' style='color: ", colors$danger, ";'></i><div><b style='color: ", colors$danger, ";'>Red:</b> Unacceptable quality. Analysis will be blocked.</div></div>",
    "</div>",

    # Validation Outcomes
    "<div class='help-info-box' style='border-left-color: ", colors$heading, ";'>",
    "<h6 style='font-weight: 600; margin-bottom: 15px;'>Validation Test Outcomes</h6>",
    "<div class='help-info-item'><i class='fa-solid fa-check-circle' style='color: ", colors$success, ";'></i><div>A green check indicates a test has passed.</div></div>",
    "<div class='help-info-item'><i class='fa-solid fa-times-circle' style='color: ", colors$danger, ";'></i><div>A red cross indicates a test has failed.</div></div>",
    "<div class='help-info-item'><i class='fa-solid fa-skull-crossbones' style='color: #343a40;'></i><div>A skull indicates a test could not be performed due to invalid data.</div></div>",
    "</div>",

    "<p style='color:", colors$text, "; line-height: 1.6; margin-top: 20px;'>",
    "Finally, the <b>Structural Agreement</b> section verifies that both datasets have matching column names and order. All three of these structural tests must pass to continue.",
    "</p>",
    "</div>"
  )

  # Combine all sections into a single container
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    instructions_section,
    additional_options_section,
    interpretation_section,
    "</div>"
  )

  return(help_text)
}
#' Generates the HTML content for the help panel on the 'User Input' page.
#'
#' @return An HTML string.
#' @noRd
help_button_page_2_text <- function() {

  # Define a modern color palette consistent with the app's theme
  colors <- list(
    primary = "#28A745",      # Main green from your theme
    heading = "#605CA8",      # Main purple from your theme
    text = "#555555",         # Soft black for readability
    accent = "#e9ecef",       # Light grey for backgrounds/borders
    warning = "#FEAB3A"       # Yellow for warnings
  )

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-sliders'></i>Analysis Parameter Guide</h5>",

    # Step 1: Transformation
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Data Transformation:</b> Select a transformation for your data. This can help stabilize variance and meet model assumptions.",
    "<div class='help-detail'>Try different options to see which yields zeta values closest to 1.</div>",
    "</div></div>",

    # Step 2: Prediction Interval
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Prediction Interval Method:</b> Choose the statistical approach for calculating prediction intervals.",
    "<div class='help-detail'>The <b>Deming</b> approach is generally recommended as it accounts for differences in non-selectivity.</div>",
    "</div></div>",

    # Step 3: PI Confidence
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Prediction Interval Confidence:</b> Specify the confidence level for the prediction intervals.",
    "<div class='help-detail'>A confidence level of <b>99%</b> is suggested for robust analysis.</div>",
    "</div></div>",

    # Step 4: Bootstrap Replicates (Conclusion Strength)
    "<div class='help-step'>",
    "<div class='help-step-number'>4</div>",
    "<div class='help-step-content'><b>Bootstrap Replicates (Conclusion Strength):</b> Define the number of bootstrap replicates for calculating the strength of the commutability conclusion.",
    "<div class='help-detail'>A higher number increases precision but takes longer. <b>1,000</b> replicates should provide stable estimates.</div>",
    "</div></div>",

    # Step 5: Bootstrap Method
    "<div class='help-step'>",
    "<div class='help-step-number'>5</div>",
    "<div class='help-step-content'><b>Bootstrap Method (Confidence Intervals):</b> Determine the method for constructing bootstrap confidence intervals for zeta values and imprecision parameters.</div>",
    "</div>",

    # Step 6: CI Confidence
    "<div class='help-step'>",
    "<div class='help-step-number'>6</div>",
    "<div class='help-step-content'><b>Bootstrap Confidence Level:</b> Set the confidence level for the estimated bootstrap confidence intervals.</div>",
    "</div>",

    # Step 7: Bootstrap Replicates (CI)
    "<div class='help-step'>",
    "<div class='help-step-number'>7</div>",
    "<div class='help-step-content'><b>Bootstrap Replicates (Confidence Intervals):</b> Input the number of bootstrap replicates for constructing the confidence intervals.",
    "<div class='help-detail'><b>2,000</b> replicates should be sufficient for stable confidence interval estimates.</div>",
    "</div></div>",

    # Step 8: M Value
    "<div class='help-step'>",
    "<div class='help-step-number'>8</div>",
    "<div class='help-step-content'><b>Set Non-Selectivity Tolerance (M):</b> This value represents the average relative increase in the prediction interval's length you are willing to tolerate due to non-selectivity.",
    "<div class='help-detail'>A larger M means accepting more non-selectivity differences; a smaller M means accepting fewer.</div>",
    "</div></div>",

    # Step 9: Zeta Value
    "<div class='help-step'>",
    "<div class='help-step-number'>9</div>",
    "<div class='help-step-content'><b>Set Critical Zeta:</b> Choose the critical value of zeta. A zeta value of 1 or less will initiate a simulation based on your selected M.",
    "<div class='help-detail'>It is recommended to select a critical value from a look-up table. The automatic look-up is most reliable when you have 20-35 clinical samples with 2-4 replicates each.</div>",
    "</div></div>",

    # Step 10: Reference Method
    "<div class='help-step'>",
    "<div class='help-step-number'>10</div>",
    "<div class='help-step-content'><b>Select a Reference Method (Optional):</b> If one of your IVD-MDs serves as a reference method, you can designate it here.",
    "<div class='help-detail'>Selecting a reference method will set it as the baseline (x-axis) for all comparisons in the subsequent analysis tabs. If left as 'none', all unique pairwise comparisons will be performed.</div>",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Important Note Section
  note_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-circle-info'></i>Important Note</h5>",
    "<div class='help-important-note'>",
    "The parameters you set in this section are crucial as they define the calculations for all subsequent analysis tabs.",
    "</div>",
    "</div>"
  )

  # Combine all sections into a single container
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    instructions_section,
    note_section,
    "</div>"
  )

  return(help_text)
}

#' Generates the HTML content for the help panel on the 'Outlier Analysis' page.
#'
#' @return An HTML string.
#' @noRd
help_button_page_3_text <- function() {

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Introduction Section
  intro_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-ruler'></i>Outlier Analysis Guide</h5>",
    "<p style='color: #555555; line-height: 1.6;'>",
    "This section is dedicated to identifying potential outliers within your <b>Clinical Sample Data</b>. Outliers are data points that deviate significantly from other observations and can unduly influence the results of your commutability analysis. By identifying them here, you can make an informed decision about whether to include or exclude them from subsequent steps.",
    "</p>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Perform the Analysis</h5>",

    # Step 1: Select Variable
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Select Variable:</b> Choose the metric you want to screen for outliers.",
    "<div class='help-detail'><b>Model Residuals:</b> Examines the errors from the regression model. This is often the most robust choice.</div>",
    "<div class='help-detail'><b>Absolute Difference:</b> Examines the raw difference between measurement pairs.</div>",
    "<div class='help-detail'><b>Log Difference:</b> Examines the difference on a logarithmic scale, which can be useful for data spanning several orders of magnitude.</div>",
    "</div></div>",

    # Step 2: Select Test
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Select Outlier Test:</b> Choose the statistical criterion for identifying outliers.",
    "<div class='help-detail'>Each test uses a different statistical method to determine if a data point is an outlier. <b>Burnett's Criterion</b> is a good starting point.</div>",
    "</div></div>",

    # Step 3: Confidence Level
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Select Confidence Level:</b> Set the statistical confidence for the test. This determines how extreme a point must be to be flagged as an outlier.",
    "<div class='help-detail'>A higher confidence level (e.g., 99%) makes the test stricter, meaning fewer points will be identified as outliers.</div>",
    "</div></div>",

    # Step 4: Analyze
    "<div class='help-step'>",
    "<div class='help-step-number'>4</div>",
    "<div class='help-step-content'><b>Run the Analysis:</b> Click the <b style='color: #605CA8;'>Analyze</b> button to perform the outlier detection based on your selections.",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Interpretation Section
  interpretation_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-magnifying-glass-chart'></i>Interpreting the Results</h5>",
    "<div class='help-important-note' style='background-color: #f0f0f8; border-left-color: #605CA8;'>",
    "The table below will populate with the results of the analysis. Any sample flagged as an <b style='color: #dc3545;'>outlier</b> will be highlighted. You can use the search and filter functions of the table to explore the results.",
    "<br><br><b>Important:</b> This tool only identifies potential outliers; it does not automatically remove them. If you wish to proceed with an analysis that excludes these outliers, you must manually remove them from your source file and <b>re-upload the cleaned dataset</b> in the 'Upload Data' tab.",
    "</div>",
    "</div>"
  )

  # Combine all sections into a single container
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    intro_section,
    instructions_section,
    interpretation_section,
    "</div>"
  )

  return(help_text)
}

#' Generates the HTML content for the help panel on the 'Model Validation' page,
#' specifically for the 'Formal Model Assessment' tab.
#'
#' @return An HTML string.
#' @noRd
help_button_page_4A_text <- function() {

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Introduction Section
  intro_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-clipboard-check'></i>Formal Model Assessment Guide</h5>",
    "<p style='color: #555555; line-height: 1.6;'>",
    "This section allows you to perform formal hypothesis tests to validate the assumptions of the Deming regression model. These tests provide statistical evidence to assess the model's suitability for your data.",
    "</p>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Perform the Tests</h5>",

    # Step 1: Significance Level
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Significance Level (alpha):</b> Choose the threshold for statistical significance for the assessment tests.",
    "<div class='help-detail'>A lower value (e.g., 1.0%) makes the test stricter.</div>",
    "</div></div>",

    # Step 2: Bootstrap Replicates
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Bootstrap Replicates:</b> Select the number of replicates for estimating the rejection rates of the tests, which relates to the strength of the conclusion.",
    "<div class='help-detail'><b>200</b> replicates should provide stable estimates without excessive computation time.</div>",
    "</div></div>",

    # Step 3: Simultaneous Testing
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Correction for Simultaneous Testing:</b> Choose whether to adjust p-values to account for multiple comparisons.",
    "<div class='help-detail'>This is recommended because performing one test for each IVD-MD comparison can increase the chance of false positives (Type I error).</div>",
    "</div></div>",

    # Step 4: Test Selection
    "<div class='help-step'>",
    "<div class='help-step-number'>4</div>",
    "<div class='help-step-content'><b>Tests to Perform:</b> Specify which model assumptions you want to test: normality, variance homogeneity (homoscedasticity), or both.",
    "</div></div>",

    # Step 5: Run Tests
    "<div class='help-step'>",
    "<div class='help-step-number'>5</div>",
    "<div class='help-step-content'><b>Run Tests:</b> Click the <b style='color: #605CA8;'>Run tests</b> button to execute the analysis.",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Combine all sections
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    intro_section,
    instructions_section,
    "</div>"
  )

  return(help_text)
}


#' Generates the HTML content for the help panel on the 'Model Validation' page,
#' specifically for the 'Model Assessment Plots' tab.
#'
#' @return An HTML string.
#' @noRd
help_button_page_4B_text <- function() {

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Introduction Section
  intro_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-chart-line'></i>Model Assessment Plots Guide</h5>",
    "<p style='color: #555555; line-height: 1.6;'>",
    "This section provides graphical tools for the informal, visual assessment of the Deming regression model assumptions. Plots can often reveal patterns, trends, or issues that are not obvious from formal tests alone.",
    "</p>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Generate Plots</h5>",

    # Step 1: Plot Type
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Choose Plot Type:</b> Select one of the five available diagnostic plots.",
    "<div class='help-detail'><b>Residuals vs Fitted:</b> Checks for constant variance and linearity.</div>",
    "<div class='help-detail'><b>Histogram of Residuals:</b> Checks if model errors are normally distributed.</div>",
    "<div class='help-detail'><b>Quantile-Quantile Plot:</b> Also checks if model errors are normally distributed.</div>",
    "<div class='help-detail'><b>SD vs Concentration:</b> Checks for homogeneity of measurement error variance.</div>",
    "<div class='help-detail'><b>CV vs Concentration:</b> Checks for homogeneity of the coefficient of variation.</div>",
    "</div></div>",

    # Step 2: Customization
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Customize Your Plot:</b> Use the options to add curves, change the color theme, customize titles and labels, and adjust the plot dimensions for the downloaded file.",
    "</div></div>",

    # Step 3: Plot and Download
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Generate and Save:</b> Click the <b style='color: #605CA8;'>Plot</b> button to generate the visual. Click it again to update after changing options. Use the <b style='color: #605CA8;'>Download</b> button to save the plot to your device.",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Combine all sections
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    intro_section,
    instructions_section,
    "</div>"
  )

  return(help_text)
}

#' Generates the HTML content for the help panel on the 'Results' page,
#' specifically for the 'Tables' tab.
#'
#' @return An HTML string.
#' @noRd
help_button_page_5A_text <- function() {

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Introduction Section
  intro_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-table-list'></i>Results Table Guide</h5>",
    "<p style='color: #555555; line-height: 1.6;'>",
    "This section presents the detailed numerical results of the commutability evaluation in an interactive table. Use the options below to filter the results and customize the table format before generating the output.",
    "</p>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Generate the Table</h5>",

    # Step 1: Filtering
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Apply Filters (Optional):</b> You can narrow down the results based on two criteria:",
    "<div class='help-detail'><b>Material Location:</b> Show only materials that fall inside or outside the calculated prediction intervals.</div>",
    "<div class='help-detail'><b>Non-selectivity:</b> Show only IVD-MD comparisons with acceptable or excessive differences in non-selectivity.</div>",
    "</div></div>",

    # Step 2: Format
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Choose Format:</b> Select how the data should be presented.",
    "<div class='help-detail'><b>Compact:</b> Consolidates related information (e.g., confidence intervals) into single columns for a concise view.</div>",
    "<div class='help-detail'><b>Expanded:</b> Presents every data point in its own dedicated column for a more detailed, wide-format table.</div>",
    "</div></div>",

    # Step 3: Calculate
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Generate the Table:</b> Click the <b style='color: #605CA8;'>Calculate</b> button to perform the analysis and display the results based on your selections.",
    "<div class='help-detail'>The table is interactive; you can sort, search, and export the data using the buttons above the table.</div>",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Combine all sections
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    intro_section,
    instructions_section,
    "</div>"
  )

  return(help_text)
}


#' Generates the HTML content for the help panel on the 'Results' page,
#' specifically for the 'Plots' tab.
#'
#' @return An HTML string.
#' @noRd
help_button_page_5B_text <- function() {

  # --- HTML Content ---

  # Tip Section
  tip_section <- paste0(
    "<div class='help-tip'>",
    "<i class='fa-solid fa-lightbulb'></i>",
    "<div>Click the 'Show Help Text' button again to hide this panel.</div>",
    "</div>"
  )

  # Introduction Section
  intro_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-chart-line'></i>Results Plot Guide</h5>",
    "<p style='color: #555555; line-height: 1.6;'>",
    "This section provides a powerful way to visualize the commutability evaluation results. Plots offer a complementary perspective to the numerical tables, making it easier to interpret relationships and identify patterns at a glance.",
    "</p>",
    "</div>"
  )

  # Instructions Section
  instructions_section <- paste0(
    "<div class='help-section'>",
    "<h5 class='help-header'><i class='fa-solid fa-list-check'></i>How to Customize and Generate Plots</h5>",

    # Step 1: Main Options
    "<div class='help-step'>",
    "<div class='help-step-number'>1</div>",
    "<div class='help-step-content'><b>Set Main Plotting Options:</b>",
    "<div class='help-detail'><b>Theme:</b> Choose from a palette of color schemes to suit your visual preference.</div>",
    "<div class='help-detail'><b>Axis Tick Density:</b> Adjust the number of ticks on the axes to enhance readability.</div>",
    "<div class='help-detail'><b>Pattern Curve:</b> Overlay different types of curves on the clinical sample data to highlight trends.</div>",
    "</div></div>",

    # Step 2: Customization
    "<div class='help-step'>",
    "<div class='help-step-number'>2</div>",
    "<div class='help-step-content'><b>Customize Labels and Dimensions:</b> You can provide custom titles for the plot and its axes. The width and height inputs control the dimensions of the plot when you download it.",
    "</div></div>",

    # Step 3: Exclusions & Download
    "<div class='help-step'>",
    "<div class='help-step-number'>3</div>",
    "<div class='help-step-content'><b>Fine-Tune and Prepare for Download:</b>",
    "<div class='help-detail'><b>Exclusions:</b> You can toggle the visibility of the clinical samples or the conclusion certainty rings for a cleaner plot.</div>",
    "<div class='help-detail'><b>Download Options:</b> Select the desired file format and resolution (DPI) for the saved image.</div>",
    "</div></div>",

    # Step 4: Generate
    "<div class='help-step'>",
    "<div class='help-step-number'>4</div>",
    "<div class='help-step-content'><b>Generate and Save:</b> Click the <b style='color: #605CA8;'>Plot</b> button to render the graph. Click it again to refresh after changing options. Use the <b style='color: #605CA8;'>Download</b> button to save the generated plot to your device.",
    "</div></div>",

    "</div>" # End of help-section
  )

  # Combine all sections
  help_text <- paste0(
    "<div class='help-container'>",
    tip_section,
    intro_section,
    instructions_section,
    "</div>"
  )

  return(help_text)
}

#' Render a styled diagnostic table
#' @param diagnostics The diagnostic list object from commutability::check_data()
#' @param type A character string, either "cs" for clinical sample or "eq" for EQA material
#' @return A kableExtra HTML table widget
render_diagnostic_table <- function(diagnostics, type = "cs") {
  current_badge <- diagnostics$badge
  current_score <- diagnostics$score
  current_IVD_MDs <- names(diagnostics$quality$number_of_NAs)

  output_tbl <- data.table(
    "Quality" = c(current_badge,
                  paste("score :", format(current_score, nsmall = 0), "/ 9"),
                  rep(NA_character_, length(current_IVD_MDs) - 2)),
    "IVD-MD" = names(diagnostics$quality$number_of_NAs),
    "Number of Samples" = diagnostics$quality$effective_number_of_samples,
    "Number of Replicates" = diagnostics$quality$average_number_of_replicates,
    "Invalid Data Fraction" = format(
      x = unlist(diagnostics$quality$fraction_of_NAs),
      nsmall = 2L,
      digits = 2L)
  )

  caption <- switch(
    current_badge,
    "perfect" = generate_quality_message(quality = "purple", sample_type = type),
    "acceptable" = generate_quality_message(quality = "green", sample_type = type),
    "questionable" = generate_quality_message(quality = "yellow", sample_type = type),
    "extremely poor" = generate_quality_message(quality = "red", sample_type = type)
  )

  badge_colors <- list(
    "perfect" = "#7851a9",
    "acceptable" = "#28A745",
    "questionable" = "#FEAB3A",
    "extremely poor" = "#B61F06"
  )
  background_color <- badge_colors[[current_badge]]

  kableExtra::kbl(x = output_tbl,
                  format = "html",
                  caption = caption,
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
    ) |>
    kableExtra::column_spec(
      column = 1,
      bold = TRUE,
      background = ifelse(current_badge == "extremely poor", "#FFF3CD", "#F8F9FA"),
      color = ifelse(current_badge == "extremely poor", "#856404", "#212529"),
      border_right = "1px solid #dee2e6",
      extra_css = "padding: 8px 12px;"
    ) |>
    kableExtra::column_spec(
      column = 5,
      color = kableExtra::cell_spec(
        unlist(diagnostics$quality$fraction_of_NAs),
        color = dplyr::case_when(
          unlist(diagnostics$quality$fraction_of_NAs) > 0.2 ~ "#d32f2f",
          unlist(diagnostics$quality$fraction_of_NAs) > 0 ~ "#ff9800",
          TRUE ~ "#2e7d32"
        ),
        bold = unlist(diagnostics$quality$fraction_of_NAs) > 0
      )
    ) |>
    kableExtra::scroll_box(width = "100%", height = "auto")
}

#' Render diagnostic test results as styled HTML text
#' @param diagnostics The diagnostic list object from commutability::check_data()
#' @param type A character string, either "cs" for clinical sample or "eq" for EQA material
#' @return An HTML object
render_diagnostic_text <- function(diagnostics, type = "cs") {

  data_type_label <- if (type == "cs") "Clinical Sample Data" else "Evaluated Material Data"

  css_unit_test <- function(title, fail = FALSE) {
    if (is.na(fail)) {
      icon_html <- "<i class='fa-solid fa-skull-crossbones' style='color: #FF9800; margin-right: 5px;'></i>"
      badge_html <- "<span class='test-badge unknown'>?</span>"
      class_name <- "test-item unknown-test"
    } else if (!fail) {
      icon_html <- "<i class='fa-solid fa-check-circle' style='color: #28a745; margin-right: 5px;'></i>"
      badge_html <- "<span class='test-badge pass'>PASS</span>"
      class_name <- "test-item pass-test"
    } else {
      icon_html <- "<i class='fa-solid fa-times-circle' style='color: #dc3545; margin-right: 5px;'></i>"
      badge_html <- "<span class='test-badge fail'>FAIL</span>"
      class_name <- "test-item fail-test"
    }
    paste0("<div class='", class_name, "'>", icon_html, "<span class='test-title'>", title, ":</span>", badge_html, "</div>")
  }

  message_mandatory_id_columns_test <- css_unit_test(
    title = paste("Test for Mandatory ID Columns in", data_type_label),
    fail = !diagnostics$validity$valid_mandatory_id_columns
  )
  message_numeric_columns_test <- css_unit_test(
    title = paste("Test for Valid IVD-MD Measurements in", data_type_label),
    fail = !diagnostics$validity$valid_numeric_columns
  )
  message_number_of_nas_test <- css_unit_test(
    title = "Test for Valid Number of Missing Values",
    fail = !diagnostics$validity$valid_number_nas
  )
  message_number_of_ivd_mds_test <- css_unit_test(
    title = "Test for Valid Number of IVD-MDs",
    fail = !diagnostics$validity$valid_number_remaining_numeric
  )

  repair_message_exclude_ivd_md <- if (is.null(diagnostics$repair$remove_these_methods)) {
    "<div class='repair-item success'><i class='fa-solid fa-check-circle' style='color: #28a745; margin-right: 5px;'></i><span class='repair-text'>No column exclusion performed in data repair.</span></div>"
  } else {
    paste0("<div class='repair-item warning'><i class='fa-solid fa-exclamation-triangle' style='color: #ffc107; margin-right: 5px;'></i><span class='repair-text'>", paste(diagnostics$repair$remove_these_methods, sep = ", "), " removed due to too much invalid data.</span></div>")
  }

  repair_message_convert_data <- if (is.null(diagnostics$repair$convert_these_methods_to_numeric)) {
    "<div class='repair-item success'><i class='fa-solid fa-check-circle' style='color: #28a745; margin-right: 5px;'></i><span class='repair-text'>No conversion of invalid values was necessary.</span></div>"
  } else {
    paste0("<div class='repair-item warning'><i class='fa-solid fa-exclamation-triangle' style='color: #ffc107; margin-right: 5px;'></i><span class='repair-text'>Attempted to convert ", paste(diagnostics$repair$convert_these_methods_to_numeric, sep = ", "), " because they were not recognized as numeric.</span></div>")
  }

  HTML(paste(
    "<div class='dashboard-container'>",
    "<div class='section-header'><i class='fa-solid fa-clipboard-list' style='color: #a5682a;'></i><span>Validation Tests for ", data_type_label, ":</span></div>",
    message_mandatory_id_columns_test,
    message_numeric_columns_test,
    message_number_of_nas_test,
    message_number_of_ivd_mds_test,
    "<div class='section-header' style='margin-top: 20px;'><i class='fa-solid fa-screwdriver-wrench' style='color: #5D5D5D;'></i><span>Repairing of ", data_type_label, ":</span></div>",
    repair_message_exclude_ivd_md,
    repair_message_convert_data,
    "</div>"
  ))
}

