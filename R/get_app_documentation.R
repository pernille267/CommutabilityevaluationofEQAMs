
#' Application Documentation UI
#'
#' Generates the help documentation using glassHelp components.
#'
#' @export
get_app_documentation <- function() {
  glassHelpCard(
    
    # --- Section 1: Overview ---
    glassHelpSection(
      title = "Application Overview",
      glassHelpTip(
        text = "This application performs commutability evaluations of External Quality Assessment Materials (EQAMs) using the Difference in Non-Selectivity (DINS) approach.",
        icon = "info-circle"
      )
    ),

    # --- Section 2: Workflow Steps ---
    glassHelpSection(
      title = "Workflow Guide",

      # Step 1: Upload
      glassHelpStep(
        number = "1",
        title = "Data Upload",
        description = paste0(
          "Upload your Clinical Sample (CS) and EQAM data files.",
          " The application supports <b>.xlsx</b> files.",
          " Ensure your data follows the required template structure."
        )
      ),

      # Step 2: Parameters
      glassHelpStep(
        number = "2",
        title = "Differences in Nonselectivity (DINS)",
        description = paste0(
          "Select the appropriate <b>Prediction Interval (PI) Method</b> and <b>Data Transformation</b>.",
          " You can use the 'Recommendations' feature to help select the best statistical model."
        )
      ),
      
      # Step 3: Outliers
      glassHelpStep(
        number = "3",
        title = "Outlier Analysis",
        description = paste0(
          "Screen your clinical sample data for potential outliers.",
          " You can choose from various statistical tests (e.g., Grubbs, Rosner).",
          " Detected outliers can be temporarily excluded from the analysis."
        )
      ),

      # Step 4: Validation
      glassHelpStep(
        number = "4",
        title = "Model Validation",
        description = paste0(
          "Assess the validity of your chosen statistical model.",
          " Use visual plots (Residuals, Q-Q) and formal tests (Normality, Homoscedasticity) to ensure your model fits the data."
        )
      ),
      
      # Step 5: Results
      glassHelpStep(
        number = "5",
        title = "Results & Reports",
        description = paste0(
          "View the final Commutability Evaluation results.",
          " Results are presented in tables (Main & Material-Wise) and plots.",
          " You can download full PDF reports or export tables to Excel."
        )
      )
    ),

    # --- Section 3: Key Concepts ---
    glassHelpSection(
      title = "Key Concepts",
      glassHelpInfoBox(
        glassHelpInfoItem(
          label = "DINS", 
          description = "Difference in Non-Selectivity. The core metric used to assess commutability."
        ),
        glassHelpInfoItem(
          label = "EQAM", 
          description = "External Quality Assessment Material. The reference material being evaluated."
        ),
         glassHelpInfoItem(
          label = "Outlier", 
          description = "A data point that differs significantly from other observations."
        )
      )
    )
  )
}
