#' Generate a quality message for the diagnostic table caption.
#'
#' @param quality A character string indicating the quality level ("purple", "green", "yellow", "red").
#' @param sample_type A character string, either "cs" or "eqam".
#' @param is_post_repair_valid A logical indicating if the data is valid for analysis *after* repair attempts.
#' @param post_repair_score An integer, the new quality score after a successful repair.
#'
#' @return A single, randomly selected character string message.
generate_quality_message <- function(quality = "purple", sample_type = "cs", is_post_repair_valid = TRUE, post_repair_score = NULL) {
  st <- ifelse(sample_type == "cs", "clinical sample", "EQA material")

  # --- Conditional Messages for 'Questionable' Quality ---
  yellow_messages <- if (is_post_repair_valid) {
    # Messages for when repair leads to a valid state
    c(
      sprintf("Initial quality of the %s data was questionable, but after automated repairs, the new score is %s/9.", st, post_repair_score),
      sprintf("Some quality issues were detected and addressed. The new quality score for the %s data is %s/9.", st, post_repair_score),
      sprintf("The %s data required minor repairs. The analysis will proceed with the corrected dataset, which has a score of %s/9.", st, post_repair_score),
      sprintf("The application has corrected inconsistencies in the %s data, resulting in a new score of %s/9.", st, post_repair_score),
      sprintf("After automated repairs, the %s data is now suitable for analysis with a score of %s/9.", st, post_repair_score)
    )
  } else {
    # Messages for when data is still invalid after repair attempt
    c(
      sprintf("This %s dataset has quality issues, and the analysis is blocked by other validation failures.", st),
      sprintf("The quality of this %s data is questionable, and critical errors in the overall data structure prevent analysis.", st),
      sprintf("While an attempt was made to repair this %s data, the overall dataset remains invalid.", st),
      sprintf("This %s data has minor issues, but a larger structural problem is preventing validation.", st),
      sprintf("Automated repairs on this %s data were insufficient to create a valid dataset for analysis.", st)
    )
  }

  # --- Conditional Messages for 'Extremely Poor' Quality ---
  red_messages <- if (is_post_repair_valid) {
    # Messages for when repair leads to a valid state
    c(
      sprintf("The original %s data was unsuitable, but after repair the quality score is now %s/9.", st, post_repair_score),
      sprintf("Critical issues were resolved by excluding methods. The new quality score for the %s data is %s/9.", st, post_repair_score),
      sprintf("The %s data required significant repairs. The analysis will proceed with the valid subset, which has a score of %s/9.", st, post_repair_score),
      sprintf("After removing invalid methods, the remaining %s data is now valid for analysis with a score of %s/9.", st, post_repair_score),
      sprintf("The application has excluded invalid columns to repair the %s dataset. The new score is %s/9.", st, post_repair_score)
    )
  } else {
    # Messages for when data is still invalid after repair attempt
    c(
      sprintf("This %s dataset contains critical errors that could not be automatically repaired.", st),
      sprintf("The quality of this %s data is insufficient for analysis, and automated repairs were not enough to resolve the issues.", st),
      sprintf("Analysis is blocked. This %s dataset has failed critical validation checks that could not be fixed automatically.", st),
      sprintf("This %s data is unsuitable for analysis. Please review the errors and re-upload.", st),
      sprintf("Due to the poor quality of this %s data, a valid analysis is not possible.", st)
    )
  }

  messages <- list(
    purple = c(
      sprintf("Impressive quality! This %s dataset is perfect for the analysis.", st),
      sprintf("Excellent work. This set of %s data is ideally suited for commutability evaluation.", st),
      sprintf("The %s data meets the highest standards for a robust commutability evaluation.", st),
      sprintf("This %s dataset demonstrates exceptional quality. Well done.", st),
      sprintf("Perfectly structured and complete. This %s data is ready to go.", st)
    ),
    green = c(
      sprintf("The %s dataset meets all necessary quality requirements.", st),
      sprintf("This %s data is acceptable for use in the commutability analysis.", st),
      sprintf("The %s data provides a sufficient basis for proceeding with the evaluation.", st),
      sprintf("Data quality for the %s set is adequate to support a reliable assessment.", st),
      sprintf("The submitted %s data is suitable for commutability evaluation.", st)
    ),
    yellow = yellow_messages,
    red = red_messages
  )

  # FIX: Map the public-facing badge name to the internal color/quality name
  quality_key <- switch(quality,
                        "perfect" = "purple",
                        "acceptable" = "green",
                        "questionable" = "yellow",
                        "extremely poor" = "red",
                        "not acceptable" = "red", # Map "not acceptable" to red messages
                        "green" # Default case
  )


  # Select the appropriate message list based on the mapped key
  selected_list <- messages[[quality_key]]

  # Sample one message randomly
  sample(selected_list, 1)
}
