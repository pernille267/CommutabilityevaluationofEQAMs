#' Perform Structural Agreement Checks
#'
#' This function encapsulates the complex logic of checking if Clinical Sample (CS)
#' and EQA Material (EQ) datasets are structurally equivalent. It handles:
#' 1. Initial repair attempts.
#' 2. Fallback logic for when datasets are broken due to specific invalid methods.
#' 3. Removing globally invalid methods before final equivalence checking.
#'
#' @param cs_data A data.table containing the raw clinical sample data.
#' @param eq_data A data.table containing the raw EQAM data.
#' @param methods_to_remove A character vector of method names to exclude (optional).
#'
#' @return A list containing the results of `commutability::check_equivalence` or a
#'         structured error list if validation fails.
#' @export
perform_structural_checks <- function(cs_data, eq_data, methods_to_remove = NULL) {

  # --- 1. Attempt Initial Repair ---
  # We try to repair without removing methods first to see if the data is fundamentally sound
  cs_data_repaired <- tryCatch(
    expr = {
      commutability::repair_data(
        data = cs_data,
        type = "cs",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
    },
    error = function(e) NULL,
    warning = function(e) NULL
  )

  eq_data_repaired <- tryCatch(
    expr = {
      commutability::repair_data(
        data = eq_data,
        type = "eqam",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
    },
    error = function(e) NULL,
    warning = function(e) NULL
  )

  # --- 2. Handle Broken Data (If Initial Repair Failed) ---
  if (is.null(cs_data_repaired) || is.null(eq_data_repaired)) {

    # If we have specific methods identified as invalid (from previous diagnostics),
    # we try to remove them and repair again.
    if (!is.null(methods_to_remove)) {

      # Check if the problematic methods actually exist in the data
      invalid_methods_in_cs <- all(methods_to_remove %in% names(cs_data))
      invalid_methods_in_eq <- all(methods_to_remove %in% names(eq_data))

      # FALLBACK 1: Invalid columns missing from one dataset
      if (!(invalid_methods_in_cs && invalid_methods_in_eq)) {
        return(list(
          "equal_names" = FALSE,
          "equal_order" = FALSE,
          "names_in_cs_data_but_not_in_eq_data" = setdiff(names(cs_data), names(eq_data)),
          "names_in_eq_data_but_not_in_cs_data" = setdiff(names(eq_data), names(cs_data)),
          "order_cs_data" = paste(names(cs_data), collapse = ", "),
          "order_eq_data" = paste(names(eq_data), collapse = ", "),
          "error" = "Tried to remove invalid columns, but they are not present in both datasets."
        ))
      }

      # Subset data to remove invalid methods
      keep_these_cs <- setdiff(names(cs_data), methods_to_remove)
      keep_these_eq <- setdiff(names(eq_data), methods_to_remove)

      cs_subset <- subset(cs_data, select = keep_these_cs)
      eq_subset <- subset(eq_data, select = keep_these_eq)

      # Attempt Second Repair on Cleaned Data
      cs_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = cs_subset,
            type = "cs",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        },
        error = function(e) NULL,
        warning = function(e) NULL
      )

      eq_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = eq_subset,
            type = "eqam",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        },
        error = function(e) NULL,
        warning = function(e) NULL
      )

      # FALLBACK 2: Second repair failed
      if (is.null(cs_data_repaired) || is.null(eq_data_repaired)) {
        return(list(
          "equal_names" = FALSE,
          "equal_order" = FALSE,
          "names_in_cs_data_but_not_in_eq_data" = setdiff(names(cs_data), names(eq_data)),
          "names_in_eq_data_but_not_in_cs_data" = setdiff(names(eq_data), names(cs_data)),
          "order_cs_data" = paste(names(cs_data), collapse = ", "),
          "order_eq_data" = paste(names(eq_data), collapse = ", "),
          "error" = "Data repair failed even after removing invalid methods."
        ))
      }

      # Attempt Equivalence Check on Repaired Data
      out <- tryCatch(
        expr = {
          commutability::check_equivalence(
            cs_data = cs_data_repaired,
            eq_data = eq_data_repaired
          )
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )

      # FALLBACK 3: Equivalence check crashed
      if (is.null(out)) {
        return(list(
          "equal_names" = FALSE,
          "equal_order" = FALSE,
          "names_in_cs_data_but_not_in_eq_data" = setdiff(names(cs_data), names(eq_data)),
          "names_in_eq_data_but_not_in_cs_data" = setdiff(names(eq_data), names(cs_data)),
          "order_cs_data" = paste(names(cs_data), collapse = ", "),
          "order_eq_data" = paste(names(eq_data), collapse = ", "),
          "error" = "Equivalence check failed after data repair."
        ))
      }

      return(out)
    }
  }

  # --- 3. Handle Standard Case (Initial Repair Succeeded) ---
  # If we have methods to remove, we must strip them from the repaired objects
  if (!is.null(methods_to_remove)) {
    keep_these_cs <- setdiff(names(cs_data_repaired), methods_to_remove)
    keep_these_eq <- setdiff(names(eq_data_repaired), methods_to_remove)

    cs_data_repaired <- subset(cs_data_repaired, select = keep_these_cs)
    eq_data_repaired <- subset(eq_data_repaired, select = keep_these_eq)
  }

  # Final check
  commutability::check_equivalence(
    cs_data = cs_data_repaired,
    eq_data = eq_data_repaired
  )
}

#' Perform a Data Check After Repair
#'
#' @param data A data.table containing the raw clinical sample or EQAM data.
#' @param prior_repair_diagnostics Output from \code{commutability::check_data()}
#'  based on the non-repaired data.
#' @param methods_to_remove A \code{character} vector or \code{NULL}. The methods
#'  that are deemed invalid in the clinical sample data, the EQAM data or both.
#' @param type A \code{character} string. Either \code{'cs'} or \code{'eqam'}
#'
#' @returns
#' The output from \code{commutability::check_data(repaired_data)} or
#' \code{prior_repair_diagnostics}, if \code{commutability::repair_data(data)}
#' fails.
#' @export
post_repair_diagnostics <- function(data, prior_repair_diagnostics, methods_to_remove = NULL, type = "cs") {

  # --- Attempt Initial Repair ---
  repaired_data <- tryCatch(
    expr = {
      commutability::repair_data(
        data = data,
        type = type
      )
    },
    error = function(e) "error",
    warning = function(w) "warning"
  )

  # --- Handle Broken Data (If Initial Repair Failed) ---
  if (is.character(repaired_data)) {
    # --- Return original diagnostics if repair resulted in error ---
    if (repaired_data == "error") {
      return(prior_repair_diagnostics)
    }
    else if (repaired_data == "warning") {
      # --- Warning occurs if data is too far from expected format ---
      # Note: Can still be repaired if invalid methods are the cause!
      if (prior_repair_diagnostics$badge == "not acceptable" && !is.null(methods_to_remove)) {
        temp_data <- subset(
          x = data,
          selected = setdiff(
            x = names(data),
            y = methods_to_remove
          )
        )
        # --- Try repairing data after removing invalid methods ---
        repaired_data <- tryCatch(
          expr = {
            commutability::repair_data(
              data = temp_data,
              type = type
            )
          },
          error = NULL,
          warning = NULL # Maybe we are able to catch the warning message??
        )

        # --- If repair was successful after removing invalid methods ---
        if (!is.null(repaired_data)) {
          return(
            commutability::check_data(
              data = repaired_data,
              type = type
            )
          )
        }
        # --- New repair was not successful, so return the prior diagnostics ---
        return(
          prior_repair_diagnostics
        )
      }
    }

    # We should not be able to come here
    return(
      prior_repair_diagnostics
    )
  }
  # --- First attempt to repair was successful ---
  return(
    commutability::repair_data(
      data = repaired_data,
      type = type
    )
  )
}
