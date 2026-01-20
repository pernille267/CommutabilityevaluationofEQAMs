#' Format Point Estimate and Confidence Interval
#'
#' Formats a point estimate along with its lower and upper bounds into a single
#' string representation.
#'
#' This function ensures rigid alignment for monospaced outputs (like console
#' logs or text tables). It processes the Point, Lower, and Upper columns
#' independently. If a value is missing (NA), it is replaced by whitespace
#' of the same width as the widest number in that column.
#'
#' @param point Numeric vector. The point estimate value.
#' @param lower Numeric vector. The lower bound of the confidence interval.
#' @param upper Numeric vector. The upper bound of the confidence interval.
#' @param decimals Integer. The number of decimal places to round to. Defaults to 2.
#'
#' @return A character vector of formatted strings.
#'
#' @importFrom scales number
#' @export
#'
#' @examples
#' # Example with partial NA values
#' pt <- c(1.23, 8, 1.341)
#' lo <- c(1.0191, NA, 1.29121)
#' up <- c(1.9, 131.22, NA)
#'
#' # Output aligns columns, replacing NAs with spaces
#' format_point_plus_interval(pt, lo, up)
#' # Returns:
#' # "1.23 (1.02-  1.90)"
#' # "8.00 (    -131.22)"
#' # "1.34 (1.29-      )"
format_point_plus_interval <- function(point, lower, upper, decimals = 2L) {
  acc <- 1 / (10^decimals)

  # Helper function to format a single column and handle alignment
  align_col <- function(x) {
    # 1. Format numbers using scales (trim = TRUE removes extra padding initially)
    #    scales::number returns NA_character_ if input is NA
    formatted <- scales::number(
      x,
      accuracy = acc,
      big.mark = ",",
      decimal.mark = ".",
      trim = TRUE
    )
    # 2. Replace NA entries with empty strings
    #    We do this so base::format treats them as content of length 0
    formatted[is.na(formatted)] <- ""

    # 3. Apply alignment using base::format
    #    This pads the shorter strings (and the empty strings) with spaces
    #    to match the widest string in the vector.
    #    justify = "right" aligns numbers correctly.
    format(formatted, justify = "right")
  }

  # Apply alignment to each component independently
  point_aligned <- align_col(point)
  lower_aligned <- align_col(lower)
  upper_aligned <- align_col(upper)

  return(
    paste0(
      point_aligned, " (", lower_aligned, "\u2013", upper_aligned, ")"
    )
  )
}

#' Format Numeric Pairs
#'
#' Formats two numeric vectors into a string representation of pairs, e.g., "(x, y)".
#'
#' This function ensures rigid alignment for monospaced outputs. It processes the
#' two input vectors independently. If a value is missing (NA), it is replaced by
#' whitespace of the same width as the widest number in that column, ensuring
#' that commas and parentheses remain vertically aligned.
#'
#' @param point_1 Numeric vector. The first value of the pair.
#' @param point_2 Numeric vector. The second value of the pair.
#' @param decimals Integer. The number of decimal places to round to. Defaults to 2.
#'
#' @return A character vector of formatted strings.
#'
#' @importFrom scales number
#' @export
#'
#' @examples
#' # Basic Usage
#' format_pair(1.234, 5.678)
#' # Returns: "(1.23, 5.68)"
#'
#' # Vectorized with partial NA values
#' x <- c(1.2, NA, 5.5)
#' y <- c(3.4, 99.1, NA)
#'
#' format_pair(x, y)
#' # Returns:
#' # "(1.20,  3.40)"
#' # "(    , 99.10)"
#' # "(5.50,      )"
format_pair <- function(point_1, point_2, decimals = 2L) {
  acc <- 1 / (10^decimals)

  # Helper function to format a single column and handle alignment
  align_col <- function(x) {
    # 1. Format numbers using scales (trim = TRUE removes extra padding initially)
    formatted <- scales::number(
      x,
      accuracy = acc,
      big.mark = ",",
      decimal.mark = ".",
      trim = TRUE
    )

    # 2. Replace NA entries with empty strings
    formatted[is.na(formatted)] <- ""

    # 3. Apply alignment using base::format (pads with spaces)
    format(formatted, justify = "right")
  }

  # Apply alignment to each component independently
  p1_aligned <- align_col(point_1)
  p2_aligned <- align_col(point_2)

  return(
    paste0(
      "(", p1_aligned, ", ", p2_aligned, ")"
    )
  )
}

format_number <- function(point, decimals = 2L, suffix = "") {
  acc <- 1 / (10^decimals)
  return(
    scales::number(
      x = point,
      accuracy = acc,
      suffix = suffix,
      big.mark = ","
    )
  )
}
