#' Glass Header Indicator
#'
#' A versatile indicator for the dashboard header. Can be used for active filters,
#' reference method status, or diagnostics overview.
#'
#' @param inputId Unique ID.
#' @param type Type of indicator: "filter" (default), "reference", or "diagnostics".
#' @param tooltip_text Initial text to display on hover.
#' @param visible Logical. Should it be visible initially? Default FALSE.
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @export
glassHeaderIndicator <- function(inputId,
                                 type = c("filter", "reference", "diagnostics"),
                                 tooltip_text = "",
                                 visible = FALSE) {

  type <- match.arg(type)

  # --- 1. Determine Inner Content based on Type ---
  inner_content <- switch(type,
                          "filter" = htmltools::tags$div(
                            class = "glass-indicator-icons",
                            shiny::icon("ruler-horizontal", class = "indicator-icon-main"),
                            shiny::icon("filter", class = "indicator-icon-sub")
                          ),
                          "reference" = htmltools::tags$div(
                            class = "glass-indicator-text-icon",
                            "R"
                          ),
                          "diagnostics" = htmltools::tags$div(
                            class = "glass-indicator-icon-single",
                            shiny::icon("heart-pulse")
                          )
  )

  # --- 2. Build CSS Classes ---
  # Base class + Type class + Visibility
  classes <- paste("glass-header-indicator", type, if (!visible) "hidden" else "")

  # --- 3. Build UI Structure ---
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = classes,

    # The Content (Icon/Text)
    inner_content,

    # The Tooltip
    htmltools::tags$div(
      class = "glass-indicator-tooltip",
      tooltip_text
    )
  )

  # --- 4. Attach Dependencies ---
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-header-indicator",
      version = "1.1.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_header_indicator.js",
      stylesheet = "glass_header_indicator.css"
    )
  )
}

#' Update Glass Header Indicator
#'
#' Updates the state, text, and styling of the header indicator.
#'
#' @param session Shiny session.
#' @param inputId ID of the indicator.
#' @param visible Logical. Show or hide?
#' @param tooltip_text String. New text for tooltip.
#' @param status String. Optional status class ("success", "warning", "error", "info").
#'        Only applies to 'diagnostics' type to change color.
#' @param icon_name String. Optional. Name of a new icon to set (e.g. "triangle-exclamation").
#'        Mostly for 'diagnostics' type.
#' @export
updateGlassHeaderIndicator <- function(session, inputId, visible = NULL, tooltip_text = NULL, status = NULL, icon_name = NULL) {
  message <- list()
  if (!is.null(visible)) message$visible <- visible
  if (!is.null(tooltip_text)) message$tooltip_text <- tooltip_text
  if (!is.null(status)) message$status <- status
  if (!is.null(icon_name)) message$icon_name <- icon_name

  session$sendInputMessage(inputId, message)
}
