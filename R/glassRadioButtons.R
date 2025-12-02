#' Glass Radio Group Input
#'
#' A custom, responsive set of toggle buttons.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Optional display label text.
#' @param label_icon Optional \code{icon()} for the label.
#' @param help_text Optional tooltip text to display on hover.
#' @param choices List of values. Can be named vector c("Label" = "val").
#' @param selected The initially selected value.
#' @param inline (Deprecated) Always inline/responsive by design.
#' @param width The width of the container.
#' @param disabled Logical. Should it be disabled on load?
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassRadioButtons <- function(inputId,
                              label = NULL,
                              label_icon = NULL,
                              help_text = NULL,
                              choices,
                              selected = NULL,
                              inline = TRUE,
                              width = "100%",
                              disabled = FALSE) {

  # --- Handle Named Vectors ---
  if (!is.null(names(choices))) {
    values <- as.character(choices)
    labels <- names(choices)
  } else {
    values <- as.character(choices)
    labels <- as.character(choices)
  }

  # --- Setting Default Selection ---
  if (is.null(selected)) selected <- values[1]

  # --- Header Generation ---
  header_html <- NULL
  if (!is.null(label) || !is.null(label_icon)) {
    header_html <- htmltools::tags$div(
      class = "glass-radio-btn-label-header",
      if (!is.null(label_icon)) htmltools::tags$div(class = "glass-radio-btn-label-icon", label_icon),
      if (!is.null(label)) htmltools::tags$span(class = "glass-radio-btn-label-text", label)
    )
  }

  # --- Help Icon Generation ---
  help_icon_html <- create_glass_help_icon(help_text, "radio")

  # --- Build Options HTML ---
  options_html <- lapply(seq_along(values), function(i) {
    val <- values[i]
    lbl <- labels[i]
    is_selected <- (val == selected)

    class_str <- if (is_selected) "glass-radio-btn selected" else "glass-radio-btn"
    if (disabled) class_str <- paste(class_str, "disabled")

    htmltools::tags$div(
      class = class_str,
      `data-value` = val, # Important for JS finding it later
      onclick = sprintf("selectGlassRadio('%s', '%s', this)", inputId, val),
      lbl
    )
  })

  # --- Container Class ---
  container_cls <- "glass-radio-group"
  if (disabled) container_cls <- paste(container_cls, "disabled")

  # Build Final UI
  ui_structure <- htmltools::tags$div(
    style = paste0("width: ", width, "; margin-bottom: 20px;"),

    # Help Icon
    help_icon_html,
    # The Header & Header Icon
    header_html,

    htmltools::tags$div(
      id = paste0("group-", inputId),
      class = container_cls,
      style = "width: 100%;",
      `data-selected` = selected,
      htmltools::tagList(options_html)
    )
  )

  # Attach Dependencies
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-radio",
      version = "1.0.1",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_radio.js",
      stylesheet = "glass_radio.css"
    )
  )
}

#' Update Glass Radio Group
#'
#' @param session The Shiny session object.
#' @param inputId The id of the input object (use simple ID).
#' @param choices Optional. New list of choices.
#' @param selected Optional. New selection.
#' @param disabled Optional. TRUE to disable.
#'
#' @export
updateGlassRadio <- function(session, inputId, choices = NULL, selected = NULL, disabled = NULL) {

  # Get full inputID
  fullId <- session$ns(inputId)
  message <- list(id = fullId)

  # Handle Choices
  if (!is.null(choices)) {
    if (!is.null(names(choices))) {
      values <- as.character(choices)
      labels <- names(choices)
    } else {
      values <- as.character(choices)
      labels <- as.character(choices)
    }
    message$values <- values
    message$labels <- labels

    # If selection not specified, default to first of new choices
    if (is.null(selected)) {
      message$selected <- values[1]
    }
  }

  # Handle Selection
  if (!is.null(selected)) {
    message$selected <- as.character(selected)
  }

  # Handle Disabled
  if (!is.null(disabled)) {
    message$disabled <- as.logical(disabled)
  }

  session$sendCustomMessage("glass-radio-update", message)
}
