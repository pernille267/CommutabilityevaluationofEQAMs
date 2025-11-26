#' Glass Dropdown Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param choices List of values to select from. Can be a named vector c("Label" = "val").
#' @param selected The initially selected value (must match the value, not the label).
#' @param disabled Logical. If \code{TRUE}, the dropdown is disabled on load.
#' @param width The width of the input.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassDropdown <- function(inputId, choices, selected = NULL, disabled = FALSE, width = "100%") {

  # 1. Handle Named Vectors
  if (!is.null(names(choices))) {
    values <- as.character(choices)
    labels <- names(choices)
  } else {
    values <- as.character(choices)
    labels <- as.character(choices)
  }

  # 2. Default Selection
  if (is.null(selected)) selected <- values[1]

  selected_idx <- which(values == selected)[1]
  if (is.na(selected_idx)) {
    warning(paste("Selected value '", selected, "' not found in choices. Defaulting to first."))
    selected_idx <- 1
    selected <- values[1]
  }
  selected_label <- labels[selected_idx]

  # 3. Handle Disabled State Classes
  container_cls <- "glass-dropdown-container"
  btn_cls       <- "glass-dropdown-btn"

  if (disabled) {
    container_cls <- paste(container_cls, "disabled")
    btn_cls       <- paste(btn_cls, "disabled")
  }

  # 4. Generate Options
  options_html <- lapply(seq_along(values), function(i) {
    val <- values[i]
    lbl <- labels[i]
    is_selected <- (val == selected)
    class_string <- if(is_selected) "glass-option selected" else "glass-option"

    htmltools::tags$div(
      class = class_string,
      `data-value` = val,
      onclick = sprintf("selectGlassOption('%s', '%s', '%s', this)", inputId, val, lbl),
      lbl
    )
  })

  # 5. Build UI
  ui_structure <- htmltools::tags$div(
    class = container_cls,
    style = paste0("width: ", width, ";"),
    id = paste0("container-", inputId),
    `data-selected` = selected,

    htmltools::tags$button(
      id = paste0("btn-", inputId),
      class = btn_cls,
      # Prevent toggle if disabled (handled by JS 'if' check, but good to have class)
      onclick = sprintf("toggleGlassMenu('%s')", inputId),
      type = "button",
      htmltools::tags$span(id = paste0("label-", inputId), selected_label),
      htmltools::tags$i(class = "fa fa-chevron-down", style = "float: right; font-size: 0.8em; margin-top: 4px;")
    ),

    htmltools::tags$div(
      id = paste0("menu-", inputId),
      class = "glass-dropdown-menu",
      htmltools::tagList(options_html)
    )
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-dropdown",
      version = "1.0.3", # Bumped version
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_dropdown.js",
      stylesheet = "glass_dropdown.css"
    )
  )
}

#' Update Glass Dropdown
#'
#' Updates the choices, selection, or disabled state of a glassDropdown on the client.
#'
#' @param session The Shiny session object.
#' @param inputId The id of the input object.
#' @param choices Optional. New list of choices (can be a named vector).
#' @param selected Optional. The value to select.
#' @param disabled Optional. TRUE to disable, FALSE to enable.
#'
#' @export
updateGlassDropdown <- function(session, inputId, choices = NULL, selected = NULL, disabled = NULL) {

  fullId <- session$ns(inputId)
  message <- list(id = fullId)

  # 1. Handle Choices (Split Labels/Values)
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
  }

  # 2. Handle Selection
  if (!is.null(selected)) {
    message$selected <- as.character(selected)

    # If we are updating selection BUT NOT choices, we need to find the label for this value
    # This assumes the value exists in the current list.
    # Ideally, one updates choices and selected together to be safe.
  }

  # 3. Handle Disabled State
  if (!is.null(disabled)) {
    message$disabled <- as.logical(disabled)
  }

  # Send Custom Message to JS
  session$sendCustomMessage("glass-dropdown-update", message)
}


