#' Glass Action Button (Split Style)
#'
#' A custom, two-tone action button.
#' Left: White pill with Icon. Right: Purple gradient with Label.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param width The width of the button (e.g., '100%', '200px').
#' @param color A character string. "purple" (default) or "green".
#' @param disabled Logical. Should the button be disabled on load?
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassButton <- function(inputId, label, icon = NULL, width = NULL, color = "purple", disabled = FALSE) {

  # Class String
  class_str <- "glass-btn"
  if (color == "green") class_str <- paste(class_str, "green")
  if (disabled) class_str <- paste(class_str, "disabled")

  # Style String
  style_str <- ""
  if (!is.null(width)) style_str <- paste0("width: ", width, ";")

  # Icon Content
  icon_html <- if (!is.null(icon)) as.character(icon) else ""

  # Build the Split Structure
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = class_str,
    style = style_str,
    role = "button",

    # Part 1: The White Icon Area
    htmltools::tags$span(
      class = "glass-btn-icon",
      htmltools::HTML(icon_html)
    ),

    # Part 2: The Gradient Label Area
    htmltools::tags$span(
      class = "glass-btn-label",
      label
    )
  )

  # Attach Dependencies
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-button",
      version = "1.0.2", # Bump version
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_button.js",
      stylesheet = "glass_button.css"
    )
  )
}

#' Glass Download Button
#'
#' A custom download button with the "Split Capsule" Glass theme.
#'
#' @param outputId The output variable to read the download from.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param width The width of the button (e.g., '100%', '200px').
#' @param disabled Logical. Should the button be disabled on load?
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassDownloadButton <- function(outputId, label = "Download", icon = shiny::icon("download"), width = NULL, disabled = FALSE) {

  # 1. Build Class String
  # We use 'glass-download-btn' for styling.
  # We use 'shiny-download-link' so Shiny knows it's a download handler.
  class_str <- "glass-download-btn shiny-download-link"
  if (disabled) class_str <- paste(class_str, "disabled")

  # 2. Build Style String
  style_str <- ""
  if (!is.null(width)) style_str <- paste0("width: ", width, ";")

  # 3. Icon Handling
  icon_html <- if (!is.null(icon)) as.character(icon) else ""

  # 4. Build HTML Structure
  # Note: It MUST be an <a> tag for downloads to work natively
  ui_structure <- htmltools::tags$a(
    id = outputId,
    class = class_str,
    style = style_str,
    href = "",
    target = "_blank",
    download = NA,

    # Part 1: White Icon Area
    htmltools::tags$span(
      class = "glass-btn-icon",
      htmltools::HTML(icon_html)
    ),

    # Part 2: Gradient Label Area
    htmltools::tags$span(
      class = "glass-btn-label",
      label
    )
  )

  # 5. Attach Dependencies
  # We reuse the same 'glass-button' dependency since they share the CSS file.
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-button",
      version = "1.0.1",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      # We don't strictly need the JS for the download button,
      # but including it doesn't hurt as it targets a different class (.glass-btn).
      script = "glass_button.js",
      stylesheet = "glass_button.css"
    )
  )
}

#' Update Glass Button
#'
#' @export
updateGlassButton <- function(session, inputId, label = NULL, icon = NULL, disabled = NULL) {
  message <- list()
  if (!is.null(label)) message$label <- label
  if (!is.null(icon)) message$icon <- as.character(icon)
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)

  session$sendInputMessage(inputId, message)
}
