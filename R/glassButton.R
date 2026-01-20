#' Glass Action Button (Split Style) with Urgent Mode
#'
#' A custom, two-tone action button.
#' Left: White pill with Icon. Right: Purple gradient with Label.
#' Can be set to "Urgent" mode to display a bouncing arrow and text above.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param width The width of the button (e.g., '100%', '200px').
#' @param color A character string. "purple" (default) or "green".
#' @param disabled Logical. Should the button be disabled on load?
#' @param urgent Logical. If TRUE, shows an animated arrow and text above the button.
#' @param urgent_text Character. The text to display when urgent (default "press me").
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassButton <- function(inputId, label, icon = NULL, width = NULL,
                        color = "purple", disabled = FALSE,
                        urgent = FALSE, urgent_text = "press me") {

  # Class String for the Button
  class_str <- "glass-btn"
  if (color == "green") class_str <- paste(class_str, "green")
  if (disabled) class_str <- paste(class_str, "disabled")

  # Style String for the Button
  style_str <- ""
  if (!is.null(width)) style_str <- paste0("width: ", width, ";")

  # Icon Content
  icon_html <- if (!is.null(icon)) as.character(icon) else ""

  # Urgent Indicator Class
  urgent_class <- "urgent-indicator"
  if (urgent) urgent_class <- paste(urgent_class, "visible")

  # 1. Build the Urgent Indicator HTML
  urgent_html <- htmltools::tags$div(
    class = urgent_class,
    htmltools::tags$span(class = "urgent-text", urgent_text),
    htmltools::tags$div(class = "urgent-arrow", htmltools::HTML("&#9660;")) # Down Arrow
  )

  # 2. Build the Button Structure
  btn_structure <- htmltools::tags$div(
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

  # 3. Wrap them together
  # The wrapper handles the vertical stacking
  ui_wrapper <- htmltools::tags$div(
    class = "glass-btn-wrapper",
    urgent_html,
    btn_structure
  )

  # Attach Dependencies
  htmltools::tagList(
    ui_wrapper,
    htmltools::htmlDependency(
      name = "glass-button",
      version = "1.0.3", # Bump version
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
#' @param session A valid Shiny session object.
#' @param inputId The input slot that will be used to access the value.
#' @param label The label to display.
#' @param icon An optional icon() to appear in the white section.
#' @param disabled Logical. Should the button be disabled?
#' @param urgent Logical. Should the urgent animation be shown?
#' @param urgent_text Character. Update the urgent message text.
#' @export
updateGlassButton <- function(session, inputId, label = NULL, icon = NULL,
                              disabled = NULL, urgent = NULL, urgent_text = NULL) {
  message <- list()
  if (!is.null(label)) message$label <- label
  if (!is.null(icon)) message$icon <- as.character(icon)
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)
  if (!is.null(urgent)) message$urgent <- as.logical(urgent)
  if (!is.null(urgent_text)) message$urgent_text <- as.character(urgent_text)

  session$sendInputMessage(inputId, message)
}
