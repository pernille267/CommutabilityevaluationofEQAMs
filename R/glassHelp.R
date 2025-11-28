#' Glass Help Components
#'
#' A set of functions to build structured, styled help content without raw HTML.
#'
#' @name glassHelp
NULL

#' @describeIn glassHelp Main container for help content
#' @param ... Content elements (sections, tips, etc.)
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassHelpCard <- function(...) {
  ui_structure <- htmltools::tags$div(
    class = "glass-help-card",
    htmltools::tagList(...)
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-help",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_help.js",
      stylesheet = "glass_help.css"
    )
  )
}

#' @describeIn glassHelp A highlighted tip box
#' @param text The text to display.
#' @param icon Name of the FontAwesome icon (default: "info-circle").
#' @export
glassHelpTip <- function(text, icon = "info-circle") {
  htmltools::tags$div(
    class = "glass-help-tip",
    htmltools::tags$i(class = paste("fa fa-", icon)),
    htmltools::tags$div(htmltools::HTML(text))
  )
}

#' @describeIn glassHelp A section with a header
#' @param title The section title.
#' @param ... Content for the section.
#' @export
glassHelpSection <- function(title, ...) {
  htmltools::tagList(
    htmltools::tags$div(
      class = "glass-help-header",
      htmltools::tags$span(title)
    ),
    htmltools::tags$div(
      class = "glass-help-section",
      htmltools::tagList(...)
    )
  )
}

#' @describeIn glassHelp A numbered or icon-marked step
#' @param number The step number or an icon tag (e.g., `icon("list")`).
#' @param title The bold title of the step.
#' @param description The body text of the step.
#' @param color Optional color override for the number circle.
#' @export
glassHelpStep <- function(number, title, description, color = NULL) {
  # Determine if number is text or an icon object
  num_content <- number

  style_str <- ""
  if (!is.null(color)) {
    style_str <- paste0("border-color: ", color, "; color: ", color, ";")
  }

  htmltools::tags$div(
    class = "glass-help-step",
    htmltools::tags$div(
      class = "glass-help-step-number",
      style = style_str,
      num_content
    ),
    htmltools::tags$div(
      class = "glass-help-step-content",
      htmltools::tags$b(title),
      htmltools::tags$br(),
      htmltools::HTML(description)
    )
  )
}

#' @describeIn glassHelp A container for list-like info items
#' @param ... `glassHelpInfoItem` elements.
#' @param border_color Color of the left border (default: #605CA8).
#' @export
glassHelpInfoBox <- function(..., border_color = "#605CA8") {
  htmltools::tags$div(
    class = "glass-help-info-box",
    style = paste0("border-left-color: ", border_color, ";"),
    htmltools::tagList(...)
  )
}

#' @describeIn glassHelp An individual item inside an info box
#' @param label The bold label text.
#' @param description The text following the label.
#' @param icon Icon name (default: "circle").
#' @param color Color of the text/icon.
#' @export
glassHelpInfoItem <- function(label, description, icon = "circle", color = "#605CA8") {
  htmltools::tags$div(
    class = "glass-help-info-item",
    htmltools::tags$i(class = paste("fa fa-", icon), style = paste0("color: ", color, ";")),
    htmltools::tags$div(
      htmltools::tags$b(style = paste0("color: ", color, ";"), label),
      ": ",
      htmltools::HTML(description)
    )
  )
}
