#' Glass Toggle Panel
#'
#' A content panel that toggles visibility based on a button click (counter).
#' Unlike conditionalPanel, this supports CSS transitions (slide/fade).
#'
#' @param triggerId The inputId of the button (glassButton) that toggles this panel.
#' @param ... Content to display inside the panel.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassTogglePanel <- function(triggerId, ...) {

  # Create the HTML structure
  ui_structure <- htmltools::tags$div(
    class = "glass-toggle-panel",
    `data-trigger` = triggerId, # JS uses this to find the correct panel
    htmltools::tags$div(
      class = "glass-toggle-content",
      htmltools::tagList(...)
    )
  )

  # Attach Dependencies (CSS/JS)
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-toggle-panel",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_toggle_panel.js",
      stylesheet = "glass_toggle_panel.css"
    )
  )
}
