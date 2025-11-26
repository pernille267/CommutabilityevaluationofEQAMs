#' Glass Dashboard Card
#'
#' A clean, collapsible container with a glass-like theme.
#'
#' @param inputId The id of the card (useful for updates).
#' @param title The title text.
#' @param ... Content to put inside the card body.
#' @param icon An optional icon() to appear in the header.
#' @param collapsible Logical. Can the user toggle visibility?
#' @param collapsed Logical. Should it start closed? (Only if collapsible=TRUE).
#' @param disabled Logical. Should the card be disabled (greyed out) on load?
#' @param width The width (e.g., "100%").
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassCard <- function(inputId, title, ..., icon = NULL, collapsible = TRUE, collapsed = FALSE, disabled = FALSE, width = "100%") {

  # 1. Class Construction
  card_classes <- "glass-card"
  if (collapsible) card_classes <- paste(card_classes, "collapsible")
  if (collapsible && collapsed) card_classes <- paste(card_classes, "collapsed")
  if (disabled) card_classes <- paste(card_classes, "disabled")

  # 2. Icon Handling
  icon_html <- if (!is.null(icon)) {
    htmltools::tags$div(class = "glass-card-icon", icon)
  } else {
    NULL
  }

  # 3. Toggle Icon (Chevron)
  toggle_html <- if (collapsible) {
    htmltools::tags$i(class = "fa fa-chevron-up glass-card-toggle")
  } else {
    NULL
  }

  # 4. Build HTML Structure
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = card_classes,
    style = paste0("width: ", width, ";"),

    # Header
    htmltools::tags$div(
      class = "glass-card-header",

      # Title Side (Left)
      htmltools::tags$div(
        class = "glass-card-title-wrap",
        icon_html,
        htmltools::tags$h4(class = "glass-card-title", title)
      ),

      # Toggle Side (Right)
      toggle_html
    ),

    # Body
    htmltools::tags$div(
      class = "glass-card-body",
      htmltools::tagList(...)
    )
  )

  # 5. Attach Dependencies
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-card",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_card.js",
      stylesheet = "glass_card.css"
    )
  )
}

#' Update Glass Dashboard Card
#'
#' Update the title, collapse state, or disabled state of a glassCard.
#'
#' @param session The Shiny session object.
#' @param inputId The id of the card.
#' @param title Optional. New title text.
#' @param collapsed Optional. TRUE/FALSE.
#' @param disabled Optional. TRUE/FALSE.
#'
#' @export
updateGlassCard <- function(session, inputId, title = NULL, collapsed = NULL, disabled = NULL) {

  # Handle Namespace
  fullId <- session$ns(inputId)

  message <- list(id = fullId)

  if (!is.null(title)) message$title <- as.character(title)
  if (!is.null(collapsed)) message$collapsed <- as.logical(collapsed)
  if (!is.null(disabled)) message$disabled <- as.logical(disabled)

  session$sendCustomMessage("glass-card-update", message)
}
