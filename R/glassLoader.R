#' Glass Loader UI Component
#'
#' Adds a global, glass-morphism styled loading overlay to the application.
#' This overlay automatically appears when Shiny is busy (with a slight delay to prevent flickering).
#'
#' @return HTML tags to be inserted into the UI.
#' @importFrom htmltools tagList tags htmlDependency
#' @export
useGlassLoader <- function() {

  # Bygg HTML-strukturen
  ui_structure <- htmltools::tags$div(
    id = "glass-loader-overlay",

    # Spinner Container
    htmltools::tags$div(
      class = "glass-loader-spinner",
      htmltools::tags$div(class = "glass-loader-ring"),
      htmltools::tags$div(class = "glass-loader-orb")
    ),

    # Message
    htmltools::tags$div(
      class = "glass-loader-text",
      "Performing statistical magic..." # Default text
    )
  )

  # Legg til avhengigheter
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-loader",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_loader.js",
      stylesheet = "glass_loader.css"
    )
  )
}

#' Update Glass Loader Message
#'
#' Updates the text displayed in the loading overlay.
#' Call this *before* triggering a heavy calculation if you want a custom message.
#'
#' @param session The Shiny session object.
#' @param message The text string to display (e.g., "Analyzing Data...").
#' @export
updateGlassLoaderText <- function(session, message) {
  session$sendCustomMessage("glass-loader-update-text", message)
}
