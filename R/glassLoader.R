#' Glass Loader
#'
#' @param id Unik ID for loaderen (namespace blir håndtert automatisk hvis session sendes)
#'
#' @return HTML for loader dependencies
#' @export
useGlassLoader <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "glass-loader",
      version = "1.0.0",
      src = c(file = "assets"),
      script = "glass_loader.js",
      stylesheet = "glass_loader.css",
      package = "CommutabilityevaluationofEQAMs"
    )
  )
}

#' Show Glass Loader
#'
#' @param id ID for loader-meldingen
#' @param text Valgfri tekst å vise under spinneren
#' @param selector (Valgfri) CSS-selector for elementet loaderen skal dekke (f.eks. "#mitt-kort-id").
#'                 Hvis NULL, dekker den hele skjermen.
#' @param session Shiny session object
#'
#' @export
showGlassLoader <- function(id = "global_loader", text = NULL, selector = NULL, session = shiny::getDefaultReactiveDomain()) {
  message <- list(id = id, text = text, selector = selector)
  session$sendCustomMessage("glass-loader-show", message)
}

#' Hide Glass Loader
#'
#' @param id ID for loader-meldingen som skal skjules
#' @param session Shiny session object
#'
#' @export
hideGlassLoader <- function(id = "global_loader", session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("glass-loader-hide", list(id = id))
}
