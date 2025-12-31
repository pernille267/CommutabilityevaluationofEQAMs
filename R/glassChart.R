#' Glass Chart Dependencies
#'
#' @return HTML dependency for glass charts (D3.js + Custom Script)
#' @export
useGlassChart <- function() {
  tagList(
    # Load D3.js from CDN (Standard for custom viz)
    htmltools::tags$script(src = "https://d3js.org/d3.v7.min.js"),
    htmltools::htmlDependency(
      name = "glass-chart",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_chart.js",
      stylesheet = "glass_chart.css"
    )
  )
}

#' Glass Chart Output (UI)
#'
#' @param outputId The ID of the container.
#' @param width Width of the container.
#' @param height Height of the container.
#' @export
glassChartOutput <- function(outputId, width = "100%", height = "600px") {
  htmltools::tags$div(
    id = outputId,
    class = "glass-chart-container",
    style = paste0("width:", width, "; height:", height, "; position: relative;")
  )
}

#' Render Glass Chart (Server)
#'
#' @param expr An expression that returns a list containing:
#'             list(cs_data=..., pb_data=..., ce_data=..., params=...)
#' @param env The environment.
#' @param quoted Is the expression quoted?
#' @export
renderGlassChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)

  shiny::renderUI({
    data_list <- func()

    # We send the data as a JSON script tag to avoid large websocket payloads freezing the UI
    # and to allow the JS to read it cleanly on init.
    json_data <- jsonlite::toJSON(data_list, dataframe = "rows", auto_unbox = TRUE)

    id <- session$ns("glass_chart_data")

    htmltools::tags$script(
      type = "application/json",
      class = "glass-chart-data-source",
      `data-target` = session$ns("ce_plots"), # Target the UI output ID
      htmltools::HTML(json_data)
    )
  })
}

#' Send Data to Glass Chart (Reactive Update)
#' This helper function sends data via session$sendCustomMessage
#' @export
updateGlassChart <- function(session, inputId, cs_data, pb_data, ce_data, params) {
  session$sendCustomMessage(
    type = "update_glass_chart",
    message = list(
      id = inputId,
      data = list(
        cs = cs_data,
        pb = pb_data,
        ce = ce_data,
        params = params
      )
    )
  )
}
