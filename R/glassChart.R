#' Glass Chart Dependencies
#'
#' @return HTML dependency for glass charts
#' @export
useGlassChart <- function() {

  # 1. More robust D3 CDN loading
  d3_head <- htmltools::tags$head(
    htmltools::tags$script(src = "https://d3js.org/d3.v7.min.js")
  )

  # 2. Check if asset file exists to prevent silent failures
  asset_path <- system.file("assets", package = "CommutabilityevaluationofEQAMs")
  if (asset_path == "") {
    # Fallback for development (adjust path to where your files actually are)
    # This helps if running locally without installing the package
    if(file.exists("assets/glass_chart.js")) {
      asset_path <- "assets"
    } else {
      warning("Glass Chart assets not found. Ensure package is installed or path is correct.")
    }
  }

  glass_dep <- htmltools::htmlDependency(
    name = "glass-chart",
    version = "3.1.0", # Match your JS version
    src = c(file = asset_path),
    script = "glass_chart.js",
    stylesheet = "glass_chart.css"
  )

  htmltools::tagList(d3_head, glass_dep)
}

#' Glass Chart Output (UI)
#' @export
glassChartOutput <- function(outputId, width = "100%", height = "600px") {
  htmltools::tags$div(
    id = outputId,
    class = "glass-chart-container",
    # Added explicit position relative, often needed for D3 tooltips/sizing
    style = paste0("width:", width, "; height:", height, "; position: relative;")
  )
}

#' Observe Glass Chart (Server)
#'
#' Call this directly in the server function (do NOT assign to output$id).
#'
#' @param outputId The ID used in glassChartOutput.
#' @param expr Expression returning list(cs=..., pb=..., ce=..., params=...)
#' @export
observeGlassChart <- function(outputId, expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  session <- shiny::getDefaultReactiveDomain()

  if (is.null(session)) {
    stop("observeGlassChart() must be called inside a Shiny server function.")
  }

  # Ensure we have the namespaced ID to send to JS
  # Note: logic handles if outputId is already namespaced or not based on how you pass it
  # Usually, inside a module, you pass "chart", and session$ns turns it into "mod1-chart"
  target_id <- session$ns(outputId)

  shiny::observe({
    data_list <- func()

    # Only proceed if data exists
    shiny::req(data_list)

    cs <- data_list$cs %||% data_list$cs_data %||% list()
    pb <- data_list$pb %||% data_list$pb_data %||% list()
    ce <- data_list$ce %||% data_list$ce_data %||% list()
    params <- data_list$params %||% list()

    session$sendCustomMessage(
      type = "update_glass_chart",
      message = list(
        id = target_id,
        data = list(
          cs = cs,
          pb = pb,
          ce = ce,
          params = params
        )
      )
    )
  })
}

#' Update Glass Chart (Low-level)
#'
#' Sends the data payload to the JavaScript via custom message.
#' Call this when you want to manually update the chart (e.g. inside an observer).
#'
#' @param session The Shiny session object.
#' @param inputId The element ID (namespaced).
#' @param cs_data Clinical sample data frame.
#' @param pb_data Prediction band data frame.
#' @param ce_data Commutability evaluation data frame.
#' @param params List of visual parameters (colors, etc).
#' @export
updateGlassChart <- function(session, inputId, cs_data, pb_data, ce_data, params) {

  # Helper to ensure List-of-Rows structure
  clean_data <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(list())

    # Convert data.frame/data.table to a generic list of rows
    # This ensures D3 receives [{}, {}, {}]
    lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
  }

  session$sendCustomMessage(
    type = "update_glass_chart",
    message = list(
      id = inputId,
      data = list(
        cs = clean_data(cs_data),
        pb = clean_data(pb_data),
        ce = clean_data(ce_data),
        params = if (is.null(params)) list() else params
      )
    )
  )
}


`%||%` <- function(a, b) if (!is.null(a)) a else b
