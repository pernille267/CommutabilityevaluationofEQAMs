#' Glass Tabset Panel
#'
#' @param inputId The id of the input object.
#' @param ... One or more glassTabPanel() items.
#' @param selected The value of the tab to select initially.
#' @param color The active color theme: "purple" (default) or "green".
#' @param boxed Logical. If TRUE, the navigation bar gets a distinct glass container style.
#'              Use this when the tabset is placed directly on the page background.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
glassTabsetPanel <- function(inputId, ..., selected = NULL, color = "purple", boxed = FALSE) {

  panels <- list(...)

  # 1. Build Navigation
  nav_items <- lapply(panels, function(panel) {
    val <- panel$attribs$`data-value`
    title <- panel$attribs$`data-title`
    icon_html <- panel$attribs$`data-icon`

    is_active <- FALSE
    if (!is.null(selected) && val == selected) {
      is_active <- TRUE
    } else if (is.null(selected) && val == panels[[1]]$attribs$`data-value`) {
      is_active <- TRUE
    }

    class_str <- if (is_active) "glass-tab-btn active" else "glass-tab-btn"

    htmltools::tags$div(
      class = class_str,
      `data-value` = val,
      onclick = sprintf("switchGlassTab('%s', '%s', this)", inputId, val),
      htmltools::HTML(paste0(icon_html, title))
    )
  })

  # 2. Build Content
  content_panels <- lapply(panels, function(panel) {
    val <- panel$attribs$`data-value`
    is_active <- FALSE
    if (!is.null(selected) && val == selected) is_active <- TRUE
    else if (is.null(selected) && val == panels[[1]]$attribs$`data-value`) is_active <- TRUE

    if (is_active) panel$attribs$class <- paste(panel$attribs$class, "active")
    return(panel)
  })

  # 3. Container Classes (Handle Color Theme)
  container_class <- "glass-tabset-container"
  if (color == "green") {
    container_class <- paste(container_class, "theme-green")
  }

  if (boxed) {
    container_class <- paste(container_class, "glass-tabset-boxed")
  }

  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = container_class,
    htmltools::tags$div(class = "glass-tab-nav", htmltools::tagList(nav_items)),
    htmltools::tags$div(class = "glass-tab-content-wrapper", htmltools::tagList(content_panels))
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-tabs",
      version = "1.0.2",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_tabs.js",
      stylesheet = "glass_tabs.css"
    )
  )
}

#' Glass Tab Panel
#' @export
glassTabPanel <- function(title, ..., value = title, icon = NULL) {
  icon_html <- if (!is.null(icon)) as.character(icon) else ""
  htmltools::tags$div(
    class = "glass-tab-pane",
    `data-value` = value,
    `data-title` = title,
    `data-icon` = icon_html,
    htmltools::tagList(...)
  )
}
