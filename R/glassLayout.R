#' Glass Layout Shell
#'
#' @param title App Title.
#' @param branding UI element for top-left.
#' @param sidebar UI element from `glassSidebar`.
#' @param header_items UI elements for header.
#' @param ... `glassRouter` content.
#'
#' @importFrom htmltools tags tagList htmlDependency
#' @export
glassPage <- function(title, branding, sidebar, header_items = NULL, ...) {

  ui <- htmltools::tags$div(
    class = "glass-shell",

    # 1. Brand
    htmltools::tags$div(class = "glass-brand", branding),

    # 2. Header
    htmltools::tags$div(
      class = "glass-header",
      htmltools::tags$div(class = "glass-app-title", title),
      htmltools::tags$div(class = "glass-header-extras", header_items)
    ),

    # 3. Sidebar
    sidebar,

    # 4. Content Router
    htmltools::tags$div(
      # 'container-fluid' FIXES THE FLUIDROW PROBLEM
      class = "glass-content-area container-fluid",
      htmltools::tagList(...)
    )
  )

  htmltools::tagList(
    # Dependencies
    htmltools::htmlDependency(
      name = "glass-layout",
      version = "2.0.1", # Bump version
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_layout.js",
      stylesheet = "glass_layout.css"
    ),
    # Common dependencies
    htmltools::tags$head(
      htmltools::tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
      # THIS FIXES THE MISSING STYLES (page-header, badges etc)
      # Assuming standard Shiny folder structure where www is mounted at root
      htmltools::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    ui
  )
}

#' Glass Sidebar Container
#'
#' @param inputId Shiny input ID to track navigation state.
#' @param ... `glassNavItem` elements.
#' @export
glassSidebar <- function(inputId, ...) {
  htmltools::tags$div(
    class = "glass-sidebar",
    `data-input-id` = inputId,
    ...
  )
}

#' Glass Navigation Item (Icon Button)
#'
#' @param tabName The ID of the page this button opens.
#' @param icon The icon to display.
#' @param title The tooltip text.
#' @param active Logical. Set TRUE for the default start page.
#' @export
glassNavItem <- function(tabName, icon, title, active = FALSE) {
  classes <- "glass-nav-item"
  if (active) classes <- paste(classes, "active")

  htmltools::tags$div(
    class = classes,
    `data-target` = tabName,
    `data-title` = title,
    icon,
    # NYTT: Legg til et dedikert element for notifikasjonsprikken
    htmltools::tags$div(class = "glass-nav-notification")
  )
}

#' Glass Page (Route)
#'
#' A container for a single "tab" of content. Replaces tabPanel.
#'
#' @param title The ID used by navigation (must match tabName in navItem).
#' @param ... Content.
#' @export
glassRoute <- function(title, ...) {
  # Note: 'title' here acts as the ID/Value for routing
  htmltools::tags$div(
    class = "glass-page",
    `data-value` = title, # Used by JS router
    htmltools::tagList(...)
  )
}

#' Glass Grid Row
#'
#' A CSS Grid container that replaces `fluidRow`.
#' Automatically handles spacing (gap) between columns.
#'
#' @param ... Content (usually `glassCol` elements).
#' @export
glassRow <- function(...) {
  htmltools::tags$div(class = "glass-row", ...)
}

#' Glass Grid Column
#'
#' A column within a `glassRow`.
#'
#' @param width Integer between 1 and 12. 12 is full width, 6 is half width.
#' @param ... Content.
#' @export
glassCol <- function(width, ...) {
  # Clamp width between 1 and 12
  w <- max(1, min(12, as.integer(width)))

  htmltools::tags$div(
    class = paste0("glass-col-", w),
    ...
  )
}

#' Update Glass Sidebar Highlight
#'
#' Sends a custom message to the frontend to toggle the notification pulse
#' on a specific sidebar item.
#'
#' @param session The Shiny session object.
#' @param tabName The `tabName` (data-target) of the item to highlight.
#' @param enable Logical. TRUE to start pulsing, FALSE to stop.
#'
#' @export
updateGlassSidebarHighlight <- function(session, tabName, enable = TRUE) {
  session$sendCustomMessage(
    type = "glass-sidebar-highlight",
    message = list(
      tabName = tabName,
      enable = enable
    )
  )
}

