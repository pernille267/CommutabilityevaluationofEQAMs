#' Glass Notification Component
#'
#' A custom, glass-morphism styled notification box.
#'
#' @param inputId The unique ID for this notification element.
#' @param label The header title of the notification.
#' @param label_icon An optional icon (shiny::icon) for the header.
#' @param value The main message body (HTML allowed).
#' @param message_type One of "info", "warning", or "error". Determines styling.
#' @param dismissible Logical. Should a close (x) button be shown?
#' @param timer Numeric. Time in milliseconds before auto-closing. 0 means no timer.
#' @param width CSS width string (e.g., "100%").
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @export
glassNotifyUser <- function(inputId,
                            label = "Notification",
                            label_icon = NULL,
                            value = "",
                            message_type = c("info", "warning", "error"),
                            dismissible = TRUE,
                            timer = 0,
                            width = "100%") {

  message_type <- match.arg(message_type)

  # --- Build CSS Class String ---
  # Start with base class and type
  class_str <- paste("glass-notification", message_type)

  # If value is empty initially, we can choose to hide it,
  # but usually we render it and let the user update it.
  # Let's keep it visible if value is provided.
  if (is.null(value) || value == "") {
    class_str <- paste(class_str, "hidden")
  }

  # --- Build Icons ---
  icon_html <- if (!is.null(label_icon)) as.character(label_icon) else ""

  # --- Build HTML Structure ---
  ui_structure <- htmltools::tags$div(
    id = inputId,
    class = class_str,
    style = paste0("width: ", width, ";"),

    # 1. Header
    htmltools::tags$div(
      class = "glass-notification-header",

      # Title Group
      htmltools::tags$div(
        class = "glass-notification-title-group",
        htmltools::tags$span(class = "glass-notification-icon-span", htmltools::HTML(icon_html)),
        htmltools::tags$span(class = "glass-notification-label-text", label)
      ),

      # Close Button
      if (dismissible) {
        htmltools::tags$div(
          class = "glass-notification-close",
          htmltools::HTML("&times;")
        )
      }
    ),

    # 2. Body
    htmltools::tags$div(
      class = "glass-notification-body",
      htmltools::HTML(value)
    ),

    # 3. Timer Bar
    htmltools::tags$div(class = "glass-notification-timer-bar")
  )

  # --- Attach Javascript Logic for Timer on Load if needed ---
  # If a timer is set on load, we need a small script to trigger the animation/hide
  # However, it's cleaner to handle logic in the binding.
  # We will attach an attribute for the JS binding to read on initialization if we wanted,
  # but for now, the update function is the primary driver for timers.

  # Attach Dependencies
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-notification",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_notification.js",
      stylesheet = "glass_notification.css"
    )
  )
}

#' Update Glass Notification
#'
#' Updates the content, type, and visibility of the glass notification.
#'
#' @param session The Shiny session object.
#' @param inputId The ID of the notification to update.
#' @param label Optional. New title text.
#' @param label_icon Optional. New icon.
#' @param value Optional. New message body.
#' @param message_type Optional. New type ("info", "warning", "error").
#' @param timer Optional. New timer duration in ms.
#'
#' @export
updateGlassNotifyUser <- function(session, inputId, label = NULL, label_icon = NULL, value = NULL, message_type = NULL, timer = NULL) {

  message <- list()

  if (!is.null(label)) message$label <- label
  if (!is.null(label_icon)) message$label_icon <- as.character(label_icon)
  if (!is.null(value)) message$value <- as.character(value)
  if (!is.null(message_type)) message$message_type <- message_type
  if (!is.null(timer)) message$timer <- as.numeric(timer)

  session$sendInputMessage(inputId, message)
}
