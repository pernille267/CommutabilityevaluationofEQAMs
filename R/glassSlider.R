#' Glass Slider Input
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Optional label for the slider input.
#' @param choices Vector of values (numbers or strings).
#' @param selected The initially selected value.
#' @param unit Optional string to append to values (e.g. " dpi").
#' @param width The width of the input, e.g., '100%'.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @importFrom jsonlite toJSON
#' @export
glassSlider <- function(inputId, label = NULL, choices, selected = NULL, unit = "", width = "100%") {

  if (is.null(selected)) selected <- choices[1]
  if (!selected %in% choices) stop(paste("Selected value '", selected, "' must be in choices"))

  selected_idx <- which(choices == selected) - 1

  # Generate Labels with Specific Edge Classes
  labels_html <- lapply(seq_along(choices), function(i) {
    is_first <- (i == 1)
    is_last  <- (i == length(choices))

    # Show First and Last always.
    # Optional: logic to show middle items could go here.
    if (is_first || is_last) {

      # Determine CSS class for alignment
      extra_class <- ""
      if (is_first) extra_class <- "glass-label-first"
      if (is_last)  extra_class <- "glass-label-last"

      # Position calculation (Middle items use %)
      # First and Last rely on CSS anchoring, but we leave the style just in case
      pct <- (i - 1) * (100 / (length(choices) - 1))

      htmltools::tags$div(
        class = paste("glass-slider-label", extra_class),
        style = paste0("left: ", pct, "%;"),
        `data-index` = i - 1,
        paste0(choices[i], unit)
      )
    } else {
      NULL
    }
  })

  ui_structure <- htmltools::tags$div(
    class = "form-group shiny-input-container",
    style = paste0("width: ", width, "; margin-bottom: 5px;"), # Tighten external margin

    if (!is.null(label)) htmltools::tags$label(class = "control-label", `for` = inputId, label),

    htmltools::tags$div(
      id = paste0("container-", inputId),
      class = "glass-slider-container",
      `data-choices` = jsonlite::toJSON(choices, auto_unbox = TRUE),
      `data-selected-index` = selected_idx,
      `data-unit` = unit,

      htmltools::tags$div(class = "glass-slider-track"),
      htmltools::tags$div(class = "glass-slider-fill"),

      htmltools::tags$div(
        class = "glass-slider-handle",
        htmltools::tags$div(class = "glass-slider-tooltip", paste0(selected, unit))
      ),

      htmltools::tagList(labels_html)
    )
  )

  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "glass-slider",
      version = "1.0.1",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_slider.js",
      stylesheet = "glass_slider.css"
    )
  )
}
