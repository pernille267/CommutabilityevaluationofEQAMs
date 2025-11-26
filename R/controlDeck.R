#' Control Deck Layout
#'
#' A clean, collapsible container for organizing inputs and results.
#'
#' @param title The main title of the deck.
#' @param ... Main configuration inputs (passed as named arguments or tagList).
#'            These will be arranged in a responsive grid.
#' @param results (Optional) Main result content (e.g. a plot or table).
#'                Ignored if `tabs` is provided.
#' @param tabs (Optional) A list of `tabPanel()` items. If provided, a tabset
#'             is generated in the results area.
#' @param collapsible Logical. Can the deck be collapsed? Default TRUE.
#' @param collapsed Logical. Start collapsed? Default FALSE.
#'
#' @importFrom htmltools tagList tags htmlDependency
#' @export
controlDeck <- function(title, ..., results = NULL, tabs = NULL, collapsible = TRUE, collapsed = FALSE) {

  # 1. Collect Options into a Grid
  options_content <- list(...)

  # Wrap each option in a div class if it's not already structured
  # This assumes users pass inputs directly like glassDropdown(...)
  options_grid <- htmltools::tags$div(
    class = "deck-options-grid",
    lapply(options_content, function(x) {
      htmltools::tags$div(class = "deck-option-item", x)
    })
  )

  # 2. Handle Results (Tabs vs Single Output)
  results_section <- NULL
  if (!is.null(tabs)) {
    # If tabs list is provided, wrap them in a tabsetPanel
    # Note: We assume 'tabs' is a list of tabPanel() objects
    results_section <- do.call(shiny::tabsetPanel, c(list(type = "pills"), tabs))
  } else if (!is.null(results)) {
    results_section <- results
  }

  # Wrap results section if it exists
  if (!is.null(results_section)) {
    results_section <- htmltools::tags$div(
      class = "deck-results",
      results_section
    )
  }

  # 3. Build Collapsible Logic
  body_id <- paste0("deck-body-", sample(1000:9999, 1))
  header_attribs <- list(class = "deck-header")

  if (collapsible) {
    header_attribs$`data-toggle` <- "collapse"
    header_attribs$`data-target` <- paste0("#", body_id)
    header_attribs$`aria-expanded` <- if (collapsed) "false" else "true"
    header_attribs$style <- "cursor: pointer;"
  }

  body_class <- "deck-body collapse"
  if (!collapsed) body_class <- paste(body_class, "in show") # 'in' for BS3, 'show' for BS4

  # 4. Build Final HTML
  ui_structure <- htmltools::tags$div(
    class = "control-deck-wrapper",
    htmltools::tags$div(
      class = "control-deck-container",

      # Header
      htmltools::tags$div(
        class = "deck-header",
        # Attributes for collapsing
        header_attribs,

        # Title Area
        htmltools::tags$div(
          class = "deck-title",
          htmltools::tags$i(class = "fa fa-sliders-h"),
          title
        ),

        # Toggle Icon (Only if collapsible)
        if (collapsible) {
          htmltools::tags$i(class = "fa fa-chevron-up deck-toggle-icon")
        }
      ),

      # Body (Options + Results)
      htmltools::tags$div(
        id = body_id,
        class = body_class,

        # The Options Grid
        options_grid,

        # The Results Area (Grid/Plots/Tabs)
        results_section
      )
    )
  )

  # 5. Attach CSS Dependency
  htmltools::tagList(
    ui_structure,
    htmltools::htmlDependency(
      name = "control-deck",
      version = "1.0.0",
      src = c(file = system.file("assets", package = "MyShinyApp")),
      stylesheet = "control_deck.css"
    )
  )
}
