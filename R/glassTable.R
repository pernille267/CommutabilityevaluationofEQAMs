#' Render a Custom Glass Table with Optional Sidebar
#'
#' @param data A data.frame or data.table (The main data rows).
#' @param col_names Optional vector of column names for `data`.
#' @param caption Optional HTML caption.
#' @param sidebar_html Optional raw HTML for the left-hand fixed sidebar (e.g. Quality Score).
#' @param sidebar_title Title for the sidebar column. Set to NULL or "" to hide the header bar.
#' @param highlight_rows Vector of row indices to highlight.
#' @param sortable Logical.
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @importFrom shiny withMathJax
#' @export
renderGlassTable <- function(data,
                             col_names = NULL,
                             caption = NULL,
                             sidebar_html = NULL,
                             sidebar_title = NULL,
                             highlight_rows = NULL,
                             sortable = TRUE) {

  if (is.null(col_names)) col_names <- names(data)
  n_cols <- length(col_names)

  # --- Define Grid Layout -----------------------------------------------------
  grid_template <- paste(rep("minmax(120px, 1fr)", n_cols), collapse = " ")
  grid_style <- paste0("grid-template-columns: ", grid_template, ";")

  # --- Build Sidebar (Left) ---------------------------------------------------
  sidebar_col <- NULL
  if (!is.null(sidebar_html)) {

    # --- Header Logic ---
    header_class <- "glass-sidebar-header"
    header_content <- sidebar_title

    if (is.null(sidebar_title) || sidebar_title == "") {
      header_class <- paste(header_class, "empty")
      header_content <- htmltools::HTML("&nbsp;")
    }

    # --- Make LaTex Math Possible with withMathJax(...) ---
    sidebar_col <- htmltools::tags$div(
      class = "glass-table-sidebar-col",
      htmltools::tags$div(class = header_class, header_content),
      htmltools::tags$div(class = "glass-sidebar-body", htmltools::HTML(sidebar_html))
    )
  }

  # --- Build Data Area (Right) ---

  # --- Header -----------------------------------------------------------------

  # --- Header Cells ---
  header_cells <- lapply(seq_along(col_names), function(i) {
    col_name <- col_names[i]
    first_val <- data[[i]][1]

    # --- Auto-Aligning Headers ---
    # Center if numeric (or pseudo numeric) and left if character string
    is_num <- is.numeric(first_val) || (is.character(first_val) && grepl("^[0-9.% ]+$", first_val))
    align <- if (is_num) "header-center" else "header-left"

    # We subtract 1 because JavaScript arrays start at 0, R starts at 1
    js_index <- i - 1

    classes <- paste("glass-table-header-cell", align)
    icon <- ""

    # --- Add Sorting Icon if sortable == TRUE ---
    if (sortable) {
      classes <- paste(classes, "sortable")
      icon <- '<i class="fas fa-sort glass-sort-icon"></i>'
    }
    # No toupper() here, keeping text soft
    htmltools::HTML(sprintf('<div class="%s" data-col-index="%d">%s %s</div>', classes, js_index, col_name, icon))
  })

  # --- Header Row ---
  # --- Make LaTex Math Possible with withMathJax(...) ---
  header_row <- htmltools::tags$div(
    class = "glass-table-head",
    style = grid_style,
    htmltools::tagList(header_cells)
  )

  # --- Body -------------------------------------------------------------------

  # --- Body Rows ---
  rows_html <- lapply(1:nrow(data), function(i) {

    # --- Highlighting Rows ---
    r_cls <- "glass-table-row"
    if (!is.null(highlight_rows) && i %in% highlight_rows) {
      r_cls <- paste(r_cls, "highlight")
    }

    # --- Body Cells ---
    cells <- lapply(names(data), function(col) {
      val <- data[i, col, with = FALSE][[1]]
      content <- htmltools::htmlEscape(as.character(val))

      # Auto-Align Body Cells (i.e., same way as Header Cells)
      is_num <- is.numeric(val) || (is.character(val) && grepl("^[0-9.% ]+$", val))
      align <- if (is_num) "cell-center" else "cell-left"

      # Construct the HTML for the Body Cells
      htmltools::HTML(sprintf('<div class="glass-table-cell %s">%s</div>', align, content))
    })

    # --- Combine HTML for Body Rows and Body Cells ---
    htmltools::tags$div(
      class = r_cls,
      style = grid_style,
      htmltools::tagList(cells)
    )
  })

  # --- Body Rows Container ---
  rows_container <- htmltools::tags$div(
    class = "glass-table-rows-container",
    htmltools::tagList(rows_html)
  )

  # --- Combine HTML for Header Row and Body Rows Container ---
  data_col <- htmltools::tags$div(
    class = "glass-table-data-col",
    header_row,
    rows_container
  )

  # --- Construct HTML for Table Caption ---
  caption_html <- if (!is.null(caption)) {
    htmltools::tags$div(
      class = "glass-table-caption",
      htmltools::HTML(caption)
    )
  }
  else {
    NULL
  }

  # --- Assemple Everything Into one Container ---------------------------------
  # Caption + Sidebar Column + Data Column
  ui <- htmltools::tags$div(
    class = "glass-table-container",
    caption_html,
    htmltools::tags$div(
      class = "glass-table-main",
      sidebar_col,
      data_col
    )
  )

  # --- Finished UI for the glassTable ---
  # Applies glass.table.js for complex behavior and glass_table.css
  # for styling and other behavior that can be set by CSS
  htmltools::tagList(
    withMathJax(),
    ui,
    htmltools::htmlDependency(
      name = "glass-table",
      version = "9.0.0",
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_table.js",
      stylesheet = "glass_table.css"
    )
  )
}
