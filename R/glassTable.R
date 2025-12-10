#' Render a Custom Glass Table (Table-based Layout)
#'
#' @param data A data.frame or data.table.
#' @param col_names Optional vector of column names.
#' @param caption Optional HTML caption.
#' @param sidebar_html Optional raw HTML for the sidebar.
#' @param sidebar_title Title for the sidebar column.
#' @param highlight_rows Vector of row indices to highlight.
#' @param sortable Logical.
#'
#' @importFrom htmltools tagList tags htmlDependency HTML
#' @importFrom data.table is.data.table as.data.table
#' @importFrom shiny withMathJax
#' @export
renderGlassTable <- function(data,
                             col_names = NULL,
                             caption = NULL,
                             sidebar_html = NULL,
                             sidebar_title = NULL,
                             highlight_rows = NULL,
                             sortable = TRUE) {

  # 1. Robust Data Handling (Fixes potential crash with plain data.frames)
  if (!data.table::is.data.table(data)) {
    data <- data.table::as.data.table(data)
  }

  if (is.null(col_names)) col_names <- names(data)

  # --- Build Sidebar (Left) ---------------------------------------------------
  sidebar_col <- NULL
  if (!is.null(sidebar_html)) {
    header_class <- "glass-sidebar-header"
    header_content <- sidebar_title

    if (is.null(sidebar_title) || sidebar_title == "") {
      header_class <- paste(header_class, "empty")
      header_content <- htmltools::HTML("&nbsp;")
    }

    sidebar_col <- htmltools::tags$div(
      class = "glass-table-sidebar-col",
      htmltools::tags$div(class = header_class, header_content),
      htmltools::tags$div(class = "glass-sidebar-body", htmltools::HTML(sidebar_html))
    )
  }

  # --- Build Data Table (Right) -----------------------------------------------

  # A. Table Header (<thead>)
  header_cells <- lapply(seq_along(col_names), function(i) {
    col_name <- col_names[i]
    first_val <- data[[i]][1]

    # Auto-Align: Center numbers, Left strings
    is_num <- is.numeric(first_val) || (is.character(first_val) && grepl("^[0-9.% ]+$", first_val))
    align <- if (is_num) "center" else "left"

    classes <- paste0("glass-th align-", align)
    icon <- ""

    if (sortable) {
      classes <- paste(classes, "sortable")
      icon <- '<i class="fas fa-sort glass-sort-icon"></i>'
    }

    # data-col-index helps JS identify the column easily
    htmltools::tags$th(
      class = classes,
      `data-col-index` = i - 1,
      htmltools::HTML(paste0(col_name, " ", icon))
    )
  })

  thead <- htmltools::tags$thead(
    htmltools::tags$tr(header_cells)
  )

  # B. Table Body (<tbody>)
  # We construct rows. Using simpler loop for clarity.
  rows_html <- lapply(1:nrow(data), function(i) {
    r_cls <- "glass-tr"
    if (!is.null(highlight_rows) && i %in% highlight_rows) {
      r_cls <- paste(r_cls, "highlight")
    }

    cells <- lapply(data, function(col_data) {
      val <- col_data[i]
      # Auto-Align Body Cells
      is_num <- is.numeric(val) || (is.character(val) && grepl("^[0-9.% ]+$", val))
      align <- if (is_num) "center" else "left"

      htmltools::tags$td(
        class = paste0("glass-td align-", align),
        as.character(val)
      )
    })

    htmltools::tags$tr(class = r_cls, cells)
  })

  tbody <- htmltools::tags$tbody(rows_html)

  # C. The Table Element
  table_html <- htmltools::tags$table(
    class = "glass-table",
    thead,
    tbody
  )

  # D. Scrollable Container for Data
  data_col <- htmltools::tags$div(
    class = "glass-table-data-col",
    table_html
  )

  pagination_footer <- htmltools::tags$div(class = "glass-pagination-container")

  # --- Caption ----------------------------------------------------------------
  caption_html <- if (!is.null(caption)) {
    htmltools::tags$div(class = "glass-table-caption", htmltools::HTML(caption))
  } else {
    NULL
  }

  # --- Final Assembly ---------------------------------------------------------
  ui <- htmltools::tags$div(
    class = "glass-table-container",
    `data-page-size` = "25", # <--- ADD THIS (Configurable in future if you want)
    caption_html,
    htmltools::tags$div(
      class = "glass-table-main",
      sidebar_col,
      data_col
    ),
    pagination_footer # <--- ADD THIS AT THE BOTTOM
  )

  htmltools::tagList(
    shiny::withMathJax(),
    ui,
    htmltools::htmlDependency(
      name = "glass-table",
      version = "13.0.0", # Major version bump for architectural change
      src = c(file = system.file("assets", package = "CommutabilityevaluationofEQAMs")),
      script = "glass_table.js",
      stylesheet = "glass_table.css"
    )
  )
}
