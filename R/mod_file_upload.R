#' File Upload and Diagnostics UI Module
#'
#' This function defines the UI for the file upload and data diagnostics tab.
#' It includes file inputs for clinical and EQA data, and sections to display
#' diagnostic results.
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition that can be included in a Shiny app.
#' @noRd
mod_file_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "version-badge",
      icon("flask"),
      "Commutability Evaluation: Beta Version S1.0"
    ),
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("upload"),
        "Upload and Check Data for Commutability Evaluation"
      ),
      actionBttn(
        inputId = ns("show_file_input_explanation"),
        label = "Show Help Text",
        icon = icon("circle-question"),
        style = "gradient",
        color = "success"
      )
    ),
    htmlOutput(outputId = ns("file_input_explanation")),
    fluidRow(
      column(
        width = 6,
        div(
          class = "dashboard-card",
          div(
            class = "card-header",
            icon("file-upload", class = "header-icon"),
            h3("Upload Data from Your Device")
          ),
          div(
            class = "card-body",
            div(
              class = "upload-section",
              h4(icon("vial", class = "section-icon"), "Upload Clinical Sample Data"),
              fileInput(
                inputId = ns("cs_data"),
                label = NULL,
                accept = c(".xlsx", ".csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              )
            ),
            div(
              class = "upload-section",
              h4(icon("chart-line", class = "section-icon"), "Upload External Quality Assessment Material Data"),
              fileInput(
                inputId = ns("eq_data"),
                label = NULL,
                accept = c(".xlsx", ".csv"),
                buttonLabel = "Browse...",
                placeholder = "No file selected"
              )
            )
          )
        )
      ),
      column(
        width = 6,
        div(
          class = "dashboard-card-drop-down",
          div(
            class = "card-header",
            icon("hammer", class = "header-icon"),
            h3("Additional Options for Uploaded Data")
          ),
          div(
            class = "card-body",
            div(
              class = "parameter-section",
              h4(icon("arrow-pointer", class = "section-icon"), "Choose a Reference Method"),
              virtualSelectInput(
                inputId = ns("reference_method"),
                label = NULL,
                choices = "none",
                selected = "none",
                multiple = FALSE,
                search = TRUE,
                disabled = TRUE
              ),
              div(
                class = "input-note",
                icon("info-circle"),
                "This is optional. If no method is selected, all pairwise comparisons will be made."
              )
            ),
            div(
              class = "parameter-section",
              h4(icon("snowflake", class = "section-icon"), "Ignore Failed Validation Tests"),
              radioGroupButtons(
                inputId = ns("ignore_invalid_data"),
                label = NULL,
                choiceNames = c("Yes", "No"),
                choiceValues = c(TRUE, FALSE),
                selected = FALSE,
                justified = TRUE,
                status = "primary"
              ),
              uiOutput(ns("warning_placeholder"))
            )
          )
        )
      )
    ),
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("stethoscope", class = "header-icon"),
        h3("Diagnostic Overview of Your Uploaded Data")
      ),
      div(
        class = "card-body",
        div(
          class = "diagnostics-section",
          h4(icon("microscope", class = "section-icon"), "Clinical Sample Data Diagnostics"),
          div(class = "table-container", shiny::tableOutput(outputId = ns("cs_table_diagnostics"))),
          htmlOutput(outputId = ns("cs_text_diagnostics"))
        ),
        div(
          class = "diagnostics-section",
          h4(icon("flask", class = "section-icon"), "External Quality Assessment Material Data Diagnostics"),
          div(class = "table-container", shiny::tableOutput(outputId = ns("eq_table_diagnostics"))),
          htmlOutput(outputId = ns("eq_text_diagnostics"))
        ),
        div(
          class = "diagnostics-section",
          h4(icon("check-double", class = "section-icon"), "Structural Agreement Between Data Diagnostics"),
          htmlOutput(outputId = ns("both_text_diagnostics"))
        )
      )
    )
  )
}

# ==============================================================================
# Module Server
# ==============================================================================

#' File Upload and Diagnostics Server Module
#'
#' This function defines the server logic for the file upload and diagnostics
#' tab. It handles reading uploaded files, performing diagnostic checks, and
#' rendering the results. It returns a list of reactive objects to be used by
#' other modules.
#'
#' @param id A character string for the namespace.
#'
#' @return A list of reactive expressions.
#' @noRd
mod_file_upload_server <- function(id) {
  moduleServer(id, function(input, output, session) {


    # --- Help Text Logic ---
    hide <- reactiveValues(hide = TRUE)
    observeEvent(input$show_file_input_explanation, {
      hide$hide <- !hide$hide
    })

    output$file_input_explanation <- renderUI({
      if (!hide$hide) {
        HTML(help_button_page_1_text())
      }
    })

    # --- File Reading with Error Handling ---
    read_data_safely <- function(file_input) {
      req(file_input)

      tryCatch({
        ext <- tools::file_ext(file_input$name)
        switch(ext,
               xlsx = data.table::as.data.table(readxl::read_excel(file_input$datapath)),
               csv = data.table::fread(file_input$datapath),
               validate("Unsupported file type. Please upload a .xlsx or .csv file.")
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Error reading file:", e$message),
          type = "error",
          duration = 10
        )
        return(NULL)
      })
    }

    current_raw_cs_data_wide <- reactive({ read_data_safely(input$cs_data) })
    current_raw_eq_data_wide <- reactive({ read_data_safely(input$eq_data) })

    # --- Diagnostics ---

    current_diagnostics_cs <- reactive({
      req(current_raw_cs_data_wide())
      commutability::check_data(data = current_raw_cs_data_wide(), type = "cs")
    })

    current_diagnostics_eq <- reactive({
      req(current_raw_eq_data_wide())
      commutability::check_data(data = current_raw_eq_data_wide(), type = "eqam")
    })

    # --- Methods to Remove Globally ---
    methods_to_remove_globally <- reactive({
      req(current_diagnostics_cs(), current_diagnostics_eq())

      unique(c(
        current_diagnostics_cs()$repair$remove_these_methods,
        current_diagnostics_eq()$repair$remove_these_methods
      ))
    })

    current_diagnostics_both <- reactive({
      req(current_raw_cs_data_wide(), current_raw_eq_data_wide(), methods_to_remove_globally())
      cs_data_repaired <- commutability::repair_data(
        data = current_raw_cs_data_wide(),
        type = "cs",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
      eq_data_repaired <- commutability::repair_data(
        data = current_raw_eq_data_wide(),
        type = "eqam",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )

      keep_these_cs <- setdiff(names(cs_data_repaired), methods_to_remove_globally())
      keep_these_eq <- setdiff(names(eq_data_repaired), methods_to_remove_globally())

      if (length(keep_these_cs) > 0) {
        cs_data_repaired <- subset(cs_data_repaired, select = keep_these_cs)
      }
      if (length(keep_these_eq) > 0) {
        eq_data_repaired <- subset(eq_data_repaired, select = keep_these_eq)
      }

      commutability::check_equivalence(
        cs_data = cs_data_repaired,
        eq_data = eq_data_repaired
      )
    })

    # --- Validity Checks ---

    current_validity <- reactive({
      req(current_diagnostics_cs(), current_diagnostics_eq(), current_diagnostics_both())
      cs_ok <- current_diagnostics_cs()$badge != "not acceptable"
      eq_ok <- current_diagnostics_eq()$badge != "not acceptable"
      both_ok <- all(isTRUE(current_diagnostics_both()$equal_names), isTRUE(current_diagnostics_both()$equal_order))
      return(cs_ok && eq_ok && both_ok)
    })



    # --- Dynamic UI Updates ---

    observe({
      is_valid <- current_validity()

      # Start with potential choices
      choices <- if (is_valid) {
        cs_data_repaired <- commutability::repair_data(data = current_raw_cs_data_wide(), type = "cs", remove_invalid_methods = FALSE, include_repair_summary = FALSE)
        setdiff(names(cs_data_repaired), c("SampleID", "ReplicateID"))
      } else {
        character(0) # Return empty character vector if not valid
      }

      # Filter out methods that should be removed globally
      to_remove <- methods_to_remove_globally()
      if (length(to_remove) > 0) {
        choices <- setdiff(choices, to_remove)
      }

      updateVirtualSelect(
        session = session,
        inputId = "reference_method",
        choices = c("none", choices),
        selected = "none",
        disable = !is_valid
      )
    })

    output$warning_placeholder <- renderUI({
      if (input$ignore_invalid_data == TRUE) {
        div(
          class = "input-warning-note",
          icon("exclamation-triangle"),
          "This is not recommended! Proceeding with invalid data may lead to unreliable results or cause the application to crash."
        )
      } else {
        NULL
      }
    })

    # --- Render Outputs ---

    output$cs_table_diagnostics <- function() {
      req(current_diagnostics_cs())
      render_diagnostic_table(current_diagnostics_cs(), type = "cs")
    }

    output$eq_table_diagnostics <- function() {
      req(current_diagnostics_eq())
      render_diagnostic_table(current_diagnostics_eq(), type = "eq")
    }

    output$cs_text_diagnostics <- renderUI({
      req(current_diagnostics_cs())
      render_diagnostic_text(current_diagnostics_cs(), type = "cs")
    })

    output$eq_text_diagnostics <- renderUI({
      req(current_diagnostics_eq())
      render_diagnostic_text(current_diagnostics_eq(), type = "eq")
    })

    output$both_text_diagnostics <- renderUI({
      req(current_diagnostics_both())
      full_diagnostics <- current_diagnostics_both()
      css_unit_test <- function(title, fail = FALSE) {
        if (!fail) {
          icon_html <- "<i class='fa-solid fa-check-circle' style='color: #28a745; margin-right: 5px;'></i>"
          badge_html <- "<span class='test-badge pass'>PASS</span>"
          class_name <- "test-item pass-test"
        } else {
          icon_html <- "<i class='fa-solid fa-times-circle' style='color: #dc3545; margin-right: 5px;'></i>"
          badge_html <- "<span class='test-badge fail'>FAIL</span>"
          class_name <- "test-item fail-test"
        }
        paste0("<div class='", class_name, "'>", icon_html, "<span class='test-title'>", title, ":</span>", badge_html, "</div>")
      }

      message_equivalent_names <- css_unit_test("Equal IVD-MD Names", fail = !full_diagnostics$equal_names)
      if (full_diagnostics$equal_names) {
        message_equivalent_order <- css_unit_test("Equal IVD-MD Column Order", fail = !full_diagnostics$equal_order)
      }
      else {
        message_equivalent_order <- css_unit_test("Equal IVD-MD Column Order", fail = TRUE)
      }

      HTML(paste(
        "<div class='dashboard-container'>",
        "<div class='section-header'><i class='fa-solid fa-clipboard-list' style='color: #a5682a;'></i><span>Structural Agreement Tests:</span></div>",
        message_equivalent_names,
        message_equivalent_order,
        "</div>"
      ))
    })

    # --- Return Values for Other Modules ---

    return(
      list(
        raw_cs_data = current_raw_cs_data_wide,
        raw_eq_data = current_raw_eq_data_wide,
        remove_ivd_mds = methods_to_remove_globally,
        reference_method = reactive({ if (input$reference_method == "none") NULL else input$reference_method }),
        is_valid = reactive({
          if (input$ignore_invalid_data == "TRUE") return(TRUE)
          current_validity()
        })
      )
    )
  })
}
