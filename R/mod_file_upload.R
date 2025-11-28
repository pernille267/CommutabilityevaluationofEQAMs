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
        "Data Upload & Validation"
      ),
      glassButton(
        inputId = ns("show_file_input_explanation"),
        label = "Show Help Text",
        icon = icon("circle-question")
      )
    ),
    glassTogglePanel(
      triggerId = ns("show_file_input_explanation"),
      help_button_page_1_text()
    ),
    fluidRow(
      column(
        width = 6,
        glassCard(
          inputId = ns("card_file_selection"),
          title = "File Selection",
          icon = icon("file-upload"),
          collapsible = FALSE,
          width = "100%",
          div(
            class = "upload-section",
            glassFileInput(
              inputId = ns("cs_data"),
              label = "Clinical Samples",
              label_icon = icon("vial"),
              accept = c(".xlsx", ".csv"),
              button_label = "Browse...",
              button_label_icon = icon("file-upload"),
              placeholder = "No clinical samples here",
              help_text = paste0(
                "Measurements from Samples that one wish to do."
              )
            )
          ),
          div(
            class = "upload-section",
            glassFileInput(
              inputId = ns("eq_data"),
              label = "EQAM Data",
              label_icon = icon("chart-line"),
              accept = c(".xlsx", ".csv"),
              button_label = "Browse...",
              button_label_icon = icon("file-upload"),
              placeholder = "No EQAM data currently selected",
              help_text = paste0(
                "EQAM is an abbreviation for external quality assessment ",
                "material. However, it does not have to be EQAM data. It ",
                "can also be certified reference material (CRM) data, or ",
                "other materials that is sensible to evaluate for ",
                "commutability."
              )
            )
          )
        )
      ),
      column(
        width = 6,
        glassCard(
          inputId = ns("card_configuration"),
          title = "Configuration",
          icon = icon("hammer"),
          collapsible = FALSE,
          width = "100%",
          div(
            class = "parameter-section",
            h4(
              icon("arrow-pointer", class = "section-icon"),
              "Reference Method"
            ),
            glassDropdown(
              inputId = ns("reference_method"),
              choices = "none",
              selected = "none",
              disabled = TRUE,
              width = "100%"
            )
          ),
          div(
            class = "parameter-section",
            h4(icon("snowflake", class = "section-icon"), "Ignore Failed Validation Tests"),
            glassRadioButtons(
              inputId = ns("ignore_invalid_data"),
              choices = c("Yes", "No"),
              selected = "No",
              width = "100%"
            ),
            uiOutput(ns("warning_placeholder"))
          )
        )
      )
    ),

    # --- Diagnostic Section ---

    # Overall Status
    glassResultCard(
      inputId = ns("card_status"),
      title = "Diagnostic Overview",
      icon = icon("stethoscope"),
      width = "100%",
      toolbar = uiOutput(
        outputId = ns("overall_diagnostic_status")
      )
    ),

    # Detailed Diagnostics (Split into separate collapsible Glass Cards)
    glassCard(
      inputId = ns("card_diag_cs"),
      title = "Clinical Sample Data Diagnostics",
      icon = icon("microscope"),
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::tableOutput(outputId = ns("cs_table_diagnostics")),
      htmlOutput(outputId = ns("cs_text_diagnostics"))
    ),

    glassCard(
      inputId = ns("card_diag_eq"),
      title = "External Quality Assessment Material Data Diagnostics",
      icon = icon("flask"),
      collapsible = TRUE,
      collapsed = TRUE,
      shiny::tableOutput(outputId = ns("eq_table_diagnostics")),
      htmlOutput(outputId = ns("eq_text_diagnostics"))
    ),

    glassCard(
      inputId = ns("card_diag_agreement"),
      title = "Structural Agreement Between Data Diagnostics",
      icon = icon("check-double"),
      collapsible = TRUE,
      collapsed = TRUE,
      htmlOutput(outputId = ns("both_text_diagnostics"))
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
  # --- Create the Module Server for the `File Upload` Section ---
  moduleServer(id, function(input, output, session) {


    # --- File Reading with Error Handling -------------------------------------
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

    # --- Read Data ------------------------------------------------------------
    current_raw_cs_data_wide <- reactive({
      req(input$cs_data)
      standardized_file <- processGlassFile(input$cs_data)
      read_data_safely(standardized_file)
    })

    current_raw_eq_data_wide <- reactive({
      req(input$eq_data)
      standardized_file <- processGlassFile(input$eq_data)
      read_data_safely(standardized_file)
    })

    # --- Diagnostics ----------------------------------------------------------

    # --- Diagnostics of Clinical Sample Data ----------------------------------
    current_diagnostics_cs <- reactive({
      req(current_raw_cs_data_wide())
      commutability::check_data(data = current_raw_cs_data_wide(), type = "cs")
    })

    # --- Diagnostics of EQAM Data ---------------------------------------------
    current_diagnostics_eq <- reactive({
      req(current_raw_eq_data_wide())
      commutability::check_data(data = current_raw_eq_data_wide(), type = "eqam")
    })

    # --- Methods to Remove Globally -------------------------------------------
    methods_to_remove_globally <- reactive({
      req(
        current_diagnostics_cs(),
        current_diagnostics_eq()
      )
      # --- Gather invalid methods from both diagnostics objects ---
      unique(c(
        current_diagnostics_cs()$repair$remove_these_methods,
        current_diagnostics_eq()$repair$remove_these_methods
      ))
    })

    # --- Equivalence Diagnostics of Clinical Sample and EQAM data -------------
    current_diagnostics_both <- reactive({
      req(
        current_raw_cs_data_wide(),
        current_raw_eq_data_wide()
      )

      # --- Attempt to repair clinical sample data ---
      cs_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = current_raw_cs_data_wide(),
            type = "cs",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        },
        error = function(e) NULL,
        warning = function(e) NULL
      )

      # --- Attempt to repair clinical sample data ---
      eq_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = current_raw_eq_data_wide(),
            type = "eqam",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        }
      )

      # --- Check if an error or a warning occured during attempted repair ---
      if (is.null(cs_data_repaired) | is.null(eq_data_repaired)) {
        # --- Check if an invalid IVD-MD is the reason ---
        # NOTE: The problematic IVD-MD must be both in clinical sample data
        # and in the EQAM data ...
        if (!is.null(methods_to_remove_globally())) {
          invalid_methods_exist_in_cs_data <- all(
            x = methods_to_remove_globally() %in% names(current_raw_cs_data_wide())
          )
          invalid_methods_exist_in_eq_data <- all(
            x = methods_to_remove_globally() %in% names(current_raw_eq_data_wide())
          )
          # Fallback if clinical sample and eqam data do not have the same names
          if (!(invalid_methods_exist_in_cs_data & invalid_methods_exist_in_eq_data)) {
            out <- list(
              "equal_names" = FALSE,
              "equal_order" = FALSE,
              "names_in_cs_data_but_not_in_eq_data" = setdiff(
                x = names(current_raw_cs_data_wide()),
                y = names(current_raw_eq_data_wide())
              ),
              "names_in_eq_data_but_not_in_cs_data" = setdiff(
                x = names(current_raw_eq_data_wide()),
                y = names(current_raw_cs_data_wide())
              ),
              "order_cs_data" = paste(
                names(current_raw_cs_data_wide()),
                collapse = ", "
              ),
              "order_eq_data" = paste(
                names(current_raw_eq_data_wide()),
                collapse = ", "
              ),
              "error" = paste0(
                "Tried to remove invalid columns, but it appears that some of ",
                "the invalid columns are not in both datasets."
              )
            )
            # --- Return fallback object ---
            # Note: Matches syntax-wise output from
            # commutability::check_equivalence
            return(out)
          }

          # Check which columns to keep
          keep_these_cs <- setdiff(
            x = names(cs_data_repaired),
            y = methods_to_remove_globally()
          )
          keep_these_eq <- setdiff(
            x = names(eq_data_repaired),
            y = methods_to_remove_globally()
          )

          # Remove invalid methods from both datasets
          cs_data_repaired <- subset(
            current_raw_cs_data_wide(),
            select = keep_these_cs
          )
          eq_data_repaired <- subset(
            current_raw_eq_data_wide(),
            select = keep_these_eq
          )

          # --- Attempt to repair clinical sample data (again) ---
          cs_data_repaired <- tryCatch(
            expr = {
              commutability::repair_data(
                data = cs_data_repaired,
                type = "cs",
                remove_invalid_methods = FALSE,
                include_repair_summary = FALSE
              )
            },
            error = function(e) NULL,
            warning = function(e) NULL
          )

          # --- Attempt to repair clinical sample data (again) ---
          eq_data_repaired <- tryCatch(
            expr = {
              commutability::repair_data(
                data = eq_data_repaired,
                type = "eqam",
                remove_invalid_methods = FALSE,
                include_repair_summary = FALSE
              )
            }
          )

          # --- Fallback if second repair attempt failed ---
          if (is.null(cs_data_repaired) | is.null(eq_data_repaired)) {
            out <- list(
              "equal_names" = FALSE,
              "equal_order" = FALSE,
              "names_in_cs_data_but_not_in_eq_data" = setdiff(
                x = names(current_raw_cs_data_wide()),
                y = names(current_raw_eq_data_wide())
              ),
              "names_in_eq_data_but_not_in_cs_data" = setdiff(
                x = names(current_raw_eq_data_wide()),
                y = names(current_raw_cs_data_wide())
              ),
              "order_cs_data" = paste(
                names(current_raw_cs_data_wide()),
                collapse = ", "
              ),
              "order_eq_data" = paste(
                names(current_raw_eq_data_wide()),
                collapse = ", "
              ),
              "error" = paste0(
                "Tried to repair data twice, but it still did not ",
                "work as intended..."
              )
            )
            # --- Return fallback object ---
            # Note: Matches syntax-wise output from
            # commutability::check_equivalence
            return(out)
          }

          # --- Check equivalence after tried to fix custom issues ---
          out <- tryCatch(
            expr = {
              commutability::check_equivalence(
                cs_data = cs_data_repaired,
                eq_data = eq_data_repaired
              )
            },
            error = function(e) NULL,
            warning = function(w) NULL
          )

          # --- Fallback if the equivalence check fails ---
          if (is.null(out)) {
            out <- list(
              "equal_names" = FALSE,
              "equal_order" = FALSE,
              "names_in_cs_data_but_not_in_eq_data" = setdiff(
                x = names(current_raw_cs_data_wide()),
                y = names(current_raw_eq_data_wide())
              ),
              "names_in_eq_data_but_not_in_cs_data" = setdiff(
                x = names(current_raw_eq_data_wide()),
                y = names(current_raw_cs_data_wide())
              ),
              "order_cs_data" = paste(
                names(current_raw_cs_data_wide()),
                collapse = ", "
              ),
              "order_eq_data" = paste(
                names(current_raw_eq_data_wide()),
                collapse = ", "
              ),
              "error" = paste0(
                "Tried to perform equivalence test after working with ",
                "weird data, but it resulted in some unknown error."
              )
            )
            # --- Return fallback object ---
            # Note: Matches syntax-wise output from
            # commutability::check_equivalence
            return(out)
          }
          return(out)
        }
      }

      # --- Remove invalid methods ---
      if (!is.null(methods_to_remove_globally())) {

        # Check which columns to keep
        keep_these_cs <- setdiff(
          x = names(cs_data_repaired),
          y = methods_to_remove_globally()
        )
        keep_these_eq <- setdiff(
          x = names(eq_data_repaired),
          y = methods_to_remove_globally()
        )
        cs_data_repaired <- subset(cs_data_repaired, select = keep_these_cs)
        eq_data_repaired <- subset(eq_data_repaired, select = keep_these_eq)
      }

      commutability::check_equivalence(
        cs_data = cs_data_repaired,
        eq_data = eq_data_repaired
      )
    })

    # --- Assess General Validity of Uploaded Data -----------------------------
    current_validity <- reactive({
      # --- Require diagnostics have been run before running ---
      req(
        current_diagnostics_cs(),
        current_diagnostics_eq(),
        current_diagnostics_both()
      )
      cs_ok <- current_diagnostics_cs()$badge != "not acceptable"
      eq_ok <- current_diagnostics_eq()$badge != "not acceptable"
      both_ok <- all(
        isTRUE(current_diagnostics_both()$equal_names),
        isTRUE(current_diagnostics_both()$equal_order),
        isTRUE(is.null(current_diagnostics_both()$error))
      )
      return(cs_ok && eq_ok && both_ok)
    })

    # --- Logic for Overall Status Badge ---------------------------------------
    # --- Notes ----------------------------------------------------------------
    # This is a single indicator that tells the user whether they can move
    # forward or not
    output$overall_diagnostic_status <- renderUI({
      # --- Require diagnostics have been run before showing ---
      req(
        current_diagnostics_cs(),
        current_diagnostics_eq(),
        current_diagnostics_both()
      )

      # Record the current validity of uploaded data
      is_valid <- current_validity()

      # Toggle whether failed validation tests should be ignored
      ignore_issues <- input$ignore_invalid_data == "Yes"

      if (is_valid) {
        status_class <- "status-ok"
        status_icon <- icon("check-circle")
        status_text <- "All Checks Passed"
      }
      else if (ignore_issues) {
        status_class <- "status-warning"
        status_icon <- icon("exclamation-triangle")
        status_text <- "Proceeding with Issues"
      }
      else {
        status_class <- "status-fail"
        status_icon <- icon("times-circle")
        status_text <- "Validation Failed"
      }

      # Construct the HTML for the badge
      div(
        class = paste(
          "diagnostic-status-badge",
          status_class
        ),
        status_icon,
        span(status_text)
      )
    })

    # --- Dynamic UI Updates ---------------------------------------------------

    # --- Observer that updates the list of valid reference method choices -----
    observe({
      # --- Record current validity at this point in time ---
      is_valid <- current_validity()

      # --- Start with all potential choices for reference methods ---
      choices <- if (is_valid) {
        cs_data_repaired <- commutability::repair_data(
          data = current_raw_cs_data_wide(),
          type = "cs",
          remove_invalid_methods = FALSE,
          include_repair_summary = FALSE
        )
        # Extract only the method columns
        setdiff(names(cs_data_repaired), c("SampleID", "ReplicateID"))
      }
      else {
        character(0) # Return empty character vector if not valid
      }

      # --- Remove methods from the list that are deemed invalid ---
      # --- Notes ---
      # (Rationale) Invalid methods should not be eligible as reference methods
      to_remove <- methods_to_remove_globally()
      if (length(to_remove) > 0) {
        choices <- setdiff(choices, to_remove)
      }

      # Handle logical check for character input
      should_enable <- is_valid | (input$ignore_invalid_data == "Yes")

      # --- Update list of choices for reference method ---
      updateGlassDropdown(
        session = session,
        inputId = "reference_method",
        choices = c("none", choices),
        selected = "none",
        disable = !should_enable
      )
    })

    # --- Deliver warning if user desires to ignore failed validation tests ----
    output$warning_placeholder <- renderUI({
      if (isTRUE(input$ignore_invalid_data == "Yes")) {
        div(
          class = "input-warning-note",
          icon("exclamation-triangle"),
          paste0(
            "This is not recommended! Proceeding with invalid data may lead ",
            "to unreliable results or cause the application to crash."
          )
        )
      }
      else {
        NULL
      }
    })

    # --- Get the Post-Repair Diagnostics for the Clinical Sample Data ---------
    post_repair_diagnostics_cs <- reactive({
      # --- Require uploaded clinical sample data and its diagnostics ---
      req(
        current_raw_cs_data_wide(),
        current_diagnostics_cs()
      )

      # --- Try to repair clinical sample data ---
      repaired_data <- tryCatch(
        expr = {
          commutability::repair_data(
            data = current_raw_cs_data_wide(),
            type = "cs",
          )
        },
        error = function(e) "error",
        warning = function(w) "warning"
      )

      # --- if character, either a warning or an error occurred ---
      if (is.character(repaired_data)) {
        # --- Return original diagnostics object if error in repair ---
        if (repaired_data == "error") {
          return(current_diagnostics_cs())
        }
        else if (repaired_data != "warning") {
          return(current_diagnostics_cs())
        }

        # --- Try to handle the warning if warning in repair ---

        # --- `not acceptable` can be due to invalid methods ---
        if (current_diagnostics_cs$badge == "not acceptable") {
          # --- Check for invalid methods ---
          if (!is.null(methods_to_remove_globally())) {
            temp_raw_cs_data_wide <- subset(
              x = current_raw_cs_data_wide(),
              selected = setdiff(
                names(current_raw_cs_data_wide()),
                methods_to_remove_globally()
              )
            )
            # --- Try repairing data after removing invalid methods ---
            repaired_data <- tryCatch(
              expr = {
                commutability::repair_data(
                  data = temp_raw_cs_data_wide,
                  type = "cs"
                )
              },
              error = function(e) NULL,
              warning = function(w) NULL
            )
            # --- Check if second repair is valid ---
            if (!is.null(repaired_data)) {
              return(
                commutability::check_data(
                  data = repaired_data,
                  type = "cs"
                )
              )
            }
            # --- We failed to handle cause of warning, return original ---
            else {
              return(
                current_diagnostics_cs()
              )
            }
          }
        }
        # --- `acceptable+` (another UNKNOWN reason for the warning) ---
        else {
          return(
            current_diagnostics_cs()
          )
        }
      }

      # --- If first repair was a success from the start ---
      commutability::check_data(data = repaired_data, type = "cs")
    })

    # --- Get the Post-Repair Diagnostics for the EQAM Data --------------------
    post_repair_diagnostics_eq <- reactive({
      # --- Require uploaded EQAM data and its diagnostics ---
      req(
        current_raw_eq_data_wide(),
        current_diagnostics_eq()
      )
      # --- Try to repair EQAM data ---
      repaired_data <- tryCatch(
        expr = {
          commutability::repair_data(
            data = current_raw_eq_data_wide(),
            type = "eqam"
          )
        },
        error = function(e) "error",
        warning = function(w) "warning"
      )

      #browser()

      # --- if character, either a warning or an error occurred ---
      if (is.character(repaired_data)) {
        # --- Return original diagnostics object if error in repair ---
        if (repaired_data == "error") {
          return(current_diagnostics_eq())
        }
        else if (repaired_data != "warning") {
          return(current_diagnostics_eq())
        }

        # --- Try to handle the warning if warning in repair ---

        # --- `not acceptable` can be due to invalid methods ---
        if (current_diagnostics_eq$badge == "not acceptable") {
          # --- Check for invalid methods ---
          if (!is.null(methods_to_remove_globally())) {
            temp_raw_eq_data_wide <- subset(
              x = current_raw_eq_data_wide(),
              selected = setdiff(
                names(current_raw_eq_data_wide()),
                methods_to_remove_globally()
              )
            )
            # --- Try repairing data after removing invalid methods ---
            repaired_data <- tryCatch(
              expr = {
                commutability::repair_data(
                  data = temp_raw_eq_data_wide,
                  type = "eqam"
                )
              },
              error = function(e) NULL,
              warning = function(w) NULL
            )
            # --- Check if second repair is valid ---
            if (!is.null(repaired_data)) {
              return(
                commutability::check_data(
                  data = repaired_data,
                  type = "eqam"
                )
              )
            }
            # --- We failed to handle cause of warning, return original ---
            else {
              return(
                current_diagnostics_eq()
              )
            }
          }
        }
        # --- `acceptable+` (another UNKNOWN reason for the warning) ---
        else {
          return(
            current_diagnostics_eq()
          )
        }
      }

      # --- If first repair was a success from the start ---
      commutability::check_data(data = repaired_data, type = "eqam")
    })

    # --- Render Diagnostics Tables --------------------------------------------

    # --- Render Clinical Sample Diagnostics Table -----------------------------
    output$cs_table_diagnostics <- function() {
      req(
        current_diagnostics_cs(),
        post_repair_diagnostics_cs()
      )
      render_diagnostic_table(
        diagnostics = current_diagnostics_cs(),
        type = "cs",
        is_post_repair_valid = current_diagnostics_cs()$badge != "not acceptable",
        post_repair_score = post_repair_diagnostics_cs()$score
      )
    }

    # --- Render EQAM Diagnostics Table ----------------------------------------
    output$eq_table_diagnostics <- function() {
      req(
        current_diagnostics_eq(),
        post_repair_diagnostics_eq()
      )
      render_diagnostic_table(
        diagnostics = current_diagnostics_eq(),
        type = "eq",
        is_post_repair_valid = current_diagnostics_eq()$badge != "not acceptable",
        post_repair_score = post_repair_diagnostics_eq()$score
      )
    }

    # --- Render Diagnostics Texts ---------------------------------------------

    # --- Render Clinical Sample Diagnostics Text ------------------------------
    output$cs_text_diagnostics <- renderUI({
      req(current_diagnostics_cs())
      render_diagnostic_text(
        current_diagnostics_cs(),
        current_diagnostics_eq(),
        type = "cs"
      )
    })

    # --- Render EQAM Diagnostics Text -----------------------------------------
    output$eq_text_diagnostics <- renderUI({
      req(current_diagnostics_eq())
      render_diagnostic_text(
        current_diagnostics_eq(),
        current_diagnostics_cs(),
        type = "eq"
      )
    })

    # --- Render Equivalence Diagnostics Text ----------------------------------
    output$both_text_diagnostics <- renderUI({
      req(current_diagnostics_both())
      full_diagnostics <- current_diagnostics_both()

      css_unit_test <- function(title, fail = FALSE) {
        if (!fail) {
          icon_html <- "<i class='fa fa-check-circle' style='color: #28a745; margin-right: 5px;'></i>"
          badge_html <- "<span class='test-badge pass'>PASS</span>"
          class_name <- "test-item pass-test"
        }
        else {
          icon_html <- "<i class='fa fa-times-circle' style='color: #dc3545; margin-right: 5px;'></i>"
          badge_html <- "<span class='test-badge fail'>FAIL</span>"
          class_name <- "test-item fail-test"
        }
        paste0("<div class='", class_name, "'>", icon_html, "<span class='test-title'>", title, ":</span>", badge_html, "</div>")
      }

      message_equivalent_names <- css_unit_test("Equal IVD-MD Names", fail = !full_diagnostics$equal_names)

      if (full_diagnostics$equal_names) {
        message_equivalent_order <- css_unit_test("Equal IVD-MD Column Order", fail = !full_diagnostics$equal_order)
      } else {
        message_equivalent_order <- css_unit_test("Equal IVD-MD Column Order", fail = TRUE)
      }

      # --- Removed <div class='dashboard-container'> wrapper ---
      HTML(paste(
        "<div class='section-header'><i class='fa fa-clipboard-list' style='color: #a5682a;'></i><span>Structural Agreement Tests:</span></div>",
        message_equivalent_names,
        message_equivalent_order
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
          if (isTRUE(input$ignore_invalid_data == "Yes")) return(TRUE)
          current_validity()
        }),
        diagnostics_cs = current_diagnostics_cs,
        diagnostics_eq = current_diagnostics_eq,
        diagnostics_both = current_diagnostics_both
      )
    )
  })
}
