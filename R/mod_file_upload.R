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
      class = "page-header",
      h1(
        class = "main-title",
        icon("upload"),
        "Data Upload & Validation"
      ),
      glassButton(
        inputId = ns("show_file_input_explanation"),
        label = "Show Help Text",
        icon = icon("circle-question"),
        color = "green"
      )
    ),
    glassTogglePanel(
      triggerId = ns("show_file_input_explanation"),
      help_button_page_1_text()
    ),
    glassRow(
      glassCol(
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
              help_text = "Measurements from Samples that one wish to do."
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
              help_text = "External Quality Assessment Material data..."
            )
          )
        )
      ),

      # Høyre Kolonne (Configuration)
      glassCol(
        width = 6,
        glassCard(
          inputId = ns("card_configuration"),
          title = "Configuration",
          icon = icon("hammer"),
          collapsible = FALSE,
          width = "100%",
          div(
            class = "parameter-section",
            glassDropdown(
              inputId = ns("reference_method"),
              label = "Reference Method",
              label_icon = icon("arrow-pointer"),
              choices = "none",
              selected = "none",
              disabled = TRUE,
              width = "100%"
            ),
            glassRadioButtons(
              inputId = ns("ignore_invalid_data"),
              label = "Ignore Failed Validation Tests",
              label_icon = icon("bug-slash"),
              choices = c("Yes", "No"),
              selected = "No",
              width = "100%"
            )
          ),
          uiOutput(ns("warning_placeholder"))
        )
      )
    ),

    # --- Diagnostic Section ---
    glassResultCard(
      inputId = ns("card_diagnostics_main"),
      title = "Diagnostic Overview",
      icon = icon("stethoscope"),
      width = "100%",
      toolbar = uiOutput(
        outputId = ns("overall_diagnostics_status")
      ),
      glassTabsetPanel(
        inputId = ns("diag_tabset"),
        glassTabPanel(
          title = "Clinical Samples",
          value = "tab_cs",
          icon = uiOutput(ns("status_icon_cs"), inline = TRUE),
          glassCard(
            inputId = ns("card_inner_cs"),
            title = "Clinical Sample Data Diagnostics",
            icon = icon("microscope"),
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            uiOutput(outputId = ns("cs_table_diagnostics")),
            htmlOutput(outputId = ns("cs_text_diagnostics"))
          )
        ),
        glassTabPanel(
          title = "EQAM Data",
          value = "tab_eq",
          icon = uiOutput(ns("status_icon_eq"), inline = TRUE),
          glassCard(
            inputId = ns("card_inner_eq"),
            title = "EQAM Data Diagnostics",
            icon = icon("vial"),
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            uiOutput(outputId = ns("eq_table_diagnostics")),
            htmlOutput(outputId = ns("eq_text_diagnostics"))
          )
        ),
        glassTabPanel(
          title = "Agreement",
          value = "tab_agreement",
          icon = uiOutput(ns("status_icon_ag"), inline = TRUE),
          glassCard(
            inputId = ns("card_inner_ag"),
            title = "Structural Agreement Diagnostics",
            icon = icon("check-double"),
            collapsible = TRUE,
            collapsed = FALSE,
            width = "100%",
            htmlOutput(outputId = ns("both_text_diagnostics"))
          )
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
  # --- Create the Module Server for the `File Upload` Section ---
  moduleServer(id, function(input, output, session) {

    # --- Helper Functions ---
    ns <- session$ns

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

      # --- Perform Structural Agreement Checks (Logic moved to utility function) ---
      out <- perform_structural_checks(
        cs_data = current_raw_cs_data_wide(),
        eq_data = current_raw_eq_data_wide(),
        methods_to_remove = methods_to_remove_globally()
      )

      return(out)

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
    output$overall_diagnostics_status <- renderUI({
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

    # --- DYNAMIC ICON RENDERING -----------------------------------------------
    # This allows the UI structure to stay in mod_file_upload_ui

    # 1. CS Icon
    output$status_icon_cs <- renderUI({
      diag <- current_diagnostics_cs()
      if (!is.null(diag) && diag$badge == "not acceptable") {
        icon("circle-exclamation", class = "text-danger")
      } else {
        icon("microscope")
      }
    })

    # 2. EQ Icon
    output$status_icon_eq <- renderUI({
      diag <- current_diagnostics_eq()
      if (!is.null(diag) && diag$badge == "not acceptable") {
        icon("circle-exclamation", class = "text-danger")
      } else {
        icon("flask")
      }
    })

    # 3. Agreement Icon
    output$status_icon_ag <- renderUI({
      diag <- current_diagnostics_both()
      if (!is.null(diag) && (!isTRUE(diag$equal_names) || !isTRUE(diag$equal_order))) {
        icon("circle-exclamation", class = "text-danger")
      } else {
        icon("check-double")
      }
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

      out <- post_repair_diagnostics(
        data = current_raw_cs_data_wide(),
        prior_repair_diagnostics = current_diagnostics_cs(),
        methods_to_remove = methods_to_remove_globally(),
        type = "cs"
      )
      # Out can be list in future. Make it easy to expand
      return(out)

    })

    # --- Get the Post-Repair Diagnostics for the EQAM Data --------------------
    post_repair_diagnostics_eq <- reactive({
      # --- Require uploaded EQAM data and its diagnostics ---
      req(
        current_raw_eq_data_wide(),
        current_diagnostics_eq()
      )

      out <- post_repair_diagnostics(
        data = current_raw_eq_data_wide(),
        prior_repair_diagnostics = current_diagnostics_eq(),
        methods_to_remove = methods_to_remove_globally(),
        type = "eqam"
      )
      # Out can be list in future. Make it easy to expand
      return(out)
    })

    # --- Render Diagnostics Tables --------------------------------------------

    # --- Render Clinical Sample Diagnostics Table -----------------------------
    output$cs_table_diagnostics <- renderUI({
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
    })

    # --- Render EQAM Diagnostics Table ----------------------------------------
    output$eq_table_diagnostics <- renderUI({
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
    })

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
      render_agreement_text(current_diagnostics_both())
    })

    outputOptions(output, "overall_diagnostics_status", suspendWhenHidden = FALSE)
    outputOptions(output, "cs_table_diagnostics", suspendWhenHidden = FALSE)
    outputOptions(output, "cs_text_diagnostics", suspendWhenHidden = FALSE)
    outputOptions(output, "eq_table_diagnostics", suspendWhenHidden = FALSE)
    outputOptions(output, "eq_text_diagnostics", suspendWhenHidden = FALSE)
    outputOptions(output, "both_text_diagnostics", suspendWhenHidden = FALSE)

    is_valid <- reactive({
      if (isTRUE(input$ignore_invalid_data == "Yes")) {
        return(TRUE)
      }
      current_validity()
    })


    # Vi lytter direkte på is_valid()
    observeEvent(is_valid(), {
      val_status <- is_valid()
      should_pulse <- isTRUE(val_status)
      # Aktiver/Deaktiver puls på "dins" fanen
      updateGlassSidebarHighlight(session, "dins", enable = should_pulse)
    })

    # --- Return Values for Other Modules ---
    return(
      list(
        raw_cs_data = current_raw_cs_data_wide,
        raw_eq_data = current_raw_eq_data_wide,
        remove_ivd_mds = methods_to_remove_globally,
        reference_method = reactive({ if (input$reference_method == "none") NULL else input$reference_method }),
        is_valid = is_valid,
        diagnostics_cs = current_diagnostics_cs,
        diagnostics_eq = current_diagnostics_eq,
        diagnostics_both = current_diagnostics_both
      )
    )
  })
}
