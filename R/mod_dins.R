#' Differences in Nonselectivity UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the differences in nonselectivity module.
#' @noRd
mod_dins_ui <- function(id) {
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
        icon("arrows-left-right-to-line"), "Evaluate Differences in Nonselectivity"
      ),
      actionBttn(
        inputId = ns("show_dins_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        style = "gradient",
        color = "success"
      )
    ),
    htmlOutput(outputId = ns("dins_explanation")),

    # Data Transformation and Model Selection Panel
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon(
          "exchange-alt",
          class = "header-icon"
        ),
        h3("Choose Transformation and Model")
      ),
      div(
        class = "card-body",
        fluidRow(
          column(
            width = 6,
            div(
              class = "parameter-section",
              h5("Transformation"),
              radioGroupButtons(
                inputId = ns("transformation"),
                label = NULL,
                choiceValues = c("identity", "ln", "boxcox"),
                choiceNames = list(
                  HTML("<i class='fa fa-ban'></i>"),
                  HTML("<i class='fa fa-chart-line'></i> Log"),
                  HTML("<i class='fa fa-divide'></i> Box-Cox")
                ),
                selected = "identity",
                status = "primary",
                justified = TRUE
              )
            )
          ),
          column(
            width = 6,
            div(
              class = "parameter-section",
              h5("Regression Model"),
              radioGroupButtons(
                inputId = ns("pi_method"),
                label = NULL,
                choiceValues = c("fg", "ss"),
                choiceNames = c(
                  "Deming",
                  "Smoothing Spline"
                ),
                selected = "fg",
                status = "primary",
                justified = TRUE
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 6,
            div(
              class = "input-note",
              icon(name = "circle-info"),
              "The symbol",
              icon(name = "ban"),
              "signify that no transformation shoud be applied."
            )
          ),
          column(
            width = 6,
            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'ss'",
                ns("pi_method")
              ),
              div(
                class = "parameter-section",
                h5("Weighted Smoothing Spline"),
                radioGroupButtons(
                  inputId = ns("ss_weighted"),
                  choiceNames = c("No", "Yes"),
                  choiceValues = c("No", "Yes"),
                  selected = "No",
                  size = "sm",
                  status = "primary",
                  justified = TRUE
                )
              )
            )
          )
        )
      )
    ),

    # Calculate zeta Values for Each IVD-MD Pair
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("calculator", class = "header-icon"),
        h3("Estimate Differences in Nonselectivity"),
        div(
          class = "text-center mb-4",
          actionBttn(inputId = ns("calculate_zetas"),
                     label = "Estimate",
                     icon = icon(name = "calculator"),
                     style = "gradient",
                     color = "royal"),
          actionBttn(inputId = ns("clear_zetas"),
                     label = NULL,
                     icon = icon(name = "trash"),
                     style = "gradient",
                     color = "primary")
        ),

      ),
      div(
        class = "card-body",
        withSpinner(ui_element = DT::DTOutput(outputId = ns("calculated_zetas")),
                    type = 6,
                    color = "#605CA8",
                    hide.ui = TRUE)
      )
    ),

    # Estimation of Imprecision
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("bullseye", class = "header-icon"),
        h3("Estimate IVD-MD Imprecision"),
        div(
          class = "text-center mb-4",
          actionBttn(inputId = ns("calculate_imprecision"),
                     label = "Estimate",
                     icon = icon("bullseye"),
                     style = "gradient",
                     color = "royal"),
          actionBttn(inputId = ns("clear_imprecision"),
                     label = NULL,
                     icon = icon(name = "trash"),
                     style = "gradient",
                     color = "primary")
        ),
      ),
      div(
        class = "card-body",
        withSpinner(ui_element = DT::DTOutput(outputId = ns("im_results")),
                    type = 6,
                    color = "#28A745",
                    hide.ui = TRUE)
      )
    ),

    # Differences in Nonselectivity Options
    div(
      class = "dashboard-card",
      div(
        class = "card-header",
        icon("balance-scale", class = "header-icon"),
        h3("Differences in Nonselectivity Options")
      ),
      div(
        class = "card-body",
        fluidRow(
          column(
            width = 6,
            div(
              class = "parameter-section",
              h5("Set M(%) - Threshold for Maximum Tolerable Differences in Nonselectivity"),
              sliderTextInput(
                inputId = ns("M"),
                label = NULL,
                animate = TRUE,
                choices = seq(0, 200, by = 5),
                selected = 25,
                post = " %",
                width = "100%"
              )
            )
          ),
          column(
            width = 6,
            uiOutput(ns("recommended_zeta_display"))
          )
        )
      )
    )
  )
}

#' Differences in Nonselectivity Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list containing outputs from the file upload module.
#'
#' @return A reactive list of user-selected parameters.
#' @noRd
mod_dins_server <- function(id, file_upload_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- Help Text Logic ---
    hide <- reactiveValues(hide = TRUE)
    observeEvent(input$show_dins_explanation, {
      hide$hide <- !hide$hide
    })

    # --- Render Help Text ---
    output$dins_explanation <- renderUI({
      if (!hide$hide) {
        HTML(help_button_page_2_text())
      }
    })

    # --- Reactive Data Preparation ---

    # --- Clinical Samples - Long-formatted - No Transformation ----------------
    raw_cs_data_long <- reactive({

      # Requires raw_cs_data and is_valid (to be TRUE) from first module
      req(
        file_upload_data$raw_cs_data(),
        file_upload_data$is_valid() == TRUE
      )

      # Try to Repair Clinical Sample Data
      cs_data_repaired <- tryCatch(
        expr = {
          commutability::repair_data(
            data = file_upload_data$raw_cs_data(),
            type = "cs",
            remove_invalid_methods = FALSE,
            include_repair_summary = FALSE
          )
        },
        error = function(e) "error",
        warning = function(w) "warning"
      )

      # If Repair Failed, it is due to invalid methods are too broken
      # Remove broken methods first and then attempt to repair again.
      if (is.character(cs_data_repaired)) {
        keep_these_cols <- setdiff(
          names(file_upload_data$raw_cs_data()),
          file_upload_data$remove_ivd_mds()
        )
        cs_data_repaired <- data.table::copy(
          file_upload_data$raw_cs_data()
        )[, keep_these_cols, with = FALSE]

        cs_data_repaired <- tryCatch(
          expr = {
            commutability::repair_data(
              data = cs_data_repaired,
              type = "cs",
              remove_invalid_methods = FALSE,
              include_repair_summary = FALSE
            )
          },
          error = function(e) "error",
          warning = function(w) "warning"
        )
        if (is.character(cs_data_repaired)) {
          return(NULL)
        }
      }
      else {
        keep_these_cols <- setdiff(
          names(file_upload_data$raw_cs_data()),
          file_upload_data$remove_ivd_mds()
        )
        cs_data_repaired <- subset(
          x = cs_data_repaired,
          select = keep_these_cols
        )
      }

      ref_method <- file_upload_data$reference_method()

      raw_data <- commutability::get_comparison_data(
        data = cs_data_repaired,
        reference = ref_method
      )

      return(raw_data)
    })

    # --- Clinical Samples - Long-formatted - With Transformation --------------
    cs_data_long <- reactive({
      req(raw_cs_data_long())
      transformation <- switch(
        input$transformation,
        "identity" = "identity",
        "ln" = "log#e",
        "boxcox" = "boxcox#0.5",
        "identity"
      )
      commutability::transform_data(
        data = raw_cs_data_long(),
        transformation = transformation
      )
    })

    # --- DINS Estimation Logic ------------------------------------------------
    # --- Activates if Relevant Button is Pressed ---
    # --- Event Reactive Checks if Estimate DINS Button is Pressed ---
    zetas_calculated <- eventReactive(input$calculate_zetas, {
      req(cs_data_long())

      zeta_method <- if (input$pi_method == "fg") {
        "ols"
      }
      else if (input$pi_method == "ss") {
        # Check if the conditional weighted input is "Yes"
        if (input$ss_weighted == "Yes") {
          "ssw"
        }
        else {
          "ss"
        }
      }

      commutability::estimate_zeta_data(
        data = cs_data_long(),
        B = NULL,
        method = zeta_method,
        M = 1,
        N = 1
      )
    })

    # --- IVD-MD Imprecision Estimation Logic ----------------------------------
    # --- Activates if Relevant Button is Pressed ---
    # --- Event Reactive Checks if Estimate Repeatability Button is Pressed ---
    imprecision_calculated <- eventReactive(input$calculate_imprecision, {
      # Requires raw_cs_data_long() to work
      req(
        raw_cs_data_long()
      )
      commutability::estimate_imprecision_data(
        data = raw_cs_data_long(),
        B = 1000L,
        type = "percentile",
        level = 0.95
      )
    })

    # --- UI Rendering for Tables ---
    # --- Control visibility of the tables ---

    # --- Show / Hide Dins Estimates Table ----
    show_zetas <- reactiveVal(FALSE)
    observeEvent(input$calculate_zetas, { show_zetas(TRUE) })
    observeEvent(input$clear_zetas, { show_zetas(FALSE) })

    # --- Show / Hide IVD-MD Repeatability Estimates Table ---
    show_imprecision <- reactiveVal(FALSE)
    observeEvent(input$calculate_imprecision, { show_imprecision(TRUE) })
    observeEvent(input$clear_imprecision, { show_imprecision(FALSE) })

    # --- Render Table of DINS Estimates ---
    output$calculated_zetas <- DT::renderDT({

      # Check first if DINS estimates should be showed
      if (show_zetas()) {
        out <- zetas_calculated()
        out$zeta <- format(
          x = out$zeta,
          nsmall = 2,
          digits = 2
        )

        names(out) <- c("IVD-MD Comparison", "zeta")

        DT::datatable(
          out,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scolllX = TRUE,
            scrollY = "400px",
            pageLength = 25,
            dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
            buttons = list(
              list(extend = 'copy', className = 'btn-dt'),
              list(extend = 'csv', className = 'btn-dt'),
              list(extend = 'excel', className = 'btn-dt'),
              list(extend = 'pdf', className = 'btn-dt'),
              list(extend = 'print', className = 'btn-dt')
            ),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('.dataTables_scrollBody').on('scroll', function() {",
              "    $(this).prev('.dataTables_scrollHead').scrollLeft($(this).scrollLeft());",
              "  });",
              "}"
            )
          )
        )
      }
    })

    # Render Imprecision Table
    output$im_results <- DT::renderDT({
      if (show_imprecision()) {
        imprecision_data <- imprecision_calculated()
        out <- MS_wise_imprecision(
          imprecision_data = imprecision_data,
          mode = "visual",
          percent = TRUE,
          rounding = 3L)

        names(out) <- c("IVD-MD", "CV % (95 % CI: lower - upper)", "SD (95 % CI: lower - upper)")

        # Hard coding. Replacing a (b, c) with a (b - c).
        out$`CV % (95 % CI: lower - upper)` <- gsub(", ", " - ", out$`CV % (95 % CI: lower - upper)`)
        out$`SD (95 % CI: lower - upper)` <- gsub(", ", " - ", out$`SD (95 % CI: lower - upper)`)

        DT::datatable(
          out,
          rownames = FALSE,
          extensions = 'Buttons',
          options = list(
            scolllX = TRUE,
            scrollY = "400px",
            pageLength = 25,
            dom = "Bfrtip", # B=Buttons, f=filtering, r=processing, t=table, i=info, p=pagination
            buttons = list(
              list(extend = 'copy', className = 'btn-dt'),
              list(extend = 'csv', className = 'btn-dt'),
              list(extend = 'excel', className = 'btn-dt'),
              list(extend = 'pdf', className = 'btn-dt'),
              list(extend = 'print', className = 'btn-dt')
            ),
            columnDefs = list(
              list(className = 'dt-center', targets = "_all")
            ),
            initComplete = JS(
              "function(settings, json) {",
              "  $(this.api().table().container()).find('.dataTables_scrollBody').on('scroll', function() {",
              "    $(this).prev('.dataTables_scrollHead').scrollLeft($(this).scrollLeft());",
              "  });",
              "}"
            )
          )
        )
      }
    })

    # --- Recommended Zeta Upper Value ---
    current_recommended_zeta_upper <- reactive({
      req(cs_data_long(), input$M)

      # This logic requires current_raw_cs_data_wide to be available from the file_upload_data reactive list
      #cs_data <- cs_data_long()

      diagnostics <- commutability::check_data(data = file_upload_data$raw_cs_data(), type = "cs")
      study_design_information <- diagnostics$quality

      maximum_n <- max(study_design_information$effective_number_of_samples, na.rm = TRUE)
      maximum_R <- ceiling(max(study_design_information$average_number_of_replicates, na.rm = TRUE))

      current_M <- as.numeric(input$M) / 100

      if (maximum_n < 20) maximum_n <- 20
      if (maximum_n > 50) maximum_n <- 50
      if (maximum_R < 2)  maximum_R <- 2
      if (maximum_R > 5)  maximum_R <- 5

      matches_n <- which(abs(commutability::look_up_table$n - maximum_n) <= 0.1)
      matches_R <- which(abs(commutability::look_up_table$R - maximum_R) <= 0.1)
      matches_M <- which(abs(commutability::look_up_table$M - current_M) <= 0.01)

      which_row <- intersect(intersect(matches_n, matches_R), matches_M)

      recommended_zeta_upper <- if (length(which_row) == 0) {
        which_row_fallback <- intersect(matches_n, matches_R)
        commutability::look_up_table$zeta[which_row_fallback][1] * (1 + current_M)^2
      } else {
        commutability::look_up_table$zeta[which_row][1]
      }

      round(recommended_zeta_upper, digits = 2L)
    })

    # ADDED: Render the recommended zeta display
    output$recommended_zeta_display <- renderUI({
      req(current_recommended_zeta_upper())
      zeta_val <- current_recommended_zeta_upper()
      withMathJax(
        div(
          class = "help-info-box",
          style = "margin-top: 0px; border-left-color: #605CA8;",
          tagList(
            div(
              class = "input-note",
              style = "font-style: normal; font-weight: 600;",
              icon("cogs"),
              sprintf("Calculated value: \\(\\hat{\\zeta}_{\\mathrm{upper}} = %s\\)", format(zeta_val, nsmall = 2, digits = 2))
            ),
            div(
              style = "margin-top: 10px; color: #6c757d; font-size: 1.25rem;",
              "Any IVD-MD pair with a \\(\\hat{\\zeta}\\) value above this limit will be deemed to have excessive differences in nonselectivity."
            )
          )
        )
      )
    })

    # --- Return values for other modules ---
    return(
      reactive({
        final_pi_method <- if (input$pi_method == "ss" && input$ss_weighted == "Yes") {
          "ssw"
        }
        else {
          input$pi_method
        }
        list(
          transformation = input$transformation,
          pi_method = final_pi_method,
          M = as.numeric(input$M) / 100,
          zeta_upper = current_recommended_zeta_upper()
        )
      })
    )
  })
}


