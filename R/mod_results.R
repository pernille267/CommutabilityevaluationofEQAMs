#' Results UI Module
#'
#' @param id A character string for the namespace.
#'
#' @return A UI definition for the results module.
#' @noRd
mod_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
    useGlassChart(),
    div(
      class = "page-header",
      h1(
        class = "main-title",
        icon("square-poll-horizontal"),
        "Commutability Evaluation Analysis Results"
      ),
      glassButton(
        inputId = ns("show_results_explanation"),
        label = "Show Help Text",
        icon = icon(name = "circle-question"),
        color = "green"
      )
    ),
    glassTogglePanel(
      triggerId = ns("show_results_explanation"),
      show_when = NULL,
      help_button_page_5A_text()
    ),
    # --- Common Card for all Three Result Panels ---
    div(
      id = ns("results_page_content"),
      style = "position: relative; min-height: 100%;",
      glassCard(
        inputId = ns("global_options"),
        title = "Global Analysis Options",
        icon = icon(name = "sliders"),
        collapsible = TRUE,
        collapsed = FALSE,
        disabled = FALSE,
        width = "100%",
        glassRow(
          glassCol(
            width = 4,
            div(
              class = "parameter-section",
              glassRadioButtons(
                inputId = ns("pi_conf_level"),
                label = "Select Desired Confidence Level",
                help_text = paste0(
                  "Confidence level here signify the nominal confidence level ",
                  "of the prediction intervals. It is generally recommended ",
                  "setting this to 99 % (the default)."
                ),
                choices = c(
                  "95 %" = 0.95,
                  "99 %" = 0.99
                ),
                selected = 0.99,
                inline = TRUE,
                width = "100%",
                disabled = FALSE
              )
            )
          ),
          glassCol(
            width = 4,
            div(
              class = "parameter-section",
              glassRadioButtons(
                inputId = ns("exclude_extrapolations"),
                label = "Exclude Extrapolated Results",
                help_text = paste0(
                  "Applies to evaluated materials within IVD-MD pairs. ",
                  "Excludes evaluated material results outside the observed ",
                  "measurement interval of the clinical samples for each IVD-MD ",
                  "pair. It is not recommended to set this to -No-."
                ),
                choices = c(
                  "Yes" = "Yes",
                  "No" = "No"
                ),
                selected = "Yes",
                inline = TRUE,
                width = "100%",
                disabled = FALSE
              )
            )
          ),
          glassCol(
            width = 4,
            div(
              style = "margin: 25px; ",
              glassButton(
                inputId = ns("update_cache"),
                label = "Run Analysis",
                icon = icon("rotate"),
                width = "100%",
                urgent = TRUE,
                urgent_text = "Press This Before Anything Else!"
              )
            )
          )
        )
      ),

      glassSpacer(),

      glassTabsetPanel(
        inputId = ns("results_tabs"),
        selected = "show_results_tables",
        color = "purple",
        boxed = TRUE,

        glassTabPanel(
          title = "Tables",
          value = "show_results_tables",
          icon = icon("table"),

          glassCard(
            inputId = ns("table_filter_and_display"),
            title = "Filtering & Table Display",
            icon = icon(name = "filter"),
            collapsible = TRUE,
            collapsed = TRUE,
            attached = TRUE,
            disabled = FALSE,
            width = "100%",
            glassRow(
              glassCol(
                width = 6,
                div(
                  class = "parameter-section",
                  glassRadioButtons(
                    inputId = ns("filter_eq_location"),
                    label = "Choose Filtering for Evaluated Material Locations",
                    help_text = paste0(
                      "PIs is an abbreviation for prediction intervals. Only ",
                      "include evaluated materials that have results inside ",
                      "or outside the established prediction intervals."
                    ),
                    choices = c(
                      "No Filters" = "n_filt",
                      "Inside PIs" = "o_inside",
                      "Outside PIs" = "o_outside"
                    ),
                    selected = "n_filt",
                    inline = TRUE,
                    width = "100%",
                    disabled = FALSE
                  )
                )
              ),
              glassCol(
                width = 6,
                div(
                  class = "parameter-section",
                  glassRadioButtons(
                    inputId = ns("filter_by_dins"),
                    label = "Choose Filtering for IVD-MD Nonselectivity Differences",
                    help_text = paste0(
                      "Applies to IVD-MD pairs. Only include IVD-MD pairs ",
                      "demonstrating acceptable or excessive differences in ",
                      "nonselectivity (i.e., DINS)."
                    ),
                    choices = c(
                      "No Filters" = "n_filt",
                      "Acceptable" = "o_ins",
                      "Excessive" = "o_dins"
                    ),
                    selected = "n_filt",
                    inline = TRUE,
                    width = "100%",
                    disabled = FALSE
                  )
                )
              )
            )
          ),
          glassTabsetPanel(
            inputId = ns("selected_table"),
            selected = "main_table",
            color = "green",
            boxed = TRUE,
            glassTabPanel(
              title = "Main",
              value = "main_table",
              icon = icon("table-list"),
              glassResultCard(
                inputId = ns("main_table_output_card"),
                title = "Commutability Evaluation Analysis Table",
                toolbar = div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  glassDropdown(
                    inputId = ns("data_format"),
                    label = NULL,
                    label_icon = NULL,
                    choices = c(
                      "Expanded" = "expanded",
                      "Compact" = "compact"
                    ),
                    selected = "compact",
                    width = "250px"
                  ),
                  glassButton(
                    inputId = ns("calculate"),
                    label = "Display",
                    icon = icon("table-list"),
                    width = "auto",
                    disabled = TRUE
                  ),
                  glassDownloadButton(
                    inputId = ns("download_main_table"),
                    label = "Download",
                    icon = icon("download"),
                    width = "auto",
                    disabled = TRUE
                  )
                ),
                icon = icon("table-list"),
                attached = TRUE,
                uiOutput(outputId = ns("ce_results"))
              )
            ),
            glassTabPanel(
              title = "Material-Wise",
              value = "material_wise_table",
              icon = icon("vial"),
              glassResultCard(
                inputId = ns("material_wise_table_output_card"),
                title = "Commutability Evaluation Results for Individual Materials",
                toolbar = div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  div(
                    style = "width: 250px;",
                    glassDropdown(
                      inputId = ns("material"),
                      choices = "none",
                      selected = "none",
                      disabled = TRUE,
                      width = "100%"
                    )
                  ),
                  glassButton(
                    inputId = ns("calculate_grid"),
                    label = "Display",
                    icon = icon("vial"),
                    width = "auto",
                    disabled = TRUE
                  ),
                  glassDownloadButton(
                    inputId = ns("download_material_wise"),
                    label = "Download",
                    icon = icon("download"),
                    width = "auto",
                    disabled = TRUE
                  )
                ),
                icon = icon("vial"),
                attached = TRUE,
                uiOutput(outputId = ns("ce_results_grid"))
              )
            ),
            glassTabPanel(
              title = "Aggregated Material-Wise",
              value = "aggregated_material_wise_table",
              icon = icon("ranking-star"),
              glassResultCard(
                inputId = ns("aggregated_material_wise_table_output_card"),
                title = "Aggregated Material-Wise Commutability Evaluation Results",
                toolbar = div(
                  style = "display: flex; align-items: center; gap: 10px;",
                  glassDropdown(
                    inputId = ns("aggregated_type"),
                    label = NULL,
                    label_icon = NULL,
                    help_text = NULL,
                    choices = c(
                      "Commutability Bias" = "bias",
                      "Commutability Conclusion" = "conclusion"
                    ),
                    selected = "conclusion",
                    disabled = FALSE,
                    width = "auto"
                  ),
                  glassButton(
                    inputId = ns("calculate_aggregated"),
                    label = "Display",
                    icon = icon("ranking-star"),
                    width = "auto",
                    disabled = TRUE
                  ),
                  glassDownloadButton(
                    inputId = ns("download_material_wise_aggregated"),
                    label = "Download",
                    icon = icon("download"),
                    width = "auto",
                    disabled = FALSE
                  )
                ),
                icon = icon("ranking-star"),
                attached = TRUE,
                uiOutput(outputId = ns("ce_results_aggregated"))
              )
            )
          )
        ),

        glassTabPanel(
          title = "Plots",
          value = "show_results_plots",
          icon = icon("chart-line"),
          glassCard(
            inputId = ns("card_plot_appearance"),
            title = "Configuration",
            icon = icon("sliders"),
            collapsible = TRUE,
            collapsed = TRUE,
            attached = TRUE,
            glassRow(
              glassCol(
                width = 4,
                h4("Labels"),
                div(
                  class = "parameter-section",
                  glassTextInput(
                    inputId = ns("title"),
                    label = "Plot Title",
                    value = "",
                    label_icon = icon("h"),
                    placeholder = "Some random title ...",
                    tooltip_empty = "Write it now!",
                    tooltip_filled = "Cool title!",
                    width = "100%",
                    help_text = "Write the desired plot title here"
                  ),
                  glassTextInput(
                    inputId = ns("x_name"),
                    label = "X-Axis Title",
                    value = "",
                    label_icon = icon("x"),
                    placeholder = "Some title for the x-axis ...",
                    tooltip_empty = "Write it here",
                    tooltip_filled = "Now that is a cool x-axis title",
                    width = "100%",
                    help_text = "What values do we see on the x-axis? Write it."
                  ),
                  glassTextInput(
                    inputId = ns("y_name"),
                    label = "Y-Axis Title",
                    value = "",
                    label_icon = icon("y"),
                    placeholder = "Some title for the x-axis ...",
                    tooltip_empty = "Write it here",
                    tooltip_filled = "Nice y-axis title!",
                    width = "100%",
                    help_text = "What values do we see on the y-axis? Write it."
                  )
                )
              ),
              glassCol(
                width = 3,
                h4("Display Options"),
                div(
                  class = "parameter-section",
                  glassDropdown(
                    inputId = ns("cs_curve"),
                    label = "Pattern Curve",
                    label_icon = icon("bezier-curve"),
                    choices = c(
                      "None" = "none",
                      "Diagonal" = "equivalence_curve",
                      "Fitted" = "fitted_curve",
                      "Flexible" = "smooth_curve"
                    ),
                    selected = "none",
                    width = "100%"
                  ),
                  glassSlider(
                    inputId = ns("tick_density"),
                    label = "Axis Ticks",
                    label_icon = icon("grip"),
                    choices = 3:20,
                    selected = 6,
                    unit = " Ticks",
                    width = "100%"
                  )
                )
              ),
              glassCol(
                width = 5,
                h4("Download Options"),
                div(
                  class = "parameter-section",
                  glassRow(
                    glassCol(
                      width = 6,
                      glassNumericInput(
                        inputId = ns("width"),
                        label = "Width",
                        value = 17.6,
                        min = 2,
                        max = 42,
                        step = 0.4,
                        label_icon = icon("arrows-left-right"),
                        width = "100%",
                        unit = "cm",
                        accept = c(2, 42),
                        warning_unacceptable = "No ... please don't."
                      )
                    ),
                    glassCol(
                      width = 6,
                      glassNumericInput(
                        inputId = ns("height"),
                        label = "Height",
                        value = 11.7,
                        min = 2,
                        max = 60,
                        step = 0.4,
                        label_icon = icon("arrows-up-down"),
                        width = "100%",
                        unit = "cm",
                        accept = c(2, 60),
                        warning_unacceptable = "No ... please don't."
                      )
                    )
                  ),
                  glassDropdown(
                    inputId = ns("plot_download_file_type"),
                    label = "Format",
                    label_icon = icon("file"),
                    choices = c(
                      "PDF" = ".pdf",
                      "PNG" = ".png",
                      "TIF" = ".tif",
                      "TIFF" = ".tiff",
                      "JPEG" = ".jpeg"
                    ),
                    selected = NULL,
                    width = "100%"
                  ),
                  glassSlider(
                    inputId = ns("plot_download_quality"),
                    label = "Quality",
                    label_icon = icon("image"),
                    choices = c(
                      100, 200, 300,
                      450, 600, 750,
                      1000, 1250, 1500
                    ),
                    selected = 300,
                    width = "100%",
                    unit = " dpi"
                  )
                )
              )
            )
          ),
          glassSpacer(),
          glassResultCard(
            inputId = ns("plot_result_card"),
            title = "Commutability Evaluation Plot",
            icon = icon("chart-line"),
            toolbar = tagList(
              glassButton(
                inputId = ns("plot"),
                label = "Plot",
                icon = icon("chart-line"),
                disabled = TRUE
              ),
              glassDownloadButton(
                inputId = ns("download_ce_plots"),
                label = "Download",
                icon = icon("download"),
                disabled = TRUE
              )
            ),
            uiOutput(
              outputId = ns("ce_plots_container")
            )
          )
        ),
        glassTabPanel(
          title = "Report",
          value = "show_results_report",
          icon = icon("paper-plane"),

          div(
            class = "dashboard-card",
            div(
              class = "card-header",
              icon("file-download", class = "header-icon"),
              h3("Download Full Analysis Report"),
              glassDownloadButton(
                inputId = ns("download_report"),
                label = "Download",
                icon = icon("download"),
                width = NULL,
                disabled = TRUE
              )
            ),
            div(
              class = "card-body"
            )
          )
        )
      )
    )
  )
}

#' Results Server Module
#'
#' @param id A character string for the namespace.
#' @param file_upload_data A reactive list from the file upload module.
#' @param mod_dins_params A reactive list from the user input module.
#' @param outlier_data A reactive list from the outlier analysis module.
#' @param model_validation_data A reactive list from the model validation module.
#'
#' @return This module does not return any values.
#' @noRd
mod_results_server <- function(id, file_upload_data, mod_dins_params, outlier_data, model_validation_data) {
  moduleServer(id, function(input, output, session) {

    cs_data_long <- reactive({
      req(file_upload_data$raw_cs_data())
      cs_data_repaired <- commutability::repair_data(
        data = file_upload_data$raw_cs_data(),
        type = "cs",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
      keep_these_cols <- setdiff(
        names(cs_data_repaired),
        file_upload_data$remove_ivd_mds()
      )
      cs_data_repaired <- subset(
        x = cs_data_repaired,
        select = keep_these_cols
      )
      ref_method <- file_upload_data$reference_method()
      raw_data <- commutability::get_comparison_data(
        data = cs_data_repaired,
        reference = ref_method
      )
      transformation <- switch(
        mod_dins_params()$transformation,
        "identity" = "identity",
        "ln" = "log#e",
        "boxcox" = "boxcox#0.5",
        "identity"
      )
      commutability::transform_data(
        data = raw_data,
        transformation = transformation
      )
    })

    eq_data_long <- reactive({
      req(file_upload_data$raw_eq_data())
      eq_data_repaired <- commutability::repair_data(
        data = file_upload_data$raw_eq_data(),
        type = "eqam",
        remove_invalid_methods = FALSE,
        include_repair_summary = FALSE
      )
      keep_these_cols <- setdiff(
        names(eq_data_repaired),
        file_upload_data$remove_ivd_mds()
      )
      eq_data_repaired <- subset(
        x = eq_data_repaired,
        select = keep_these_cols
      )
      ref_method <- file_upload_data$reference_method()
      raw_data <- commutability::get_comparison_data(
        data = eq_data_repaired,
        reference = ref_method
      )
      transformation <- switch(
        mod_dins_params()$transformation,
        "identity" = "identity",
        "ln" = "log#e",
        "boxcox" = "boxcox#0.5",
        "identity"
      )
      commutability::transform_data(
        data = raw_data,
        transformation = transformation
      )
    })

    results_data_cache <- reactiveVal(NULL)
    download_main_table_cache <- reactiveVal(NULL)
    download_material_wise_cache <- reactiveVal(NULL)
    download_material_wise_aggregated_cache <- reactiveVal(NULL)
    plot_cache <- reactiveVal(NULL)

    toggle_downloads <- function(enable = FALSE) {
      state <- !enable
      updateGlassButton(session, "download_main_table", disabled = state)
      updateGlassButton(session, "download_material_wise", disabled = state)
      updateGlassButton(session, "download_material_wise_aggregated", disabled = state)
      updateGlassButton(session, "download_ce_plots", disabled = state)
      updateGlassButton(session, "download_report", disabled = state)
    }

    btn_state <- reactiveValues(
      is_fresh_start = TRUE,
      global_dirty = FALSE
    )

    local_state <- reactiveValues(
      main_clicked = FALSE,
      grid_clicked = FALSE,
      aggregated_clicked = FALSE,
      plot_clicked = FALSE,
      main_fresh = FALSE,
      grid_fresh = FALSE,
      aggregated_fresh = FALSE,
      plot_fresh = FALSE
    )

    # Changes in 'raw_cs_data' or 'raw_eq_data' --> 'fresh start' state
    observeEvent(c(file_upload_data$raw_cs_data(), file_upload_data$raw_eq_data()), {

      # Mark as a fresh start
      btn_state$is_fresh_start <- TRUE
      btn_state$global_dirty <- TRUE

      local_state$main_clicked <- FALSE
      local_state$grid_clicked <- FALSE
      local_state$aggregated_clicked <- FALSE
      local_state$plot_clicked <- FALSE
      local_state$main_fresh <- FALSE
      local_state$grid_fresh <- FALSE
      local_state$aggregated_fresh <- FALSE
      local_state$plot_fresh <- FALSE

      updateGlassButton(session, "update_cache", label = "Run Analysis", urgent = TRUE, urgent_text = "Update Results First!")
      updateGlassButton(session, "calculate", label = "Display", disabled = TRUE)
      updateGlassButton(session, "calculate_grid", label = "Display", disabled = TRUE)
      updateGlassButton(session, "calculate_aggregated", label = "Display", disabled = TRUE)
      updateGlassButton(session, "plot", label = "Plot", disabled = TRUE)

      toggle_downloads(enable = FALSE)

      results_data_cache(NULL)
      plot_cache(NULL)

    }, ignoreInit = TRUE)

    # Parameter Changes -> Disable Local Buttons (Not Resetting Labels)
    observeEvent(
      eventExpr = {
        c(
          file_upload_data$is_valid(),
          file_upload_data$reference_method(),
          mod_dins_params(),
          input$pi_conf_level
        )
      },
      handlerExpr = {

        btn_state$global_dirty <- TRUE

        updateGlassButton(session, "calculate", disabled = TRUE)
        updateGlassButton(session, "calculate_grid", disabled = TRUE)
        updateGlassButton(session, "calculate_aggregated", disabled = TRUE)
        updateGlassButton(session, "plot", disabled = TRUE)

        toggle_downloads(enable = FALSE)

        updateGlassButton(
          session, "update_cache",
          disabled = FALSE,
          urgent = TRUE,
          urgent_text = "Must Press Again!"
        )
      },
      ignoreInit = TRUE
    )

    # Local Input Changes (Re-Enable Locals if Global not dirty)

    # Helper function to safely enable
    check_and_enable <- function(id, is_dirty_flag) {
      if (!btn_state$global_dirty) {
        #local_state[[is_dirty_flag]] <- FALSE # It's no longer fresh (display doesn't match input)
        updateGlassButton(session, id, disabled = FALSE)
      }
    }

    # --- Re-enable 'calculate' button if some settings change ---
    observeEvent(c(input$exclude_extrapolations, input$filter_eq_location, input$filter_by_dins, input$data_format), {
      check_and_enable("calculate", "main_fresh")
    })

    # --- Re-enable 'calculate_grid' and 'calculate_aggregated' if some settings change ---
    observeEvent(c(input$exclude_extrapolations, input$filter_eq_location, input$filter_by_dins, input$material), {
      check_and_enable("calculate_grid", "grid_fresh")
      check_and_enable("calculate_aggregated", "aggregated_fresh")
    })

    observeEvent(
      eventExpr = {
        c(
          input$exclude_extrapolations,
          input$title,
          input$x_name,
          input$y_name,
          input$cs_curve,
          input$tick_density,
          input$width,
          input$height,
          input$plot_download_file_type,
          input$plot_download_quality
        )
      },
      handlerExpr = {
        check_and_enable("plot", "plot_fresh")
      }
    )

    observeEvent(
      input$update_cache,
      handlerExpr = {

        showGlassLoader(
          id = "results_global_loader",
          text = "Running Commutability Evaluation Analysis ...",
          selector = paste0("#", session$ns("results_page_content")),
          session = session
        )

        on.exit(hideGlassLoader(id = "results_global_loader", session = session))

        if (isTRUE(file_upload_data$is_valid())) {
          dins_params <- mod_dins_params()
          data_without_NA_values <- na.omit(object = cs_data_long())
          new_data_without_NA_values <- na.omit(object = eq_data_long())
          upper_zeta_val <- if (dins_params$zeta_upper <= 1) {
            NULL
          }
          else {
            dins_params$zeta_upper
          }

          result <- commutability::do_commutability_evaluation(
            data = data_without_NA_values,
            new_data = new_data_without_NA_values,
            B = 100,
            N = 100,
            method_pi = dins_params$pi_method,
            method_bs = "BCa",
            level_pi = as.numeric(input$pi_conf_level),
            level_bs = 0.95,
            M = dins_params$M,
            upper_zeta = upper_zeta_val
          )

          results_data_cache(result)

          btn_state$global_dirty <- FALSE
          local_state$main_fresh <- FALSE
          local_state$grid_fresh <- FALSE
          local_state$aggregated_fresh <- FALSE
          local_state$plot_fresh <- FALSE

          updateGlassButton(session, "calculate", label = "Display", disabled = FALSE)
          updateGlassButton(session, "calculate_grid", label = "Display", disabled = FALSE)
          updateGlassButton(session, "calculate_aggregated", label = "Display", disabled = FALSE)
          updateGlassButton(session, "plot", label = "Plot", disabled = FALSE)

          toggle_downloads(enable = TRUE)

          updateGlassButton(session, "update_cache", label = "Update Results", urgent = FALSE)
          if (btn_state$is_fresh_start) {
            btn_state$is_fresh_start <- FALSE
          }

          debug_ce_results <<- result
        }
        else {
          results_data_cache(
            list(error = "Validation tests do not pass, so calculations are not possible")
          )
        }
      })


    filtered_ce_data <- reactive({
      req(results_data_cache())
      results_list <- results_data_cache()

      if (!is.null(results_list$error)) return(NULL)

      dt <- data.table::copy(results_list$merged_ce_data)

      if (any("extrapolate" == names(dt))) {
        ep_filter <- switch(
          input$exclude_extrapolations,
          "Yes" = which(dt$extrapolate == 0L),
          seq_len(nrow(dt))
        )
      }
      else {
        warning(
          "extrapolate not found in merged_ce_data. Could not apply filter.",
          immediate. = TRUE
        )
        ep_filter <- seq_len(nrow(dt))
      }

      if (any("pi_inside" == names(dt))) {
        pi_filter <- switch(
          input$filter_eq_location,
          "o_inside" = which(dt$pi_inside == 1L),
          "o_outside" = which(dt$pi_inside == 0L),
          seq_len(nrow(dt))
        )
      }
      else {
        warning(
          "pi_inside not found in merged_ce_data. Could not apply filter.",
          immediate. = TRUE
        )
        pi_filter <- seq_len(nrow(dt))
      }


      if (any("dins_conclusion" == names(dt))) {
        dins_filter <- switch(
          input$filter_by_dins,
          "o_ins" = which(dt$dins_conclusion == 0L),
          "o_dins" = which(dt$dins_conclusion == 1L),
          seq_len(nrow(dt))
        )
      }
      else {
        warning(
          "dins_filter not found in merged_ce_data. Could not apply filter.",
          immediate. = TRUE
        )
        dins_filter <- seq_len(nrow(dt))
      }


      combined_filter <- intersect(
        x = ep_filter,
        y = intersect(
          x = pi_filter,
          y = dins_filter
        )
      )

      # --- Keep only rows that satisfy the three given filters ---
      if (length(combined_filter) > 0) {
        dt <- dt[combined_filter]
      }

      return(dt)
    })

    observeEvent(input$calculate, {
      local_state$main_fresh <- TRUE
      local_state$main_clicked <- TRUE
      updateGlassButton(session, "calculate", label = "Refresh", disabled = TRUE)
    })

    observeEvent(input$calculate_grid, {
      local_state$grid_fresh <- TRUE
      local_state$grid_clicked <- TRUE
      updateGlassButton(session, "calculate_grid", label = "Refresh", disabled = TRUE)
    })

    observeEvent(input$calculate_aggregated, {
      local_state$aggregated_fresh <- TRUE
      local_state$aggregated_clicked <- TRUE
      updateGlassButton(session, "calculate_aggregated", label = "Refresh", disabled = TRUE)
    })

    observeEvent(input$plot, {
      local_state$plot_fresh <- TRUE
      local_state$plot_clicked <- TRUE
      updateGlassButton(session, "plot", label = "Refresh", disabled = TRUE)
    })

    ce_results_display <- eventReactive(input$calculate, {
      output_tbl <- filtered_ce_data()

      # --- If NULL (error state) or empty, handle that ---
      if (is.null(output_tbl)) {
        results_list <- results_data_cache()
        # --- Check if cache is empty ---
        if (is.null(results_list)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "There is not data ..."
              ),
              caption = paste0(
                "There is results to display yet. Did you remember to ",
                "press the Run Analysis Button under the Global Analysis ",
                "Options panel (the big one all the way to the right there)?"
              ),
              sortable = FALSE
            )
          )
        }
        # --- Check if cache contain $error
        if (!is.null(results_list$error)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "Error: see table caption"
              ),
              caption = paste0(
                "We found an error which made it impossible to display any ",
                "results to you: ",
                results_list$error,
                ".",
                "Maybe you should go back to the Data Upload module ",
                "(first module) to see if the validation tests passed. ",
                "If all these tests passed and you still see this error, ",
                "please contact the developer of this application."
              ),
              sortable = FALSE
            )
          )
        }
      }

      output_tbl_compact <- data.table::data.table(
        "IVD-MD Comparison" = output_tbl$comparison,
        "ID of Evaluated Material" = output_tbl$SampleID,
        "zeta (lwr - upr)" = format_point_plus_interval(
          point = output_tbl$zeta,
          lower = output_tbl$zeta_ci_lwr,
          upper = output_tbl$zeta_ci_upr,
          decimals = 2L
        ),
        "zeta upper" = scales::number(
          output_tbl$zeta_upper,
          accuracy = 1e-2
        ),
        "Differences in Nonselectivity is" = ifelse(
          output_tbl$dins_conclusion == 1L,
          "Deemed Excessive",
          "Deemed Acceptable"
        ),
        "Measurements" = format_pair(
          point_1 = output_tbl$MP_B,
          point_2 = output_tbl$MP_A,
          decimals = 2L
        ),
        "Prediction (lwr - upr)" = format_point_plus_interval(
          point = output_tbl$prediction,
          lower = output_tbl$pi_lwr,
          upper = output_tbl$pi_upr,
          decimals = 2L
        ),
        "Evaluated Material is" = ifelse(
          output_tbl$pi_inside == 1L,
          "Inside PI",
          "Outside PI"
        ),
        "Conclusion Strength (%)" = scales::number(
          sapply(
            X = seq_len(nrow(output_tbl)),
            FUN = function(row_id) {
              if (is.na(output_tbl$pi_inside[row_id])) {
                NA_real_
              }
              else if (output_tbl$pi_inside[row_id] == 1L) {
                output_tbl$inside_rate[row_id] * 100
              }
              else {
                100 - output_tbl$inside_rate[row_id] * 100
              }
            },
            simplify = TRUE,
            USE.NAMES = FALSE
          ),
          accuracy = 1e-1,
          suffix = " %"
        )
      )

      output_tbl_expanded <- data.table::data.table(
        "IVD-MD Comparison" = output_tbl$comparison,
        "ID of Evaluated Material" = output_tbl$SampleID,
        "zeta" = format_number(output_tbl$zeta),
        "zeta_ci_lwr" = format_number(output_tbl$zeta_ci_lwr),
        "zeta_ci_upr" = format_number(output_tbl$zeta_ci_upr),
        "zeta_upper" = format_number(output_tbl$zeta_upper),
        "Differences in Nonselectivity is" = ifelse(
          output_tbl$dins_conclusion == 1L,
          "Deemed Excessive",
          "Deemed Acceptable"
        ),
        "x" = format_number(output_tbl$MP_B),
        "y" = format_number(output_tbl$MP_A),
        "prediction" = format_number(output_tbl$prediction),
        "pi_lwr" = format_number(output_tbl$pi_lwr),
        "pi_upr" = format_number(output_tbl$pi_upr),
        "Evaluated Material is" = ifelse(
          output_tbl$pi_inside == 1L,
          "Inside PI",
          "Outside PI"
        ),
        "Conclusion Strength (%)" = scales::number(
          sapply(
            X = seq_len(nrow(output_tbl)),
            FUN = function(row_id) {
              if (is.na(output_tbl$pi_inside[row_id])) {
                NA_real_
              }
              else if (output_tbl$pi_inside[row_id] == 1L) {
                output_tbl$inside_rate[row_id] * 100
              }
              else {
                100 - output_tbl$inside_rate[row_id] * 100
              }
            },
            simplify = TRUE,
            USE.NAMES = FALSE
          ),
          accuracy = 1e-1,
          suffix = " %"
        )
      )


      cache_fill <- list(
        "output_compact" = output_tbl_compact,
        "output_expanded" = output_tbl_expanded
      )
      download_main_table_cache(cache_fill)


      conclusion_badges <- list(
        "Deemed Acceptable" = glassBadge(
          label = "Deemed Acceptable",
          color = "green",
          shape = "pill",
          tooltip = "Differences in nonselectivity is deemed acceptable for this IVD-MD comparison.",
          tooltip_mode = "hover",
          glow = FALSE
        ),
        "Inside PI" = glassBadge(
          label = "Inside PI",
          color = "green",
          shape = "pill",
          tooltip = "This evaluated material is inside the PI for this IVD-MD comparison. This does not automatically imply that it is commutable!",
          tooltip_mode = "hover",
          glow = FALSE
        ),
        "Outside PI" = glassBadge(
          label = "Outside PI",
          color = "red",
          shape = "pill",
          tooltip = "This evaluated material is outside the PI for this IVD-MD comparison. This does not automatically imply that it is noncommutable!",
          tooltip_mode = "hover",
          glow = TRUE
        ),
        "Deemed Excessive" = glassBadge(
          label = "Deemed Excessive",
          color = "orange",
          shape = "pill",
          tooltip = "Differences in nonselectivity is deemed excessive (unacceptable) for this IVD-MD comparison.",
          tooltip_mode = "hover",
          glow = FALSE
        ),
        "100.0 %" = glassBadge(
          label = "Maximum",
          color = "green",
          shape = "pill",
          tooltip = NULL,
          tooltip_mode = "hover",
          glow = FALSE
        )
      )

      compact_output <- renderGlassTable(
        data = output_tbl_compact,
        col_names = c(
          "IVD-MD Comparison",
          "ID of Evaluated Material",
          "\\(\\hat{\\zeta} (95 \\% \\, \\mathrm{PBCI}: \\hat{\\zeta}_{\\mathrm{lower}}-\\hat{\\zeta}_{\\mathrm{upper}}) \\)",
          "\\(\\zeta_{\\mathrm{critical}}\\)",
          "Differences in Nonselectivity is",
          "Measurements",
          "Prediction \\((\\mathrm{PI}_{\\mathrm{lower}}-\\mathrm{PI}_{\\mathrm{upper}})\\)",
          "Evaluated Material is",
          "Conclusion Strength (%)"
        ),
        highlight_cells = conclusion_badges,
        caption = paste0(
          "Commutability evaluation results for every evaluated material ",
          "across every IVD-MD comparison. Large tables will be cut into ",
          "multiple tables. If this applies to this table, you can navigate ",
          "between the tables just below the table."
        ),
        sortable = TRUE
      )

      expanded_output <- renderGlassTable(
        data = output_tbl_expanded,
        col_names = c(
          "IVD-MD Comparison",
          "ID of Evaluated Material",
          "\\(\\hat{\\zeta}\\)",
          "\\(\\hat{\\zeta}_{\\mathrm{lower}}\\)",
          "\\(\\hat{\\zeta}_{\\mathrm{upper}}\\)",
          "\\(\\zeta_{\\mathrm{critical}}\\)",
          "Differences in Nonselectivity is",
          "IVD-MD (\\(x\\)-axis)",
          "IVD-MD (\\(y\\)-axis)",
          "Prediction",
          "\\(\\mathrm{PI}_{\\mathrm{lower}}\\)",
          "\\(\\mathrm{PI}_{\\mathrm{upper}}\\)",
          "Evaluated Material is",
          "Conclusion Strength (%)"
        ),
        highlight_cells = conclusion_badges,
        caption = paste0(
          "Commutability evaluation results for every evaluated material ",
          "across every IVD-MD comparison. Large tables will be cut into ",
          "multiple tables. If this applies to this table, you can navigate ",
          "between the tables just below the table."
        ),
        sortable = TRUE
      )

      return(
        list(
          "expanded_output" = expanded_output,
          "compact_output" = compact_output
        )
      )

    })

    output$ce_results <- renderUI({
      req(ce_results_display())
      if (input$data_format == "compact") {
        ce_results_display()$compact_output
      }
      else {
        ce_results_display()$expanded_output
      }
    })

    ce_results_grid_display <- eventReactive(input$calculate_grid, {
      # --- Get filtered 'merged_ce_data' ---
      output_tbl <- filtered_ce_data()

      # --- If NULL (error state) or empty, handle that ---
      if (is.null(output_tbl)) {
        results_list <- results_data_cache()
        # --- Check if cache is empty ---
        if (is.null(results_list)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "There is not data ..."
              ),
              caption = paste0(
                "There is results to display yet. Did you remember to ",
                "press the Run Analysis Button under the Global Analysis ",
                "Options panel (the big one all the way to the right there)?"
              ),
              sortable = FALSE
            )
          )
        }
        # --- Check if cache contain $error
        if (!is.null(results_list$error)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "Error: see table caption"
              ),
              caption = paste0(
                "We found an error which made it impossible to display any ",
                "results to you: ",
                results_list$error,
                ".",
                "Maybe you should go back to the Data Upload module ",
                "(first module) to see if the validation tests passed. ",
                "If all these tests passed and you still see this error, ",
                "please contact the developer of this application."
              ),
              sortable = FALSE
            )
          )
        }
      }

      output_tbl <- data.table::copy(output_tbl)[, list(
        "conclusion" = ifelse(
          pi_inside == 1 & dins_conclusion == 0,
          "C",
          ifelse(
            pi_inside == 0 & dins_conclusion == 0,
            "NC",
            ifelse(
              pi_inside == 1 & dins_conclusion == 1,
              "EI",
              "EO"
            )
          )
        )
      ), by = c("comparison", "SampleID")]

      output_tbl[, c("Method1", "Method2") := tstrsplit(comparison, " - ", fixed = TRUE)]
      all_methods <- sort(unique(c(output_tbl$Method1, output_tbl$Method2)))
      output_tbl_list <- lapply(
        X = split(output_tbl, by = "SampleID"),
        FUN = function(dt_subset) {
          dt_subset[, Method1 := factor(Method1, levels = all_methods)]
          dt_subset[, Method2 := factor(Method2, levels = all_methods)]
          wide_dt <- dcast.data.table(
            data = dt_subset,
            formula = Method1 ~ Method2,
            value.var = "conclusion",
            drop = FALSE
          )
          names(wide_dt)[which(names(wide_dt) == "Method1")] <- "IVD-MD"
          return(wide_dt)
        })

      particular_material <- input$material

      if (particular_material != "none") {

        cache_fill <- output_tbl_list[[particular_material]]
        cache_fill[nrow(cache_fill), 1] <- particular_material
        download_material_wise_cache(cache_fill)

        conclusion_badges <- list(
          "C" = glassBadge(
            label = "C",
            color = "green",
            shape = "pill",
            tooltip = "Commutable: Inside PI and Acceptable DINS",
            tooltip_mode = "hover",
            glow = TRUE
          ),
          "NC" = glassBadge(
            label = "NC",
            color = "red",
            shape = "pill",
            tooltip = "Non-Commutable: Outside PI but Acceptable DINS",
            tooltip_mode = "hover",
            glow = TRUE
          ),
          "EI" = glassBadge(
            label = "EI",
            color = "gray",
            shape = "pill",
            tooltip = "Inside PI but Excessive DINS",
            tooltip_mode = "hover"
          ),
          "EO" = glassBadge(
            label = "EO",
            color = "orange",
            shape = "pill",
            tooltip = "Outside PI and Excessive DINS",
            tooltip_mode = "hover",
            glow = FALSE # Make the bad ones pulse!
          )
        )

        renderGlassTable(
          data = output_tbl_list[[particular_material]],
          caption = paste0(
            "Commutability evaluation results for ",
            particular_material,
            "."
          ),
          highlight_cells = conclusion_badges,
          sortable = FALSE
        )
      }
    })

    output$ce_results_grid <- renderUI({
      req(ce_results_grid_display())
      ce_results_grid_display()
    })

    ce_results_aggregated_display <- eventReactive(input$calculate_aggregated, {

      output_tbl <- filtered_ce_data()

      if (is.null(output_tbl)) {
        results_list <- results_data_cache()
        if (is.null(results_list)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "There is not data ..."
              ),
              caption = paste0(
                "There is results to display yet. Did you remember to ",
                "press the Run Analysis Button under the Global Analysis ",
                "Options panel (the big one all the way to the right there)?"
              ),
              sortable = FALSE
            )
          )
        }
        # --- Check if cache contain $error
        if (!is.null(results_list$error)) {
          return(
            renderGlassTable(
              data = data.table::data.table(
                "What is going on" = "Error: see table caption"
              ),
              caption = paste0(
                "We found an error which made it impossible to display any ",
                "results to you: ",
                results_list$error,
                ".",
                "Maybe you should go back to the Data Upload module ",
                "(first module) to see if the validation tests passed. ",
                "If all these tests passed and you still see this error, ",
                "please contact the developer of this application."
              ),
              sortable = FALSE
            )
          )
        }
      }

      output_tbl_by_conclusion <- data.table::copy(output_tbl)[, list(
        "conclusion" = ifelse(
          pi_inside == 1 & dins_conclusion == 0,
          "C",
          ifelse(
            pi_inside == 0 & dins_conclusion == 0,
            "NC",
            ifelse(
              pi_inside == 1 & dins_conclusion == 1,
              "EI",
              "EO"
            )
          )
        )
      ), by = c("comparison", "SampleID")]

      output_tbl_by_conclusion <- output_tbl_by_conclusion[, list(
        "num_C" = sum(conclusion == "C"),
        "num_NC" = sum(conclusion == "NC"),
        "Commutable" = paste0(
          sum(conclusion == "C"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "C") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        ),
        "Noncommutable" = paste0(
          sum(conclusion == "NC"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "NC") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        ),
        "Inside PI (Excessive DINS)" = paste0(
          sum(conclusion == "EI"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "EI") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        ),
        "Outside PI (Excessive DINS)" = paste0(
          sum(conclusion == "EO"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "EO") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        ),
        "Inside PI" = paste0(
          sum(conclusion == "EI" | conclusion == "C"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "EI" | conclusion == "C") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        ),
        "Outside PI" = paste0(
          sum(conclusion == "EO" | conclusion == "NC"),
          " / ",
          length(conclusion),
          " (",
          format(
            x = sum(conclusion == "EO" | conclusion == "NC") / length(conclusion) * 100,
            digits = 2,
            nsmall = 2
          ),
          "%)"
        )
      ), by = list(SampleID)]

      output_tbl_by_conclusion[, "Score" := num_C - num_NC]

      output_tbl_by_conclusion[, `:=` (
        "num_C" = NULL,
        "num_NC" = NULL
      )]

      data.table::setcolorder(
        output_tbl_by_conclusion,
        c("SampleID", "Score", "Commutable", "Noncommutable",
          "Inside PI (Excessive DINS)", "Outside PI (Excessive DINS)",
          "Inside PI", "Outside PI")
      )

      setorder(output_tbl_by_conclusion, -Score)

      by_conclusion_table <- renderGlassTable(
        data = output_tbl_by_conclusion,
        caption = paste0(
          "Aggregated commutability evaluation conclusions for ",
          "all evaluated material",
          "."
        ),
        sortable = TRUE
      )

      is_log_transform <- (mod_dins_params()$transformation == "ln")
      
      output_tbl_by_bias <- data.table::copy(output_tbl)[, list(
        "Bias" = MP_A - prediction,
        "Absolute Bias" = if (is_log_transform) {
          exp(abs(MP_A - prediction))
        } 
        else {
          abs(MP_A - prediction)
        },
        "Relative Bias" = if (is_log_transform) {
          abs(MP_A - prediction) * 100
        } 
        else {
          abs((MP_A - prediction) / prediction) * 100
        },
        "Noncommutability Bias" = ifelse(
          MP_A - prediction > 0,
          ifelse(
            MP_A - pi_upr > 0,
            MP_A - pi_upr,
            0
          ),
          ifelse(
            MP_A - pi_lwr > 0,
            0,
            MP_A - pi_lwr
          )
        ),
        "Absolute Noncommutability Bias" = ifelse(
          MP_A - prediction > 0,
          ifelse(
            MP_A - pi_upr > 0,
            abs(MP_A - pi_upr),
            0
          ),
          ifelse(
            MP_A - pi_lwr > 0,
            0,
            abs(MP_A - pi_lwr)
          )
        ),
        "Relative Noncommutability Bias" = ifelse(
          MP_A - prediction > 0,
          ifelse(
            MP_A - pi_upr > 0,
            abs((MP_A - pi_upr) / pi_upr) * 100,
            0
          ),
          ifelse(
            MP_A - pi_lwr > 0,
            0,
            abs((MP_A - pi_lwr) / pi_lwr) * 100
          )
        )
      ), by = c("comparison", "SampleID")]

      output_tbl_by_bias_sorter <- output_tbl_by_bias$`Relative Bias`
      output_tbl_by_bias_sorter_by <- output_tbl_by_bias$comparison
      output_tbl_by_bias_sorter <- unname(
        tapply(
          X = output_tbl_by_bias_sorter,
          INDEX = output_tbl_by_bias_sorter_by,
          FUN = mean,
          na.rm = TRUE,
          simplify = TRUE
        )
      )
      output_tbl_by_bias_sorter <- order(
        output_tbl_by_bias_sorter,
        decreasing = FALSE,
        na.last = TRUE
      )

      output_tbl_by_bias <- output_tbl_by_bias[, lapply(
        X = .SD,
        FUN = function(bias_component) {
          mean_bias_component <- mean(bias_component, na.rm = TRUE)
          lower_bias_component <- min(bias_component, na.rm = TRUE)
          upper_bias_component <- max(bias_component, na.rm = TRUE)
          return(
            paste0(
              format(mean_bias_component, digits = 2, nsmall = 2),
              " (",
              format(lower_bias_component, digits = 2, nsmall = 2),
              " - ",
              format(upper_bias_component, digits = 2, nsmall = 2),
              ")"
            )
          )
        }
      ), .SDcols = c(
        "Bias",
        "Absolute Bias",
        "Relative Bias",
        "Noncommutability Bias",
        "Absolute Noncommutability Bias",
        "Relative Noncommutability Bias"
      ), by = "SampleID"]

      if (nrow(output_tbl_by_bias) == length(output_tbl_by_bias_sorter)) {
        output_tbl_by_bias <- output_tbl_by_bias[output_tbl_by_bias_sorter]
      }

      # --- Calculate ranking based on mean Relative Bias ---
      # --- Extract mean values from the formatted strings for ranking ---
      mean_relative_bias <- sapply(
        output_tbl_by_bias$`Relative Bias`,
        function(x) {
          as.numeric(strsplit(x, " ")[[1]][1])
        }
      )

      m <- nrow(output_tbl_by_bias)
      ranking_scores <- m - rank(mean_relative_bias, ties.method = "average", na.last = "keep") + 1

      output_tbl_by_bias[, "Score" := round(ranking_scores, 1)]

      output_tbl_by_bias[, `:=` (
        "Bias" = NULL,
        "Noncommutability Bias" = NULL
      )]

      data.table::setcolorder(
        output_tbl_by_bias,
        c("SampleID", "Score", "Absolute Bias", "Relative Bias", 
          "Absolute Noncommutability Bias", "Relative Noncommutability Bias")
      )

      display_column_names <- c(
        "ID of Evaluated Material",
        "Score",
        "Absolute Bias",
        "Relative Bias (%)",
        #"Noncommutability Bias",
        "Absolute Noncommutability Bias",
        "Relative Noncommutability Bias (%)"
      )

      by_bias_table <- renderGlassTable(
        data = output_tbl_by_bias,
        col_names = display_column_names,
        caption = paste0(
          "Aggregated (across all IVD-MD pairs) commutability evaluation biases",
          " for all evaluated material."
        ),
        sortable = TRUE
      )

      cache_fill <- list(
        "by_conclusion_table" = output_tbl_by_conclusion,
        "by_bias_table" = output_tbl_by_bias
      )
      download_material_wise_aggregated_cache(cache_fill)

      return(
        list(
          "by_conclusion_table" = by_conclusion_table,
          "by_bias_table" = by_bias_table
        )
      )

    })

    output$ce_results_aggregated <- renderUI({
      req(ce_results_aggregated_display())

      if (input$aggregated_type == "conclusion") {
        ce_results_aggregated_display()$by_conclusion_table
      }
      else {
        ce_results_aggregated_display()$by_bias_table
      }
    })

    plot_object <- eventReactive(input$plot, {
      results_list <- results_data_cache()
      req(
        results_list,
        !is.null(results_list$merged_ce_data),
        !is.null(results_list$merged_pb_data),
        file_upload_data$is_valid()
      )

      temp_ce_data <- data.table::copy(results_list$merged_ce_data)

      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(temp_ce_data$extrapolate == 0L),
        seq_len(nrow(temp_ce_data))
      )

      na_filter <- which(
        (!is.na(temp_ce_data$MP_A)) & (!is.na(temp_ce_data$MP_B))
      )

      combined_filter <- intersect(
        x = ep_filter,
        y = na_filter
      )

      if (length(combined_filter) > 0) {
        temp_ce_data <- temp_ce_data[combined_filter]
      }

      raw_cs <- data.table::copy(cs_data_long())

      raw_cs <- na.omit(raw_cs)

      cs_agg <- raw_cs[, fasteqa::fun_of_replicates(.SD), by = "comparison"]
      temp_pb_data <- data.table::copy(results_list$merged_pb_data)

      ce_plot <- commutability::plot_commutability_evaluation_plots(
        cs_data = cs_agg,
        ce_data = temp_ce_data,
        pb_data = temp_pb_data,
        exclude_rings = FALSE,
        exclude_cs = FALSE,
        plot_theme = "default",
        additional_arguments = list(
          "main_title" = input$title,
          "sub_title" = "",
          "x_name" = input$x_name,
          "y_name" = input$y_name,
          "n_breaks" = as.numeric(input$tick_density),
          "curve" = (input$cs_curve != "none"),
          "curve_type" = input$cs_curve,
          "hide_prediction_intervals" = TRUE
        )
      ) +
      theme(
        legend.position = "bottom"
      )

      plot_cache(ce_plot)

      js_params <- list(
        n_breaks = as.numeric(input$tick_density),
        exclude_cs = FALSE,
        hide_prediction_intervals = FALSE,
        pb_fill = "#28A745",
        pb_border = "black",
        point_size = 1.5,
        comparison_fill = "#5BCEFA"
      )

      list(
        "static_plot" = ce_plot,
        "interactive_data" = list(
          "cs" = cs_agg,
          "pb" = temp_pb_data,
          "ce" = temp_ce_data,
          "params" = js_params
        )
      )

    })

    output$ce_plots_container <- renderUI({
      payload <- req(plot_object())
      d <- payload$interactive_data

      # Calculate required height based on number of panels (facets)
      n_panels <- length(unique(d$ce$comparison))
      if (n_panels == 0) return(div("No data to display"))

      n_cols <- ceiling(sqrt(n_panels))
      n_rows <- ceiling(n_panels / n_cols)

      # 50px margins + 300px per row (adjust as preferred)
      row_height <- 300
      total_height_px <- 50 + (n_rows * row_height)

      # Important: Use session$ns() here because renderUI generates HTML
      glassChartOutput(
        outputId = session$ns("ce_plots"),
        width = "100%",
        height = paste0(total_height_px, "px")
      )
    })

    observeEvent(plot_object(), {
      payload <- plot_object()
      d <- payload$interactive_data

      # The HTML ID we created above
      chart_id <- session$ns("ce_plots")

      session$onFlushed(function() {
        updateGlassChart(
          session = session,
          inputId = chart_id,
          cs_data = d$cs,
          pb_data = d$pb,
          ce_data = d$ce,
          params  = d$params
        )
      }, once = TRUE)
    })

    optimal_dims <- reactive({
      results_list <- results_data_cache()

      req(
        results_list,
        !is.null(results_list$merged_ce_data)
      )
      n_panels <- length(unique(results_list$merged_ce_data$comparison))

      if (n_panels == 0) {
        return(list(
          width = 17.6,
          height = 11.7
        ))
      }

      n_cols <- ceiling(sqrt(n_panels))
      n_rows <- ceiling(n_panels / n_cols)

      optimal_width <- 6 + (n_cols * 6)
      optimal_height <- 3 + (n_rows * 6)

      optimal_width <- max(12, min(40, optimal_width))
      optimal_height <- max(10, min(50, optimal_height))

      return(
        list(
          width = round(optimal_width, 1),
          height = round(optimal_height, 1)
        )
      )
    })

    observeEvent(optimal_dims(), {
      dims <- optimal_dims()
      updateGlassNumericInput(
        session = session,
        inputId = "width",
        value = dims$width
      )
      updateGlassNumericInput(
        session = session,
        inputId = "height",
        value = dims$height
      )
    })

    observe({
      req(results_data_cache())

      results_list <- results_data_cache()

      if (!is.null(results_list$error)) {
        return(NULL)
      }

      output_tbl <- data.table::copy(results_list$merged_ce_data)

      ep_filter <- switch(
        input$exclude_extrapolations,
        "Yes" = which(output_tbl$extrapolate == 0L),
        seq_len(nrow(output_tbl))
      )

      pi_filter <- switch(input$filter_eq_location,
                          "o_inside" = which(output_tbl$pi_inside == 1L),
                          "o_outside" = which(output_tbl$pi_inside == 0L),
                          seq_len(nrow(output_tbl))
      )

      dins_filter <- switch(input$filter_by_dins,
                            "o_ins" = which(output_tbl$dins_conclusion == 0L),
                            "o_dins" = which(output_tbl$dins_conclusion == 1L),
                            seq_len(nrow(output_tbl))
      )

      combined_filter <- intersect(
        x = ep_filter,
        y = intersect(
          x = pi_filter,
          y = dins_filter
        )
      )

      # --- Keep only rows that satisfy the three given filters ---
      if (length(combined_filter) > 0) {
        output_tbl <- output_tbl[combined_filter]
      }

      choices <- unique(output_tbl$SampleID)
      if (length(choices) == 0) {
        choices <- "None Left After Filtering"
      }

      updateGlassDropdown(
        session = session,
        inputId = "material",
        choices = choices,
        selected = choices[1],
        disabled = !file_upload_data$is_valid()
      )
    })

    observeEvent(input$download_main_table, {
      req(download_main_table_cache())
      bundle <- download_main_table_cache()
      fmt <- input$data_format
      params <- mod_dins_params()
      
      fname <- paste0(
        "ceapkfcr_", fmt, "_ce_main_table_", toupper(params$pi_method), "_",
        paste0("M_used_", params$M), "_", "generated_on_", Sys.Date(), ".xlsx"
      )
      
      dl_url <- session$registerDataObj(
        name = paste0("dl_main_", as.integer(Sys.time())),
        data = list(b = bundle, f = fmt),
        filter = function(data, req) {
          tmp <- tempfile(fileext = ".xlsx")
          to_dl <- if (data$f == "compact") data$b$output_compact else data$b$output_expanded
          
          if (is.null(to_dl)) {
             writexl::write_xlsx(data.table::data.table("Error" = "No Data"), path = tmp)
          } else {
             writexl::write_xlsx(to_dl, path = tmp, col_names = TRUE, format_headers = TRUE)
          }
          
          shiny::httpResponse(
             200, content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             headers = list("Content-Disposition" = paste0("attachment; filename=\"", fname, "\"")),
             content = readBin(tmp, "raw", file.info(tmp)$size)
          )
        }
      )
      triggerGlassDownload(session, "download_main_table", url = dl_url, filename = fname)
    })

    observeEvent(input$download_material_wise, {
      req(download_material_wise_cache())
      bundle <- download_material_wise_cache()
      mat <- input$material
      params <- mod_dins_params()
      
      fname <- paste0(
        "ceapkfcr_", "ce_conclusion_table_", "for_", mat, "_", toupper(params$pi_method), "_",
        paste0("M_used_", params$M), "_", "generated_on_", Sys.Date(), ".xlsx"
      )
      
      dl_url <- session$registerDataObj(
        name = paste0("dl_mat_", as.integer(Sys.time())),
        data = list(b = bundle),
        filter = function(data, req) {
          tmp <- tempfile(fileext = ".xlsx")
          if (is.null(data$b)) {
             writexl::write_xlsx(data.table::data.table("Error" = "No Data"), path = tmp)
          } else {
             writexl::write_xlsx(data$b, path = tmp, col_names = TRUE, format_headers = TRUE)
          }
          shiny::httpResponse(
             200, content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             headers = list("Content-Disposition" = paste0("attachment; filename=\"", fname, "\"")),
             content = readBin(tmp, "raw", file.info(tmp)$size)
          )
        }
      )
      triggerGlassDownload(session, "download_material_wise", url = dl_url, filename = fname)
    })

    observeEvent(input$download_material_wise_aggregated, {
      req(download_material_wise_aggregated_cache())
      bundle <- download_material_wise_aggregated_cache()
      agg_type <- input$aggregated_type
      params <- mod_dins_params()
      
      fname <- paste0(
        "ceapkfcr_", "ce_aggregated_", agg_type, "_table_",
        toupper(params$pi_method), "_", paste0("M_used_", params$M), "_",
        "generated_on_", Sys.Date(), ".xlsx"
      )
      
      dl_url <- session$registerDataObj(
        name = paste0("dl_agg_", as.integer(Sys.time())),
        data = list(b = bundle, t = agg_type),
        filter = function(data, req) {
          tmp <- tempfile(fileext = ".xlsx")
          to_dl <- if (data$t == "conclusion") data$b$by_conclusion_table else data$b$by_bias_table
          if (is.null(to_dl)) {
             writexl::write_xlsx(data.table::data.table("Error" = "No Data"), path = tmp)
          } else {
             writexl::write_xlsx(to_dl, path = tmp, col_names = TRUE, format_headers = TRUE)
          }
          shiny::httpResponse(
             200, content_type = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
             headers = list("Content-Disposition" = paste0("attachment; filename=\"", fname, "\"")),
             content = readBin(tmp, "raw", file.info(tmp)$size)
          )
        }
      )
      triggerGlassDownload(session, "download_material_wise_aggregated", url = dl_url, filename = fname)
    })

    observeEvent(input$download_ce_plots, {
      bundle <- plot_object()
      req(bundle$static_plot)
      
      params <- mod_dins_params()
      ftype <- input$plot_download_file_type
      w <- as.numeric(input$width)
      h <- as.numeric(input$height)
      q <- as.numeric(input$plot_download_quality)
      
      fname <- paste0(
        "ce_plots_", toupper(params$pi_method), "_", params$transformation, "_",
        paste0("M_used_", params$M), "generated_on_", Sys.Date(), ftype
      )
      
      dl_url <- session$registerDataObj(
        name = paste0("dl_plot_", as.integer(Sys.time())),
        data = list(p = bundle$static_plot, w=w, h=h, q=q, ftype=ftype),
        filter = function(data, req) {
          tmp <- tempfile(fileext = data$ftype)
          ggplot2::ggsave(tmp, plot = data$p, width = data$w, height = data$h, units = "cm", dpi = data$q)
          shiny::httpResponse(
             200, content_type = "application/octet-stream",
             headers = list("Content-Disposition" = paste0("attachment; filename=\"", fname, "\"")),
             content = readBin(tmp, "raw", file.info(tmp)$size)
          )
        }
      )
      triggerGlassDownload(session, "download_ce_plots", url = dl_url, filename = fname)
    })

    observeEvent(input$download_report, {
      toast_id <- showGlassToast(
        message = "Generating report... This may take a moment.",
        title = "Report Generation",
        type = "message",
        duration = NULL,
        closeButton = FALSE,
        session = session
      )
      on.exit(removeGlassToast(toast_id, session = session))

      params <- list(
          cs_data = tryCatch(file_upload_data$raw_cs_data(), error = function(e) NA),
          eq_data = tryCatch(file_upload_data$raw_eq_data(), error = function(e) NA),
          diagnostics_cs = tryCatch(file_upload_data$diagnostics_cs(), error = function(e) NA),
          diagnostics_eq = tryCatch(file_upload_data$diagnostics_eq(), error = function(e) NA),
          diagnostics_both = tryCatch(file_upload_data$diagnostics_both(), error = function(e) NA),
          outlier_results = tryCatch(outlier_data$results(), error = function(e) NA),
          outlier_params = tryCatch(outlier_data$params(), error = function(e) NA),
          formal_assessment_results = tryCatch(model_validation_data$formal_results(), error = function(e) NA),
          assessment_plot = tryCatch(model_validation_data$assessment_plot(), error = function(e) NA),
          assessment_plot_type = tryCatch(model_validation_data$assessment_plot_type(), error = function(e) NA),
          dins_params = tryCatch(mod_dins_params(), error = function(e) NA),
          ce_results = tryCatch(results_data_cache(), error = function(e) NA),
          ce_plot = tryCatch(plot_object(), error = function(e) NA)
      )
      
      temp_dir <- tempdir()
      report_path <- system.file("report.Rmd", package = "CommutabilityevaluationofEQAMs")
      temp_report <- file.path(temp_dir, "report.Rmd")
      file.copy(report_path, temp_report, overwrite = TRUE)
      
      rendered_report <- rmarkdown::render(
          input = temp_report,
          output_format = "pdf_document",
          params = params,
          envir = new.env(parent = globalenv()),
          intermediates_dir = temp_dir
      )
      
      fname <- paste0("Commutability-Evaluation-Report-", Sys.Date(), ".", "pdf")
      
      dl_url <- session$registerDataObj(
        name = paste0("dl_rep_", as.integer(Sys.time())),
        data = list(path = rendered_report),
        filter = function(data, req) {
          shiny::httpResponse(
             200, content_type = "application/pdf",
             headers = list("Content-Disposition" = paste0("attachment; filename=\"", fname, "\"")),
             content = readBin(data$path, "raw", file.info(data$path)$size)
          )
        }
      )
      triggerGlassDownload(session, "download_report", url = dl_url, filename = fname)
    }, ignoreInit = TRUE)

    outputOptions(output, "ce_results", suspendWhenHidden = FALSE)
    outputOptions(output, "ce_results_grid", suspendWhenHidden = FALSE)
    outputOptions(output, "ce_results_aggregated", suspendWhenHidden = FALSE)
    outputOptions(output, "ce_plots_container", suspendWhenHidden = FALSE)
  })
}
