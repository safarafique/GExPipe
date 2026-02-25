# ==============================================================================
# SERVER.R - Main Server File (Modular Structure)
# ==============================================================================
# 
# This file initializes reactive values and sources all server modules:
#   - server_download.R: Step 1 - Data download
#   - server_qc.R: Step 2 - QC & Visualization
#   - server_normalize.R: Step 3 - Normalization
#   - server_groups.R: Step 4 - Group selection (already exists)
#   - server_batch.R: Step 5 - Batch correction
#   - server_results.R: Step 6 - Differential Gene Expression Analysis
# ==============================================================================

server <- function(input, output, session) {
  
  # Guided tour (Cicerone) ------------------------------------------------------
  guide <- tryCatch({
    if (requireNamespace("cicerone", quietly = TRUE)) {
      cicerone::Cicerone$
        new()$
        step(
          el = "sidebar_menu",
          title = "Navigation",
          description = "Use this sidebar to navigate through the 16-step pipeline."
        )$
        step(
          el = "analysis_type",
          title = "Platform",
          description = "Choose your data type: RNA-seq, Microarray, or Merged."
        )$
        step(
          el = "start_processing",
          title = "Start",
          description = "Click here to begin downloading and processing your datasets."
        )
    } else {
      NULL
    }
  }, error = function(e) NULL)

  # Reactive values - shared across all modules
  rv <- reactiveValues(
    show_analysis = FALSE,
    disease_name = "",
    micro_expr_list = list(),
    micro_eset_list = list(),
    micro_metadata_list = list(),
    micro_cel_paths = list(),  # CEL file paths per GSE for optional RMA normalization
    rna_counts_list = list(),
    rna_metadata_list = list(),
    all_genes_list = list(),
    common_genes = NULL,
    combined_expr_raw = NULL,
    combined_expr = NULL,
    unified_metadata = NULL,
    expr_filtered = NULL,
    batch_corrected = NULL,
    de_results = NULL,
    sig_genes = NULL,
    # DE method and raw counts for DESeq2
    de_method = "limma",
    raw_counts_for_deseq2 = NULL,
    raw_counts_metadata = NULL,
    download_complete = FALSE,
    normalization_complete = FALSE,
    groups_applied = FALSE,
    batch_complete = FALSE,
    # WGCNA
    wgcna_prepared = FALSE,
    wgcna_complete = FALSE,
    datExpr = NULL,
    wgcna_top_variable_genes = NULL,
    wgcna_gene_variance_table = NULL,
    wgcna_sample_info = NULL,
    wgcna_sample_tree = NULL,
    soft_threshold = NULL,
    soft_threshold_powers = NULL,
    moduleColors = NULL,
    dynamicColors = NULL,
    MEs = NULL,
    geneTree = NULL,
    trait_data = NULL,
    moduleTraitCor = NULL,
    moduleTraitPvalue = NULL,
    gene_metrics = NULL,
    geneModuleMembership = NULL,
    MMPvalue = NULL,
    wgcna_log_messages = character(0),
    ME_correlation = NULL,
    ME_tree = NULL,
    significant_modules = NULL,
    # Common genes (DEG & WGCNA)
    common_genes_de_wgcna = NULL,
    common_genes_df = NULL,
    common_genes_deg_n = 0L,
    common_genes_wgcna_n = 0L,
    go_bp = NULL,
    go_mf = NULL,
    go_cc = NULL,
    kegg_enrichment = NULL,
    # PPI (common genes)
    ppi_graph = NULL,
    ppi_hub_scores = NULL,
    ppi_consensus_hubs = NULL,
    ppi_hub_rankings = NULL,
    ppi_interactive_genes = NULL,
    ppi_non_interactive_genes = NULL,
    ppi_gene_status_table = NULL,
    ppi_complete = FALSE,
    ppi_centrality_filtered_genes = NULL,
    ppi_centrality_weights = NULL,
    ppi_centrality_table = NULL,
    extracted_data_ml = NULL,
    ml_lasso_df = NULL,
    ml_rf_importance = NULL,
    ml_svm_ranking = NULL,
    ml_boruta_df = NULL,
    ml_splsda_df = NULL,
    ml_xgboost_df = NULL,
    ml_common_genes = NULL,
    ml_venn_sets = NULL,
    ml_methods_run = NULL,
    ml_x = NULL,
    ml_y = NULL,
    ml_complete = FALSE,
    nomogram_complete = FALSE,
    nomogram_model = NULL,
    nomogram_train_data = NULL,
    nomogram_validation_data = NULL,
    nomogram_available_genes = NULL,
    nomogram_optimal_threshold = NULL,
    nomogram_train_metrics = NULL,
    nomogram_val_metrics = NULL,
    nomogram_train_roc = NULL,
    nomogram_val_roc = NULL,
    nomogram_model_diagnostics = NULL,
    nomogram_performance_comparison = NULL,
    nomogram_cal_train = NULL,
    nomogram_cal_validation = NULL,
    nomogram_dca_train = NULL,
    nomogram_dca_val = NULL,
    nomogram_ci_train = NULL,
    nomogram_ci_val = NULL,
    # Validation mode ("external" or "internal")
    validation_mode = "external",
    # External validation dataset (cross-module)
    external_validation_expr = NULL,
    external_validation_outcome = NULL,
    external_validation_group_col = NULL,
    external_validation_n_disease = NULL,
    external_validation_n_normal = NULL,
    external_validation_gene_names = NULL,
    external_validation_raw_expr = NULL,
    external_validation_metadata = NULL,
    ext_val_log = NULL,
    ext_val_raw_expr = NULL,
    ext_val_metadata = NULL,
    ext_val_downloaded = NULL,
    ext_val_de_results = NULL,
    ext_val_sig_genes = NULL,
    nom_ext_val_raw_expr = NULL,
    nom_ext_val_metadata = NULL,
    nom_ext_val_downloaded = NULL,
    # External validation nomogram results
    nomogram_ext_val_data = NULL,
    nomogram_ext_val_metrics = NULL,
    nomogram_ext_val_roc = NULL,
    nomogram_ext_cal = NULL,
    nomogram_ext_dca = NULL,
    nomogram_ext_ci = NULL,
    gsea_result = NULL,
    gsea_target_genes = NULL,
    gsea_results_by_gene = NULL,
    gsea_complete = FALSE,
    immune_raw = NULL,
    immune_matrix = NULL,
    immune_data = NULL,
    immune_long = NULL,
    immune_method = NULL,
    immune_cell_cols = NULL,
    immune_complete = FALSE,
    # timers
    download_start = NULL,
    download_running = FALSE,
    download_complete_at = NULL,
    auto_save_after_download_done = FALSE,
    normalize_start = NULL,
    normalize_running = FALSE,
    batch_start = NULL,
    batch_running = FALSE,
    de_start = NULL,
    de_running = FALSE,
    wgcna_start = NULL,
    wgcna_running = FALSE
  )

  # Welcome page: expose show_analysis for conditionalPanel; "Go to Analysis" sets it TRUE
  output$show_analysis <- reactive(rv$show_analysis)
  outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)
  observeEvent(input$go_to_analysis, {
    rv$show_analysis <- TRUE
  })

  # Source all server modules
  source("server/server_download.R", local = TRUE)
  source("server/server_qc.R", local = TRUE)
  source("server/server_normalize.R", local = TRUE)
  source("server/server_batch.R", local = TRUE)
  source("server/server_results.R", local = TRUE)
  source("server/server_wgcna.R", local = TRUE)
  source("server/server_common_genes.R", local = TRUE)
  source("server/server_ppi.R", local = TRUE)
  source("server/server_ml.R", local = TRUE)
  source("server/server_validation.R", local = TRUE)
  source("server/server_roc.R", local = TRUE)
  source("server/server_nomogram.R", local = TRUE)
  source("server/server_gsea.R", local = TRUE)
  source("server/server_immune.R", local = TRUE)
  
  # Call module functions
  server_download(input, output, session, rv)
  server_qc(input, output, session, rv)
  server_normalize(input, output, session, rv)
  server_batch(input, output, session, rv)
  server_results(input, output, session, rv)
  server_wgcna(input, output, session, rv)
  server_common_genes(input, output, session, rv)
  server_ppi(input, output, session, rv)
  server_ml(input, output, session, rv)
  server_validation(input, output, session, rv)
  server_roc(input, output, session, rv)
  server_nomogram(input, output, session, rv)
  server_gsea(input, output, session, rv)
  server_immune(input, output, session, rv)
  source("server/server_results_summary.R", local = TRUE)
  server_results_summary(input, output, session, rv)

  # Step 4: Group selection module (already exists as separate file)
  source("server/server_groups.R", local = TRUE)
  server_groups(input, output, session, rv)
  
  # ==============================================================================
  # TRACK DE METHOD SELECTION (sync input → rv for access in all modules)
  # ==============================================================================
  observe({
    rv$de_method <- input$de_method
  })
  
  observe({
    rv$disease_name <- trimws(if (is.null(input$disease_name)) "" else input$disease_name)
  })
  
  # Auto-switch to limma if user selects microarray-only and a count-based method
  observeEvent(input$analysis_type, {
    if (input$analysis_type == "microarray" &&
        !is.null(input$de_method) &&
        input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      updateRadioButtons(session, "de_method", selected = "limma")
      showNotification(
        tags$div(icon("info-circle"),
                 tags$strong(" DE method switched to limma."),
                 " DESeq2, edgeR, and limma-voom require RNA-seq count data and are not applicable to microarray."),
        type = "warning", duration = 6)
    }
  })
  
  # ==============================================================================
  # PIPELINE PROGRESS TRACKER (dynamic, reactive, clickable)
  # ==============================================================================
  output$pipeline_progress <- renderUI({
    # Define each pipeline step: id, label, icon, tab-name, completion status, running status
    is_count_based <- isTRUE(rv$de_method %in% c("deseq2", "edger", "limma_voom"))
    
    # Build step list — when DESeq2/edgeR, normalize step shows as "Auto" (always done) 
    steps <- list(
      list(id = "download",   label = "Download",     icon = "download",        tab = "download",       done = isTRUE(rv$download_complete),        running = isTRUE(rv$download_running)))
    steps <- c(steps, list(
      list(id = "qc",         label = "QC",            icon = "chart-bar",       tab = "qc",             done = isTRUE(rv$download_complete),        running = FALSE)))
    if (!is_count_based) {
      steps <- c(steps, list(
        list(id = "normalize",  label = "Normalize",     icon = "balance-scale",   tab = "normalize",      done = isTRUE(rv$normalization_complete),   running = isTRUE(rv$normalize_running))))
    } else {
      steps <- c(steps, list(
        list(id = "normalize",  label = "Norm (Auto)",   icon = "magic",           tab = "normalize",      done = isTRUE(rv$normalization_complete),   running = isTRUE(rv$normalize_running))))
    }
    de_label <- if (rv$de_method == "deseq2") {
      "DE (DESeq2)"
    } else if (rv$de_method == "edger") {
      "DE (edgeR)"
    } else if (rv$de_method == "limma_voom") {
      "DE (limma-voom)"
    } else {
      "DE (limma)"
    }
    steps <- c(steps, list(
      list(id = "groups",     label = "Groups",        icon = "users",           tab = "groups",         done = isTRUE(rv$groups_applied),           running = FALSE),
      list(id = "batch",      label = "Batch",         icon = "filter",          tab = "batch",          done = isTRUE(rv$batch_complete),           running = isTRUE(rv$batch_running)),
      list(id = "de",         label = de_label,        icon = "dna",             tab = "results",        done = !is.null(rv$de_results),             running = isTRUE(rv$de_running)),
      list(id = "wgcna",      label = "WGCNA",         icon = "project-diagram", tab = "wgcna",          done = isTRUE(rv$wgcna_complete),           running = isTRUE(rv$wgcna_running)),
      list(id = "common",     label = "Common Genes",  icon = "venus-double",    tab = "common_genes",   done = length(rv$common_genes_de_wgcna) > 0, running = FALSE),
      list(id = "ppi",        label = "PPI",           icon = "project-diagram", tab = "ppi",            done = isTRUE(rv$ppi_complete),             running = FALSE),
      list(id = "ml",         label = "ML",            icon = "brain",           tab = "ml",             done = isTRUE(rv$ml_complete),              running = FALSE),
      list(id = "validation", label = "Validation",    icon = "shield-alt",      tab = "validation",     done = !is.null(rv$validation_mode) && (rv$validation_mode == "internal" || !is.null(rv$external_validation_expr)), running = FALSE),
      list(id = "roc",        label = "ROC",           icon = "chart-line",      tab = "roc",            done = !is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0, running = FALSE),
      list(id = "nomogram",   label = "Nomogram",      icon = "calculator",      tab = "nomogram",       done = isTRUE(rv$nomogram_complete),        running = FALSE),
      list(id = "gsea",       label = "GSEA",          icon = "project-diagram", tab = "gsea",           done = isTRUE(rv$gsea_complete),            running = FALSE),
      list(id = "immune",     label = "Immune",        icon = "shield-alt",      tab = "immune",         done = isTRUE(rv$immune_complete),          running = FALSE)
    ))
    
    n_done <- sum(vapply(steps, function(s) isTRUE(s$done), logical(1)))
    n_total <- length(steps)
    pct <- round(n_done / n_total * 100)
    
    # Build step chips
    step_chips <- lapply(steps, function(s) {
      if (isTRUE(s$running)) {
        cls <- "pipeline-step running"
        ic  <- "spinner fa-spin"
      } else if (isTRUE(s$done)) {
        cls <- "pipeline-step done"
        ic  <- "check-circle"
      } else {
        cls <- "pipeline-step pending"
        ic  <- s$icon
      }
      tags$span(
        class = cls,
        `data-tab` = s$tab,
        title = paste0("Go to: ", s$label),
        tags$i(class = paste0("fa fa-", ic, " step-icon")),
        s$label
      )
    })
    
    tags$div(
      class = "pipeline-tracker",
      tags$div(
        class = "pipeline-tracker-header",
        tags$span(class = "pipeline-tracker-title",
                  icon("tasks"), " Pipeline Progress"),
        tags$span(class = "pipeline-tracker-pct",
                  paste0(n_done, " / ", n_total, " steps \u2014 ", pct, "%"))
      ),
      tags$div(class = "progress",
        tags$div(class = "progress-bar",
                 role = "progressbar",
                 style = paste0("width: ", pct, "%;"),
                 `aria-valuenow` = pct,
                 `aria-valuemin` = "0",
                 `aria-valuemax` = "100")
      ),
      tags$div(class = "pipeline-steps-row", step_chips)
    )
  })
  
  # ==============================================================================
  # STEP GUARDS: disable "Run" buttons until prerequisites are met
  # ==============================================================================
  observe({
    # Step 3: Normalize – requires download complete
    shinyjs::toggleState("apply_normalization",
                         condition = isTRUE(rv$download_complete))
    # Step 4: Extract groups – requires download complete
    shinyjs::toggleState("extract_groups_btn",
                         condition = isTRUE(rv$download_complete))
    # Step 4: Apply groups – requires download complete (groups extracted checked inside handler)
    shinyjs::toggleState("apply_groups_btn",
                         condition = isTRUE(rv$download_complete))
    # Step 5: Batch correction – requires groups applied
    shinyjs::toggleState("apply_batch",
                         condition = isTRUE(rv$groups_applied))
    # Step 6: DE analysis – requires batch complete
    shinyjs::toggleState("run_de",
                         condition = isTRUE(rv$batch_complete))
    # Step 7: WGCNA prepare – requires batch complete
    shinyjs::toggleState("prepare_wgcna",
                         condition = isTRUE(rv$batch_complete))
    # Step 7: Pick soft threshold – requires WGCNA prepared
    shinyjs::toggleState("pick_soft_threshold",
                         condition = isTRUE(rv$wgcna_prepared))
    # Step 7: Run WGCNA network – requires WGCNA prepared
    shinyjs::toggleState("run_wgcna",
                         condition = isTRUE(rv$wgcna_prepared))
    # Step 7: Module-trait – requires WGCNA complete
    shinyjs::toggleState("calculate_module_trait",
                         condition = isTRUE(rv$wgcna_complete))
    # Step 7: ME relationships – requires WGCNA complete
    shinyjs::toggleState("calculate_me_relationships",
                         condition = isTRUE(rv$wgcna_complete))
    # Step 7: Identify significant modules – requires WGCNA complete
    shinyjs::toggleState("identify_significant_modules",
                         condition = isTRUE(rv$wgcna_complete))
    # Step 8: Common genes – requires DE + WGCNA
    shinyjs::toggleState("compute_common_genes",
                         condition = !is.null(rv$de_results) && isTRUE(rv$wgcna_complete))
    # Step 8: GO enrichment – requires common genes
    shinyjs::toggleState("run_go_enrichment",
                         condition = length(rv$common_genes_de_wgcna) > 0)
    # Step 8: KEGG enrichment – requires common genes
    shinyjs::toggleState("run_kegg_enrichment",
                         condition = length(rv$common_genes_de_wgcna) > 0)
    # Step 8: Extract ML data (common genes path) – requires common genes
    shinyjs::toggleState("extract_ml_data_common_genes",
                         condition = length(rv$common_genes_de_wgcna) > 0)
    # Step 9: PPI – requires common genes
    shinyjs::toggleState("run_ppi",
                         condition = length(rv$common_genes_de_wgcna) > 0)
    # Step 9: Extract ML data (PPI path) – requires PPI complete
    shinyjs::toggleState("extract_ml_data",
                         condition = isTRUE(rv$ppi_complete))
    # Step 10: ML – requires extracted data
    shinyjs::toggleState("run_ml",
                         condition = !is.null(rv$extracted_data_ml))
    # Step 11: Validation Setup – no run guard needed (choice-based)
    # Step 12: ROC is reactive (no run button) – no guard needed
    # Step 13: Nomogram – requires batch + common genes
    shinyjs::toggleState("run_nomogram",
                         condition = isTRUE(rv$batch_complete) &&
                           (length(rv$ml_common_genes) > 0 || length(rv$common_genes_de_wgcna) > 0))
    # Step 14: GSEA – requires batch corrected data
    shinyjs::toggleState("run_gsea",
                         condition = isTRUE(rv$batch_complete) &&
                           (length(rv$ml_common_genes) > 0 || length(rv$common_genes_de_wgcna) > 0))
    # Step 15: Immune – requires batch corrected data
    shinyjs::toggleState("run_immune",
                         condition = isTRUE(rv$batch_complete))
  })
  
  # Dynamic "Next" button on QC tab: shows correct destination
  output$qc_next_button <- renderUI({
    if (!is.null(input$de_method) && input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      actionButton("next_to_normalize",
                   tagList(icon("arrow-right"), " Next: Select Groups (Normalize auto-handled)"),
                   class = "btn-success btn-lg",
                   style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")
    } else {
      actionButton("next_to_normalize", "Next: Normalize Data",
                   icon = icon("arrow-right"), class = "btn-success btn-lg",
                   style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;")
    }
  })

  # ==============================================================================
  # NEXT BUTTON: move to next tab (applies to all steps)
  # ==============================================================================
  # Tab order: download → qc → [normalize] → groups → batch → results → wgcna → ...
  # When DESeq2, skip normalize (Step 3) entirely.
  observeEvent(input$next_page_download, { updateTabItems(session, "sidebar_menu", "qc") })
  observeEvent(input$next_to_normalize, {
    if (!is.null(input$de_method) && input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      # Skip normalize → go directly to groups (count-based methods auto-handle normalization)
      updateTabItems(session, "sidebar_menu", "groups")
    } else {
      updateTabItems(session, "sidebar_menu", "normalize")
    }
  })
  observeEvent(input$next_page_normalize,{ updateTabItems(session, "sidebar_menu", "groups") })
  observeEvent(input$go_to_groups,       { updateTabItems(session, "sidebar_menu", "groups") })
  observeEvent(input$go_to_groups_from_norm, { updateTabItems(session, "sidebar_menu", "groups") })
  observeEvent(input$next_page_groups,   { updateTabItems(session, "sidebar_menu", "batch") })
  observeEvent(input$next_to_batch_btn,  { updateTabItems(session, "sidebar_menu", "batch") })
  observeEvent(input$go_to_results,      { updateTabItems(session, "sidebar_menu", "results") })
  observeEvent(input$next_page_batch,    { updateTabItems(session, "sidebar_menu", "results") })
  observeEvent(input$next_page_results,  { updateTabItems(session, "sidebar_menu", "wgcna") })
  observeEvent(input$next_page_wgcna,    { updateTabItems(session, "sidebar_menu", "common_genes") })
  observeEvent(input$next_page_common_genes_end, { updateTabItems(session, "sidebar_menu", "ppi") })
  observeEvent(input$next_page_common_genes_to_ml, { updateTabItems(session, "sidebar_menu", "ml") })
  observeEvent(input$next_page_ppi,      { updateTabItems(session, "sidebar_menu", "ml") })
  observeEvent(input$next_page_ml,       { updateTabItems(session, "sidebar_menu", "download") })
  observeEvent(input$next_page_ml_to_roc, { updateTabItems(session, "sidebar_menu", "validation") })
  observeEvent(input$next_page_ml_to_validation, { updateTabItems(session, "sidebar_menu", "validation") })
  observeEvent(input$next_page_roc,       { updateTabItems(session, "sidebar_menu", "download") })
  observeEvent(input$next_page_roc_to_nomogram, { updateTabItems(session, "sidebar_menu", "nomogram") })
  observeEvent(input$next_page_roc_to_gsea, { updateTabItems(session, "sidebar_menu", "gsea") })
  observeEvent(input$next_page_nomogram_to_gsea, { updateTabItems(session, "sidebar_menu", "gsea") })
  observeEvent(input$next_page_nomogram_to_results, { updateTabItems(session, "sidebar_menu", "results_summary") })
  observeEvent(input$next_page_gsea,        { updateTabItems(session, "sidebar_menu", "download") })
  observeEvent(input$next_page_gsea_to_immune, { updateTabItems(session, "sidebar_menu", "immune") })
  observeEvent(input$next_page_immune_to_results, { updateTabItems(session, "sidebar_menu", "results_summary") })
  observeEvent(input$next_page_roc_to_results, { updateTabItems(session, "sidebar_menu", "results_summary") })

  # ==============================================================================
  # SAVE WORKSPACE - save current step to app directory + download .rds (resume via Load)
  # ==============================================================================
  # Build serializable state (drop elements that would break saveRDS)
  make_workspace_state <- function() {
    current_step <- input$sidebar_menu
    if (is.null(current_step) || !nzchar(current_step)) current_step <- "download"
    tryCatch({
      raw_list <- reactiveValuesToList(rv)
      raw_list$saved_step <- current_step
      drop_names <- c("download_start", "normalize_start", "batch_start", "de_start", "wgcna_start")
      for (d in drop_names) raw_list[[d]] <- NULL
      state <- list()
      for (nm in names(raw_list)) {
        tryCatch({
          serialize(raw_list[[nm]], NULL)
          state[[nm]] <- raw_list[[nm]]
        }, error = function(e) NULL)
      }
      if (!"saved_step" %in% names(state)) state$saved_step <- current_step
      state
    }, error = function(e) {
      list(saved_step = current_step, saved_note = "Minimal save; full state could not be read.")
    })
  }

  output$download_workspace <- downloadHandler(
    filename = function() {
      tryCatch({
        custom <- trimws(input$workspace_save_filename)
        if (is.null(custom) || !nzchar(custom)) {
          step <- input$sidebar_menu
          if (is.null(step) || !nzchar(step)) step <- "workspace"
          return(paste0("app_saved_state_", step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
        }
        base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
        base <- sub("_+$", "", base)
        if (!nzchar(base)) base <- "workspace"
        paste0(base, ".rds")
      }, error = function(e) paste0("workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
    },
    content = function(file) {
      err_msg <- NULL
      tryCatch({
        state <- make_workspace_state()
        current_step <- state$saved_step
        # Save to app directory first (so file is always saved even if download is blocked)
        app_dir <- getwd()
        save_subdir <- "saved_workspaces"
        save_dir <- file.path(app_dir, save_subdir)
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        custom <- trimws(input$workspace_save_filename)
        if (nzchar(custom)) {
          base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
          base <- sub("_+$", "", base)
          if (!nzchar(base)) base <- "workspace"
          fname <- paste0(base, ".rds")
        } else {
          fname <- paste0("app_saved_state_", current_step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        }
        app_path <- file.path(save_dir, fname)
        saved_to_disk <- FALSE
        if (dir.exists(save_dir)) {
          tryCatch({
            saveRDS(state, app_path)
            saved_to_disk <- TRUE
          }, error = function(e2) NULL)
        }
        # Always write to file for browser download (required for Shiny download to complete)
        saveRDS(state, file)
        if (saved_to_disk) {
          showNotification(
            paste0("Saved! File is in folder '", save_subdir, "'. Load it at Step 1 to return to this step."),
            type = "message", duration = 8
          )
        } else {
          showNotification("Saved! Use Step 1 → Load and choose this file to return to this step.", type = "message", duration = 8)
        }
      }, error = function(e) {
        err_msg <<- conditionMessage(e)
      })
      if (!is.null(err_msg)) {
        # Fallback: save minimal state so user at least gets a file and can return to step
        minimal <- list(saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu, saved_note = "Minimal save; some data could not be serialized.")
        tryCatch({
          saveRDS(minimal, file)
          showNotification("Saved minimal state (some data was skipped). Load at Step 1 to return to your step.", type = "warning", duration = 8)
        }, error = function(e2) {
          showNotification(paste("Save failed:", err_msg), type = "error", duration = 10)
        })
      }
    }
  )

  # Auto-save 5 minutes after download completes (user data saved to saved_workspaces)
  observe({
    if (!isTRUE(rv$download_complete)) return()
    if (is.null(rv$download_complete_at)) rv$download_complete_at <- Sys.time()
    if (isTRUE(rv$auto_save_after_download_done)) return()
    elapsed_sec <- as.numeric(difftime(Sys.time(), rv$download_complete_at, units = "secs"))
    if (elapsed_sec < 300) {
      invalidateLater(60000, session)
      return()
    }
    rv$auto_save_after_download_done <- TRUE
    tryCatch({
      state <- make_workspace_state()
      current_step <- state$saved_step
      save_dir <- file.path(getwd(), "saved_workspaces")
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      fname <- paste0("auto_save_after_download_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      app_path <- file.path(save_dir, fname)
      saveRDS(state, app_path)
      showNotification(
        paste0("Auto-save: data saved to saved_workspaces/", fname, ". Load at Step 1 to return to this state."),
        type = "message",
        duration = 8
      )
    }, error = function(e) {
      tryCatch({
        minimal <- list(saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu, saved_note = "Auto-save minimal state.")
        save_dir <- file.path(getwd(), "saved_workspaces")
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        fname <- paste0("auto_save_after_download_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        saveRDS(minimal, file.path(save_dir, fname))
        showNotification(paste0("Auto-save (minimal) to saved_workspaces/", fname, "."), type = "warning", duration = 8)
      }, error = function(e2) NULL)
    })
  })

  # Save to folder only (no browser download) - works even when download is blocked
  observeEvent(input$save_workspace_to_folder, {
    tryCatch({
      state <- make_workspace_state()
      current_step <- state$saved_step
      app_dir <- getwd()
      save_subdir <- "saved_workspaces"
      save_dir <- file.path(app_dir, save_subdir)
      if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
      custom <- trimws(input$workspace_save_filename)
      if (is.null(custom)) custom <- ""
      if (nzchar(custom)) {
        base <- gsub("[^A-Za-z0-9_.-]+", "_", custom)
        base <- sub("_+$", "", base)
        if (!nzchar(base)) base <- "workspace"
        fname <- paste0(base, ".rds")
      } else {
        fname <- paste0("app_saved_state_", current_step, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
      }
      app_path <- file.path(save_dir, fname)
      saveRDS(state, app_path)
      showNotification(
        paste0("Saved to '", save_subdir, "/", fname, "'. Load it at Step 1 (Download) to return to this step."),
        type = "message", duration = 8
      )
    }, error = function(e) {
      tryCatch({
        minimal <- list(saved_step = if (is.null(input$sidebar_menu) || !nzchar(input$sidebar_menu)) "download" else input$sidebar_menu, saved_note = "Minimal save.")
        save_dir <- file.path(getwd(), "saved_workspaces")
        if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE, showWarnings = FALSE)
        fname <- paste0("workspace_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds")
        saveRDS(minimal, file.path(save_dir, fname))
        showNotification(paste0("Saved minimal state to saved_workspaces/", fname, ". Load at Step 1."), type = "warning", duration = 8)
      }, error = function(e2) {
        showNotification(paste("Save failed:", conditionMessage(e)), type = "error", duration = 10)
      })
    })
  })

  # ==============================================================================
  # LOAD SAVED DATA (Download tab) - restore state and go to the step where user saved
  # ==============================================================================
  get_saved_workspaces_dir <- function() {
    d <- file.path(getwd(), "saved_workspaces")
    if (dir.exists(d)) return(d)
    d <- file.path(".", "saved_workspaces")
    if (dir.exists(d)) return(normalizePath(d, winslash = "/"))
    file.path(getwd(), "saved_workspaces")
  }

  output$load_from_folder_ui <- renderUI({
    save_dir <- get_saved_workspaces_dir()
    if (!dir.exists(save_dir)) return(selectInput("load_from_folder_file", NULL, choices = c("(No saved workspaces)" = ""), width = "100%"))
    f <- list.files(save_dir, pattern = "\\.rds$", full.names = FALSE)
    if (length(f) == 0) return(selectInput("load_from_folder_file", NULL, choices = c("(No .rds files)" = ""), width = "100%"))
    choices <- setNames(f, f)
    selectInput("load_from_folder_file", NULL, choices = c("— Select file —" = "", choices), width = "100%")
  })

  observeEvent(input$skip_load_btn, {
    showNotification("You can start a new analysis: choose platform above and click 'Start Processing' below.", type = "message", duration = 5)
  })

  load_restore_state <- function(state) {
    if (!is.list(state) || is.null(state$saved_step)) {
      showNotification("Invalid saved file (missing step). Use a file saved by this app.", type = "error", duration = 6)
      return(invisible(NULL))
    }
    step <- state$saved_step
    if (!is.character(step) || !nzchar(step)) step <- "download"
    for (nm in setdiff(names(state), "saved_step")) {
      tryCatch({ rv[[nm]] <- state[[nm]] }, error = function(e) NULL)
    }
    if (!is.null(rv$combined_expr) && (is.matrix(rv$combined_expr) || is.data.frame(rv$combined_expr)) && nrow(rv$combined_expr) > 0) {
      rv$download_complete <- TRUE
    }
    if (!is.null(rv$batch_corrected) && (is.matrix(rv$batch_corrected) || is.data.frame(rv$batch_corrected)) && nrow(rv$batch_corrected) > 0) {
      rv$batch_complete <- TRUE
      rv$groups_applied <- TRUE
    }
    if (!is.null(rv$expr_filtered) && (is.matrix(rv$expr_filtered) || is.data.frame(rv$expr_filtered)) && nrow(rv$expr_filtered) > 0) {
      rv$normalization_complete <- TRUE
    }
    if (!is.null(rv$moduleColors) && length(rv$moduleColors) > 0 && !is.null(rv$gene_metrics) && !is.null(rv$significant_modules)) {
      rv$wgcna_complete <- TRUE
      rv$wgcna_prepared <- TRUE
    }
    if (!is.null(rv$common_genes_de_wgcna) && length(rv$common_genes_de_wgcna) > 0) {
      if (is.null(rv$common_genes_df)) rv$common_genes_df <- data.frame(Gene = rv$common_genes_de_wgcna, stringsAsFactors = FALSE)
    }
    if ((!is.null(rv$ppi_graph) || isTRUE(rv$ppi_complete)) && length(rv$ppi_interactive_genes) > 0) {
      rv$ppi_complete <- TRUE
    }
    if (!is.null(rv$gsea_results_by_gene) && length(rv$gsea_results_by_gene) > 0) {
      rv$gsea_complete <- TRUE
    } else if (!is.null(rv$gsea_result) && inherits(rv$gsea_result, "enrichResult") && nrow(rv$gsea_result@result) > 0) {
      rv$gsea_complete <- TRUE
    }
    if (!is.null(rv$immune_matrix) && nrow(rv$immune_matrix) > 0) {
      rv$immune_complete <- TRUE
    }
    if (isTRUE(rv$ml_complete) || (!is.null(rv$ml_common_genes) && length(rv$ml_common_genes) > 0)) {
      rv$ml_complete <- TRUE
    }
    if (isTRUE(rv$nomogram_complete) || (!is.null(rv$nomogram_model) && inherits(rv$nomogram_model, "rms"))) {
      rv$nomogram_complete <- TRUE
    }
    valid_tabs <- c("download", "qc", "normalize", "groups", "batch", "results", "wgcna", "common_genes", "ppi", "ml", "validation", "roc", "nomogram", "gsea", "immune", "results_summary")
    if (!step %in% valid_tabs) step <- "download"
    if (!is.null(state$disease_name) && nzchar(state$disease_name)) {
      updateTextInput(session, "disease_name", value = as.character(state$disease_name))
    }
    updateTabItems(session, "sidebar_menu", step)
    showNotification("Workspace loaded. Continue from the step shown.", type = "message", duration = 6)
  }

  observeEvent(input$load_from_folder_btn, {
    req(input$load_from_folder_file)
    if (!nzchar(trimws(input$load_from_folder_file))) {
      showNotification("Select a file from the dropdown first.", type = "warning", duration = 4)
      return()
    }
    save_dir <- get_saved_workspaces_dir()
    fname <- trimws(input$load_from_folder_file)
    path <- file.path(save_dir, fname)
    if (!file.exists(path)) {
      showNotification("File not found in saved_workspaces.", type = "error", duration = 5)
      return()
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    tryCatch({
      con <- file(path, "rb")
      on.exit(close(con), add = TRUE)
      state <- readRDS(con, refhook = NULL)
      if (is.null(state)) stop("File is empty or invalid.")
      load_restore_state(state)
    }, error = function(e) {
      msg <- conditionMessage(e)
      # Reset dropdown so user can select another file or click "Continue without loading"
      f <- list.files(get_saved_workspaces_dir(), pattern = "\\.rds$", full.names = FALSE)
      choices <- if (length(f) > 0) c("— Select file —" = "", setNames(f, f)) else c("(No .rds files)" = "")
      updateSelectInput(session, "load_from_folder_file", choices = choices, selected = "")
      if (grepl("unknown input format|invalid.*format|not a serialized|error in read", msg, ignore.case = TRUE)) {
        showNotification(
          "Load failed: file is not a valid workspace save. Use a .rds saved by this app, select another file, or click \"Continue without loading\" to start a new analysis.",
          type = "error",
          duration = 12
        )
      } else if (grepl("connection|reading from", msg, ignore.case = TRUE)) {
        showNotification(
          "Load failed: file may be corrupted or in use. Select another file or click \"Continue without loading\" to proceed.",
          type = "error",
          duration = 12
        )
      } else {
        showNotification(
          paste0("Load failed: ", msg, " Select another file or click \"Continue without loading\" to start a new analysis."),
          type = "error",
          duration = 10
        )
      }
    })
  })

  # Load from uploaded .rds file (any location)
  observeEvent(input$load_uploaded_btn, {
    req(input$upload_workspace_file)
    # fileInput can return a data frame; get datapath safely (first row if multiple)
    up <- input$upload_workspace_file
    path <- if (is.data.frame(up)) as.character(up$datapath)[1] else as.character(up$datapath)
    path <- path[!is.na(path) & nzchar(path)][1]
    if (is.null(path) || is.na(path) || !nzchar(path)) {
      showNotification("No file selected. Please choose a .rds workspace file first.", type = "warning", duration = 5)
      return()
    }
    if (!file.exists(path)) {
      showNotification("Uploaded file not found. The file may be too large (max 500 MB) or the upload was interrupted. Try again or use \"Load from folder\" if the file is in saved_workspaces.", type = "error", duration = 8)
      shinyjs::reset("upload_workspace_file")
      return()
    }
    # Copy to a temp file and read from it (avoids path/encoding issues and Shiny temp cleanup)
    tmpfile <- tempfile(fileext = ".rds")
    on.exit(unlink(tmpfile, force = TRUE), add = TRUE)
    if (!file.copy(path, tmpfile, overwrite = TRUE)) {
      showNotification("Could not read the uploaded file. Try again or use \"Load from folder\".", type = "error", duration = 6)
      shinyjs::reset("upload_workspace_file")
      return()
    }
    tryCatch({
      state <- readRDS(tmpfile)
      if (is.null(state)) stop("File is empty or invalid.")
      load_restore_state(state)
      shinyjs::reset("upload_workspace_file")
    }, error = function(e) {
      msg <- conditionMessage(e)
      shinyjs::reset("upload_workspace_file")
      if (grepl("unknown input format|invalid.*format|not a serialized|error in read", msg, ignore.case = TRUE)) {
        showNotification(
          "Load failed: file is not a valid workspace save. Use a .rds saved by this app, upload another file, or click \"Continue without loading\" to start a new analysis.",
          type = "error",
          duration = 12
        )
      } else if (grepl("connection|reading from", msg, ignore.case = TRUE)) {
        showNotification(
          "Load failed: file may be corrupted or in use. Upload another file or click \"Continue without loading\" to proceed.",
          type = "error",
          duration = 12
        )
      } else {
        showNotification(
          paste0("Load failed: ", msg, " Upload another file or click \"Continue without loading\" to start a new analysis."),
          type = "error",
          duration = 10
        )
      }
    })
  })

  # ==============================================================================
  # INTERNET STATUS (ONLINE/OFFLINE)
  # ==============================================================================
  
  observeEvent(input$online_status, {
    # When user goes offline, warn them (useful for GEO downloads)
    if (isFALSE(input$online_status)) {
      showNotification(
        tags$div(
          icon("exclamation-triangle"),
          tags$strong("Internet connection lost (Offline)."),
          tags$p("Downloads may fail until you’re back online.", style = "margin-top: 6px;")
        ),
        type = "error",
        duration = 8
      )
    }
  }, ignoreInit = TRUE)
  
  # Start guided tour when user clicks the header button
  observeEvent(input$start_tour, {
    if (!is.null(guide)) {
      safe_run({
        guide$init()$start()
      }, step_name = "Guided tour", session = session)
    } else {
      showNotification("Guided tour is not available (cicerone package not installed).", type = "warning", duration = 6)
    }
  })
  
  # ==============================================================================
  # RESET FUNCTIONALITY
  # ==============================================================================
  
  show_reset_modal <- function() {
    showModal(modalDialog(
      title = tags$span(icon("exclamation-triangle"), " Reset & start new analysis"),
      tags$div(
        style = "font-size: 16px;",
        tags$p(tags$strong("Clear all previous data and analysis?"), 
               style = "color: #e74c3c; margin-bottom: 10px;"),
        tags$p("All downloaded data and results will be removed. You can then enter new GSE IDs and run again from Step 1."),
        tags$p(tags$em("Save your workspace first (sidebar or Results tab) if you want to keep the current analysis."), style = "color: #7f8c8d; margin-bottom: 10px;"),
        tags$p("This will clear:"),
        tags$ul(
          tags$li("Downloaded datasets and normalized data"),
          tags$li("Group selections and batch correction"),
          tags$li("Differential expression, WGCNA, PPI, ML, ROC, GSEA, Immune deconvolution results")
        ),
        tags$p(tags$strong("This action cannot be undone."), 
               style = "color: #c0392b; margin-top: 12px; font-weight: bold;")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_reset", "Yes, clear all & start new", 
                    class = "btn-danger",
                    style = "background: linear-gradient(135deg, #e74c3c 0%, #c0392b 100%); 
                             border: none; color: white; font-weight: bold;")
      ),
      easyClose = FALSE,
      size = "m"
    ))
  }
  
  observeEvent(input$reset_app, { show_reset_modal() })
  
  observeEvent(input$confirm_reset, {
    # Remove modal
    removeModal()
    
    # Show progress notification
    showNotification(
      tags$div(
        icon("spinner", class = "fa-spin"),
        tags$strong("Restarting application and clearing previous analysis...")
      ),
      type = "default",
      duration = 2
    )
    
    # Reset PPI module state (app_ppi is created in server_ppi.R, same environment)
    tryCatch({
      if (exists("app_ppi")) {
        app_ppi$applied_mode <- NULL
        app_ppi$applied_top_n <- NULL
        app_ppi$applied_manual_genes <- NULL
      }
    }, error = function(e) NULL)
    
    # Reset all reactive values (removes previous analysis; nothing is saved unless user already saved workspace)
    rv$disease_name <- ""
    rv$micro_expr_list <- list()
    rv$micro_eset_list <- list()
    rv$micro_metadata_list <- list()
    rv$micro_cel_paths <- list()
    rv$rna_counts_list <- list()
    rv$rna_metadata_list <- list()
    rv$all_genes_list <- list()
    rv$common_genes <- NULL
    rv$combined_expr_raw <- NULL
    rv$combined_expr <- NULL
    rv$unified_metadata <- NULL
    rv$expr_filtered <- NULL
    rv$batch_corrected <- NULL
    rv$de_results <- NULL
    rv$sig_genes <- NULL
    rv$de_method <- "limma"
    rv$raw_counts_for_deseq2 <- NULL
    rv$raw_counts_metadata <- NULL
    rv$download_complete <- FALSE
    rv$normalization_complete <- FALSE
    rv$groups_applied <- FALSE
    rv$batch_complete <- FALSE
    rv$wgcna_prepared <- FALSE
    rv$wgcna_complete <- FALSE
    rv$datExpr <- NULL
    rv$wgcna_top_variable_genes <- NULL
    rv$wgcna_gene_variance_table <- NULL
    rv$wgcna_sample_info <- NULL
    rv$wgcna_sample_tree <- NULL
    rv$soft_threshold <- NULL
    rv$soft_threshold_powers <- NULL
    rv$moduleColors <- NULL
    rv$dynamicColors <- NULL
    rv$MEs <- NULL
    rv$geneTree <- NULL
    rv$trait_data <- NULL
    rv$moduleTraitCor <- NULL
    rv$moduleTraitPvalue <- NULL
    rv$gene_metrics <- NULL
    rv$geneModuleMembership <- NULL
    rv$MMPvalue <- NULL
    rv$wgcna_log_messages <- character(0)
    rv$ME_correlation <- NULL
    rv$ME_tree <- NULL
    rv$significant_modules <- NULL
    rv$common_genes_de_wgcna <- NULL
    rv$common_genes_df <- NULL
    rv$common_genes_deg_n <- 0L
    rv$common_genes_wgcna_n <- 0L
    rv$go_bp <- NULL
    rv$go_mf <- NULL
    rv$go_cc <- NULL
    rv$kegg_enrichment <- NULL
    rv$ppi_graph <- NULL
    rv$ppi_hub_scores <- NULL
    rv$ppi_consensus_hubs <- NULL
    rv$ppi_hub_rankings <- NULL
    rv$ppi_interactive_genes <- NULL
    rv$ppi_non_interactive_genes <- NULL
    rv$ppi_gene_status_table <- NULL
    rv$ppi_complete <- FALSE
    rv$ppi_centrality_filtered_genes <- NULL
    rv$ppi_centrality_weights <- NULL
    rv$ppi_centrality_table <- NULL
    rv$extracted_data_ml <- NULL
    rv$ml_lasso_df <- NULL
    rv$ml_rf_importance <- NULL
    rv$ml_svm_ranking <- NULL
    rv$ml_common_genes <- NULL
    rv$ml_venn_sets <- NULL
    rv$ml_x <- NULL
    rv$ml_y <- NULL
    rv$ml_complete <- FALSE
    rv$ml_lasso_genes <- NULL
    rv$ml_elastic_df <- NULL
    rv$ml_elastic_top_genes <- NULL
    rv$ml_ridge_df <- NULL
    rv$ml_ridge_top_genes <- NULL
    rv$ml_rf_top_genes <- NULL
    rv$ml_svm_top_genes <- NULL
    rv$ml_boruta_df <- NULL
    rv$ml_boruta_top_genes <- NULL
    rv$ml_splsda_df <- NULL
    rv$ml_splsda_top_genes <- NULL
    rv$ml_xgboost_df <- NULL
    rv$ml_xgboost_top_genes <- NULL
    rv$ml_methods_run <- NULL
    rv$nomogram_complete <- FALSE
    rv$nomogram_model <- NULL
    rv$nomogram_train_data <- NULL
    rv$nomogram_validation_data <- NULL
    rv$nomogram_available_genes <- NULL
    rv$nomogram_optimal_threshold <- NULL
    rv$nomogram_train_metrics <- NULL
    rv$nomogram_val_metrics <- NULL
    rv$nomogram_train_roc <- NULL
    rv$nomogram_val_roc <- NULL
    rv$nomogram_model_diagnostics <- NULL
    rv$nomogram_performance_comparison <- NULL
    rv$nomogram_cal_train <- NULL
    rv$nomogram_cal_validation <- NULL
    rv$nomogram_dca_train <- NULL
    rv$nomogram_dca_val <- NULL
    rv$nomogram_ci_train <- NULL
    rv$nomogram_ci_val <- NULL
    # Validation mode
    rv$validation_mode <- "external"
    # External validation
    rv$external_validation_expr <- NULL
    rv$external_validation_outcome <- NULL
    rv$external_validation_group_col <- NULL
    rv$external_validation_n_disease <- NULL
    rv$external_validation_n_normal <- NULL
    rv$external_validation_gene_names <- NULL
    rv$external_validation_raw_expr <- NULL
    rv$external_validation_metadata <- NULL
    rv$ext_val_log <- NULL
    rv$ext_val_raw_expr <- NULL
    rv$ext_val_metadata <- NULL
    rv$ext_val_downloaded <- NULL
    rv$ext_val_de_results <- NULL
    rv$ext_val_sig_genes <- NULL
    rv$nom_ext_val_raw_expr <- NULL
    rv$nom_ext_val_metadata <- NULL
    rv$nom_ext_val_downloaded <- NULL
    rv$nomogram_ext_val_data <- NULL
    rv$nomogram_ext_val_metrics <- NULL
    rv$nomogram_ext_val_roc <- NULL
    rv$nomogram_ext_cal <- NULL
    rv$nomogram_ext_dca <- NULL
    rv$nomogram_ext_ci <- NULL
    rv$gsea_result <- NULL
    rv$gsea_target_genes <- NULL
    rv$gsea_results_by_gene <- NULL
    rv$gsea_complete <- FALSE
    rv$immune_raw <- NULL
    rv$immune_matrix <- NULL
    rv$immune_data <- NULL
    rv$immune_long <- NULL
    rv$immune_method <- NULL
    rv$immune_cell_cols <- NULL
    rv$immune_complete <- FALSE

    # Reset timers
    rv$download_start <- NULL
    rv$download_running <- FALSE
    rv$download_complete_at <- NULL
    rv$auto_save_after_download_done <- FALSE
    rv$normalize_start <- NULL
    rv$normalize_running <- FALSE
    rv$batch_start <- NULL
    rv$batch_running <- FALSE
    rv$de_start <- NULL
    rv$de_running <- FALSE
    rv$wgcna_start <- NULL
    rv$wgcna_running <- FALSE
    
    # Reset UI inputs so user can start a new analysis from defaults
    updateRadioButtons(session, "analysis_type", selected = "rnaseq")
    updateRadioButtons(session, "de_method", selected = "limma")
    updateTextInput(session, "disease_name", value = "")
    updateTextAreaInput(session, "rnaseq_gses", value = "GSE50760, GSE104836")
    updateTextAreaInput(session, "microarray_gses", value = "")
    updateNumericInput(session, "logfc_cutoff", value = 0.5)
    updateNumericInput(session, "padj_cutoff", value = 0.05)
    updateNumericInput(session, "top_genes", value = 50)
    
    # Navigate to Step 1 (Download) so user starts from the beginning
    updateTabItems(session, "sidebar_menu", "download")
    
    # Success: previous analysis removed; user can add new GSE and run again
    Sys.sleep(0.5)
    showNotification(
      tags$div(
        icon("check-circle"),
        tags$strong("Reset complete."),
        tags$p("All previous data cleared. Enter new GSE IDs above and click Start Processing to run again.", style = "margin-top: 5px;")
      ),
      type = "success",
      duration = 6
    )
  })
}
