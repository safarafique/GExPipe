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

  # Notify once if some packages failed to load (app still runs with reduced functionality)
  missing_pkgs <- getOption("omniVerse.missingPkgs", character(0))
  if (length(missing_pkgs) > 0L) {
    tryCatch({
      shiny::showNotification(
        paste0("Some packages could not be loaded: ", paste(head(missing_pkgs, 5), collapse = ", "),
               if (length(missing_pkgs) > 5) paste0(" and ", length(missing_pkgs) - 5, " more") else "", ". ",
               "Install with: BiocManager::install(\"GExPipe\") for full functionality."),
        type = "warning", duration = 12, session = session
      )
    }, error = function(e) NULL)
  }

  # Guided tour (Cicerone) ------------------------------------------------------
  guide <- tryCatch({
    if (requireNamespace("cicerone", quietly = TRUE)) {
      cicerone::Cicerone$
        new()$
        step(
          el = "sidebar_menu",
          title = "Navigation",
          description = "Use this sidebar to navigate through the 15-step pipeline."
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
  source("server/server_packages.R", local = TRUE)
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
  
  # Call module functions
  server_packages(input, output, session, rv)
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

  # Pipeline progress UI + step-guard observers (implemented under R/).
  gexp_register_pipeline_observers(input, output, session, rv)
  # QC next button + tab navigation observers (implemented under R/).
  gexp_register_navigation_observers(input, output, session, rv)
  # Workspace save / load (implemented under R/).
  gexp_register_workspace_observers(input, output, session, rv)
  # Online/offline notice + User Guideline modal (implemented under R/).
  gexp_register_help_observers(input, output, session, rv)
}

