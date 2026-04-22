gexp_app_server <- function(input, output, session) {
  missing_pkgs <- getOption("omniVerse.missingPkgs", character(0))
  if (length(missing_pkgs) > 0L) {
    tryCatch(
      {
        shiny::showNotification(
          paste0(
            "Some packages could not be loaded: ", paste(utils::head(missing_pkgs, 5), collapse = ", "),
            if (length(missing_pkgs) > 5) paste0(" and ", length(missing_pkgs) - 5, " more") else "", "."
          ),
          type = "warning",
          duration = 12,
          session = session
        )
      },
      error = function(e) NULL
    )
  }

  guide <- tryCatch(
    {
      if (requireNamespace("cicerone", quietly = TRUE)) {
        cicerone::Cicerone$
          new()$
          step(el = "sidebar_menu", title = "Navigation", description = "Use this sidebar to navigate through the 15-step pipeline.")$
          step(el = "analysis_type", title = "Platform", description = "Choose your data type: RNA-seq, Microarray, or Merged.")$
          step(el = "start_processing", title = "Start", description = "Click here to begin downloading and processing your datasets.")
      } else {
        NULL
      }
    },
    error = function(e) NULL
  )

  rv <- shiny::reactiveValues(
    show_analysis = FALSE,
    disease_name = "",
    micro_expr_list = list(),
    micro_eset_list = list(),
    micro_metadata_list = list(),
    micro_cel_paths = list(),
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
    de_method = "limma",
    raw_counts_for_deseq2 = NULL,
    raw_counts_metadata = NULL,
    download_complete = FALSE,
    normalization_complete = FALSE,
    groups_applied = FALSE,
    batch_complete = FALSE,
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
    common_genes_de_wgcna = NULL,
    common_genes_df = NULL,
    common_genes_deg_n = 0L,
    common_genes_wgcna_n = 0L,
    go_bp = NULL,
    go_mf = NULL,
    go_cc = NULL,
    kegg_enrichment = NULL,
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
    validation_mode = "external",
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

  output$show_analysis <- shiny::reactive(rv$show_analysis)
  shiny::outputOptions(output, "show_analysis", suspendWhenHidden = FALSE)

  output$analysis_dashboard <- shiny::renderUI({
    shiny::req(rv$show_analysis)
    gexp_app_analysis_dashboard_ui()
  })
  shiny::outputOptions(output, "analysis_dashboard", suspendWhenHidden = FALSE)

  shiny::observeEvent(input$go_to_analysis, {
    rv$show_analysis <- TRUE
  })

  # Step 4: keep DE method and disease name available across modules
  shiny::observe({
    rv$de_method <- input$de_method
  })
  shiny::observe({
    rv$disease_name <- trimws(if (is.null(input$disease_name)) "" else input$disease_name)
  })

  shiny::observeEvent(input$analysis_type, {
    if (identical(input$analysis_type, "microarray") &&
      !is.null(input$de_method) &&
      input$de_method %in% c("deseq2", "edger", "limma_voom")) {
      shiny::updateRadioButtons(session, "de_method", selected = "limma")
      shiny::showNotification(
        shiny::tags$div(
          shiny::icon("info-circle"),
          shiny::tags$strong(" DE method switched to limma."),
          " DESeq2, edgeR, and limma-voom require RNA-seq count data and are not applicable to microarray."
        ),
        type = "warning",
        duration = 6
      )
    }
  })

  # Heavy work deferred to first flush: sourcing ~15 server modules was blocking the initial
  # server tick and shinytest2 session stability. UI already ran phase-1 attach in test mode.
  session$onFlushed(function() {
    if (!isTRUE(getOption("gexpipe.attach.done", FALSE))) {
      options(gexpipe.attach.allow_full_now = TRUE)
      gexp_app_attach_packages()
      options(gexpipe.attach.allow_full_now = NULL)
    }

    gexp_register_pipeline_observers(input, output, session, rv)
    gexp_register_navigation_observers(input, output, session, rv)
    gexp_register_workspace_observers(input, output, session, rv)
    gexp_register_help_observers(input, output, session, rv)

    server_packages(input, output, session, rv)
    server_download(input, output, session, rv)
    server_qc(input, output, session, rv)
    server_normalize(input, output, session, rv)
    server_groups(input, output, session, rv)
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
    server_results_summary(input, output, session, rv)

    shiny::observeEvent(input$start_tour, {
      if (!is.null(guide)) {
        tryCatch(guide$init()$start(), error = function(e) NULL)
      }
    })
  }, once = TRUE)
}
