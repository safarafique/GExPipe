## Shiny UI tab modules live in R/ui_*.R (installed with the package).
## Thin accessors attach dependencies then return the tab UI object.

.gexp_ui_tab <- function(obj_name) {
  gexp_app_attach_packages()
  obj <- get(obj_name, envir = asNamespace("GExPipe"), inherits = FALSE)
  force(obj)
}

gexp_ui_qc <- function() .gexp_ui_tab("ui_qc")
gexp_ui_normalize <- function() .gexp_ui_tab("ui_normalize")
gexp_ui_groups <- function() .gexp_ui_tab("ui_groups")
gexp_ui_batch <- function() .gexp_ui_tab("ui_batch")
gexp_ui_results <- function() .gexp_ui_tab("ui_results")
gexp_ui_consensus <- function() .gexp_ui_tab("ui_consensus")
gexp_ui_wgcna <- function() .gexp_ui_tab("ui_wgcna")
gexp_ui_common_genes <- function() .gexp_ui_tab("ui_common_genes")
gexp_ui_ppi <- function() .gexp_ui_tab("ui_ppi")
gexp_ui_ml <- function() .gexp_ui_tab("ui_ml")
gexp_ui_validation <- function() .gexp_ui_tab("ui_validation")
gexp_ui_roc <- function() .gexp_ui_tab("ui_roc")
gexp_ui_nomogram <- function() .gexp_ui_tab("ui_nomogram")
gexp_ui_gsea <- function() .gexp_ui_tab("ui_gsea")
gexp_ui_results_summary <- function() .gexp_ui_tab("ui_results_summary")

#' Green Next-tab button used at the end of pipeline steps
#' @noRd
gexp_ui_next_tab_button <- function(id, label) {
  shiny::fluidRow(
    shinydashboard::box(
      width = 12, status = "info", solidHeader = FALSE,
      shiny::tags$div(
        class = "next-btn",
        style = "text-align: center; padding: 20px 0;",
        shiny::actionButton(
          id,
          shiny::tagList(shiny::icon("arrow-right"), " ", label),
          class = "btn-success btn-lg",
          style = "font-size: 18px; padding: 12px 30px; border-radius: 25px;"
        )
      )
    )
  )
}

#' LogFC / adj.P / heatmap-gene inputs for one DE track
#' @noRd
gexp_ui_de_threshold_inputs <- function(id_logfc, id_padj, id_top) {
  shiny::tagList(
    shiny::numericInput(
      id_logfc,
      shiny::tags$span(
        "LogFC cutoff:",
        shiny::tags$i(
          class = "fa fa-question-circle param-help",
          `data-toggle` = "tooltip", `data-placement` = "top",
          title = "Log2 fold-change threshold. Genes with |log2FC| above this value are considered differentially expressed.<br><b>0.5</b> = mild (1.4-fold), <b>1.0</b> = strong (2-fold)."
        )
      ),
      0.5, step = 0.1
    ),
    shiny::numericInput(
      id_padj,
      shiny::tags$span(
        "Adj. P-value:",
        shiny::tags$i(
          class = "fa fa-question-circle param-help",
          `data-toggle` = "tooltip", `data-placement` = "top",
          title = "Benjamini-Hochberg adjusted p-value cutoff.<br><b>0.05</b> = standard (5% FDR), <b>0.01</b> = stringent."
        )
      ),
      0.05, step = 0.01
    ),
    shiny::numericInput(
      id_top,
      shiny::tags$span(
        "Heatmap Genes:",
        shiny::tags$i(
          class = "fa fa-question-circle param-help",
          `data-toggle` = "tooltip", `data-placement` = "top",
          title = "Number of top DE genes to show in the heatmap, ranked by adjusted p-value."
        )
      ),
      50, step = 10
    )
  )
}

#' RNA-seq (left) and microarray (right) columns for Parallel DE
#' @noRd
gexp_ui_parallel_two_col <- function(rna_ui, micro_ui) {
  shiny::fluidRow(
    shiny::column(6, rna_ui),
    shiny::column(6, micro_ui)
  )
}

#' Two side-by-side run logs shown only in Parallel DE (RNA-seq left)
#' @noRd
gexp_ui_parallel_run_logs <- function(micro_id, rna_id) {
  shiny::conditionalPanel(
    condition = "input.analysis_type == 'parallel'",
    gexp_ui_parallel_two_col(
      shinydashboard::box(
        title = shiny::tags$span(shiny::icon("dna"), " RNA-seq run log"),
        width = 12, status = "info", solidHeader = TRUE,
        shiny::verbatimTextOutput(rna_id)
      ),
      shinydashboard::box(
        title = shiny::tags$span(shiny::icon("th"), " Microarray run log"),
        width = 12, status = "warning", solidHeader = TRUE,
        shiny::verbatimTextOutput(micro_id)
      )
    )
  )
}

.gexpipe_de_method_label <- function(de_method) {
  de_method <- if (is.null(de_method) || !nzchar(de_method)) "limma" else de_method
  switch(
    de_method,
    deseq2 = "DESeq2",
    edger = "edgeR",
    limma_voom = "limma-voom",
    limma = "limma",
    de_method
  )
}

.gexpipe_is_count_de <- function(de_method) {
  !is.null(de_method) && de_method %in% c("deseq2", "edger", "limma_voom")
}

#' Auto-mode note: what Step 2 will do for this DE method
#' @noRd
gexpipe_ui_norm_auto_guide <- function(analysis_type, de_method) {
  de_method <- if (is.null(de_method) || !nzchar(de_method)) "limma" else de_method
  # Merged DE is one limma on a shared log matrix — always normalize RNA-seq.
  if (identical(analysis_type, "merged")) de_method <- "limma"
  count_de <- .gexpipe_is_count_de(de_method)
  de_lab <- .gexpipe_de_method_label(de_method)
  show_rna <- !identical(analysis_type, "microarray")
  show_micro <- !identical(analysis_type, "rnaseq")
  items <- list()
  if (isTRUE(show_rna)) {
    items <- c(items, list(if (isTRUE(count_de)) {
      shiny::tags$li(
        shiny::tags$strong("RNA-seq DE (", de_lab, "): "),
        "raw counts are kept. Step 2 will ",
        shiny::tags$strong("not"),
        " apply TMM or log2 for DE — ", de_lab, " normalizes internally. Sample plots may still use a light log scale."
      )
    } else {
      shiny::tags$li(
        shiny::tags$strong("RNA-seq DE (limma): "),
        "Auto uses ",
        shiny::tags$strong("TMM + log2-CPM"),
        " for counts, or ",
        shiny::tags$strong("log2(x+1)"),
        " if FPKM/TPM is detected."
      )
    }))
  }
  if (isTRUE(show_micro)) {
    items <- c(items, list(shiny::tags$li(
      shiny::tags$strong("Microarray: "),
      "Auto picks quantile, log2+quantile, RMA (Affymetrix CEL), or Agilent normexp from the files."
    )))
  }
  extra <- if (identical(analysis_type, "parallel")) {
    "One Apply runs both sides. No common-gene intersection and no global quantile. Click Apply Normalization."
  } else if (identical(analysis_type, "merged")) {
    "Then common genes, then global quantile (on by default). One matrix for limma. Click Apply Normalization."
  } else {
    "Click Apply Normalization, then go to QC."
  }
  shiny::tags$div(
    class = "alert alert-info",
    style = "margin: 10px 0 0 0; font-size: 13px; line-height: 1.55;",
    shiny::icon("magic"),
    shiny::tags$strong(" Auto (recommended). "),
    "No method radios. GExPipe matches Step 1 DE method and the downloaded data.",
    do.call(shiny::tags$ul, c(list(style = "margin: 8px 0 0 0; padding-left: 18px;"), items)),
    shiny::tags$p(extra, style = "margin: 8px 0 0 0;")
  )
}

#' Manual-mode details so the user picks a scale that matches DE
#' @noRd
gexpipe_ui_norm_manual_guide <- function(analysis_type, de_method) {
  de_method <- if (is.null(de_method) || !nzchar(de_method)) "limma" else de_method
  if (identical(analysis_type, "merged")) de_method <- "limma"
  count_de <- .gexpipe_is_count_de(de_method)
  de_lab <- .gexpipe_de_method_label(de_method)
  show_rna <- !identical(analysis_type, "microarray")
  show_micro <- !identical(analysis_type, "rnaseq")
  head_msg <- if (isTRUE(show_rna) && isTRUE(count_de)) {
    shiny::tags$p(
      shiny::icon("exclamation-triangle"),
      shiny::tags$strong(paste0(" RNA-seq DE is ", de_lab, ". ")),
      "Do not apply TMM or log2 for DE. Those engines need raw counts. RNA-seq method radios stay hidden so the run cannot use the wrong scale.",
      style = "margin: 0 0 8px 0;"
    )
  } else if (isTRUE(show_rna)) {
    shiny::tags$p(
      shiny::icon("info-circle"),
      shiny::tags$strong(" RNA-seq DE is limma. "),
      "Pick TMM + log2-CPM for raw/estimated counts, or log2(x+1) if the matrix is FPKM/TPM. Do not pick DESeq2/edgeR on FPKM.",
      style = "margin: 0 0 8px 0;"
    )
  } else {
    shiny::tags$p(
      shiny::icon("info-circle"),
      shiny::tags$strong(" Microarray only. "),
      "Pick the array method that matches your files. DE is limma on this scale.",
      style = "margin: 0 0 8px 0;"
    )
  }
  bullets <- list()
  if (isTRUE(show_rna)) {
    bullets <- c(
      bullets,
      list(
        shiny::tags$li(shiny::tags$strong("Counts + DESeq2 / edgeR / limma-voom:"), " skip RNA-seq TMM/log2."),
        shiny::tags$li(shiny::tags$strong("Counts + limma:"), " TMM + log2-CPM."),
        shiny::tags$li(shiny::tags$strong("FPKM / TPM + limma:"), " log2(x+1).")
      )
    )
  }
  if (isTRUE(show_micro)) {
    bullets <- c(
      bullets,
      list(
        shiny::tags$li(shiny::tags$strong("Already log2:"), " Quantile."),
        shiny::tags$li(shiny::tags$strong("Processed, not log:"), " log2 then quantile."),
        shiny::tags$li(shiny::tags$strong("Affymetrix CEL:"), " RMA. ", shiny::tags$strong("Agilent:"), " normexp + quantile.")
      )
    )
  }
  shiny::tags$div(
    class = "alert alert-warning",
    style = "margin: 10px 0 12px 0; font-size: 13px; line-height: 1.55;",
    shiny::tags$strong("Manual — details so DE stays on the right scale."),
    head_msg,
    do.call(shiny::tags$ul, c(list(style = "margin: 0; padding-left: 18px;"), bullets)),
    if (identical(analysis_type, "parallel")) {
      shiny::tags$p(
        "Parallel: RNA-seq left, microarray right. One Apply still runs both. No global quantile.",
        style = "margin: 8px 0 0 0;"
      )
    } else if (identical(analysis_type, "merged")) {
      shiny::tags$p(
        "Merged: pick one RNA method and one array method, then common genes. Keep global quantile on unless you have a reason to turn it off.",
        style = "margin: 8px 0 0 0;"
      )
    } else {
      NULL
    }
  )
}

#' Format one Parallel platform run log
#' @noRd
gexpipe_format_separate_run_log <- function(run_n, platform, body) {
  hr <- paste(rep("\u2501", 56L), collapse = "")
  paste0(
    hr, "\n",
    "RUN ", run_n, " \u2014 ", platform, " (separate pipeline)\n",
    hr, "\n",
    body
  )
}

#' Short label for the four analysis types
#' @noRd
gexpipe_analysis_type_label <- function(analysis_type) {
  switch(
    if (is.null(analysis_type) || !nzchar(analysis_type)) "microarray" else analysis_type,
    rnaseq = "RNA-seq only",
    microarray = "Microarray only",
    merged = "Merged (joint DE)",
    parallel = "Parallel DE, then merge",
    analysis_type
  )
}

#' Where later steps take their DEG list from
#' @noRd
gexpipe_deg_source_label <- function(analysis_type) {
  switch(
    if (is.null(analysis_type) || !nzchar(analysis_type)) "microarray" else analysis_type,
    rnaseq = "Step 6 RNA-seq DEGs",
    microarray = "Step 6 microarray DEGs (limma)",
    merged = "Step 6 joint limma DEGs (common genes)",
    parallel = "Step 7 RNA-seq \u2229 microarray DEGs (same direction)",
    "Step 6 DEGs"
  )
}

#' Classic ggplot theme for on-screen and 300 dpi exports
#' @noRd
gexpipe_pub_theme <- function(base_size = 13) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold", size = base_size + 2, hjust = 0, color = "#111827"
      ),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, color = "#4B5563"),
      axis.title = ggplot2::element_text(face = "bold", color = "#111827"),
      axis.text = ggplot2::element_text(color = "#1F2937"),
      legend.title = ggplot2::element_text(face = "bold", color = "#111827"),
      legend.text = ggplot2::element_text(color = "#1F2937"),
      panel.grid.major.y = ggplot2::element_line(color = "#F3F4F6", linewidth = 0.35),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#111827", linewidth = 0.4),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )
}

#' FDR colour scale used on GO / KEGG bars and dots
#' @noRd
gexpipe_pub_padj_scale <- function(aesthetics = "colour") {
  cols <- c("#3C5488", "#4DBBD5", "#E64B35")
  if (identical(aesthetics, "fill")) {
    ggplot2::scale_fill_gradientn(colours = cols, name = "FDR")
  } else {
    ggplot2::scale_colour_gradientn(colours = cols, name = "FDR")
  }
}

#' Type-aware About box for steps that share one path after WGCNA
#' @noRd
gexpipe_ui_later_step_about <- function(step, analysis_type) {
  at <- if (is.null(analysis_type) || !nzchar(analysis_type)) "microarray" else analysis_type
  type_lab <- gexpipe_analysis_type_label(at)
  deg_lab <- gexpipe_deg_source_label(at)
  type_chip <- shiny::tags$p(
    shiny::tags$span(
      class = "label label-primary",
      style = "font-size: 12px; padding: 4px 8px;",
      type_lab
    ),
    style = "margin-bottom: 8px;"
  )
  shared_tail <- shiny::tags$p(
    shiny::tags$strong("All four types:"),
    " After Step 9 the path is the same (PPI optional \u2192 ML \u2192 validation \u2192 ROC \u2192 nomogram \u2192 GSEA \u2192 summary).",
    style = "margin-bottom: 0;"
  )
  body <- switch(
    step,
    common_genes = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " Overlap ", shiny::tags$strong(deg_lab),
        " with genes in significant WGCNA modules (the network was built from top-variable genes, not DEGs). Then run GO / KEGG.",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("This type:"),
        if (identical(at, "parallel")) {
          " Apply Step 7 first. WGCNA used one platform matrix (VST or array). Step 9 intersects consensus DEGs with those modules."
        } else if (identical(at, "merged")) {
          " One joint DE list and one merged WGCNA matrix. No Step 7."
        } else if (identical(at, "rnaseq")) {
          " RNA-seq DEGs \u2229 modules from the RNA VST network. No Step 7."
        } else {
          " Microarray DEGs \u2229 modules from the array network. No Step 7."
        },
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    ppi = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " STRING PPI on Step 9 common genes; rank hubs for interpretation and optional ML features.",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("Requirements:"),
        " Compute common genes in Step 9. Same for all four analysis types.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    ml = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " Rank genes that separate Normal vs Disease. Extract expression first (Step 9 common genes, or PPI hubs).",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("This type:"),
        " Labels come from Step 4. The expression matrix is the processed platform used in WGCNA.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    validation = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " External GEO cohort or internal 70/30 split. Prefer a validation GSE on the same technology as the WGCNA / ML matrix.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    roc = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " Per-gene ROC / AUC on ML consensus genes (training, plus external if loaded).",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("Tip:"),
        " AUC \u2265 0.8 is useful discrimination. A large training-vs-external drop suggests overfitting or platform shift.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    nomogram = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " Linear combination of ML genes for a clinical-style score.",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("Requirements:"),
        " Step 5 matrix + groups, and ML genes (Step 11) or Step 9 common genes.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    gsea = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " Rank the processed expression matrix by each signature gene and test MSigDB collections.",
        style = "margin-bottom: 8px;"
      ),
      shiny::tags$p(
        shiny::tags$strong("This type:"),
        " Ranking uses the same continuous matrix as WGCNA / ML.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    results_summary = shiny::tagList(
      type_chip,
      shiny::tags$p(
        shiny::tags$strong("Purpose:"),
        " One place for the run story and key figures. Download 300 dpi PNG / PDF from each earlier step.",
        style = "margin-bottom: 8px;"
      ),
      shared_tail
    ),
    shiny::tagList(type_chip, shared_tail)
  )
  shiny::tagList(body)
}

#' Register About boxes for later steps
#' @noRd
gexpipe_register_later_step_abouts <- function(input, output) {
  output$common_genes_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("common_genes", input$analysis_type)
  })
  output$ppi_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("ppi", input$analysis_type)
  })
  output$ml_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("ml", input$analysis_type)
  })
  output$validation_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("validation", input$analysis_type)
  })
  output$roc_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("roc", input$analysis_type)
  })
  output$nomogram_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("nomogram", input$analysis_type)
  })
  output$gsea_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("gsea", input$analysis_type)
  })
  output$results_summary_about_ui <- shiny::renderUI({
    gexpipe_ui_later_step_about("results_summary", input$analysis_type)
  })
  invisible(NULL)
}
