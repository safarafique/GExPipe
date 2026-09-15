# ==============================================================================
# SERVER_CONSENSUS.R - Step 7: RNA-seq ∩ microarray DEG consensus
# ==============================================================================

server_consensus <- function(input, output, session, rv) {

  output$consensus_next_button_ui <- renderUI({
    label <- " Next: WGCNA Analysis"
    actionButton(
      "next_page_consensus",
      tagList(icon("arrow-right"), label),
      class = "btn-primary btn-lg",
      style = "font-size: 16px; padding: 12px 30px; border-radius: 25px; font-weight: bold;"
    )
  })


  .mixed <- function() {
    isTRUE(gexpipe_has_mixed_platforms(rv$unified_metadata))
  }

  .same_direction_rule <- function() {
    mode <- if (is.null(input$consensus_mode_parallel) || !nzchar(input$consensus_mode_parallel)) {
      "auto"
    } else {
      input$consensus_mode_parallel
    }
    if (identical(mode, "manual") && !is.null(input$consensus_same_direction)) {
      return(isTRUE(input$consensus_same_direction))
    }
    isTRUE(gexpipe_parallel_consensus_defaults()$same_direction)
  }

  .build_consensus <- function() {
    if (is.null(rv$sig_genes_rna) || is.null(rv$sig_genes_micro)) {
      return(NULL)
    }
    same_dir <- .same_direction_rule()
    .gexpipe_call(
      "gexpipe_consensus_degs",
      rv$sig_genes_rna,
      rv$sig_genes_micro,
      require_same_direction = same_dir
    )
  }

  output$consensus_parallel_guide_ui <- renderUI({
    mode <- if (is.null(input$consensus_mode_parallel)) "auto" else input$consensus_mode_parallel
    if (identical(mode, "manual")) {
      tags$div(
        class = "alert alert-warning",
        style = "margin: 8px 0 10px 0; font-size: 13px; line-height: 1.55;",
        tags$strong("Manual — optional same-direction filter."),
        tags$ul(
          style = "margin: 6px 0 0 0; padding-left: 18px;",
          tags$li("Keep the box checked unless you have a reason to keep opposite-direction overlap."),
          tags$li("This list is for Step 9 (DEG ∩ WGCNA modules). WGCNA does not use it.")
        )
      )
    } else {
      tags$div(
        class = "alert alert-info",
        style = "margin: 8px 0 10px 0; font-size: 13px; line-height: 1.55;",
        icon("magic"),
        tags$strong(" Auto (recommended). "),
        "Keep genes significant on both RNA-seq and microarray in the ",
        tags$strong("same direction"),
        ". WGCNA (Step 8) still uses top-variable genes on one processed matrix, not this list."
      )
    }
  })

  output$consensus_status_ui <- renderUI({
    if (!.mixed() || !isTRUE(rv$merge_after_de)) {
      return(tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        tags$strong(" Consensus is only for Parallel DE, then merge."),
        " RNA-seq only, microarray only, and Merged (Both) use the Step 6 DEG list as before."
      ))
    }
    if (is.null(rv$sig_genes_rna) || is.null(rv$sig_genes_micro)) {
      return(tags$div(
        class = "alert alert-warning",
        icon("exclamation-triangle"),
        tags$strong(" Run Step 6 first."),
        " Parallel mode runs RNA-seq DE and microarray DE separately. Then apply consensus here."
      ))
    }
    if (isTRUE(rv$consensus_complete) && !is.null(rv$sig_genes) && nrow(rv$sig_genes) > 0) {
      return(tags$div(
        class = "alert alert-success",
        icon("check-circle"),
        tags$strong(" Consensus applied."),
        " Later steps use ", format(nrow(rv$sig_genes), big.mark = ","),
        " genes significant on both platforms",
        if (isTRUE(rv$consensus_same_direction)) " with the same direction." else "."
      ))
    }
    tags$div(
      class = "alert alert-primary",
      icon("object-ungroup"),
      tags$strong(" Ready."),
      " Review the overlap, then Apply consensus. Step 9 uses this list ∩ WGCNA modules. WGCNA itself does not use DEGs."
    )
  })

  .consensus_count_card <- function(icon_name, title, meaning, n, bg, fg = "#fff") {
    tags$div(
      style = paste0(
        "background:", bg, "; color:", fg, "; border-radius: 10px; padding: 14px 16px; min-height: 118px;"
      ),
      tags$div(
        style = "display: flex; align-items: center; gap: 8px; margin-bottom: 6px;",
        icon(icon_name, class = "fa-lg"),
        tags$strong(title, style = "font-size: 15px;")
      ),
      tags$div(style = "font-size: 28px; font-weight: 700; line-height: 1.1;", format(n, big.mark = ",")),
      tags$div(style = "font-size: 12px; opacity: 0.95; margin-top: 6px; line-height: 1.35;", meaning)
    )
  }

  output$consensus_count_legend_ui <- renderUI({
    out <- tryCatch(.build_consensus(), error = function(e) NULL)
    n_disc <- if (!is.null(out)) out$n_discordant else 0L
    tags$div(
      class = "alert alert-secondary",
      style = "margin: 0 15px 12px 15px; padding: 12px 16px; background: #f8fafc; border: 1px solid #cbd5e1; border-radius: 8px;",
      tags$p(
        tags$strong(icon("key"), " How to read these counts"),
        style = "margin: 0 0 8px 0; font-size: 14px;"
      ),
      tags$ul(
        style = "margin: 0; padding-left: 20px; font-size: 13px; line-height: 1.7; color: #334155;",
        tags$li(icon("dna"), " ", tags$strong("RNA-seq DEGs"), " — significant on RNA-seq only (Step 6)."),
        tags$li(icon("th"), " ", tags$strong("Microarray DEGs"), " — significant on microarray only (Step 6)."),
        tags$li(
          icon("circle"), " ", tags$strong("Common"),
          " — in ", tags$em("both"), " lists (Venn center). Opposite direction is still counted here."
        ),
        tags$li(
          icon("check-double"), " ", tags$strong("Consensus"),
          " — Common genes with the ", tags$em("same"), " direction. This is what Apply keeps for Step 9.",
          if (is.finite(n_disc) && n_disc > 0L) {
            tagList(" ", tags$span(style = "color: #b45309;", paste0("(", n_disc, " common genes dropped as opposite direction).")))
          } else {
            NULL
          }
        )
      )
    )
  })

  output$consensus_count_cards_ui <- renderUI({
    out <- tryCatch(.build_consensus(), error = function(e) NULL)
    n_rna <- if (!is.null(rv$sig_genes_rna)) nrow(rv$sig_genes_rna) else 0L
    n_micro <- if (!is.null(rv$sig_genes_micro)) nrow(rv$sig_genes_micro) else 0L
    n_common <- if (!is.null(out)) out$n_overlap else 0L
    n_cons <- if (!is.null(out)) out$n_consensus else 0L
    fluidRow(
      column(3, .consensus_count_card(
        "dna", "RNA-seq DEGs", "Significant on RNA-seq (Step 6).",
        n_rna, "#16a34a"
      )),
      column(3, .consensus_count_card(
        "th", "Microarray DEGs", "Significant on microarray (Step 6).",
        n_micro, "#2563eb"
      )),
      column(3, .consensus_count_card(
        "circle", "Common", "In both lists (Venn center). Direction may differ.",
        n_common, "#0891b2"
      )),
      column(3, .consensus_count_card(
        "check-double", "Consensus", "Common + same direction. Apply keeps this list.",
        n_cons, "#ca8a04"
      ))
    )
  })

  .draw_consensus_venn <- function() {
    out <- .build_consensus()
    if (is.null(out)) {
      plot.new()
      text(0.5, 0.5, "Run separate DE in Step 6 first.", cex = 1.1, col = "gray40")
      return(invisible(NULL))
    }
    rna_n <- .gexpipe_de_gene_ids(rv$sig_genes_rna)
    micro_n <- .gexpipe_de_gene_ids(rv$sig_genes_micro)
    sets <- list(RNAseq = rna_n, Microarray = micro_n)
    grid::grid.newpage()
    vp <- VennDiagram::venn.diagram(
      x = sets,
      category.names = c("Bulk RNA-seq DE", "Microarray DE"),
      filename = NULL,
      output = TRUE,
      disable.logging = TRUE,
      fill = c("#27ae60", "#3498db"),
      alpha = 0.5,
      cex = 1.6,
      cat.cex = 1.25,
      cat.fontface = "bold",
      cat.pos = c(-20, 20),
      cat.dist = c(0.05, 0.05),
      margin = 0.12,
      main = "Common DEGs (center)",
      main.cex = 1.3,
      main.fontface = "bold"
    )
    grid::grid.draw(vp)
    note <- paste0(
      "Common = ", format(out$n_overlap, big.mark = ","),
      " genes in both DE lists"
    )
    if (isTRUE(input$consensus_same_direction) && out$n_discordant > 0L) {
      note <- paste0(
        note, "  |  Consensus = ", format(out$n_consensus, big.mark = ","),
        " (dropped ", out$n_discordant, " opposite-direction)"
      )
    }
    grid::grid.text(note, x = 0.5, y = 0.04, gp = grid::gpar(cex = 0.95, col = "#34495e"))
    invisible(NULL)
  }

  output$consensus_venn_plot <- renderPlot({
    .draw_consensus_venn()
  })

  output$consensus_table <- renderDT({
    out <- .build_consensus()
    if (is.null(out) || nrow(out$table) < 1L) {
      return(datatable(data.frame(Note = "No consensus genes yet."), options = list(dom = "t"), rownames = FALSE))
    }
    show <- out$table[, c("Gene", "logFC_rna", "logFC_micro", "adj.P.Val_rna", "adj.P.Val_micro", "Direction"), drop = FALSE]
    datatable(show, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE) %>%
      formatRound(columns = c("logFC_rna", "logFC_micro", "adj.P.Val_rna", "adj.P.Val_micro"), digits = 4)
  })

  observeEvent(input$apply_consensus, {
    if (!.mixed()) {
      rv$consensus_complete <- TRUE
      showNotification("Single-platform analysis: consensus skipped. Using Step 6 DEGs.", type = "message", duration = 5)
      return()
    }
    if (is.null(rv$sig_genes_rna) || is.null(rv$sig_genes_micro)) {
      showNotification("Run Step 6 (separate RNA-seq and microarray DE) first.", type = "error", duration = 6)
      return()
    }
    out <- .build_consensus()
    if (is.null(out) || length(out$genes) < 1L) {
      showNotification(
        "No consensus genes. Relax Step 6 cutoffs or turn off same-direction if you want the union of overlap regardless of sign.",
        type = "warning",
        duration = 8
      )
      return()
    }
    rv$consensus_result <- out
    rv$consensus_same_direction <- .same_direction_rule()
    rv$sig_genes <- out$table
    rv$de_results <- out$table
    rv$consensus_complete <- TRUE
    showNotification(
      tags$div(
        icon("check-circle"),
        tags$strong(" Consensus applied."),
        paste0(
          " ", format(out$n_consensus, big.mark = ","),
          " same-platform-pair DEGs stored for Step 9 (DEG ∩ modules). Next: WGCNA on one processed matrix (not this list)."
        )
      ),
      type = "message",
      duration = 6
    )
  })

  output$download_consensus_genes <- downloadHandler(
    filename = function() "Consensus_DEGs_RNAseq_Microarray.csv",
    content = function(file) {
      out <- .build_consensus()
      req(out)
      write.csv(out$table, file, row.names = FALSE)
      write.csv(out$table, file.path(CSV_EXPORT_DIR(), "Consensus_DEGs_RNAseq_Microarray.csv"), row.names = FALSE)
    }
  )

  output$download_consensus_rna_only <- downloadHandler(
    filename = function() "DEGs_RNA_only.csv",
    content = function(file) {
      req(rv$sig_genes_rna)
      write.csv(rv$sig_genes_rna, file, row.names = FALSE)
    }
  )

  output$download_consensus_micro_only <- downloadHandler(
    filename = function() "DEGs_Microarray_only.csv",
    content = function(file) {
      req(rv$sig_genes_micro)
      write.csv(rv$sig_genes_micro, file, row.names = FALSE)
    }
  )

  .save_venn <- function(file, device) {
    if (device == "png") {
      png(file, width = 7, height = 6, units = "in", res = 150)
    } else if (device == "jpeg") {
      jpeg(file, width = 7, height = 6, units = "in", res = 150, quality = 95)
    } else {
      pdf(file, width = 7, height = 6)
    }
    .draw_consensus_venn()
    dev.off()
  }

  output$download_consensus_venn_png <- downloadHandler(
    filename = function() "Consensus_Venn.png",
    content = function(file) .save_venn(file, "png")
  )
  output$download_consensus_venn_jpg <- downloadHandler(
    filename = function() "Consensus_Venn.jpg",
    content = function(file) .save_venn(file, "jpeg")
  )
  output$download_consensus_venn_pdf <- downloadHandler(
    filename = function() "Consensus_Venn.pdf",
    content = function(file) .save_venn(file, "pdf")
  )
}
