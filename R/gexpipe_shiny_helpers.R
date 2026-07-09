# Internal helpers used by the Shiny app.
# Kept in R/ so they can be tested and reused outside inst/shinyapp/.

# ==============================================================================
# GLOBAL PLOT THEME (publication-quality, international standard)
# ==============================================================================
theme_publication <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2, hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, hjust = 0.5, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "gray30"),
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "gray70", linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "gray95", color = "gray70"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

# Color palette for consistent, accessible plots (colorblind-friendly where possible)
palette_primary <- c(
  "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B",
  "#95C623", "#4A90A4", "#E94F37", "#6A4C93", "#88D498"
)

# ==============================================================================
# GLOBAL LOGGING & ERROR HANDLING HELPERS
# ==============================================================================
.gexpipe_log_file <- function(log_dir = getwd(), date = Sys.Date()) {
  file.path(log_dir, paste0("omniverse_log_", format(date, "%Y%m%d"), ".txt"))
}

# Write a line to the log (and to console)
app_log <- function(msg, level = "INFO") {
  log_file <- .gexpipe_log_file()
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [", level, "] ", msg)
  # Always try to append to log file; failures should not crash the app
  tryCatch(
    {
      write(line, file = log_file, append = TRUE)
    },
    error = function(e) {
      message("Logging failed: ", conditionMessage(e))
    }
  )
  message(line)
}

# Wrapper for robust error handling with logging + user-friendly notification
safe_run <- function(expr, step_name = "this step", session = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      app_log(paste0("Error in ", step_name, ": ", conditionMessage(e)), level = "ERROR")
      shiny::showNotification(
        shiny::tags$div(
          shiny::tags$strong(paste0("Error in ", step_name, ":")),
          shiny::tags$p(conditionMessage(e)),
          shiny::tags$p(
            "Check your inputs and try again. A detailed message has been written to the log file in the app working directory.",
            style = "font-size: 12px; color: #555;"
          )
        ),
        type = "error",
        duration = 10,
        session = session
      )
      NULL
    }
  )
}

# ==============================================================================
# GLOBAL VARIABLES
# ==============================================================================
# Image export: all downloaded plot images use this DPI (publication quality)
IMAGE_DPI <- 300L

# Reusable Shiny UI download button groups for plot exports (PNG/JPG/PDF).
gexp_ui_plot_download_jpg_pdf <- function(jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
  shiny::tags$div(
    class = "gexp-plot-download-bar",
    style = "margin-top: 8px;",
    shiny::downloadButton(jpg_id, shiny::tagList(shiny::icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    shiny::downloadButton(pdf_id, shiny::tagList(shiny::icon("download"), " PDF"), class = btn_class)
  )
}

gexp_ui_plot_download_bar <- function(png_id, jpg_id, pdf_id, btn_class = "btn-success btn-sm") {
  shiny::tags$div(
    class = "gexp-plot-download-bar",
    style = "margin-top: 8px;",
    shiny::downloadButton(png_id, shiny::tagList(shiny::icon("download"), " PNG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    shiny::downloadButton(jpg_id, shiny::tagList(shiny::icon("download"), " JPG (300 DPI)"), class = btn_class, style = "margin-right: 6px;"),
    shiny::downloadButton(pdf_id, shiny::tagList(shiny::icon("download"), " PDF"), class = btn_class)
  )
}

# Open a graphics device for plot export (png/jpg/pdf inferred from file extension).
gexp_plot_device_open <- function(file, width, height, bg = "white", type = NULL) {
  if (is.null(type)) {
    ext <- tolower(sub(".*\\.", "", basename(file)))
    type <- if (ext %in% c("jpg", "jpeg")) "jpg" else ext
  }
  type <- match.arg(type, c("png", "jpg", "pdf"))
  if (type == "png") {
    grDevices::png(file, width = width * IMAGE_DPI, height = height * IMAGE_DPI, res = IMAGE_DPI, bg = bg)
  } else if (type == "jpg") {
    grDevices::jpeg(file, width = width, height = height, res = IMAGE_DPI, units = "in", bg = bg, quality = 95)
  } else {
    grDevices::pdf(file, width = width, height = height, bg = bg)
  }
  invisible(NULL)
}

# Save a ggplot object using file extension to pick device.
gexp_ggsave_from_file <- function(file, plot, width, height, dpi = IMAGE_DPI) {
  if (is.null(plot)) return(invisible(NULL))
  ext <- tolower(sub(".*\\.", "", basename(file)))
  device <- if (ext %in% c("jpg", "jpeg")) "jpeg" else if (ext == "pdf") "pdf" else "png"
  ggplot2::ggsave(file, plot = plot, width = width, height = height, dpi = dpi, units = "in", bg = "white", device = device)
  invisible(NULL)
}

# ==============================================================================
# ML METHODS VENN / UPSET (selected methods; common count includes zeros)
# ==============================================================================

#' Suggest a phenotype column for group assignment
#'
#' Prefers explicit condition columns; falls back to \code{title} for GEO
#' series where treatment is only described in sample titles (e.g. GSE108413).
#'
#' @param col_names Character vector of pData column names.
#' @return Column name or empty string.
#' @keywords internal
gexp_suggest_group_column <- function(col_names) {
  col_names <- unique(as.character(col_names[!is.na(col_names) & nzchar(trimws(col_names))]))
  if (length(col_names) == 0L) {
    return("")
  }
  cond <- col_names[grepl(
    "condition|disease|treatment|group|status|type|characteristics.*ch1",
    col_names,
    ignore.case = TRUE
  )]
  if (length(cond) > 0L) {
    return(cond[[1L]])
  }
  if ("title" %in% col_names) {
    return("title")
  }
  likely <- col_names[grepl(
    "characteristics|tissue|source|description|sample",
    col_names,
    ignore.case = TRUE
  )]
  if (length(likely) > 0L) {
    return(likely[[1L]])
  }
  col_names[[1L]]
}

#' Normalize phenotype labels for group extraction (e.g. parse GEO title text)
#'
#' For GEO series like GSE108413, treatment is only in \code{title}, e.g.
#' "islet preparation 1 under control condition". This extracts the condition
#' substring so replicate-specific titles collapse to shared groups.
#'
#' @param vals Character vector of phenotype values.
#' @param col_name Metadata column name (optional).
#' @return Normalized character vector, same length as \code{vals}.
#' @keywords internal
gexp_normalize_group_labels <- function(vals, col_name = NULL) {
  vals <- as.character(vals)
  if (length(vals) == 0L) {
    return(vals)
  }
  out <- trimws(vals)
  use_title_rules <- is.null(col_name) || grepl("title", col_name, ignore.case = TRUE)
  if (isTRUE(use_title_rules)) {
    under_cond <- grepl("\\sunder\\s", out, ignore.case = TRUE)
    if (any(under_cond)) {
      out[under_cond] <- trimws(sub("^.*\\sunder\\s+", "", out[under_cond], ignore.case = TRUE))
    }
    colon_cond <- grepl(":", out) & !grepl("^GSM", out, ignore.case = TRUE)
    if (any(colon_cond)) {
      parts <- strsplit(out[colon_cond], ":", fixed = TRUE)
      out[colon_cond] <- vapply(parts, function(x) trimws(x[[length(x)]]), character(1))
    }
  }
  out
}

#' Suggest Normal / Disease / None for an extracted group label
#' @keywords internal
gexp_suggest_group_category <- function(label) {
  if (is.null(label) || is.na(label) || !nzchar(trimws(as.character(label)))) {
    return("None")
  }
  g_lower <- tolower(trimws(as.character(label)))
  if (grepl("normal|control|healthy|wild.?type|untreated|vehicle|mock|sham|baseline", g_lower)) {
    return("Normal")
  }
  if (grepl("disease|tumor|cancer|metastatic|patient|treat|stimul|cytokine|case|infected|infection|diabetic|lesion", g_lower)) {
    return("Disease")
  }
  "None"
}

# Display names for ML method keys (checkbox values in Step 10).
#' @noRd
gexp_ml_method_display_names <- function() {
  c(
    lasso = "LASSO",
    elastic = "Elastic Net",
    ridge = "Ridge",
    rf = "Random Forest",
    svm = "SVM-RFE",
    boruta = "Boruta",
    splsda = "sPLS-DA",
    xgboost = "XGBoost+SHAP"
  )
}

# Build Venn/UpSet gene lists for every selected ML method (empty if method failed).
#' @noRd
gexp_ml_venn_sets_for_selected <- function(gene_lists, methods_sel) {
  if (is.null(methods_sel) || length(methods_sel) == 0) return(list())
  display <- gexp_ml_method_display_names()
  out <- vector("list", length(methods_sel))
  names(out) <- vapply(methods_sel, function(key) {
    nm <- unname(display[[key]])
    if (is.na(nm) || !nzchar(nm)) key else nm
  }, character(1))
  for (i in seq_along(methods_sel)) {
    nm <- names(out)[[i]]
    genes <- gene_lists[[nm]]
    out[[i]] <- if (is.null(genes)) character(0) else unique(as.character(genes))
  }
  out
}

# Count genes common to all selected ML methods (0 when no overlap).
#' @noRd
gexp_ml_common_gene_count <- function(sets) {
  if (is.null(sets) || length(sets) == 0) return(0L)
  sets <- sets[vapply(sets, function(x) !is.null(x), logical(1))]
  if (length(sets) == 0) return(0L)
  if (length(sets) == 1L) return(length(sets[[1]]))
  length(Reduce(intersect, sets))
}

# Color palette for ML method Venn / UpSet plots (up to 8 methods).
#' @noRd
gexp_ml_venn_palette <- function(n) {
  base <- c("#E53935", "#1E88E5", "#43A047", "#FB8C00", "#8E24AA", "#009688", "#795548", "#607D8B")
  if (n <= length(base)) base[seq_len(n)] else rep(base, length.out = n)
}

# Prepare UpSet binary matrix for ML method gene lists.
#' @noRd
gexp_ml_prepare_upset_df <- function(sets) {
  if (is.null(sets) || length(sets) == 0) return(NULL)
  all_genes <- unique(unlist(sets, use.names = FALSE))
  if (length(all_genes) == 0) {
    upset_matrix <- matrix(0L, nrow = 0L, ncol = length(sets))
    colnames(upset_matrix) <- names(sets)
    return(as.data.frame(upset_matrix))
  }
  upset_matrix <- matrix(0L, nrow = length(all_genes), ncol = length(sets))
  rownames(upset_matrix) <- all_genes
  colnames(upset_matrix) <- names(sets)
  for (i in seq_along(sets)) {
    g <- sets[[i]]
    if (length(g) > 0) upset_matrix[intersect(all_genes, g), i] <- 1L
  }
  as.data.frame(upset_matrix)
}

# Draw ML methods overlap plot (Venn for 2–5 non-empty sets; UpSet otherwise).
#' @noRd
gexp_draw_ml_methods_venn <- function(sets,
                                      title = "Common Genes Across Selected ML Methods",
                                      text_scale = 1,
                                      show_footer = TRUE) {
  if (is.null(sets) || length(sets) < 2L) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Select and run 2+ ML methods to view overlap.", cex = 1.1 * text_scale, col = "gray40")
    return(invisible(NULL))
  }

  n_common <- gexp_ml_common_gene_count(sets)
  n_sets <- length(sets)
  fill_colors <- gexp_ml_venn_palette(n_sets)
  set_sizes <- vapply(sets, length, integer(1))
  any_empty <- any(set_sizes == 0L)
  non_empty <- set_sizes > 0L

  .draw_footer <- function() {
    if (!isTRUE(show_footer)) return(invisible(NULL))
    size_txt <- paste(sprintf("%s (%d)", names(sets), set_sizes), collapse = "  |  ")
    common_col <- if (n_common == 0L) "#C0392B" else "#1B5E20"
    grid::grid.text(
      size_txt,
      x = 0.5, y = 0.055,
      gp = grid::gpar(fontsize = 9 * text_scale, col = "gray30", fontface = "plain")
    )
    grid::grid.text(
      sprintf("Common to all selected methods: %d", n_common),
      x = 0.5, y = 0.025,
      gp = grid::gpar(fontsize = 11 * text_scale, col = common_col, fontface = "bold")
    )
    invisible(NULL)
  }

  use_upset <- n_sets > 5L || any_empty || sum(non_empty) < 2L
  if (use_upset) {
    upset_df <- gexp_ml_prepare_upset_df(sets)
    if (is.null(upset_df) || ncol(upset_df) == 0L) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, "No genes returned by selected ML methods.", cex = 1.1 * text_scale, col = "gray40")
      return(invisible(NULL))
    }
    if (nrow(upset_df) == 0L) {
      graphics::plot.new()
      graphics::par(mar = c(7, 4, 4, 2))
      graphics::barplot(
        set_sizes,
        names.arg = rep("", length(set_sizes)),
        col = fill_colors,
        border = NA,
        main = title,
        ylab = "Genes selected",
        ylim = c(0, max(1, max(set_sizes) * 1.2))
      )
      graphics::mtext(paste(names(sets), collapse = "  |  "), side = 1, line = 3.5, cex = 0.75 * text_scale)
      .draw_footer()
      return(invisible(NULL))
    }
    max_set_size <- max(c(set_sizes, 1L))
    all_query <- list(list(
      query = UpSetR::intersects,
      params = colnames(upset_df),
      color = if (n_common == 0L) "#C0392B" else "#1B5E20",
      active = TRUE,
      query.name = "All methods"
    ))
    ts <- c(1.45, 1.15, 1.15, 1.05, 1.45, 1.15) * text_scale
    tryCatch({
      UpSetR::upset(
        upset_df,
        sets = colnames(upset_df),
        keep.order = TRUE,
        order.by = "freq",
        empty.intersections = "on",
        queries = all_query,
        main.bar.color = if (n_common == 0L) "#BDC3C7" else "#3498db",
        sets.bar.color = fill_colors,
        matrix.color = "#27AE60",
        point.size = 3.2 * text_scale,
        line.size = 0.9,
        text.scale = ts,
        mb.ratio = c(0.62, 0.38),
        set_size.show = TRUE,
        set_size.scale_max = max(1L, as.integer(ceiling(max_set_size * 1.15))),
        mainbar.y.label = "Intersection size",
        sets.x.label = "Genes per method"
      )
      .draw_footer()
    }, error = function(e) {
      graphics::plot.new()
      graphics::text(0.5, 0.5, paste("UpSet error:", conditionMessage(e)), cex = 1 * text_scale, col = "red")
    })
    return(invisible(NULL))
  }

  sets_draw <- lapply(sets, function(g) unique(as.character(g)))
  cat_names <- names(sets_draw)
  grid::grid.newpage()
  vp <- tryCatch(
    VennDiagram::venn.diagram(
      x = sets_draw,
      category.names = cat_names,
      filename = NULL,
      output = TRUE,
      disable.logging = TRUE,
      print.mode = "raw",
      scaled = TRUE,
      euler.d = TRUE,
      lwd = 2.2,
      lty = "solid",
      col = grDevices::adjustcolor("white", alpha.f = 0.85),
      fill = fill_colors,
      alpha = 0.58,
      cex = 1.35 * text_scale,
      fontface = "bold",
      fontfamily = "sans",
      cat.cex = 1.12 * text_scale,
      cat.fontface = "bold",
      cat.fontfamily = "sans",
      cat.col = fill_colors,
      cat.default.pos = "outer",
      margin = 0.11,
      rotation = -12,
      main = title,
      main.cex = 1.25 * text_scale,
      main.fontface = "bold",
      main.dist = 0.06
    ),
    error = function(e) NULL
  )
  if (is.null(vp)) {
    graphics::plot.new()
    graphics::text(0.5, 0.5, "Could not draw Venn diagram for these gene sets.", cex = 1.1 * text_scale, col = "gray40")
    return(invisible(NULL))
  }
  grid::grid.draw(vp)
  if (n_common == 0L) {
    grid::grid.text(
      "0",
      x = 0.5, y = 0.5,
      gp = grid::gpar(fontsize = 22 * text_scale, fontface = "bold", col = "#C0392B")
    )
  }
  .draw_footer()
  invisible(NULL)
}

# CSV export directory: all CSV downloads are also written here (app working directory)
CSV_EXPORT_DIR <- function() {
  d <- file.path(getwd(), "csv_exports")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# ==============================================================================
# MICROARRAY PLATFORM / ID MAPPING HELPERS
# ==============================================================================
# A compact mapping of common GPL IDs to Bioconductor annotation packages.
# If a platform is not listed (or the package isn't installed), mapping falls back
# to GPL annotation tables and/or biomaRt where possible.
platform_to_annot <- list(
  "GPL96" = "hgu133a.db",
  "GPL97" = "hgu133b.db",
  "GPL201" = "hgu133a.db",
  "GPL570" = "hgu133plus2.db",
  "GPL571" = "hgu133a2.db",
  "GPL6244" = "hugene10sttranscriptcluster.db",
  "GPL6104" = "illuminahumanv2.db",
  "GPL6947" = "illuminahumanht12v3.db",
  "GPL10558" = "illuminahumanht12v4.db",
  "GPL10904" = "illuminahumanht12v4.db",
  "GPL11532" = "hugene11sttranscriptcluster.db",
  "GPL14951" = "hugene10sttranscriptcluster.db",
  "GPL15088" = "hugene20sttranscriptcluster.db",
  "GPL15314" = "hugene11sttranscriptcluster.db",
  "GPL16043" = "primeview.db",
  "GPL16311" = "hugene20sttranscriptcluster.db",
  "GPL16686" = "hugene21sttranscriptcluster.db",
  "GPL17585" = "hta20transcriptcluster.db",
  "GPL17586" = "hta20transcriptcluster.db",
  "GPL17692" = "hugene21sttranscriptcluster.db",
  "GPL18990" = "hugene21sttranscriptcluster.db",
  "GPL19251" = "hta20transcriptcluster.db",
  "GPL19859" = "hgu133plus2.db",
  "GPL19983" = "hgu133a2.db",
  "GPL20265" = "hugene21sttranscriptcluster.db",
  "GPL21061" = "hugene20sttranscriptcluster.db",
  "GPL21509" = "hta20transcriptcluster.db",
  "GPL21559" = "hugene21sttranscriptcluster.db",
  "GPL21970" = "hgu133plus2.db",
  "GPL22286" = "hugene11sttranscriptcluster.db",
  "GPL22995" = "hugene10sttranscriptcluster.db",
  "GPL23126" = "clariomdhuman.db",
  "GPL23159" = "clariomdhuman.db",
  "GPL23270" = "clarionshuman.db",
  # Clariom S / ENSG transcript IDs (symbols via org.Hs.eg.db ENSEMBL, not probe .db)
  "GPL30033" = NA_character_,
  "GPL21827" = NA_character_,  # Arraystar LncRNA V4 — use GPL table / biomaRt
  "GPL26963" = NA_character_,  # Arraystar LncRNA V5 — use GPL table / biomaRt
  "GPL23432" = "hgu133plus2.db",
  "GPL24299" = "clarionshuman.db",
  "GPL24532" = "hugene21sttranscriptcluster.db",
  "GPL24676" = "illuminahumanht12v3.db",
  "GPL25249" = "clarionshuman.db",
  "GPL25336" = "hugene21sttranscriptcluster.db",
  "GPL25381" = "clarionshuman.db",
  "GPL26944" = "hugene21sttranscriptcluster.db",
  "GPL28718" = "hugene10sttranscriptcluster.db",
  "GPL29829" = "hgu133a.db",
  "GPL30572" = "hugene21sttranscriptcluster.db",
  # Non-human (optional): these map to non-human symbols; downstream assumes human.
  "GPL1261" = "mouse4302.db",
  "GPL1312" = "mgu74av2.db"
)

# HuGene/HTA .db packages for PROBEID -> SYMBOL (Affymetrix transcript cluster IDs, e.g. 2824546_st)
HUGENE_DB_PACKAGES <- c(
  "hugene10sttranscriptcluster.db", "hugene11sttranscriptcluster.db",
  "hugene20sttranscriptcluster.db", "hugene21sttranscriptcluster.db",
  "hta20transcriptcluster.db"
)

# GPL -> biomaRt probe-ID attribute (human). Used when GPL table doesn't provide symbols.
GPL_to_biomart_probe_attr <- c(
  "GPL96" = "affy_hg_u133a",
  "GPL97" = "affy_hg_u133b",
  "GPL201" = "affy_hg_u133a",
  "GPL570" = "affy_hg_u133_plus_2",
  "GPL571" = "affy_hg_u133a2",
  "GPL15207" = "affy_hg_u133_plus_2",
  "GPL17586" = "affy_hta_2_0",
  "GPL13158" = "affy_hg_u133_plus_2",
  "GPL21970" = "affy_hg_u133_plus_2",
  "GPL23432" = "affy_hg_u133_plus_2",
  "GPL1352" = "affy_hg_u133_plus_2",
  "GPL3921" = "affy_hg_u133_plus_2",
  "GPL19859" = "affy_hg_u133_plus_2",
  "GPL10739" = "affy_hg_u133_plus_2",
  "GPL6244" = "affy_hugene_1_0_st_v1",
  "GPL16686" = "affy_hugene_2_0_st_v1",
  "GPL11532" = "affy_hugene_1_1_st_v1",
  "GPL15088" = "affy_hugene_2_0_st_v1",
  "GPL16311" = "affy_hugene_2_0_st_v1",
  "GPL21061" = "affy_hugene_2_0_st_v1",
  "GPL10558" = "illumina_humanht_12_v4",
  "GPL6947" = "illumina_humanht_12_v3",
  "GPL6104" = "illumina_humanht_12_v3"
)

#' Return TRUE when a single ID looks like a microarray probe (not HGNC)
#' @keywords internal
.gexpipe_id_looks_like_probe <- function(id) {
  if (length(id) == 0L) {
    return(logical(0))
  }
  id <- as.character(id)
  vapply(id, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) {
      return(FALSE)
    }
    grepl("_at$|_x_at$|_st$|probe[0-9]*$|^ILMN_|^A_[0-9]+_P[0-9]+", x, ignore.case = TRUE) ||
      grepl("^(ASHG|BEAD)", x, ignore.case = TRUE) ||
      grepl("\\(\\+\\)|^E1A_|_r[0-9]+_", x, ignore.case = TRUE) ||
      (grepl("^ENSG", x, ignore.case = TRUE) && grepl("_at$", x, ignore.case = TRUE)) ||
      nchar(x) > 18L
  }, logical(1))
}

#' Accept a mapped symbol vector for overlap (relaxed vs strict org.Hs.eg.db check)
#' @keywords internal
.gexpipe_accept_mapped_symbols <- function(sym, n_total = length(sym), min_rate = 0.05) {
  sym <- as.character(sym)
  if (length(sym) != n_total || n_total == 0L) {
    return(FALSE)
  }
  .geo_na_vals <- c("", "---", "--", "-", "N/A", "n/a", "NA", "na", "null",
                    "NULL", "none", "NONE", ".", "0", "no match", "no symbol",
                    "unknown", "UNKNOWN")
  sym[sym %in% .geo_na_vals | is.na(sym)] <- NA_character_
  valid <- !is.na(sym) & nzchar(trimws(sym))
  if (sum(valid) < max(3L, ceiling(n_total * min_rate))) {
    return(FALSE)
  }
  if (mean(.gexpipe_id_looks_like_probe(sym[valid]), na.rm = TRUE) > 0.1) {
    return(FALSE)
  }
  TRUE
}

#' Directories to search for cached GEO GPL files (GSE download folders first)
#' @keywords internal
.gexpipe_gpl_destdirs <- function(gse_id = NULL) {
  dirs <- character(0)
  wd <- getwd()
  dirs <- c(dirs, file.path(wd, "micro_data"))
  if (!is.null(gse_id) && nzchar(gse_id)) {
    dirs <- c(dirs, file.path(wd, "micro_data", gse_id))
  }
  opt_micro <- getOption("gexpipe.micro_dir", NULL)
  if (!is.null(opt_micro) && nzchar(opt_micro)) {
    dirs <- c(dirs, opt_micro)
  }
  dirs <- c(dirs, tempdir(), wd)
  unique(dirs[nzchar(dirs) & dir.exists(dirs)])
}

#' Extract gene symbols from ExpressionSet fData (GeneName, Symbol, Entrez, GB_ACC, ...)
#' @keywords internal
.gexpipe_extract_fdata_symbols <- function(fdata) {
  if (is.null(fdata) || !is.data.frame(fdata) || ncol(fdata) == 0L) {
    return(NULL)
  }
  n <- nrow(fdata)
  cn <- colnames(fdata)
  cn_low <- tolower(trimws(cn))
  .geo_na_vals <- c("", "---", "--", "-", "N/A", "n/a", "NA", "na", "null",
                    "NULL", "none", "NONE", ".", "0", "no match", "no symbol",
                    "unknown", "UNKNOWN")

  .clean_col <- function(vals) {
    vals <- as.character(vals)
    vals[vals %in% .geo_na_vals | is.na(vals)] <- NA_character_
    vals <- trimws(vals)
    vals[!nzchar(vals)] <- NA_character_
    vals
  }

  sym_cands_low <- c(
    "gene symbol", "gene.symbol", "gene_symbol", "genesymbol", "genename",
    "gene name", "gene_name", "symbol", "hgnc_symbol", "hgnc symbol",
    "hgnc.symbol", "official symbol", "official_symbol", "gene sym", "genesym"
  )
  for (i in seq_along(cn)) {
    if (cn_low[i] %in% sym_cands_low) {
      sym <- .clean_col(fdata[[cn[i]]])
      if (.gexpipe_accept_mapped_symbols(sym, n)) {
        return(sym)
      }
    }
  }
  idx <- grep("gene.*(sym|symbol|name)|hgnc|^genename$", cn_low)
  for (j in idx) {
    sym <- .clean_col(fdata[[cn[j]]])
    if (.gexpipe_accept_mapped_symbols(sym, n)) {
      return(sym)
    }
  }
  acc_idx <- grep("gb_?acc|refseq|entrez|gene.?id", cn_low)
  acc_idx <- acc_idx[!grepl("probe|spot|symbol|name", cn_low[acc_idx])]
  hs_db <- .gexpipe_hs_db()
  if (length(acc_idx) > 0L && !is.null(hs_db)) {
    for (j in acc_idx) {
      keys <- .clean_col(fdata[[cn[j]]])
      if (sum(!is.na(keys)) < max(3L, ceiling(n * 0.05))) {
        next
      }
      mapped <- tryCatch(
        if (mean(grepl("^[0-9]+$", keys[!is.na(keys)]), na.rm = TRUE) > 0.5) {
          suppressMessages(AnnotationDbi::mapIds(
            hs_db, keys = keys, column = "SYMBOL",
            keytype = "ENTREZID", multiVals = "first"
          ))
        } else {
          suppressMessages(AnnotationDbi::mapIds(
            hs_db, keys = keys, column = "SYMBOL",
            keytype = "ACCNUM", multiVals = "first"
          ))
        },
        error = function(e) NULL
      )
      if (!is.null(mapped)) {
        mapped <- as.character(mapped)
        if (.gexpipe_accept_mapped_symbols(mapped, n)) {
          return(mapped)
        }
      }
    }
  }
  NULL
}

#' Return TRUE when row IDs are confirmed HGNC gene symbols
#' @keywords internal
gexpipe_ids_are_verified_symbols <- function(ids, min_hit_rate = 0.5) {
  ids <- as.character(ids[!is.na(ids) & nzchar(trimws(ids))])
  if (length(ids) == 0L) {
    return(FALSE)
  }
  # Never treat probe-like / Arraystar / Clariom IDs as HGNC symbols
  if (isTRUE(.gexpipe_is_probe_like_ids(ids))) {
    return(FALSE)
  }
  sample_ids <- head(unique(ids), min(80L, length(unique(ids))))
  non_symbol_rate <- mean(
    grepl("_at$|_x_at$|_st$|^ENSG|^ENST|^ENTREZ|^NM_|^NR_", sample_ids, ignore.case = TRUE) |
      grepl("^[0-9]+$", sample_ids) |
      grepl("^[A-Z]{1,2}[0-9]{5,}", sample_ids) |
      grepl("^(ASHG|ILMN_|A_[0-9]+_P[0-9]+|BEAD)", sample_ids, ignore.case = TRUE) |
      grepl("\\(\\+\\)|^E1A_|_r[0-9]+_", sample_ids) |
      nchar(sample_ids) > 14L,
    na.rm = TRUE
  )
  if (non_symbol_rate > 0.1) {
    return(FALSE)
  }

  hs_db <- .gexpipe_hs_db()
  if (is.null(hs_db)) {
    return(all(nchar(sample_ids) <= 10L) &&
      mean(grepl("^[A-Za-z][A-Za-z0-9-]*$", sample_ids), na.rm = TRUE) > 0.9)
  }

  mapped <- tryCatch(
    # suppressMessages: AnnotationDbi::mapIds reports unmapped keys to the console
    suppressMessages(AnnotationDbi::mapIds(
      hs_db, keys = sample_ids, column = "SYMBOL",
      keytype = "SYMBOL", multiVals = "first"
    )),
    error = function(e) NULL
  )
  if (is.null(mapped)) {
    return(FALSE)
  }
  mapped <- as.character(mapped)
  hit <- mean(!is.na(mapped) & toupper(mapped) == toupper(sample_ids), na.rm = TRUE)
  isTRUE(hit >= min_hit_rate)
}

#' Return TRUE when IDs should be converted before common-gene overlap
#' @keywords internal
gexpipe_ids_need_symbol_conversion <- function(ids) {
  if (isTRUE(.gexpipe_is_probe_like_ids(ids))) {
    return(TRUE)
  }
  head_ids <- head(as.character(ids), min(50L, length(ids)))
  if (length(head_ids) > 0L && any(.gexpipe_id_looks_like_probe(head_ids))) {
    return(TRUE)
  }
  !gexpipe_ids_are_verified_symbols(ids)
}

#' Strip Affymetrix suffixes and version from Ensembl-like keys
#' @keywords internal
.gexpipe_clean_ensembl_keys <- function(ids) {
  ids <- as.character(ids)
  ids <- sub("_x_at$", "", ids, ignore.case = TRUE)
  ids <- sub("_at$", "", ids, ignore.case = TRUE)
  gsub("\\..*", "", ids)
}

#' Return TRUE when IDs look like microarray probe IDs (not gene symbols)
#' @keywords internal
.gexpipe_is_probe_like_ids <- function(ids) {
  ids <- as.character(ids[!is.na(ids) & nzchar(trimws(ids))])
  if (length(ids) == 0L) {
    return(FALSE)
  }
  sample_ids <- head(ids, min(300L, length(ids)))
  mean(
    grepl("_at$|_x_at$|_st$|probe[0-9]*$|^ILMN_|^A_[0-9]+_P[0-9]+", sample_ids, ignore.case = TRUE) |
      grepl("^(ASHG|BEAD)", sample_ids, ignore.case = TRUE) |
      grepl("\\(\\+\\)|^E1A_|_r[0-9]+_", sample_ids, ignore.case = TRUE) |
      grepl("^ENSG", sample_ids, ignore.case = TRUE) |
      nchar(sample_ids) > 18L,
    na.rm = TRUE
  ) > 0.15
}

# Detect gene ID format for logging and UI (returns human-readable label)
detect_gene_id_format <- function(ids) {
  ids <- as.character(ids[!is.na(ids) & nzchar(trimws(ids))])
  if (length(ids) == 0) {
    return("Unknown")
  }
  head_ids <- head(ids, min(50L, length(ids)))
  if (length(head_ids) > 0L && any(.gexpipe_id_looks_like_probe(head_ids))) {
    if (mean(grepl("^ENSG", head_ids, ignore.case = TRUE), na.rm = TRUE) > 0.3) {
      return("Ensembl ID")
    }
    return("Microarray probe-like ID")
  }
  sample_ids <- head(ids, min(300, length(ids)))
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
    return("Affymetrix HuGene probe (_st)")
  }
  if (mean(grepl("_at$|_x_at$", sample_ids), na.rm = TRUE) > 0.5) {
    return("Affymetrix HG-U133 probe (_at)")
  }
  if (mean(grepl("probe[0-9]*$", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.3) {
    return("Microarray probe-like ID")
  }
  if (mean(grepl("^(ASHG|ILMN_|A_[0-9]+_P[0-9]+|BEAD)", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.15) {
    return("Microarray probe-like ID")
  }
  if (mean(grepl("\\(\\+\\)|^E1A_|_r[0-9]+_", sample_ids), na.rm = TRUE) > 0.15) {
    return("Microarray probe-like ID")
  }
  if (.gexpipe_is_probe_like_ids(ids)) {
    if (mean(grepl("^ENSG", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.3) {
      return("Ensembl ID")
    }
    return("Microarray probe-like ID")
  }
  if (mean(grepl("^ENSG", sample_ids, ignore.case = TRUE) & grepl("_at$", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.3) {
    return("Affymetrix HG-U133 probe (_at)")
  }
  if (mean(grepl("^ENSG", sample_ids), na.rm = TRUE) > 0.5) {
    return("Ensembl ID")
  }
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7) {
    return("Entrez ID")
  }
  if (mean(grepl("^[A-Z]{1,2}[0-9]{5,}([.][0-9]+)?$", sample_ids), na.rm = TRUE) > 0.5) {
    return("GenBank/EMBL accession")
  }
  if (gexpipe_ids_are_verified_symbols(ids)) {
    return("Gene symbol (HGNC)")
  }
  "Unknown / mixed"
}

.gexpipe_hs_db <- function() {
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    can_install <- tryCatch(
      utils::getFromNamespace(".gexpipe_runtime_install_enabled", "GExPipe")(),
      error = function(e) isTRUE(getOption("gexpipe.auto_install", FALSE))
    )
    if (isTRUE(can_install)) {
      message("org.Hs.eg.db not found - attempting auto-install...")
      tryCatch(
        utils::getFromNamespace(".gexpipe_bioc_install", "GExPipe")(
          "org.Hs.eg.db", ask = FALSE, quiet = TRUE,
          update = FALSE, lib = .libPaths()[1]
        ),
        error   = function(e) message("  Auto-install failed: ", conditionMessage(e)),
        warning = function(w) NULL
      )
    } else {
      message("org.Hs.eg.db not found - install org.Hs.eg.db with BiocManager.")
    }
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    return(NULL)   # graceful fallback - caller must handle NULL
  }
  org.Hs.eg.db::org.Hs.eg.db
}

# Map Affymetrix HuGene _st probe IDs to gene symbols using Bioconductor .db (PROBEID -> SYMBOL).
probe_ids_to_symbol_hugene_db <- function(probe_ids, gpl_id = NULL) {
  probe_ids <- as.character(probe_ids)
  pkgs_to_try <- character(0)
  if (!is.null(gpl_id) && nzchar(gpl_id) && (gpl_id %in% names(platform_to_annot))) {
    pkg <- platform_to_annot[[gpl_id]]
    if (!is.null(pkg) && !is.na(pkg) && nzchar(pkg) && (pkg %in% HUGENE_DB_PACKAGES)) {
      pkgs_to_try <- pkg
    }
  }
  if (length(pkgs_to_try) == 0) pkgs_to_try <- HUGENE_DB_PACKAGES

  for (pkg in pkgs_to_try) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    db <- tryCatch(get(pkg, envir = asNamespace(pkg)), error = function(e) NULL)
    if (is.null(db)) next

    if (any(grepl("_st$", probe_ids))) {
      stripped <- sub("_st$", "", probe_ids)
      out2 <- tryCatch(
        AnnotationDbi::mapIds(db, keys = stripped, column = "SYMBOL", keytype = "PROBEID", multiVals = "first"),
        error = function(e) NULL
      )
      if (!is.null(out2)) {
        out2 <- as.character(out2)
        if (sum(!is.na(out2)) > length(probe_ids) * 0.1) {
          names(out2) <- probe_ids
          return(out2)
        }
      }
    }

    out <- tryCatch(
      AnnotationDbi::mapIds(db, keys = probe_ids, column = "SYMBOL", keytype = "PROBEID", multiVals = "first"),
      error = function(e) NULL
    )
    if (!is.null(out)) {
      out <- as.character(out)
      if (sum(!is.na(out)) > length(probe_ids) * 0.1) {
        names(out) <- probe_ids
        return(out)
      }
    }
  }

  NULL
}

# Map probe IDs to gene symbols using GEO platform (GPL) annotation table.
probe_ids_to_symbol_gpl <- function(probe_ids, gpl_id, gse_id = NULL) {
  tryCatch({
    if (is.null(gpl_id) || !nzchar(gpl_id)) {
      return(NULL)
    }
    probe_ids <- as.character(probe_ids)

    .fetch_gpl <- function(destdir_val) {
      tryCatch(
        # suppressMessages: GEOquery prints cache/download status for GPL fetch
        suppressMessages(GEOquery::getGEO(gpl_id, destdir = destdir_val)),
        error = function(e) NULL
      )
    }
    gpl_raw <- NULL
    for (dd in .gexpipe_gpl_destdirs(gse_id)) {
      gpl_raw <- .fetch_gpl(dd)
      if (!is.null(gpl_raw)) {
        break
      }
    }
    if (is.null(gpl_raw)) {
      gpl_raw <- .fetch_gpl(tempdir())
    }
    # getGEO may return a bare GPL object or a list wrapping one (GEOquery version-dependent)
    .unwrap_gpl <- function(x) {
      if (inherits(x, "GPL")) return(x)
      if (is.list(x)) {
        els <- Filter(function(e) inherits(e, "GPL"), x)
        if (length(els) > 0) return(els[[1L]])
      }
      NULL
    }
    gpl <- .unwrap_gpl(gpl_raw)
    if (is.null(gpl)) return(NULL)

    tab <- tryCatch(GEOquery::Table(gpl), error = function(e) NULL)
    if (is.null(tab) || nrow(tab) == 0 || ncol(tab) == 0) {
      return(NULL)
    }
    tab <- as.data.frame(tab, stringsAsFactors = FALSE)
    if (is.null(colnames(tab))) return(NULL)

    cn     <- colnames(tab)
    cn_low <- tolower(trimws(cn))

    # --- Probe ID column (case-insensitive) ---
    probe_col <- NULL
    probe_cands_low <- c("id", "probe_id", "probe id", "probeid", "spot_id",
                         "id_ref", "idref", "probe")
    for (i in seq_along(cn)) {
      if (cn_low[i] %in% probe_cands_low) { probe_col <- cn[i]; break }
    }
    if (is.null(probe_col)) {
      probe_idx <- which(grepl("(^id$|id_ref|probe|spot)", cn_low))
      if (length(probe_idx) > 0) probe_col <- cn[probe_idx[1]]
    }
    if (is.null(probe_col) || !probe_col %in% cn) return(NULL)

    # --- Gene symbol column (case-insensitive; includes Arraystar GeneName) ---
    symbol_col <- NULL
    sym_cands_low <- c(
      "gene symbol", "gene.symbol", "gene_symbol", "genesymbol", "genename",
      "gene name", "gene_name", "symbol", "hgnc_symbol", "hgnc symbol",
      "hgnc.symbol", "gene sym", "genesym", "official symbol", "official_symbol",
      "gene", "orf", "associated_gene", "associated gene"
    )
    for (i in seq_along(cn)) {
      if (cn_low[i] %in% sym_cands_low) { symbol_col <- cn[i]; break }
    }
    if (is.null(symbol_col)) {
      idx <- grep("gene.*(sym|symbol|name)|hgnc|^genename$", cn_low)
      if (length(idx) > 0) symbol_col <- cn[idx[1]]
    }
    if (is.null(symbol_col)) {
      idx <- grep("symbol|genename", cn_low)
      idx <- idx[!grepl("probe|spot|id$|accession|sequence|title|desc", cn_low[idx])]
      if (length(idx) > 0) symbol_col <- cn[idx[1]]
    }
    # Arraystar / Agilent often store Entrez or RefSeq instead of symbols
    acc_col <- NULL
    acc_cands_low <- c(
      "gb_acc", "gbacc", "genbank", "refseq", "refseqaccession", "refseq_id",
      "entrez_gene_id", "entrezgene", "entrez gene", "gene_id", "geneid"
    )
    for (i in seq_along(cn)) {
      if (cn_low[i] %in% acc_cands_low) { acc_col <- cn[i]; break }
    }
    if (is.null(acc_col)) {
      idx <- grep("gb_?acc|refseq|entrez|gene.?id", cn_low)
      idx <- idx[!grepl("probe|spot|symbol|name", cn_low[idx])]
      if (length(idx) > 0) acc_col <- cn[idx[1]]
    }

    # --- Assignment / description fallback ---
    assignment_col <- NULL
    if (is.null(symbol_col)) {
      assign_cands_low <- c("gene assignment", "gene_assignment", "gene assignment.1")
      for (i in seq_along(cn)) {
        if (cn_low[i] %in% assign_cands_low) { assignment_col <- cn[i]; break }
      }
      if (is.null(assignment_col)) {
        idx <- grep("gene.*assign", cn_low)
        if (length(idx) > 0) assignment_col <- cn[idx[1]]
      }
    }
    # Heuristic fallback: any column with keywords but not the probe column
    if (is.null(symbol_col) && is.null(acc_col) &&
        (is.null(assignment_col) || !assignment_col %in% cn)) {
      candidate_idx <- which(
        grepl("symbol|gene|assign|description|title|annotation|refseq|entrez", cn_low) &
          !(cn %in% c(probe_col))
      )
      if (length(candidate_idx) > 0) {
        assignment_col <- cn[candidate_idx[1]]
      } else {
        return(NULL)
      }
    }

    tab[[probe_col]] <- as.character(tab[[probe_col]])
    convert_via_acc <- FALSE
    if (!is.null(symbol_col)) {
      tab[[symbol_col]] <- as.character(tab[[symbol_col]])
      symbol_source_col <- symbol_col
    } else if (!is.null(acc_col) && acc_col %in% cn) {
      tab[[acc_col]] <- as.character(tab[[acc_col]])
      symbol_source_col <- acc_col
      convert_via_acc <- TRUE
    } else {
      tab[[assignment_col]] <- as.character(tab[[assignment_col]])
      tab[[assignment_col]] <- vapply(tab[[assignment_col]], function(x) {
        x <- as.character(x)
        if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
        x <- trimws(x)
        if (grepl("//", x, fixed = TRUE)) {
          parts <- trimws(unlist(strsplit(x, "\\s*//\\s*")))
          cand <- parts[grepl("^[A-Za-z][A-Za-z0-9._-]*$", parts)]
          if (length(cand) > 0) return(cand[1])
        }
        toks <- trimws(unlist(strsplit(x, "\\s*///\\s*|\\s*;\\s*|\\s*,\\s*")))
        toks <- toks[grepl("^[A-Za-z][A-Za-z0-9._-]*$", toks)]
        if (length(toks) > 0) toks[1] else NA_character_
      }, character(1))
      symbol_source_col <- assignment_col
    }
    if (!symbol_source_col %in% cn) return(NULL)

    # Filter out rows with missing/placeholder symbols
    .geo_na_vals <- c("", "---", "--", "-", "N/A", "n/a", "NA", "na", "null",
                      "NULL", "none", "NONE", ".", "0", "no match", "no symbol",
                      "unknown", "UNKNOWN")
    tab <- tab[
      !is.na(tab[[symbol_source_col]]) &
        !tab[[symbol_source_col]] %in% .geo_na_vals &
        nzchar(trimws(tab[[symbol_source_col]])),
      , drop = FALSE
    ]
    if (nrow(tab) == 0) {
      return(NULL)
    }

    gpl_probes     <- as.character(tab[[probe_col]])
    gpl_probes_low <- tolower(trimws(gpl_probes))
    ids_low        <- tolower(trimws(probe_ids))

    idx <- match(probe_ids, gpl_probes)
    # Try case-insensitive exact match
    if (mean(is.na(idx)) > 0.5) {
      idx2 <- match(ids_low, gpl_probes_low)
      if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
    }
    # Strip Affymetrix _st suffix
    if (mean(is.na(idx)) > 0.5 && any(grepl("_st$", probe_ids))) {
      idx2 <- match(sub("_st$", "", probe_ids), gpl_probes)
      if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
    }
    # Strip _PROBE[0-9]* suffix (CodeLink format: 000106CB1_PROBE1 -> 000106CB1)
    if (mean(is.na(idx)) > 0.5 && any(grepl("_PROBE[0-9]*$", probe_ids, ignore.case = TRUE))) {
      ids_stripped <- sub("_PROBE[0-9]*$", "", probe_ids, ignore.case = TRUE)
      idx2 <- match(ids_stripped, gpl_probes)
      if (sum(!is.na(idx2)) <= sum(!is.na(idx))) {
        idx2 <- match(tolower(ids_stripped), gpl_probes_low)
      }
      if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
    }
    # Generic: strip everything after the last underscore
    if (mean(is.na(idx)) > 0.5 && any(grepl("_.+$", probe_ids))) {
      idx2 <- match(sub("_.+$", "", probe_ids), gpl_probes)
      if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
    }

    out <- tab[[symbol_source_col]][idx]
    out <- vapply(out, function(x) {
      x <- as.character(x)
      if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NA_character_)
      x <- trimws(x)
      toks <- unlist(strsplit(x, "\\s*///\\s*|\\s*//\\s*|\\s*;\\s*|\\s*,\\s*"))
      toks <- trimws(toks)
      toks <- toks[nzchar(toks)]
      if (length(toks) == 0) return(NA_character_)
      toks[1]
    }, character(1))
    # Accession / Entrez columns need one more hop to HGNC symbols
    if (isTRUE(convert_via_acc) && sum(!is.na(out)) > 0L) {
      hs_db <- .gexpipe_hs_db()
      if (!is.null(hs_db)) {
        keys <- as.character(out)
        mapped <- rep(NA_character_, length(keys))
        entrez_idx <- grepl("^[0-9]+$", keys)
        if (any(entrez_idx)) {
          ent <- tryCatch(
            suppressMessages(AnnotationDbi::mapIds(
              hs_db, keys = keys[entrez_idx], column = "SYMBOL",
              keytype = "ENTREZID", multiVals = "first"
            )),
            error = function(e) NULL
          )
          if (!is.null(ent)) mapped[entrez_idx] <- as.character(ent)
        }
        acc_idx <- is.na(mapped) & !is.na(keys) & grepl("^[A-Z]{1,2}[0-9]", keys)
        if (any(acc_idx)) {
          acc <- tryCatch(
            suppressMessages(AnnotationDbi::mapIds(
              hs_db, keys = keys[acc_idx], column = "SYMBOL",
              keytype = "ACCNUM", multiVals = "first"
            )),
            error = function(e) NULL
          )
          if (!is.null(acc)) mapped[acc_idx] <- as.character(acc)
        }
        # Keep already-looking symbols
        keep_sym <- is.na(mapped) & !is.na(keys) &
          grepl("^[A-Za-z][A-Za-z0-9-]*$", keys) & nchar(keys) <= 20L
        mapped[keep_sym] <- keys[keep_sym]
        out <- mapped
      }
    }
    names(out) <- probe_ids
    if (sum(!is.na(out)) > length(probe_ids) * 0.05) {
      return(out)
    }
    NULL
  }, error = function(e) {
    NULL
  })
}

# Map probe IDs to gene symbols via biomaRt (fallback). Returns named vector or NULL.
probe_ids_to_symbol_biomart <- function(probe_ids, gpl_id = NULL) {
  if (!requireNamespace("biomaRt", quietly = TRUE)) {
    return(NULL)
  }
  probe_ids <- as.character(probe_ids)

  # Use [ not [[ to avoid "subscript out of bounds" when gpl_id is not in the named vector
  attr_name <- if (!is.null(gpl_id) && nzchar(gpl_id) && gpl_id %in% names(GPL_to_biomart_probe_attr)) {
    GPL_to_biomart_probe_attr[gpl_id][[1L]]
  } else NULL
  if (is.null(attr_name) || is.na(attr_name)) {
    sample_ids <- head(probe_ids[!is.na(probe_ids) & nzchar(probe_ids)], 200)
    if (length(sample_ids) == 0) {
      return(NULL)
    }
    if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
      attrs_to_try <- c(
        "affy_hta_2_0", "affy_hugene_1_0_st_v1", "affy_hugene_2_0_st_v1",
        "affy_hg_u133_plus_2", "affy_hg_u133a", "affy_hg_u133b"
      )
    } else {
      attrs_to_try <- c(
        "affy_hg_u133_plus_2", "affy_hg_u133a", "affy_hg_u133b",
        "affy_hugene_1_0_st_v1", "affy_hugene_2_0_st_v1"
      )
    }
  } else {
    attrs_to_try <- attr_name
  }

  mart <- tryCatch(biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl"), error = function(e) NULL)
  if (is.null(mart)) {
    return(NULL)
  }

  unique_ids <- unique(probe_ids[!is.na(probe_ids) & nzchar(probe_ids)])
  if (length(unique_ids) == 0) {
    return(NULL)
  }

  ids_to_use <- list(unique_ids)
  if (any(grepl("_st$", unique_ids))) {
    ids_to_use <- c(ids_to_use, list(unique(sub("_st$", "", unique_ids))))
  }

  batch_size <- 5000L
  for (probe_attr in attrs_to_try) {
    for (id_set in ids_to_use) {
      all_res <- NULL
      for (start in seq(1L, length(id_set), by = batch_size)) {
        chunk <- id_set[start:min(start + batch_size - 1L, length(id_set))]
        res <- tryCatch(
          biomaRt::getBM(
            attributes = c(probe_attr, "hgnc_symbol"),
            filters = probe_attr,
            values = chunk,
            mart = mart
          ),
          error = function(e) NULL
        )
        if (is.null(res) || nrow(res) == 0) next
        colnames(res) <- c("probe", "symbol")
        res <- res[!is.na(res$symbol) & nzchar(res$symbol), , drop = FALSE]
        if (nrow(res) == 0) next
        all_res <- rbind(all_res, res)
      }

      if (is.null(all_res) || nrow(all_res) == 0) next
      idx <- match(probe_ids, all_res$probe)
      if (mean(is.na(idx)) > 0.5 && any(grepl("_st$", probe_ids))) {
        idx <- match(sub("_st$", "", probe_ids), all_res$probe)
      }

      out <- all_res$symbol[idx]
      names(out) <- probe_ids
      if (sum(!is.na(out)) > length(probe_ids) * 0.1) {
        return(out)
      }
    }
  }

  NULL
}

# Map microarray probe IDs to gene symbols.
map_microarray_ids <- function(micro_expr, fdata, micro_eset = NULL, gse_id = NULL) {
  probe_ids <- rownames(micro_expr)
  n_probes  <- length(probe_ids)

  # Safety helper: only return a symbol vector when its length matches the probe count.
  .safe_sym <- function(sym) {
    if (is.null(sym)) return(NULL)
    sym <- as.character(sym)
    if (length(sym) == n_probes) sym else NULL
  }

  # fData from GEO (GeneName / Symbol / Entrez) — primary path for Arraystar, Agilent, etc.
  if (!is.null(fdata) && is.data.frame(fdata) && nrow(fdata) == n_probes) {
    fd_sym <- .gexpipe_extract_fdata_symbols(fdata)
    checked <- .safe_sym(fd_sym)
    if (!is.null(checked)) {
      return(checked)
    }
  }

  # Common GEO missing-value placeholders to treat as NA
  .geo_na_vals <- c("", "---", "--", "-", "N/A", "n/a", "NA", "na", "null",
                    "NULL", "none", "NONE", ".", "0", "no match", "no symbol",
                    "unknown", "UNKNOWN")

  gene_symbol_col <- NULL
  if (!is.null(fdata) && is.data.frame(fdata) && ncol(fdata) > 0) {
    cn     <- colnames(fdata)
    cn_low <- tolower(trimws(cn))
    # 1) Exact case-insensitive match against known symbol column names
    sym_cands_low <- c("gene symbol", "gene.symbol", "gene_symbol", "genesymbol",
                       "genename", "gene name", "gene_name",
                       "symbol", "gene sym", "hgnc_symbol", "hgnc symbol",
                       "hgnc.symbol", "official symbol", "official_symbol",
                       "gene.sym", "genesym")
    for (i in seq_along(cn)) {
      if (cn_low[i] %in% sym_cands_low) { gene_symbol_col <- cn[i]; break }
    }
    # 2) Regex fallback: column name contains "gene" AND ("sym" or "symbol" or "name")
    if (is.null(gene_symbol_col)) {
      idx <- grep("gene.*(sym|symbol|name)|hgnc|^genename$", cn_low)
      if (length(idx) > 0) gene_symbol_col <- cn[idx[1]]
    }
    # 3) Broader regex: any column whose name contains "symbol" (not probe/id)
    if (is.null(gene_symbol_col)) {
      idx <- grep("symbol", cn_low)
      idx <- idx[!grepl("probe|spot|id$|accession|sequence|title|desc", cn_low[idx])]
      if (length(idx) > 0) gene_symbol_col <- cn[idx[1]]
    }
  }
  if (!is.null(gene_symbol_col)) {
    gene_symbols <- tryCatch(as.character(fdata[[gene_symbol_col]]), error = function(e) NULL)
    if (!is.null(gene_symbols)) {
      gene_symbols[gene_symbols %in% .geo_na_vals | is.na(gene_symbols)] <- NA
      checked <- .safe_sym(gene_symbols)
      valid_sym <- checked[!is.na(checked) & nzchar(trimws(checked))]
      if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.05 &&
        .gexpipe_accept_mapped_symbols(checked, n_probes)) {
        return(checked)
      }
    }
  }

  platform_id <- tryCatch(
    {
      if (!is.null(micro_eset)) Biobase::annotation(micro_eset) else NULL
    },
    error = function(e) NULL
  )

  annot_pkg <- NULL
  if (!is.null(platform_id) && nzchar(platform_id) && platform_id %in% names(platform_to_annot)) {
    annot_pkg <- platform_to_annot[[platform_id]]
  }
  if (!is.null(annot_pkg) && !is.na(annot_pkg) && nzchar(annot_pkg) && requireNamespace(annot_pkg, quietly = TRUE)) {
    db <- tryCatch(get(annot_pkg, envir = asNamespace(annot_pkg)), error = function(e) NULL)
    gene_symbols <- tryCatch(
      {
        if (is.null(db)) {
          NULL
        } else {
          AnnotationDbi::mapIds(db,
            keys = probe_ids, column = "SYMBOL",
            keytype = "PROBEID", multiVals = "first"
          )
        }
      },
      error = function(e) NULL
    )
    checked <- .safe_sym(gene_symbols)
    if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
      return(checked)
    }
  }

  sample_ids <- head(probe_ids[!is.na(probe_ids) & probe_ids != ""], min(100, length(probe_ids)))
  if (length(sample_ids) == 0) {
    return(probe_ids)
  }

  # If these already look like verified HGNC symbols, keep as-is
  if (gexpipe_ids_are_verified_symbols(probe_ids)) {
    return(probe_ids)
  }

  # Ensembl IDs (including Affymetrix probes like ENSG00000000003_at)
  if (any(grepl("^ENSG", sample_ids, ignore.case = TRUE))) {
    clean_keys <- .gexpipe_clean_ensembl_keys(probe_ids)
    gene_symbols <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        AnnotationDbi::mapIds(hs_db,
          keys = clean_keys, column = "SYMBOL",
          keytype = "ENSEMBL", multiVals = "first"
        )
      },
      error = function(e) NULL
    )
    checked <- .safe_sym(gene_symbols)
    if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
      return(checked)
    }
  }

  # Affymetrix _at probes via platform .db (PROBEID -> SYMBOL)
  if (mean(grepl("_at$|_x_at$", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.2 &&
    !is.null(annot_pkg) && nzchar(annot_pkg) && requireNamespace(annot_pkg, quietly = TRUE)) {
    db <- tryCatch(get(annot_pkg, envir = asNamespace(annot_pkg)), error = function(e) NULL)
    if (!is.null(db)) {
      gene_symbols <- tryCatch(
        AnnotationDbi::mapIds(db, keys = probe_ids, column = "SYMBOL", keytype = "PROBEID", multiVals = "first"),
        error = function(e) NULL
      )
      checked <- .safe_sym(gene_symbols)
      if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
        return(checked)
      }
    }
  }

  # GEO GPL annotation table (custom/CodeLink/Agilent/Arraystar probe IDs)
  if (.gexpipe_is_probe_like_ids(probe_ids) || (!is.null(platform_id) && nzchar(platform_id))) {
    gene_symbols <- probe_ids_to_symbol_gpl(probe_ids, platform_id, gse_id = gse_id)
    checked <- .safe_sym(gene_symbols)
    if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.05) {
      return(checked)
    }
  }

  # Entrez IDs
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7) {
    gene_symbols <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        AnnotationDbi::mapIds(hs_db,
          keys = as.character(probe_ids), column = "SYMBOL",
          keytype = "ENTREZID", multiVals = "first"
        )
      },
      error = function(e) NULL
    )
    checked <- .safe_sym(gene_symbols)
    if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
      return(checked)
    }
  }

  # Fallback: try ALIAS (many probe IDs appear as aliases)
  gene_symbols <- tryCatch(
    {
      hs_db <- .gexpipe_hs_db()
      AnnotationDbi::mapIds(hs_db,
        keys = as.character(probe_ids), column = "SYMBOL",
        keytype = "ALIAS", multiVals = "first"
      )
    },
    error = function(e) NULL
  )
  checked <- .safe_sym(gene_symbols)
  if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
    return(checked)
  }

  # HuGene .db (PROBEID -> SYMBOL) for _st probe IDs
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
    gene_symbols <- probe_ids_to_symbol_hugene_db(probe_ids, platform_id)
    checked <- .safe_sym(gene_symbols)
    if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
      return(checked)
    }
  }

  # biomaRt
  gene_symbols <- probe_ids_to_symbol_biomart(probe_ids, platform_id)
  checked <- .safe_sym(gene_symbols)
  if (!is.null(checked) && sum(!is.na(checked)) > n_probes * 0.1) {
    return(checked)
  }

  probe_ids
}

# Entrez ID -> gene symbol via biomaRt (fallback)
entrez_to_symbol_biomart <- function(ids) {
  if (!requireNamespace("biomaRt", quietly = TRUE)) {
    return(NULL)
  }
  if (is.null(ids) || length(ids) == 0) {
    return(NULL)
  }

  ids <- as.character(ids)
  ids_clean <- gsub("\\.0+$", "", ids)
  keep <- !is.na(ids_clean) & nzchar(trimws(ids_clean)) & grepl("^[0-9]+$", ids_clean)
  if (sum(keep) == 0) {
    return(NULL)
  }

  unique_entrez <- unique(ids_clean[keep])
  mart <- tryCatch(biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl"), error = function(e) NULL)
  if (is.null(mart)) {
    return(NULL)
  }

  out <- rep(NA_character_, length(ids))
  batch_size <- 5000L
  for (start in seq(1L, length(unique_entrez), by = batch_size)) {
    chunk <- unique_entrez[start:min(start + batch_size - 1L, length(unique_entrez))]
    res <- tryCatch(
      biomaRt::getBM(
        attributes = c("entrezgene_id", "hgnc_symbol"),
        filters = "entrezgene_id",
        values = as.integer(chunk),
        mart = mart
      ),
      error = function(e) NULL
    )
    if (is.null(res) || nrow(res) == 0) next
    res <- res[!is.na(res$hgnc_symbol) & nzchar(trimws(res$hgnc_symbol)), , drop = FALSE]
    if (nrow(res) == 0) next
    res$entrezgene_id <- as.character(res$entrezgene_id)

    idx <- match(ids_clean, res$entrezgene_id)
    mapped <- res$hgnc_symbol[idx]
    out[!is.na(mapped)] <- as.character(mapped[!is.na(mapped)])
  }

  if (sum(!is.na(out)) > length(ids) * 0.1) out else NULL
}

# Convert any gene ID (probe, Entrez, Ensembl, or symbol) to gene symbol for overlap
any_id_to_symbol <- function(ids, gpl_id = NULL, gse_id = NULL) {
  if (is.null(ids) || length(ids) == 0) {
    return(ids)
  }
  ids <- as.character(ids)
  ids[is.na(ids) | trimws(ids) == ""] <- NA
  sample_ids <- head(ids[!is.na(ids)], min(500, length(ids)))
  if (length(sample_ids) == 0) {
    return(ids)
  }
  probe_like <- grepl(
    "_at$|_st$|_x_at$|probe[0-9]*$|^ILMN_|^A_[0-9]+_P[0-9]+|^ASHG|\\(\\+\\)|^E1A_",
    sample_ids,
    ignore.case = TRUE
  )

  if (gexpipe_ids_are_verified_symbols(ids)) {
    return(ids)
  }

  # GPL/platform annotation first when probe-like or platform is known
  if (.gexpipe_is_probe_like_ids(ids) || (!is.null(gpl_id) && nzchar(gpl_id))) {
    gpl_sym <- probe_ids_to_symbol_gpl(ids, gpl_id, gse_id = gse_id)
    if (!is.null(gpl_sym) && length(gpl_sym) == length(ids) && sum(!is.na(gpl_sym)) > length(ids) * 0.05) {
      return(as.character(gpl_sym))
    }
    if (mean(probe_like, na.rm = TRUE) > 0.2) {
      db_sym <- probe_ids_to_symbol_hugene_db(ids, gpl_id)
      if (!is.null(db_sym) && length(db_sym) == length(ids) && sum(!is.na(db_sym)) > length(ids) * 0.05) {
        return(as.character(db_sym))
      }
    }
    bm_probe <- probe_ids_to_symbol_biomart(ids, gpl_id)
    if (!is.null(bm_probe) && length(bm_probe) == length(ids) && sum(!is.na(bm_probe)) > length(ids) * 0.05) {
      return(as.character(bm_probe))
    }
  }

  # GenBank / EMBL accessions (e.g. AB000409)
  if (mean(grepl("^[A-Z]{1,2}[0-9]{5,}([.][0-9]+)?$", sample_ids), na.rm = TRUE) > 0.3) {
    sym <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        # suppressMessages: AnnotationDbi::mapIds (ACCNUM -> SYMBOL)
        suppressMessages(AnnotationDbi::mapIds(hs_db,
          keys = ids, column = "SYMBOL",
          keytype = "ACCNUM", multiVals = "first"
        ))
      },
      error = function(e) NULL
    )
    if (!is.null(sym) && length(sym) == length(ids) && sum(!is.na(sym)) > length(ids) * 0.05) {
      return(as.character(sym))
    }
  }

  # Ensembl (strip Affymetrix _at suffix when present)
  if (any(grepl("^ENSG", sample_ids, ignore.case = TRUE))) {
    clean <- .gexpipe_clean_ensembl_keys(ids)
    sym <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        # suppressMessages: AnnotationDbi::mapIds (ENSEMBL -> SYMBOL)
        suppressMessages(AnnotationDbi::mapIds(hs_db,
          keys = clean, column = "SYMBOL",
          keytype = "ENSEMBL", multiVals = "first"
        ))
      },
      error = function(e) NULL
    )
    if (!is.null(sym) && length(sym) == length(ids) && sum(!is.na(sym)) > length(ids) * 0.05) {
      return(as.character(sym))
    }
  }

  # Entrez
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.3) {
    keys_entrez <- gsub("\\.0+$", "", ids)
    sym <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        # suppressMessages: AnnotationDbi::mapIds (ENTREZID -> SYMBOL)
        suppressMessages(AnnotationDbi::mapIds(hs_db,
          keys = keys_entrez, column = "SYMBOL",
          keytype = "ENTREZID", multiVals = "first"
        ))
      },
      error = function(e) NULL
    )
    if (!is.null(sym) && sum(!is.na(sym)) > length(ids) * 0.1) {
      return(as.character(sym))
    }

    bm_sym <- entrez_to_symbol_biomart(ids)
    if (!is.null(bm_sym) && sum(!is.na(bm_sym)) > length(ids) * 0.1) {
      return(bm_sym)
    }
  }

  # ALIAS
  sym <- tryCatch(
    {
      hs_db <- .gexpipe_hs_db()
      # suppressMessages: AnnotationDbi::mapIds (ALIAS -> SYMBOL)
      suppressMessages(AnnotationDbi::mapIds(hs_db,
        keys = ids, column = "SYMBOL",
        keytype = "ALIAS", multiVals = "first"
      ))
    },
    error = function(e) NULL
  )
  if (!is.null(sym) && sum(!is.na(sym)) > length(ids) * 0.1) {
    return(as.character(sym))
  }

  # Probe IDs - try Bioconductor .db, then GPL annotation table, then biomaRt.
  # GPL annotation is tried for ALL probe-like formats (not just _at/_st),
  # so CodeLink, Illumina BEADCHIP, and other non-Affymetrix arrays are handled.
  if (mean(probe_like, na.rm = TRUE) > 0.2 ||
      mean(grepl("^(ASHG|ILMN_|A_[0-9]+_P[0-9]+|BEAD)", sample_ids, ignore.case = TRUE), na.rm = TRUE) > 0.2 ||
      (!is.null(gpl_id) && nzchar(gpl_id))) {
    if (mean(probe_like, na.rm = TRUE) > 0.2) {
      db_sym <- probe_ids_to_symbol_hugene_db(ids, gpl_id)
      if (!is.null(db_sym) && length(db_sym) == length(ids) && sum(!is.na(db_sym)) > length(ids) * 0.1) {
        return(as.character(db_sym))
      }
    }
    # GPL annotation table: always try when gpl_id is available
    if (!is.null(gpl_id) && nzchar(gpl_id)) {
      gpl_sym <- probe_ids_to_symbol_gpl(ids, gpl_id)
      if (!is.null(gpl_sym) && length(gpl_sym) == length(ids) && sum(!is.na(gpl_sym)) > length(ids) * 0.05) {
        return(as.character(gpl_sym))
      }
    }
    bm <- probe_ids_to_symbol_biomart(ids, gpl_id)
    if (!is.null(bm) && length(bm) == length(ids) && sum(!is.na(bm)) > length(ids) * 0.1) {
      return(as.character(bm))
    }
  }

  ids
}

# Check GSE platform(s) and return the platform ID chosen for processing.
get_platform_for_gse <- function(gse_id) {
  gse_id <- trimws(toupper(gse_id))
  if (!grepl("^GSE[0-9]+", gse_id)) {
    warning("Invalid GSE id: ", gse_id)
    return(NULL)
  }
  gse <- tryCatch(
    GEOquery::getGEO(GEO = gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = tempdir()),
    error = function(e) NULL
  )
  if (is.null(gse)) {
    return(NULL)
  }

  platforms <- if (inherits(gse, "list")) {
    unique(vapply(gse, function(x) Biobase::annotation(x), character(1)))
  } else {
    unique(Biobase::annotation(gse))
  }
  if (length(platforms) == 0) {
    return(NULL)
  }

  known <- names(platform_to_annot)
  hit <- platforms[platforms %in% known]
  if (length(hit) > 0) {
    return(hit[1])
  }

  platforms[1]
}

# Run annotation and download for a GSE (utility helper; not used by Shiny flow directly).
run_gse_annotation_and_download <- function(gse_id, dest_dir = getwd(), save_annotated = TRUE) {
  gse_id <- trimws(toupper(gse_id))
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  gse <- tryCatch(
    GEOquery::getGEO(GEO = gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = dest_dir),
    error = function(e) {
      return(invisible(NULL))
    }
  )
  if (is.null(gse)) {
    return(invisible(NULL))
  }

  micro_eset <- if (inherits(gse, "list")) gse[[1]] else gse
  platform_id <- Biobase::annotation(micro_eset)
  micro_expr <- Biobase::exprs(micro_eset)
  fdata <- Biobase::fData(micro_eset)

  gene_symbols <- map_microarray_ids(micro_expr, fdata, micro_eset, gse_id = gse_id)
  rownames(micro_expr) <- gene_symbols
  keep <- !is.na(rownames(micro_expr)) & rownames(micro_expr) != ""
  micro_expr <- micro_expr[keep, , drop = FALSE]
  if (any(duplicated(rownames(micro_expr)))) micro_expr <- limma::avereps(micro_expr, ID = rownames(micro_expr))

  if (isTRUE(save_annotated)) {
    out_file <- file.path(dest_dir, paste0(gse_id, "_annotated_", platform_id, ".rds"))
    saveRDS(list(expr = micro_expr, platform = platform_id, gse_id = gse_id), out_file)
  }

  invisible(list(expr = micro_expr, platform = platform_id, gse_id = gse_id, eset = micro_eset))
}

# ==============================================================================
# CORE ID MAPPING / DOWNLOAD / NORMALIZATION HELPERS
# ==============================================================================

# Manual-style ID -> symbol conversion used in pipeline_download_to_batch.R
# Reused here so the Shiny app and manual script give identical results.
convert_ids_to_symbols_simple <- function(gene_ids) {
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    can_install <- tryCatch(
      utils::getFromNamespace(".gexpipe_runtime_install_enabled", "GExPipe")(),
      error = function(e) isTRUE(getOption("gexpipe.auto_install", FALSE))
    )
    if (isTRUE(can_install)) {
      message("org.Hs.eg.db not found - attempting auto-install...")
      tryCatch(
        utils::getFromNamespace(".gexpipe_bioc_install", "GExPipe")(
          "org.Hs.eg.db", ask = FALSE, quiet = TRUE,
          update = FALSE, lib = .libPaths()[1]
        ),
        error   = function(e) message("  Auto-install failed: ", conditionMessage(e)),
        warning = function(w) NULL
      )
    } else {
      message("org.Hs.eg.db not found - install org.Hs.eg.db with BiocManager.")
    }
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    warning("org.Hs.eg.db unavailable (network issue?) - gene IDs returned as-is without symbol conversion.")
    return(gene_ids)   # graceful fallback: keep original IDs, don't crash
  }
  db <- org.Hs.eg.db::org.Hs.eg.db
  gene_ids <- as.character(gene_ids)
  sample_ids <- head(gene_ids[!is.na(gene_ids) & gene_ids != ""], min(100, length(gene_ids)))
  if (length(sample_ids) == 0) {
    return(gene_ids)
  }
  if (gexpipe_ids_are_verified_symbols(gene_ids)) {
    return(gene_ids)
  }
  # Ensembl IDs
  if (any(grepl("^ENSG", sample_ids, ignore.case = TRUE))) {
    clean <- .gexpipe_clean_ensembl_keys(gene_ids)
    sym <- tryCatch(
      # suppressMessages: AnnotationDbi::mapIds (ENSEMBL -> SYMBOL)
      suppressMessages(AnnotationDbi::mapIds(db,
        keys = clean, column = "SYMBOL",
        keytype = "ENSEMBL", multiVals = "first"
      )),
      error = function(e) NULL
    )
    if (!is.null(sym) && sum(!is.na(sym)) > length(gene_ids) * 0.05) {
      return(as.character(sym))
    }
  }
  # Entrez IDs
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.5) {
    keys_entrez <- gsub("\\.0+$", "", as.character(gene_ids))
    sym <- tryCatch(
      # suppressMessages: AnnotationDbi::mapIds (ENTREZID -> SYMBOL)
      suppressMessages(AnnotationDbi::mapIds(db,
        keys = keys_entrez, column = "SYMBOL",
        keytype = "ENTREZID", multiVals = "first"
      )),
      error = function(e) NULL
    )
    if (!is.null(sym) && sum(!is.na(sym)) > length(gene_ids) * 0.1) {
      return(as.character(sym))
    }
  }
  gene_ids
}

# Convert RNA-seq IDs to symbols - wrapper so app == manual script
convert_rnaseq_ids <- function(gene_ids, gse_id = NULL) {
  if (gexpipe_ids_are_verified_symbols(gene_ids)) {
    return(gene_ids)
  }
  sym <- any_id_to_symbol(gene_ids, gpl_id = NULL, gse_id = gse_id)
  if (!is.null(sym) && length(sym) == length(gene_ids) && sum(!is.na(sym)) > length(gene_ids) * 0.05) {
    return(as.character(sym))
  }
  convert_ids_to_symbols_simple(gene_ids)
}

# Download NCBI-generated raw count matrices (multiple genome versions).
# Returns first successful file (for backward compatibility).
download_ncbi_raw_counts <- function(gse_id, dest_dir) {
  genome_versions <- c(
    "GRCh38.p13_NCBI", "GRCh38.p14_NCBI", "GRCh38.p12_NCBI",
    "GRCh38.p11_NCBI", "GRCh38.p10_NCBI", "GRCh37.p13_NCBI",
    "GRCh38_NCBI", "GRCh37_NCBI"
  )
  for (genome in genome_versions) {
    filename <- paste0(gse_id, "_raw_counts_", genome, ".tsv.gz")
    dest_file <- file.path(dest_dir, filename)
    if (file.exists(dest_file) && file.info(dest_file)$size > 1000) {
      return(dest_file)
    }
    url <- paste0(
      "https://www.ncbi.nlm.nih.gov/geo/download/?type=rnaseq_counts&acc=",
      gse_id, "&format=file&file=", filename
    )
    result <- tryCatch(
      {
        download.file(url, dest_file, mode = "wb", quiet = TRUE)
        if (file.exists(dest_file) && file.info(dest_file)$size > 1000) {
          return(dest_file)
        }
        if (file.exists(dest_file)) file.remove(dest_file)
        NULL
      },
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }
  return(NULL)
}

# Try all NCBI genome versions and return the path with the MOST rows (fullest matrix).
download_ncbi_raw_counts_best <- function(gse_id, dest_dir) {
  genome_versions <- c(
    "GRCh38.p13_NCBI", "GRCh38.p14_NCBI", "GRCh38.p12_NCBI",
    "GRCh38.p11_NCBI", "GRCh38.p10_NCBI", "GRCh37.p13_NCBI",
    "GRCh38_NCBI", "GRCh37_NCBI"
  )
  best_path <- NULL
  best_nrow <- 0L
  for (genome in genome_versions) {
    filename <- paste0(gse_id, "_raw_counts_", genome, ".tsv.gz")
    dest_file <- file.path(dest_dir, filename)
    if (!file.exists(dest_file) || file.info(dest_file)$size < 1000) {
      url <- paste0(
        "https://www.ncbi.nlm.nih.gov/geo/download/?type=rnaseq_counts&acc=",
        gse_id, "&format=file&file=", filename
      )
      tryCatch(
        {
          download.file(url, dest_file, mode = "wb", quiet = TRUE)
        },
        error = function(e) NULL,
        warning = function(w) NULL
      )
    }
    if (file.exists(dest_file) && file.info(dest_file)$size > 1000) {
      n <- tryCatch(
        {
          df <- .gexpipe_fread_counts(dest_file, nrows = 500000L)
          nrow(df)
        },
        error = function(e) 0L
      )
      if (n > best_nrow) {
        best_nrow <- n
        best_path <- dest_file
      }
    }
  }
  best_path
}

#' Spearman correlation matrix without warnings on zero-variance columns
#' @keywords internal
gexpipe_spearman_cor <- function(x) {
  if (is.null(x) || (!is.matrix(x) && !is.data.frame(x))) {
    stop("x must be a matrix or data.frame.")
  }
  x <- as.matrix(x)
  n <- ncol(x)
  cn <- colnames(x)
  if (is.null(cn)) {
    cn <- paste0("V", seq_len(n))
    colnames(x) <- cn
  }
  if (n < 2L) {
    return(matrix(1, nrow = n, ncol = n, dimnames = list(cn, cn)))
  }
  v <- apply(x, 2, stats::var, na.rm = TRUE)
  keep <- !is.na(v) & v > 0
  out <- matrix(NA_real_, nrow = n, ncol = n, dimnames = list(cn, cn))
  if (sum(keep) < 2L) {
    diag(out) <- 1
    return(out)
  }
  sub <- stats::cor(x[, keep, drop = FALSE], method = "spearman", use = "pairwise.complete.obs")
  idx <- which(keep)
  out[idx, idx] <- sub
  diag(out) <- ifelse(is.na(diag(out)), 1, diag(out))
  out[!keep, !keep] <- diag(1, sum(!keep))
  out
}

#' Capture verbose GEOquery console output without suppressMessages()
#' @keywords internal
.gexpipe_geo_quiet <- function(expr) {
  res <- NULL
  err <- NULL
  tryCatch(
    {
      invisible(utils::capture.output(
        res <- force(expr),
        file = nullfile()
      ))
    },
    error = function(e) {
      err <<- e
    }
  )
  if (!is.null(err)) {
    stop(err)
  }
  res
}

#' Read tabular count files (GEO supplementary files are often messy)
#' @keywords internal
.gexpipe_fread_counts <- function(path, ...) {
  withCallingHandlers(
    data.table::fread(path, data.table = FALSE, ...),
    warning = function(w) {
      if (grepl("Column name|EOF|stopped early", conditionMessage(w), ignore.case = TRUE)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

# Read count matrix (handles .gz)
read_count_matrix <- function(file_path) {
  read_with_suppress <- function(path) {
    .gexpipe_fread_counts(path)
  }
  if (grepl("\\.gz$", file_path, ignore.case = TRUE)) {
    df <- tryCatch(
      read_with_suppress(file_path),
      error = function(e) {
        if (!requireNamespace("R.utils", quietly = TRUE)) {
          stop("Cannot read .gz file: R.utils is required. Install with: install.packages(\"R.utils\").")
        }
        R.utils::gunzip(file_path, remove = FALSE, overwrite = TRUE)
        tmp <- gsub("\\.gz$", "", file_path, ignore.case = TRUE)
        out <- .gexpipe_fread_counts(tmp)
        if (file.exists(tmp)) file.remove(tmp)
        out
      }
    )
  } else {
    df <- read_with_suppress(file_path)
  }
  return(df)
}

# Classify groups
classify_groups <- function(group_info, normal_keywords, disease_keywords) {
  group_clean <- tolower(trimws(as.character(group_info)))
  normal_clean <- tolower(trimws(normal_keywords))
  disease_clean <- tolower(trimws(disease_keywords))

  final_groups <- rep(NA_character_, length(group_clean))
  final_groups[group_clean %in% disease_clean] <- "Disease"
  final_groups[group_clean %in% normal_clean & is.na(final_groups)] <- "Normal"

  keep_samples <- !is.na(final_groups)
  return(list(groups = final_groups, keep = keep_samples))
}

# Normalize microarray (quantile; with tracking)
normalize_microarray <- function(expr_matrix, dataset_name = NULL, method = "quantile") {
  initial_genes <- nrow(expr_matrix)

  expr_matrix <- expr_matrix[rowSums(is.na(expr_matrix)) < ncol(expr_matrix), ]
  expr_matrix <- expr_matrix[, colSums(is.na(expr_matrix)) < nrow(expr_matrix)]

  genes_after_na_removal <- nrow(expr_matrix)

  max_val <- max(expr_matrix, na.rm = TRUE)
  log2_applied <- FALSE
  if (max_val > 50) {
    min_val <- min(expr_matrix, na.rm = TRUE)
    if (min_val < 0) expr_matrix <- expr_matrix - min_val + 1
    expr_matrix <- log2(expr_matrix + 1)
    log2_applied <- TRUE
  }

  expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")

  attr(expr_norm, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_na_removal = genes_after_na_removal,
    final_genes = nrow(expr_norm),
    log2_applied = log2_applied,
    method = method
  )

  return(expr_norm)
}

# RMA normalization from CEL files (probe-level). Returns matrix with probe rownames, or NULL on failure.
GPL_USE_OLIGO <- c(
  "GPL6244", "GPL16686", "GPL11532", "GPL17585", "GPL17586", "GPL15088",
  "GPL16311", "GPL21061", "GPL13915", "GPL19251", "GPL26944", "GPL21559",
  "GPL24532", "GPL25336", "GPL18990", "GPL17692", "GPL20265", "GPL30572",
  "GPL14951", "GPL28718", "GPL15314", "GPL17556"
)

normalize_microarray_rma <- function(cel_paths, platform_id, dataset_name = NULL) {
  if (length(cel_paths) == 0) {
    return(NULL)
  }
  cel_paths <- cel_paths[file.exists(cel_paths)]
  if (length(cel_paths) == 0) {
    return(NULL)
  }

  use_oligo <- platform_id %in% GPL_USE_OLIGO
  if (use_oligo && requireNamespace("oligo", quietly = TRUE)) {
    return(tryCatch(
      {
        affyBatch <- oligo::read.celfiles(cel_paths)
        expr_rma <- oligo::rma(affyBatch)
        mat <- Biobase::exprs(expr_rma)
        attr(mat, "normalization_info") <- list(
          initial_genes = nrow(mat), final_genes = nrow(mat),
          method = "RMA (oligo)", log2_applied = TRUE
        )
        mat
      },
      error = function(e) NULL
    ))
  }

  if (requireNamespace("affy", quietly = TRUE)) {
    return(tryCatch(
      {
        affyBatch <- affy::ReadAffy(filenames = cel_paths)
        expr_rma <- affy::rma(affyBatch)
        mat <- Biobase::exprs(expr_rma)
        attr(mat, "normalization_info") <- list(
          initial_genes = nrow(mat), final_genes = nrow(mat),
          method = "RMA (affy)", log2_applied = TRUE
        )
        mat
      },
      error = function(e) NULL
    ))
  }

  NULL
}

# Normalize RNA-seq (TMM + logCPM; with tracking)
normalize_rnaseq <- function(count_matrix, dataset_name = NULL, method = "TMM") {
  initial_genes <- nrow(count_matrix)

  count_matrix <- as.matrix(count_matrix)
  mode(count_matrix) <- "numeric"
  count_matrix <- round(count_matrix)
  count_matrix[count_matrix < 0] <- 0

  keep <- rowSums(count_matrix >= 10) >= 3
  count_matrix <- count_matrix[keep, ]

  genes_after_filtering <- nrow(count_matrix)
  genes_removed <- initial_genes - genes_after_filtering

  dge <- edgeR::DGEList(counts = count_matrix)
  if (method == "TMM") {
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
  }
  logcpm_matrix <- edgeR::cpm(dge, log = TRUE, prior.count = 1)

  attr(logcpm_matrix, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_filtering = genes_after_filtering,
    genes_removed = genes_removed,
    final_genes = nrow(logcpm_matrix),
    method = method
  )

  return(logcpm_matrix)
}
