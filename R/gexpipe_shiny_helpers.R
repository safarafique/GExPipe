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

# Detect gene ID format for logging and UI (returns human-readable label)
detect_gene_id_format <- function(ids) {
  ids <- as.character(ids[!is.na(ids) & nzchar(trimws(ids))])
  if (length(ids) == 0) {
    return("Unknown")
  }
  sample_ids <- head(ids, min(300, length(ids)))
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
    return("Affymetrix HuGene probe (_st)")
  }
  if (mean(grepl("_at$|_x_at$", sample_ids), na.rm = TRUE) > 0.5) {
    return("Affymetrix HG-U133 probe (_at)")
  }
  if (mean(grepl("^ENSG", sample_ids), na.rm = TRUE) > 0.5) {
    return("Ensembl ID")
  }
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7) {
    return("Entrez ID")
  }
  if (mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.6 &&
    mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3) {
    return("Gene symbol (HGNC)")
  }
  "Unknown / mixed"
}

.gexpipe_hs_db <- function() {
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    message("org.Hs.eg.db not found — attempting auto-install...")
    tryCatch(
      BiocManager::install("org.Hs.eg.db", ask = FALSE, quiet = TRUE,
                           update = FALSE, lib = .libPaths()[1]),
      error   = function(e) message("  Auto-install failed: ", conditionMessage(e)),
      warning = function(w) NULL
    )
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    return(NULL)   # graceful fallback — caller must handle NULL
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
probe_ids_to_symbol_gpl <- function(probe_ids, gpl_id) {
  if (is.null(gpl_id) || !nzchar(gpl_id)) {
    return(NULL)
  }
  probe_ids <- as.character(probe_ids)

  gpl <- tryCatch(
    suppressMessages(GEOquery::getGEO(gpl_id, destdir = tempdir())),
    error = function(e) NULL
  )
  if (is.null(gpl)) {
    return(NULL)
  }

  tab <- tryCatch(if (inherits(gpl, "GPL")) GEOquery::Table(gpl) else NULL, error = function(e) NULL)
  if (is.null(tab) || nrow(tab) == 0) {
    return(NULL)
  }

  probe_col <- NULL
  for (cand in c("ID", "PROBE_ID", "Probe", "probe_id", "SPOT_ID")) {
    if (cand %in% colnames(tab)) {
      probe_col <- cand
      break
    }
  }
  if (is.null(probe_col)) {
    return(NULL)
  }

  symbol_col <- NULL
  for (cand in c(
    "Gene Symbol", "GENE_SYMBOL", "Gene.symbol", "Symbol", "gene_symbol",
    "SYMBOL", "GeneSymbol", "Gene_Symbol"
  )) {
    if (cand %in% colnames(tab)) {
      symbol_col <- cand
      break
    }
  }
  if (is.null(symbol_col)) {
    return(NULL)
  }

  tab[[probe_col]] <- as.character(tab[[probe_col]])
  tab[[symbol_col]] <- as.character(tab[[symbol_col]])
  tab <- tab[!is.na(tab[[symbol_col]]) & nzchar(trimws(tab[[symbol_col]])), , drop = FALSE]
  if (nrow(tab) == 0) {
    return(NULL)
  }

  gpl_probes <- tab[[probe_col]]
  idx <- match(probe_ids, gpl_probes)
  if (mean(is.na(idx)) > 0.5 && any(grepl("_st$", probe_ids))) {
    idx2 <- match(sub("_st$", "", probe_ids), gpl_probes)
    if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
  }

  out <- tab[[symbol_col]][idx]
  names(out) <- probe_ids
  if (sum(!is.na(out)) > length(probe_ids) * 0.1) {
    return(out)
  }
  NULL
}

# Map probe IDs to gene symbols via biomaRt (fallback). Returns named vector or NULL.
probe_ids_to_symbol_biomart <- function(probe_ids, gpl_id = NULL) {
  if (!requireNamespace("biomaRt", quietly = TRUE)) {
    return(NULL)
  }
  probe_ids <- as.character(probe_ids)

  attr_name <- if (!is.null(gpl_id) && nzchar(gpl_id)) GPL_to_biomart_probe_attr[[gpl_id]] else NULL
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

  possible_symbol_cols <- c(
    "Gene Symbol", "Gene.symbol", "GENE_SYMBOL", "Symbol", "gene_symbol", "SYMBOL",
    "GeneSymbol", "gene.symbol", "Gene_Symbol"
  )
  gene_symbol_col <- NULL
  if (!is.null(fdata) && ncol(fdata) > 0) {
    for (col in possible_symbol_cols) {
      if (col %in% colnames(fdata)) {
        gene_symbol_col <- col
        break
      }
    }
  }
  if (!is.null(gene_symbol_col)) {
    gene_symbols <- as.character(fdata[[gene_symbol_col]])
    gene_symbols[gene_symbols == "" | is.na(gene_symbols)] <- NA
    return(gene_symbols)
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
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }

  sample_ids <- head(probe_ids[!is.na(probe_ids) & probe_ids != ""], min(100, length(probe_ids)))
  if (length(sample_ids) == 0) {
    return(probe_ids)
  }

  # If these already look like symbols, keep as-is (verify against org.Hs.eg.db when available)
  is_likely_symbol <- mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.7 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3
  if (is_likely_symbol && !any(grepl("^[0-9]{5,}", sample_ids))) {
    verified <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        mapped <- AnnotationDbi::mapIds(hs_db,
          keys = head(sample_ids, 10), column = "SYMBOL",
          keytype = "SYMBOL", multiVals = "first"
        )
        sum(!is.na(mapped)) / min(10, length(sample_ids)) > 0.5
      },
      error = function(e) FALSE
    )
    if (verified) {
      return(probe_ids)
    }
  }

  # Ensembl IDs
  if (any(grepl("^ENSG", sample_ids))) {
    clean_keys <- gsub("\\..*", "", probe_ids)
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
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
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
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
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
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
  }

  # HuGene .db (PROBEID -> SYMBOL) for _st probe IDs
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
    gene_symbols <- probe_ids_to_symbol_hugene_db(probe_ids, platform_id)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }

  # GEO GPL annotation table
  gene_symbols <- probe_ids_to_symbol_gpl(probe_ids, platform_id)
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
  }

  # biomaRt
  gene_symbols <- probe_ids_to_symbol_biomart(probe_ids, platform_id)
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
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
any_id_to_symbol <- function(ids, gpl_id = NULL) {
  if (is.null(ids) || length(ids) == 0) {
    return(ids)
  }
  ids <- as.character(ids)
  ids[is.na(ids) | trimws(ids) == ""] <- NA
  sample_ids <- head(ids[!is.na(ids)], min(500, length(ids)))
  if (length(sample_ids) == 0) {
    return(ids)
  }

  is_likely_symbol <- mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.6 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.4 &&
    mean(grepl("^ENSG", sample_ids), na.rm = TRUE) < 0.5 &&
    mean(grepl("_at$|_st$|_x_at$", sample_ids), na.rm = TRUE) < 0.2
  if (is_likely_symbol) {
    return(ids)
  }

  # Ensembl
  if (any(grepl("^ENSG", sample_ids))) {
    clean <- gsub("\\..*", "", ids)
    sym <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
        suppressMessages(AnnotationDbi::mapIds(hs_db,
          keys = clean, column = "SYMBOL",
          keytype = "ENSEMBL", multiVals = "first"
        ))
      },
      error = function(e) NULL
    )
    if (!is.null(sym) && length(sym) == length(ids) && sum(!is.na(sym)) > length(ids) * 0.1) {
      return(as.character(sym))
    }
  }

  # Entrez
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.3) {
    keys_entrez <- gsub("\\.0+$", "", ids)
    sym <- tryCatch(
      {
        hs_db <- .gexpipe_hs_db()
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

  # Probe IDs
  if (mean(grepl("_at$|_st$|_x_at$", sample_ids), na.rm = TRUE) > 0.2) {
    db_sym <- probe_ids_to_symbol_hugene_db(ids, gpl_id)
    if (!is.null(db_sym) && sum(!is.na(db_sym)) > length(ids) * 0.1) {
      return(as.character(db_sym))
    }
    gpl_sym <- probe_ids_to_symbol_gpl(ids, gpl_id)
    if (!is.null(gpl_sym) && sum(!is.na(gpl_sym)) > length(ids) * 0.1) {
      return(as.character(gpl_sym))
    }
    bm <- probe_ids_to_symbol_biomart(ids, gpl_id)
    if (!is.null(bm) && sum(!is.na(bm)) > length(ids) * 0.1) {
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

# Manual-style ID → symbol conversion used in pipeline_download_to_batch.R
# Reused here so the Shiny app and manual script give identical results.
convert_ids_to_symbols_simple <- function(gene_ids) {
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    message("org.Hs.eg.db not found — attempting auto-install...")
    tryCatch(
      BiocManager::install("org.Hs.eg.db", ask = FALSE, quiet = TRUE,
                           update = FALSE, lib = .libPaths()[1]),
      error   = function(e) message("  Auto-install failed: ", conditionMessage(e)),
      warning = function(w) NULL
    )
  }
  if (!requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    warning("org.Hs.eg.db unavailable (network issue?) — gene IDs returned as-is without symbol conversion.")
    return(gene_ids)   # graceful fallback: keep original IDs, don't crash
  }
  db <- org.Hs.eg.db::org.Hs.eg.db
  gene_ids <- as.character(gene_ids)
  sample_ids <- head(gene_ids[!is.na(gene_ids) & gene_ids != ""], min(100, length(gene_ids)))
  if (length(sample_ids) == 0) {
    return(gene_ids)
  }
  # Already gene symbols?
  if (mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.7 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3) {
    return(gene_ids)
  }
  # Ensembl IDs
  if (any(grepl("^ENSG", sample_ids))) {
    clean <- gsub("\\..*", "", gene_ids)
    sym <- tryCatch(
      suppressMessages(AnnotationDbi::mapIds(db,
        keys = clean, column = "SYMBOL",
        keytype = "ENSEMBL", multiVals = "first"
      )),
      error = function(e) NULL
    )
    if (!is.null(sym) && sum(!is.na(sym)) > length(gene_ids) * 0.1) {
      return(as.character(sym))
    }
  }
  # Entrez IDs
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.5) {
    keys_entrez <- gsub("\\.0+$", "", as.character(gene_ids))
    sym <- tryCatch(
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

# Convert RNA-seq IDs to symbols – wrapper so app == manual script
convert_rnaseq_ids <- function(gene_ids, gse_id = NULL) {
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
          df <- suppressWarnings(data.table::fread(dest_file, data.table = FALSE, nrows = 500000L))
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

# Read count matrix (handles .gz)
read_count_matrix <- function(file_path) {
  read_with_suppress <- function(path) {
    suppressWarnings(data.table::fread(path, data.table = FALSE))
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
        out <- suppressWarnings(data.table::fread(tmp, data.table = FALSE))
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
