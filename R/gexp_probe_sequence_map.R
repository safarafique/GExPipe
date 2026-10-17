# ==============================================================================
# GEXP_PROBE_SEQUENCE_MAP.R - last-resort probe -> gene symbol mapping by sequence
# ==============================================================================
# Some GEO platforms (e.g. GPL16956, Arraystar human lncRNA V3) publish no gene
# symbol, accession or coordinate for their probes - only the probe SEQUENCE.
# Every annotation-based route then fails and the dataset keeps probe IDs, so it
# shares no genes with other datasets. This fallback matches each probe
# sequence exactly (both strands) against Ensembl human transcripts (cDNA +
# ncRNA) and keeps a symbol only when all matching transcripts belong to ONE
# gene. Reference files and per-platform results are cached on disk, so the
# download (~100 MB) and the matching happen once per computer / platform.
# Turn off with options(gexpipe.probe_sequence_map = FALSE).
# ==============================================================================

.gexpipe_probe_seq_cache_dir <- function() {
  d <- file.path(tools::R_user_dir("GExPipe", which = "cache"), "probe_sequence_map")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

#' Ensembl human transcript FASTA files (cDNA + ncRNA), downloaded once
#' @return character paths, or NULL when they cannot be obtained
#' @keywords internal
.gexpipe_probe_seq_reference <- function() {
  d <- .gexpipe_probe_seq_cache_dir()
  base <- getOption("gexpipe.ensembl_fasta_base",
                    "https://ftp.ensembl.org/pub/current_fasta/homo_sapiens")
  files <- c(cdna = "cdna/Homo_sapiens.GRCh38.cdna.all.fa.gz",
             ncrna = "ncrna/Homo_sapiens.GRCh38.ncrna.fa.gz")
  gz_ok <- function(f) {
    file.exists(f) && file.size(f) > 1e6 &&
      !inherits(try(utils::tail(readLines(gzfile(f), warn = FALSE), 1L), silent = TRUE), "try-error")
  }
  out <- file.path(d, basename(files))
  old_to <- getOption("timeout"); on.exit(options(timeout = old_to), add = TRUE)
  options(timeout = max(3600, old_to))
  for (i in seq_along(files)) {
    if (gz_ok(out[i])) next
    for (attempt in seq_len(3L)) {
      message("GExPipe: downloading Ensembl ", names(files)[i], " sequences for probe mapping (once) ...")
      tmp <- paste0(out[i], ".part")
      ok <- tryCatch(utils::download.file(paste0(base, "/", files[i]), tmp, mode = "wb", quiet = TRUE) == 0,
                     error = function(e) FALSE)
      if (ok && gz_ok(tmp)) { file.rename(tmp, out[i]); break }
      unlink(tmp)
    }
    if (!gz_ok(out[i])) return(NULL)
  }
  out
}

#' Map probes to gene symbols by exact sequence match to Ensembl transcripts
#'
#' @param probe_ids Row IDs of the expression matrix.
#' @param gpl_id GEO platform accession whose annotation holds probe sequences.
#' @param gse_id Optional GSE (for the GPL cache location).
#' @return list(symbols = character vector aligned to probe_ids, NA when not
#'   unique; n_mapped; n_with_sequence; note), or NULL when not possible.
#' @keywords internal
.gexpipe_map_probes_by_sequence <- function(probe_ids, gpl_id, gse_id = NULL) {
  if (isFALSE(getOption("gexpipe.probe_sequence_map", TRUE))) return(NULL)
  if (is.null(gpl_id) || !nzchar(gpl_id)) return(NULL)
  if (!requireNamespace("Biostrings", quietly = TRUE)) {
    return(list(symbols = NULL, note = "install Biostrings to map probes by sequence: BiocManager::install('Biostrings')"))
  }
  cache_csv <- file.path(.gexpipe_probe_seq_cache_dir(), paste0(gpl_id, "_sequence_map.csv"))
  if (file.exists(cache_csv)) {
    mp <- utils::read.csv(cache_csv, stringsAsFactors = FALSE)
  } else {
    tab <- .gexpipe_fetch_gpl_table_df(gpl_id, gse_id = gse_id)
    if (is.null(tab)) return(NULL)
    id_col  <- intersect(c("ID", "ID_REF", "ProbeName", "Probe_ID"), colnames(tab))[1]
    seq_col <- grep("^(sequence|probe.?sequence|seq)$", colnames(tab), ignore.case = TRUE, value = TRUE)[1]
    if (is.na(id_col) || is.na(seq_col)) return(NULL)
    s <- toupper(gsub("\\s", "", as.character(tab[[seq_col]])))
    keep <- grepl("^[ACGT]{25,}$", s)
    if (sum(keep) < 100L) return(NULL)
    ref <- .gexpipe_probe_seq_reference()
    if (is.null(ref)) {
      return(list(symbols = NULL, note = "Ensembl transcript sequences could not be downloaded (check internet)"))
    }
    tx  <- do.call(c, lapply(ref, Biostrings::readDNAStringSet))
    hdr <- names(tx)
    gene <- ifelse(grepl("gene_symbol:", hdr, fixed = TRUE), sub(".*gene_symbol:(\\S+).*", "\\1", hdr),
                   sub(".*gene:(ENSG[0-9]+).*", "\\1", hdr))
    seqs <- Biostrings::DNAStringSet(s[keep])
    hits <- vector("list", length(seqs))
    for (strand in seq_len(2L)) {
      q <- if (strand == 1L) seqs else Biostrings::reverseComplement(seqs)
      for (w in unique(Biostrings::width(q))) {
        ix <- which(Biostrings::width(q) == w)
        h  <- Biostrings::vwhichPDict(Biostrings::PDict(q[ix]), tx)
        tr <- rep(seq_along(h), lengths(h)); pr <- ix[unlist(h)]
        if (!length(pr)) next
        g <- split(gene[tr], pr)
        for (k in names(g)) { kk <- as.integer(k); hits[[kk]] <- unique(c(hits[[kk]], g[[k]])) }
      }
    }
    sym <- vapply(hits, function(g) if (length(g) == 1L && !grepl("^ENSG", g)) g else NA_character_, character(1))
    mp <- data.frame(ID = as.character(tab[[id_col]][keep]), Symbol = sym,
                     N_Genes_Hit = lengths(hits), stringsAsFactors = FALSE)
    utils::write.csv(mp, cache_csv, row.names = FALSE)
  }
  symbols <- mp$Symbol[match(as.character(probe_ids), mp$ID)]
  list(symbols = symbols, n_mapped = sum(!is.na(symbols)), n_with_sequence = sum(as.character(probe_ids) %in% mp$ID),
       n_ambiguous = sum(mp$N_Genes_Hit[match(as.character(probe_ids), mp$ID)] > 1L, na.rm = TRUE),
       note = sprintf("Ensembl transcripts (%s); cached in %s", basename(dirname(cache_csv)), cache_csv))
}

# ==============================================================================
# Symbol harmonisation: outdated gene symbols -> current official HGNC symbols
# ==============================================================================
# Platforms annotated in different years name the same gene differently
# (e.g. MARCH6 -> MARCHF6, HIST1H2BC -> H2BC4, C7orf41 -> MTPN2), so the same
# gene would not overlap between datasets. A symbol that is not an official
# symbol is replaced when it is an alias of exactly ONE official gene;
# ambiguous aliases, LOC/ENSG IDs and official symbols are left unchanged.

.gexpipe_symbol_env <- new.env(parent = emptyenv())

#' @return list(ids = updated symbols, n = number changed, examples = "OLD->NEW, ...")
#' @keywords internal
.gexpipe_harmonize_symbols <- function(ids) {
  ids <- as.character(ids)
  db <- tryCatch(.gexpipe_hs_db(), error = function(e) NULL)
  if (is.null(db)) return(list(ids = ids, n = 0L, examples = ""))
  if (is.null(.gexpipe_symbol_env$official)) {
    .gexpipe_symbol_env$official <- AnnotationDbi::keys(db, keytype = "SYMBOL")
  }
  off <- .gexpipe_symbol_env$official
  need <- unique(ids[!is.na(ids) & !ids %in% off & grepl("^[A-Za-z][A-Za-z0-9.-]*$", ids) &
                     !grepl("^(LOC[0-9]+|ENS[A-Z]*[0-9]+|LINC[0-9]+)$", ids)])
  if (!length(need)) return(list(ids = ids, n = 0L, examples = ""))
  m <- tryCatch(suppressMessages(AnnotationDbi::select(db, keys = need, keytype = "ALIAS", columns = "SYMBOL")),
                error = function(e) NULL)
  if (is.null(m) || !nrow(m)) return(list(ids = ids, n = 0L, examples = ""))
  m <- unique(m[!is.na(m$SYMBOL), c("ALIAS", "SYMBOL")])
  n_per <- table(m$ALIAS)
  m <- m[m$ALIAS %in% names(n_per)[n_per == 1L], ]
  map <- stats::setNames(m$SYMBOL, m$ALIAS)
  hit <- ids %in% names(map)
  out <- ids; out[hit] <- map[ids[hit]]
  ex <- utils::head(unique(paste0(ids[hit], "->", out[hit])), 4L)
  list(ids = out, n = sum(hit), examples = paste(ex, collapse = ", "))
}

#' Convert leftover Entrez / Ensembl / RefSeq / GenBank IDs to gene symbols
#'
#' Some annotation routes end on an ID column (e.g. an Entrez "GENE" column)
#' instead of symbols. When most row IDs are still such IDs, map them with
#' org.Hs.eg.db; IDs that do not map are dropped (NA).
#' @return list(ids, n, type) - type is NULL when no conversion was needed
#' @keywords internal
.gexpipe_ids_to_symbols_final <- function(ids) {
  ids <- as.character(ids)
  smp <- .gexpipe_stratified_id_sample(ids[!is.na(ids)], 500L)
  if (!length(smp)) return(list(ids = ids, n = 0L, type = NULL))
  type <- if (mean(grepl("^[0-9]+$", smp)) > 0.5) "ENTREZID"
          else if (mean(grepl("^ENSG[0-9]+([.][0-9]+)?$", smp)) > 0.5) "ENSEMBL"
          else if (mean(grepl("^[NX][MR]_[0-9]+([.][0-9]+)?$", smp)) > 0.5) "REFSEQ"
          else if (mean(grepl("^[A-Z]{1,2}[0-9]{5,8}([.][0-9]+)?$", smp)) > 0.5) "ACCNUM"
          else NULL
  if (is.null(type)) return(list(ids = ids, n = 0L, type = NULL))
  db <- tryCatch(.gexpipe_hs_db(), error = function(e) NULL)
  if (is.null(db)) return(list(ids = ids, n = 0L, type = NULL))
  keys <- if (type == "ENTREZID") ids else sub("[.][0-9]+$", "", ids)
  sym <- tryCatch(as.character(.gexpipe_map_ids(db, keys = keys, column = "SYMBOL", keytype = type,
                                                multiVals = "first")), error = function(e) NULL)
  if (is.null(sym) || sum(!is.na(sym)) < max(3L, 0.05 * length(ids))) return(list(ids = ids, n = 0L, type = NULL))
  list(ids = sym, n = sum(!is.na(sym)), type = type)
}

#' TRUE when most IDs are official human symbols (harmonisation is human-only)
#' @keywords internal
.gexpipe_mostly_human_symbols <- function(ids, min_rate = 0.3) {
  db <- tryCatch(.gexpipe_hs_db(), error = function(e) NULL)
  if (is.null(db)) return(FALSE)
  if (is.null(.gexpipe_symbol_env$official)) {
    .gexpipe_symbol_env$official <- AnnotationDbi::keys(db, keytype = "SYMBOL")
  }
  smp <- .gexpipe_stratified_id_sample(as.character(ids[!is.na(ids)]), 1000L)
  length(smp) > 0L && mean(smp %in% .gexpipe_symbol_env$official) >= min_rate
}
