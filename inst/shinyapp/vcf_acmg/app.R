# VCF Annotation & ACMG Classification App
# Annotation: VariantAnnotation (Bioconductor) + MyVariant.info REST API (httr2)
# No Python, reticulate, InterVar, or myvariant package required.
#
# Required packages:
#   Bioconductor: BiocManager::install(c("VariantAnnotation", "GenomicRanges"))
#   CRAN:         install.packages(c("shiny","bslib","DT","dplyr","httr2","jsonlite"))

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(dplyr)
  library(VariantAnnotation)
  library(GenomicRanges)
  library(httr2)
  library(jsonlite)
})

# ── MyVariant.info REST client ────────────────────────────────────────────────
# Batches variant IDs in chunks of 1000 (hard API limit).
# Returns a flat data.frame with one row per queried ID.

query_myvariant_api <- function(ids, fields) {
  if (length(ids) == 0) {
    return(data.frame(
      query_id = character(0), Gene = character(0),
      Consequence = character(0), REVEL = numeric(0),
      AF = numeric(0), stringsAsFactors = FALSE
    ))
  }

  field_str <- paste(fields, collapse = ",")
  chunks    <- split(ids, ceiling(seq_along(ids) / 1000))

  safe_field <- function(x, path) {
    # Drill into a nested list/df by dot-separated path
    parts <- strsplit(path, "\\.")[[1]]
    val   <- x
    for (p in parts) {
      if (is.null(val))             return(NA)
      if (is.data.frame(val))      val <- tryCatch(val[[p]], error = function(e) NA)
      else if (is.list(val))       val <- tryCatch(val[[p]], error = function(e) NA)
      else                          return(NA)
      if (is.null(val) || length(val) == 0) return(NA)
    }
    if (length(val) > 1) val <- val[[1]]
    if (is.null(val) || length(val) == 0) NA else val
  }

  parse_chunk <- function(hits) {
    lapply(hits, function(h) {
      data.frame(
        query_id    = if (!is.null(h[["query"]])) h[["query"]] else NA_character_,
        Gene        = {
          v <- safe_field(h, "dbnsfp.genename")
          if (is.na(v)) NA_character_ else as.character(v[[1]])
        },
        Consequence = {
          v <- safe_field(h, "dbnsfp.consequence")
          if (is.na(v)) NA_character_ else as.character(v[[1]])
        },
        REVEL       = suppressWarnings(as.numeric(safe_field(h, "dbnsfp.revel.score"))),
        AF          = suppressWarnings(as.numeric(safe_field(h, "gnomad_exome.af.af"))),
        stringsAsFactors = FALSE
      )
    })
  }

  all_rows <- lapply(chunks, function(chunk) {
    tryCatch({
      resp <- httr2::request("https://myvariant.info/v1/variant") |>
        httr2::req_method("POST") |>
        httr2::req_body_form(
          ids    = paste(chunk, collapse = ","),
          fields = field_str
        ) |>
        httr2::req_timeout(60) |>
        httr2::req_retry(max_tries = 3, backoff = ~ 2) |>
        httr2::req_perform()

      hits <- httr2::resp_body_json(resp, simplifyVector = FALSE)
      if (length(hits) == 0) return(NULL)
      dplyr::bind_rows(parse_chunk(hits))
    }, error = function(e) {
      warning("MyVariant.info query failed for a chunk: ", conditionMessage(e))
      NULL
    })
  })

  dplyr::bind_rows(all_rows)
}

# ── ACMG Scoring Engine ───────────────────────────────────────────────────────

calculate_acmg_tier <- function(row) {
  pvs <- sum(as.logical(c(row[["PVS1"]])), na.rm = TRUE)
  ps  <- sum(as.logical(c(row[["PS1"]], row[["PS4"]])), na.rm = TRUE)
  pm  <- sum(as.logical(c(row[["PM1"]], row[["PM2"]], row[["PM4"]], row[["PM5"]])), na.rm = TRUE)
  pp  <- sum(as.logical(c(row[["PP2"]], row[["PP3"]], row[["PP5"]])), na.rm = TRUE)
  ba  <- sum(as.logical(c(row[["BA1"]])), na.rm = TRUE)
  bs  <- sum(as.logical(c(row[["BS1"]])), na.rm = TRUE)
  bp  <- sum(as.logical(c(row[["BP1"]], row[["BP3"]], row[["BP4"]], row[["BP6"]], row[["BP7"]])), na.rm = TRUE)

  # Benign
  if (ba >= 1)             return("Benign")
  if (bs >= 2)             return("Benign")

  # Likely Benign
  if (bs >= 1 && bp >= 1)  return("Likely Benign")
  if (bp >= 2)             return("Likely Benign")

  # Pathogenic
  if (pvs >= 1 && ps >= 1)              return("Pathogenic")
  if (pvs >= 1 && pm >= 2)              return("Pathogenic")
  if (pvs >= 1 && pm >= 1 && pp >= 1)   return("Pathogenic")
  if (pvs >= 1 && pp >= 2)              return("Pathogenic")
  if (ps >= 2)                          return("Pathogenic")
  if (ps >= 1 && pm >= 3)               return("Pathogenic")
  if (ps >= 1 && pm >= 2 && pp >= 2)    return("Pathogenic")

  # Likely Pathogenic
  if (pvs >= 1 && pm >= 1)              return("Likely Pathogenic")
  if (ps >= 1 && pm >= 1)               return("Likely Pathogenic")
  if (ps >= 1 && pm >= 2)               return("Likely Pathogenic")
  if (ps >= 1 && pp >= 2)               return("Likely Pathogenic")
  if (pm >= 3)                          return("Likely Pathogenic")
  if (pm >= 2 && pp >= 2)               return("Likely Pathogenic")
  if (pm >= 1 && pp >= 4)               return("Likely Pathogenic")

  "VUS"
}

# ── Annotation Pipeline ───────────────────────────────────────────────────────

annotate_vcf <- function(vcf_path, genome_build, set_progress) {

  # 1. Read VCF
  set_progress(0.05, "Reading VCF file...")
  vcf <- tryCatch(
    VariantAnnotation::readVcf(vcf_path, genome = genome_build),
    error = function(e) stop("Failed to read VCF: ", conditionMessage(e))
  )
  if (nrow(vcf) == 0) stop("VCF file contains no variant records.")

  # 2. Extract locus info
  set_progress(0.15, "Extracting variant coordinates...")
  rd  <- rowRanges(vcf)
  ref <- as.character(rd$REF)
  alt <- vapply(rd$ALT, function(x) as.character(x[1L]), character(1L))

  base_df <- data.frame(
    CHROM = as.character(seqnames(rd)),
    POS   = start(rd),
    REF   = ref,
    ALT   = alt,
    stringsAsFactors = FALSE
  )

  # 3. Build HGVS genomic IDs (SNPs only for API)
  set_progress(0.25, "Building variant query IDs...")
  base_df$query_id <- paste0(
    "chr", sub("^chr", "", base_df$CHROM), ":g.",
    base_df$POS, base_df$REF, ">", base_df$ALT
  )
  snp_mask <- nchar(base_df$REF) == 1L & nchar(base_df$ALT) == 1L
  snp_ids  <- base_df$query_id[snp_mask]

  # 4. Query MyVariant.info REST API
  set_progress(0.35, paste0("Querying MyVariant.info for ", length(snp_ids), " SNPs..."))
  mv_fields <- c(
    "dbnsfp.genename",
    "dbnsfp.consequence",
    "dbnsfp.revel.score",
    "gnomad_exome.af.af"
  )
  mv_df <- query_myvariant_api(snp_ids, mv_fields)

  # 5. Merge annotation back to full variant set
  set_progress(0.60, "Merging annotation data...")
  if (!is.null(mv_df) && nrow(mv_df) > 0) {
    annotated <- dplyr::left_join(base_df, mv_df, by = "query_id")
  } else {
    annotated <- base_df
    annotated$Gene        <- NA_character_
    annotated$Consequence <- NA_character_
    annotated$REVEL       <- NA_real_
    annotated$AF          <- NA_real_
  }
  annotated$query_id <- NULL

  # Coerce, impute
  annotated$AF    <- suppressWarnings(as.numeric(annotated$AF))
  annotated$REVEL <- suppressWarnings(as.numeric(annotated$REVEL))
  annotated$AF[is.na(annotated$AF)]                         <- 0
  annotated$Gene[is.na(annotated$Gene)]                     <- "Unknown"
  annotated$Consequence[is.na(annotated$Consequence)]       <- "unknown"

  # 6. 18 ACMG criteria flags
  set_progress(0.75, "Calculating 18 ACMG criteria flags...")
  csq_has <- function(csq, pats) grepl(paste(pats, collapse = "|"), csq, ignore.case = TRUE)

  annotated <- annotated |>
    dplyr::mutate(
      # Population frequency
      BA1  = AF > 0.05,
      BS1  = AF > 0.01 & AF <= 0.05,
      PM2  = AF < 0.0001 | AF == 0,

      # Functional consequence
      PVS1 = csq_has(Consequence, c("frameshift", "stop_gained",
                                     "splice_acceptor_variant", "splice_donor_variant")),
      PM4  = csq_has(Consequence, c("inframe_insertion", "inframe_deletion", "stop_lost")),
      BP7  = csq_has(Consequence, c("synonymous_variant")),

      # In-silico (REVEL)
      PP3  = !is.na(REVEL) & REVEL > 0.75,
      BP4  = !is.na(REVEL) & REVEL < 0.25,

      # Placeholders — FALSE until ClinVar / domain DB wired in
      PS1  = FALSE,   # Same AA change as established pathogenic variant (ClinVar)
      PS4  = FALSE,   # Significantly increased prevalence in patients vs. controls
      PM1  = FALSE,   # Located in mutational hot spot or well-established functional domain
      PM5  = FALSE,   # Novel missense at codon with different pathogenic missense
      PP2  = FALSE,   # Missense in gene with low benign missense variant rate
      PP5  = FALSE,   # Reputable source (ClinVar) reports Pathogenic
      BP1  = FALSE,   # Missense in gene where only LOF variants cause disease
      BP3  = FALSE,   # In-frame indel in repeat region without known function
      BP6  = FALSE    # Reputable source (ClinVar) reports Benign
    )

  # 7. ACMG classification
  set_progress(0.90, "Running ACMG combining algorithm...")
  annotated$Baseline_Classification <- apply(annotated, 1, calculate_acmg_tier)

  set_progress(1.0, "Done.")
  annotated
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- bslib::page_sidebar(
  title = "VCF Annotator & ACMG Classifier",
  theme = bslib::bs_theme(bootswatch = "flatly", version = 5),

  sidebar = bslib::sidebar(
    width = 290,

    fileInput(
      inputId  = "vcf_file",
      label    = "Upload Raw VCF File",
      accept   = c(".vcf", ".vcf.gz"),
      multiple = FALSE
    ),

    selectInput(
      inputId  = "genome_build",
      label    = "Reference Genome Build",
      choices  = c("hg38", "hg19"),
      selected = "hg38"
    ),

    actionButton(
      inputId = "run_btn",
      label   = "Analyze & Annotate",
      icon    = icon("dna"),
      class   = "btn-primary w-100 mt-2"
    ),

    hr(),

    downloadButton(
      outputId = "download_btn",
      label    = "Export Full Results (CSV)",
      class    = "btn-outline-secondary w-100"
    ),

    hr(),

    uiOutput("summary_ui")
  ),

  bslib::card(
    bslib::card_header("ACMG Variant Classification Results"),
    DT::dataTableOutput("results_table"),
    full_screen = TRUE
  )
)

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  rv <- reactiveValues(annotated = NULL)

  observeEvent(input$run_btn, {
    req(input$vcf_file)
    rv$annotated <- NULL

    withProgress(message = "Initializing...", value = 0, {
      tryCatch({
        rv$annotated <- annotate_vcf(
          vcf_path     = input$vcf_file$datapath,
          genome_build = input$genome_build,
          set_progress = function(v, msg) setProgress(value = v, message = msg)
        )
      }, error = function(e) {
        showNotification(
          paste("Annotation failed:", conditionMessage(e)),
          type = "error", duration = NULL
        )
      })
    })
  })

  # ── Summary pill counts ──────────────────────────────────────────────────
  output$summary_ui <- renderUI({
    req(rv$annotated)
    tbl <- table(rv$annotated$Baseline_Classification)
    tier_colors <- c(
      Pathogenic          = "danger",
      "Likely Pathogenic" = "warning",
      VUS                 = "secondary",
      "Likely Benign"     = "success",
      Benign              = "success"
    )
    tags$div(
      tags$strong("Classification summary:"),
      tags$br(),
      lapply(names(tbl), function(t) {
        col <- if (!is.na(tier_colors[t])) tier_colors[t] else "secondary"
        tags$span(
          class = paste0("badge bg-", col, " me-1 mb-1"),
          paste0(t, ": ", tbl[[t]])
        )
      })
    )
  })

  # ── Display columns (hidden: 18 ACMG flags) ──────────────────────────────
  display_cols <- c("CHROM","POS","REF","ALT","Gene","Consequence",
                    "AF","REVEL","Baseline_Classification")

  output$results_table <- DT::renderDataTable({
    req(rv$annotated)
    df      <- rv$annotated
    present <- intersect(display_cols, names(df))
    df_disp <- df[, present, drop = FALSE]

    DT::datatable(
      df_disp,
      rownames   = FALSE,
      selection  = "single",
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        dom        = "Bfrtip",
        buttons    = list("colvis", "copy"),
        pageLength = 25,
        scrollX    = TRUE,
        autoWidth  = TRUE
      )
    ) |>
      DT::formatRound(
        columns = intersect(c("AF", "REVEL"), present),
        digits  = 6
      ) |>
      DT::formatStyle(
        columns         = "Baseline_Classification",
        backgroundColor = DT::styleEqual(
          c("Pathogenic","Likely Pathogenic","VUS","Likely Benign","Benign"),
          c("#c0392b",   "#e67e22",          "#f1c40f","#82e0aa",  "#27ae60")
        ),
        color = DT::styleEqual(
          c("Pathogenic","Likely Pathogenic","VUS","Likely Benign","Benign"),
          c("white",     "white",            "black","black",      "white")
        ),
        fontWeight = "bold"
      )
  })

  # ── CSV export (full data including boolean flags) ───────────────────────
  output$download_btn <- downloadHandler(
    filename = function() paste0("acmg_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content  = function(file) {
      req(rv$annotated)
      write.csv(rv$annotated, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
