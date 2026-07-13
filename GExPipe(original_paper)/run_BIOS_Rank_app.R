#!/usr/bin/env Rscript
## BIOS-Rank one-click app — works on ANY GEO series with a binary phenotype
##
## The METHOD is always the same. Only sample labels differ across GSE files.
## Workflow: Load GSE → choose phenotype column → map levels to Disease/Normal
##           → Run BIOS → download top-k
##
## Windows: double-click run_BIOS_Rank_app.bat
## Or:      Rscript run_BIOS_Rank_app.R

## ---- bootstrap -------------------------------------------------------------
need_cran <- c("shiny", "DT")
need_bioc <- c("GEOquery", "Biobase", "limma")
inst <- rownames(installed.packages())
miss_cran <- setdiff(need_cran, inst)
if (length(miss_cran)) {
  message("Installing CRAN: ", paste(miss_cran, collapse = ", "))
  install.packages(miss_cran, repos = "https://cloud.r-project.org")
}
miss_bioc <- setdiff(need_bioc, rownames(installed.packages()))
if (length(miss_bioc)) {
  message("Installing Bioconductor: ", paste(miss_bioc, collapse = ", "))
  if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
  BiocManager::install(miss_bioc, update = FALSE, ask = FALSE)
}

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(GEOquery)
  library(Biobase)
  library(limma)
})

scale01 <- function(x) {
  x <- as.numeric(x)
  r <- range(x, na.rm = TRUE)
  if (!all(is.finite(r)) || diff(r) < .Machine$double.eps) return(rep(0, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

guess_role <- function(level) {
  x <- tolower(as.character(level))
  if (grepl("normal|healthy|control|nontumor|non-tumor|adjacent|unaffected", x)) return("Normal")
  if (grepl("tumor|tumour|cancer|carcinoma|adenocarcinoma|disease|case|patient|crc|tumor", x)) return("Disease")
  "Skip"
}

## ---- UI --------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: Segoe UI, sans-serif; }
    .help-box { background:#f6f8fa; border:1px solid #d0d7de; padding:12px; border-radius:8px; margin-bottom:12px; }
  "))),
  titlePanel("BIOS-Rank — apply to any GEO dataset"),
  div(class = "help-box",
      strong("One method for every dataset."),
      " BIOS-Rank always does: limma Disease vs Normal → multi-evidence score → top-k genes.",
      " Paper examples used CRC GSEs only for validation — the algorithm is not limited to those IDs.",
      " For a new GSE you only need to tell the app which samples are Disease vs Normal."
  ),
  sidebarLayout(
    sidebarPanel(
      h4("1. Load GEO"),
      textInput("gse", "GEO accession (any GSE*)", value = "GSE9348"),
      actionButton("load", "Load dataset", class = "btn-info", width = "100%"),
      br(), br(),
      h4("2. Map phenotype (required)"),
      uiOutput("pheno_col_ui"),
      uiOutput("level_map_ui"),
      hr(),
      h4("3. Run BIOS"),
      numericInput("k", "Top-k genes", value = 20, min = 5, max = 200),
      actionButton("run", "Run BIOS-Rank", class = "btn-primary btn-lg", width = "100%"),
      br(), br(),
      verbatimTextOutput("status"),
      downloadButton("dl_top", "Download top-k CSV"),
      downloadButton("dl_all", "Download all genes CSV")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top BIOS genes", br(), DTOutput("tbl")),
        tabPanel("Sample labels", br(),
                 helpText("Check that Disease/Normal look correct before running."),
                 DTOutput("samp_tbl")),
        tabPanel("How it works", br(),
                 tags$pre(
"GENERAL ALGORITHM (same for every dataset)
------------------------------------------
1. Expression matrix from GEO (any platform / disease)
2. You map sample labels → Disease vs Normal
3. limma: ~ Condition
4. BIOS score = mean(Ec, Ep, Em, Es)
     Ec = disease strength from DE
     Ep = 1 on single-platform data
     Em/Es = programme/stability proxies
5. Return top-k disease-linked genes

k is your choice (10, 20, 50, ...).
For full cross-platform Ep + WGCNA/ML, merge two assays
with the paper scripts; this app is the universal single-GSE entry point."
                 ))
      )
    )
  )
)

## ---- server ----------------------------------------------------------------
server <- function(input, output, session) {
  geo <- reactiveVal(NULL)      # list(expr_raw eset-like pieces)
  res <- reactiveVal(NULL)
  destdir <- file.path(tempdir(), "GEO_BIOS_app")
  dir.create(destdir, showWarnings = FALSE, recursive = TRUE)

  observeEvent(input$load, {
    res(NULL)
    gse_id <- toupper(trimws(input$gse))
    withProgress(message = paste("Loading", gse_id), value = 0.2, {
      out <- tryCatch({
        gset <- getGEO(gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = destdir)
        if (length(gset) > 1L) {
          ns <- vapply(gset, ncol, integer(1))
          gset <- gset[[which.max(ns)]]
        } else gset <- gset[[1]]
        pdata <- as.data.frame(pData(gset), stringsAsFactors = FALSE)
        ## candidate phenotype columns
        cand <- unique(c(
          intersect(c("title", "source_name_ch1", "characteristics_ch1"), names(pdata)),
          grep("characteristics|disease|group|status|tissue|diagnosis|type",
               names(pdata), value = TRUE, ignore.case = TRUE)
        ))
        if (!length(cand)) cand <- names(pdata)[seq_len(min(5L, ncol(pdata)))]
        list(gse_id = gse_id, eset = gset, pdata = pdata, cand = cand)
      }, error = function(e) e)

      if (inherits(out, "error")) {
        showNotification(conditionMessage(out), type = "error", duration = 12)
        geo(NULL)
      } else {
        geo(out)
        showNotification(paste("Loaded", out$gse_id, "with", ncol(out$eset), "samples"),
                         type = "message")
      }
    })
  })

  output$pheno_col_ui <- renderUI({
    g <- geo(); req(g)
    selectInput("pheno_col", "Phenotype column in GEO metadata",
                choices = g$cand, selected = g$cand[1])
  })

  levels_in_col <- reactive({
    g <- geo(); req(g); req(input$pheno_col)
    col <- input$pheno_col
    if (!col %in% names(g$pdata)) return(character())
    unique(trimws(as.character(g$pdata[[col]])))
  })

  output$level_map_ui <- renderUI({
    levs <- levels_in_col()
    req(length(levs) > 0)
    ## limit UI size
    if (length(levs) > 40) {
      return(tagList(
        helpText("Too many unique labels in this column (", length(levs),
                 "). Pick a cleaner characteristics column, or use title with fewer patterns."),
        selectInput("pheno_col2_hint", NULL, choices = geo()$cand)
      ))
    }
    lapply(seq_along(levs), function(i) {
      lv <- levs[i]
      selectInput(
        inputId = paste0("map_", i),
        label = paste0("Label: ", substr(lv, 1, 80)),
        choices = c("Disease", "Normal", "Skip"),
        selected = guess_role(lv)
      )
    })
  })

  mapped_condition <- reactive({
    g <- geo(); req(g); req(input$pheno_col)
    levs <- levels_in_col()
    req(length(levs) > 0, length(levs) <= 40)
    raw <- trimws(as.character(g$pdata[[input$pheno_col]]))
    cond <- rep(NA_character_, length(raw))
    for (i in seq_along(levs)) {
      role <- input[[paste0("map_", i)]]
      if (is.null(role) || role == "Skip") next
      cond[raw == levs[i]] <- role
    }
    cond
  })

  output$samp_tbl <- renderDT({
    g <- geo(); req(g)
    cond <- tryCatch(mapped_condition(), error = function(e) NULL)
    df <- data.frame(
      Sample = rownames(g$pdata),
      Phenotype_raw = if (!is.null(input$pheno_col) && input$pheno_col %in% names(g$pdata))
        as.character(g$pdata[[input$pheno_col]]) else NA_character_,
      Mapped = if (is.null(cond)) NA_character_ else cond,
      stringsAsFactors = FALSE
    )
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })

  observeEvent(input$run, {
    g <- geo()
    if (is.null(g)) {
      showNotification("Load a GEO dataset first.", type = "warning")
      return()
    }
    cond <- tryCatch(mapped_condition(), error = function(e) NULL)
    if (is.null(cond) || length(unique(na.omit(cond))) < 2L) {
      showNotification("Map at least one label to Disease and one to Normal.", type = "error")
      return()
    }

    withProgress(message = "Running BIOS-Rank", value = 0.3, {
      out <- tryCatch({
        eset <- g$eset
        expr <- exprs(eset)
        ok <- !is.na(cond)
        expr <- expr[, ok, drop = FALSE]
        condf <- factor(cond[ok], levels = c("Normal", "Disease"))
        if (nlevels(condf) < 2L || any(table(condf) == 0))
          stop("Need ≥1 Normal and ≥1 Disease after mapping.")

        fd <- fData(eset)
        sym_col <- grep("symbol|gene.?symbol|gene_assignment", names(fd),
                        ignore.case = TRUE, value = TRUE)
        if (length(sym_col)) {
          syms <- as.character(fd[rownames(expr), sym_col[1]])
          syms <- trimws(sub("\\s*//.*$", "", syms))
          keep <- !is.na(syms) & nzchar(syms) & syms != "---"
          expr <- expr[keep, , drop = FALSE]
          syms <- syms[keep]
          if (anyDuplicated(syms)) expr <- limma::avereps(expr, ID = syms)
          else rownames(expr) <- syms
        }
        if (max(expr, na.rm = TRUE) > 100) expr <- log2(expr + 1)

        setProgress(0.7, detail = "limma + BIOS")
        design <- model.matrix(~ condf)
        colnames(design) <- c("Intercept", "ConditionDisease")
        fit <- eBayes(lmFit(expr, design))
        tt <- topTable(fit, coef = "ConditionDisease", number = Inf, sort.by = "none")
        genes <- rownames(tt)
        beta_c <- abs(tt$logFC)
        padj <- tt$adj.P.Val
        Ec <- scale01((-log10(pmax(padj, 1e-300))) * beta_c)
        Ep <- rep(1, length(genes))
        sig <- padj < 0.05
        strong <- padj < 0.01 & beta_c > 1
        Em <- as.numeric(sig & beta_c >= stats::quantile(beta_c[sig], 0.8, na.rm = TRUE))
        Es <- ifelse(strong, 1, ifelse(sig, 0.33, 0))
        BIOS <- (Ec + Ep + Em + Es) / 4
        tab <- data.frame(
          Gene = genes, logFC = tt$logFC, adj.P.Val = padj,
          Ec = Ec, Ep = Ep, Em = Em, Es = Es, BIOS_Rank = as.numeric(BIOS),
          stringsAsFactors = FALSE
        )
        tab <- tab[order(-tab$BIOS_Rank, tab$adj.P.Val), ]
        list(
          all = tab,
          top = head(tab, as.integer(input$k)),
          summary = sprintf(
            "%s | n=%d (Disease=%d, Normal=%d) | BIOS top-%d",
            g$gse_id, length(condf), sum(condf == "Disease"), sum(condf == "Normal"),
            as.integer(input$k)
          )
        )
      }, error = function(e) e)

      if (inherits(out, "error")) {
        showNotification(conditionMessage(out), type = "error", duration = 12)
        res(NULL)
      } else {
        res(out)
        showNotification(out$summary, type = "message")
      }
    })
  })

  output$status <- renderText({
    r <- res()
    if (is.null(geo())) return("Step 1: Load any GSE accession.")
    if (is.null(r)) return("Step 2–3: Map Disease/Normal, then Run BIOS-Rank.")
    r$summary
  })

  output$tbl <- renderDT({
    r <- res(); req(r)
    datatable(r$top, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  output$dl_top <- downloadHandler(
    filename = function() {
      g <- geo(); paste0(if (is.null(g)) "BIOS" else g$gse_id, "_BIOS_top", input$k, ".csv")
    },
    content = function(file) {
      r <- res(); req(r)
      utils::write.csv(r$top, file, row.names = FALSE)
    }
  )
  output$dl_all <- downloadHandler(
    filename = function() {
      g <- geo(); paste0(if (is.null(g)) "BIOS" else g$gse_id, "_BIOS_all_genes.csv")
    },
    content = function(file) {
      r <- res(); req(r)
      utils::write.csv(r$all, file, row.names = FALSE)
    }
  )
}

message("Starting BIOS-Rank universal GEO app…")
options(shiny.launch.browser = TRUE)
shinyApp(ui, server, options = list(launch.browser = TRUE))
)
