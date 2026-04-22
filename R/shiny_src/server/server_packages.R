# ==============================================================================
# SERVER_PACKAGES.R  – Package Status Dashboard
# Checks every GExPipe dependency at startup, and on user request.
# ==============================================================================

server_packages <- function(input, output, session, rv) {

  # ── Package metadata table ─────────────────────────────────────────────────
  # category: "Required" | "Core" | "Optional"
  # step:     free-text label shown in the "Used For" column
  .pkg_meta <- list(
    # ── Required (Shiny UI stack) ──────────────────────────────────────────
    list(pkg = "shiny",          cat = "Required", step = "App framework — core"),
    list(pkg = "shinydashboard", cat = "Required", step = "Dashboard layout"),
    list(pkg = "shinyjs",        cat = "Required", step = "UI helpers (disable / enable buttons)"),
    list(pkg = "DT",             cat = "Required", step = "Interactive data tables"),
    # ── Core analysis packages ─────────────────────────────────────────────
    list(pkg = "GEOquery",       cat = "Core", step = "Step 1 · Download GEO datasets"),
    list(pkg = "Biobase",        cat = "Core", step = "Step 1 · ExpressionSet handling"),
    list(pkg = "data.table",     cat = "Core", step = "Step 1 · Fast file reading"),
    list(pkg = "R.utils",        cat = "Core", step = "Step 1 · .gz decompression"),
    list(pkg = "AnnotationDbi",  cat = "Core", step = "Step 1–2 · Gene ID mapping"),
    list(pkg = "org.Hs.eg.db",   cat = "Core", step = "Step 1–2 · Entrez → symbol conversion"),
    list(pkg = "limma",          cat = "Core", step = "Step 3 · Normalize | Step 6 · DE (microarray)"),
    list(pkg = "DESeq2",         cat = "Core", step = "Step 6 · DE Analysis (count data)"),
    list(pkg = "edgeR",          cat = "Core", step = "Step 6 · DE Analysis (RNA-seq)"),
    list(pkg = "sva",            cat = "Core", step = "Step 5 · Batch correction (ComBat)"),
    list(pkg = "WGCNA",          cat = "Core", step = "Step 7 · Co-expression network"),
    list(pkg = "dynamicTreeCut", cat = "Core", step = "Step 7 · WGCNA module detection"),
    list(pkg = "clusterProfiler",cat = "Core", step = "Step 8 · GO / KEGG enrichment"),
    list(pkg = "enrichplot",     cat = "Core", step = "Step 8 · Enrichment visualizations"),
    list(pkg = "msigdbr",        cat = "Core", step = "Step 14 · GSEA gene set database"),
    list(pkg = "STRINGdb",       cat = "Core", step = "Step 9 · PPI interaction network"),
    list(pkg = "igraph",         cat = "Core", step = "Step 9 · PPI graph analysis"),
    list(pkg = "ggraph",         cat = "Core", step = "Step 9 · PPI network visualization"),
    list(pkg = "tidygraph",      cat = "Core", step = "Step 9 · Tidy graph operations"),
    list(pkg = "randomForest",   cat = "Core", step = "Step 10 · ML — Random Forest"),
    list(pkg = "caret",          cat = "Core", step = "Step 10 · ML — model training wrapper"),
    list(pkg = "e1071",          cat = "Core", step = "Step 10 · ML — SVM"),
    list(pkg = "glmnet",         cat = "Core", step = "Step 10 · ML — LASSO / Ridge"),
    list(pkg = "kernlab",        cat = "Core", step = "Step 10 · ML — SVM-RFE"),
    list(pkg = "pROC",           cat = "Core", step = "Step 12 · ROC curve analysis"),
    list(pkg = "ggplot2",        cat = "Core", step = "All steps · visualizations"),
    list(pkg = "ggpubr",         cat = "Core", step = "Publication-ready figures"),
    list(pkg = "ggrepel",        cat = "Core", step = "Step 6 · Volcano plot labels"),
    list(pkg = "pheatmap",       cat = "Core", step = "QC / DE / WGCNA heatmaps"),
    list(pkg = "RColorBrewer",   cat = "Core", step = "Color palettes for plots"),
    list(pkg = "gridExtra",      cat = "Core", step = "Multi-panel plot layouts"),
    list(pkg = "circlize",       cat = "Core", step = "Step 7 · WGCNA circular plots"),
    list(pkg = "VennDiagram",    cat = "Core", step = "Step 8 · Common gene Venn diagram"),
    list(pkg = "UpSetR",         cat = "Core", step = "Step 8 · UpSet intersection plot"),
    list(pkg = "dplyr",          cat = "Core", step = "Data manipulation throughout"),
    list(pkg = "tidyr",          cat = "Core", step = "Data reshaping"),
    list(pkg = "tibble",         cat = "Core", step = "Tidy data frames"),
    list(pkg = "reshape2",       cat = "Core", step = "Data reshaping"),
    list(pkg = "corrplot",       cat = "Core", step = "Correlation matrix plots"),
    list(pkg = "scales",         cat = "Core", step = "Plot axis scale helpers"),
    # ── Optional packages ──────────────────────────────────────────────────
    list(pkg = "cicerone",       cat = "Optional", step = "Guided tour (User Guideline button)"),
    list(pkg = "biomaRt",        cat = "Optional", step = "Step 1–2 · Ensembl gene ID mapping"),
    list(pkg = "Boruta",         cat = "Optional", step = "Step 10 · ML — Boruta feature selection"),
    list(pkg = "mixOmics",       cat = "Optional", step = "Step 10 · ML — sPLS-DA"),
    list(pkg = "xgboost",        cat = "Optional", step = "Step 10 · ML — XGBoost"),
    list(pkg = "SHAPforxgboost", cat = "Optional", step = "Step 10 · XGBoost SHAP importance"),
    list(pkg = "rms",            cat = "Optional", step = "Step 13 · Nomogram model (rms::lrm)"),
    list(pkg = "rmda",           cat = "Optional", step = "Step 13 · Decision curve analysis")
  )

  # ── Helper: check one package, return a named list ─────────────────────────
  .check_one <- function(meta) {
    pkg <- meta$pkg
    ok  <- requireNamespace(pkg, quietly = TRUE)
    ver <- if (ok) {
      tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "?")
    } else {
      NA_character_
    }
    list(
      Package  = pkg,
      Category = meta$cat,
      Used_For = meta$step,
      Installed = ok,
      Version  = ver
    )
  }

  # ── Reactive: full check result (data frame) ───────────────────────────────
  rv_pkg <- shiny::reactiveValues(
    df       = NULL,
    checking = FALSE,
    last_checked = NULL
  )

  # ── Run check immediately when this module loads ───────────────────────────
  shiny::observe({
    if (is.null(rv_pkg$df)) {
      rows <- lapply(.pkg_meta, .check_one)
      rv_pkg$df <- do.call(rbind.data.frame, c(lapply(rows, as.data.frame), list(stringsAsFactors = FALSE)))
      rv_pkg$last_checked <- format(Sys.time(), "%H:%M:%S")
    }
  })

  # ── Re-check button ────────────────────────────────────────────────────────
  shiny::observeEvent(input$pkg_recheck_btn, {
    rv_pkg$checking <- TRUE
    shinyjs::disable("pkg_recheck_btn")
    shinyjs::disable("pkg_install_btn")

    rows <- lapply(.pkg_meta, .check_one)
    rv_pkg$df <- do.call(rbind.data.frame, c(lapply(rows, as.data.frame), list(stringsAsFactors = FALSE)))
    rv_pkg$last_checked <- format(Sys.time(), "%H:%M:%S")
    rv_pkg$checking <- FALSE

    shinyjs::enable("pkg_recheck_btn")
    shinyjs::enable("pkg_install_btn")

    shiny::showNotification(
      shiny::tagList(shiny::icon("check-circle"), " Package check complete."),
      type = "message", duration = 3
    )
  })

  # ── Install missing button ─────────────────────────────────────────────────
  shiny::observeEvent(input$pkg_install_btn, {
    df <- rv_pkg$df
    if (is.null(df)) return()

    miss_req <- df$Package[!df$Installed & df$Category %in% c("Required", "Core")]
    miss_opt <- df$Package[!df$Installed & df$Category == "Optional"]
    all_miss  <- c(miss_req, miss_opt)

    if (length(all_miss) == 0) {
      shiny::showNotification(
        shiny::tagList(shiny::icon("check-circle"), " All packages are already installed!"),
        type = "message", duration = 4
      )
      return()
    }

    shinyjs::disable("pkg_install_btn")
    shinyjs::disable("pkg_recheck_btn")

    shiny::showNotification(
      shiny::tagList(
        shiny::icon("spinner", class = "fa-spin"),
        shiny::tags$strong(paste0(" Installing ", length(all_miss), " package(s)...")),
        shiny::tags$p(
          "This runs in a background process. The R session will be busy — do not close.",
          style = "font-size:12px; margin:4px 0 0 0; opacity:.85;"
        )
      ),
      type = "message", duration = NULL, id = "pkg_install_notif"
    )

    tryCatch(
      {
        # .gexpipe_batch_install exists in two variants:
        #   utils_shiny_app.R  ->  .gexpipe_batch_install(pkgs)
        #   global.R           ->  .gexpipe_batch_install(pkgs, label)
        # Detect which is in scope and call accordingly.
        fn <- tryCatch(match.fun(".gexpipe_batch_install"), error = function(e) NULL)
        if (!is.null(fn)) {
          if (length(formals(fn)) >= 2) {
            fn(all_miss, "Packages tab install")
          } else {
            fn(all_miss)
          }
        } else {
          stop("Install function not available. Run: BiocManager::install(c(",
               paste0('"', all_miss, '"', collapse = ", "), "))")
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Install error:", conditionMessage(e)),
          type = "error", duration = 8
        )
      }
    )

    shiny::removeNotification("pkg_install_notif")

    # Re-check after install
    rows <- lapply(.pkg_meta, .check_one)
    rv_pkg$df <- do.call(rbind.data.frame, c(lapply(rows, as.data.frame), list(stringsAsFactors = FALSE)))
    rv_pkg$last_checked <- format(Sys.time(), "%H:%M:%S")

    still_miss <- sum(!rv_pkg$df$Installed & rv_pkg$df$Category %in% c("Required", "Core"))
    if (still_miss == 0) {
      shiny::showNotification(
        shiny::tagList(shiny::icon("check-circle"), " All required packages installed successfully!"),
        type = "message", duration = 5
      )
    } else {
      shiny::showNotification(
        shiny::tagList(
          shiny::icon("exclamation-triangle"),
          paste0(" ", still_miss, " required package(s) still missing. Check internet connection and try again.")
        ),
        type = "warning", duration = 8
      )
    }

    shinyjs::enable("pkg_install_btn")
    shinyjs::enable("pkg_recheck_btn")
  })

  # ── Helper: build HTML badge strings ──────────────────────────────────────
  .cat_badge <- function(cat) {
    cls <- switch(cat,
      "Required" = "cat-req",
      "Core"     = "cat-core",
      "Optional" = "cat-opt",
      "cat-core"
    )
    paste0('<span class="', cls, '">', cat, '</span>')
  }

  .status_badge <- function(installed, cat) {
    if (installed) {
      '<span class="pkg-ok">&#10003;&nbsp;Installed</span>'
    } else if (cat == "Optional") {
      '<span class="pkg-opt">&#9888;&nbsp;Missing&nbsp;(optional)</span>'
    } else {
      '<span class="pkg-miss">&#10007;&nbsp;Missing&nbsp;(required)</span>'
    }
  }

  # ── Summary stat cards ─────────────────────────────────────────────────────
  .stat_card <- function(num, lbl, cls) {
    shiny::tags$div(
      class = paste("pkg-stat-card", cls),
      style = "margin-bottom:18px;",
      shiny::tags$div(class = "stat-num", num),
      shiny::tags$div(class = "stat-lbl", lbl)
    )
  }

  output$pkg_stat_total <- shiny::renderUI({
    df <- rv_pkg$df
    n  <- if (is.null(df)) "…" else nrow(df)
    .stat_card(n, "Total Packages", "total")
  })

  output$pkg_stat_ok <- shiny::renderUI({
    df <- rv_pkg$df
    n  <- if (is.null(df)) "…" else sum(df$Installed)
    .stat_card(n, "Installed ✓", "ok")
  })

  output$pkg_stat_miss_req <- shiny::renderUI({
    df <- rv_pkg$df
    n  <- if (is.null(df)) "…" else sum(!df$Installed & df$Category %in% c("Required", "Core"))
    .stat_card(n, "Required Missing", "miss-req")
  })

  output$pkg_stat_miss_opt <- shiny::renderUI({
    df <- rv_pkg$df
    n  <- if (is.null(df)) "…" else sum(!df$Installed & df$Category == "Optional")
    .stat_card(n, "Optional Missing", "miss-opt")
  })

  # ── Action status line ─────────────────────────────────────────────────────
  output$pkg_action_status <- shiny::renderUI({
    t <- rv_pkg$last_checked
    if (is.null(t)) {
      shiny::tags$span(shiny::icon("info-circle"), " Click Re-check to refresh statuses.")
    } else {
      shiny::tags$span(
        shiny::icon("clock"),
        paste0(" Last checked at ", t, ".  Re-check any time or click Install to fix missing packages.")
      )
    }
  })

  # ── Main DT table ──────────────────────────────────────────────────────────
  output$pkg_status_table <- DT::renderDataTable({
    df <- rv_pkg$df
    shiny::req(!is.null(df))

    # Build display columns with HTML
    display <- data.frame(
      Package  = paste0('<code style="font-size:13px;font-weight:700;color:#2c3e50;">', df$Package, '</code>'),
      Category = vapply(df$Category, .cat_badge, character(1)),
      Used_For = df$Used_For,
      Status   = mapply(.status_badge, df$Installed, df$Category, SIMPLIFY = TRUE, USE.NAMES = FALSE),
      Version  = ifelse(
        df$Installed,
        paste0('<span class="pkg-ver">', df$Version, '</span>'),
        '<span style="color:#aaa;font-size:12px;">—</span>'
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
    colnames(display) <- c("Package", "Category", "Used For", "Status", "Version")

    DT::datatable(
      display,
      escape    = FALSE,
      rownames  = FALSE,
      selection = "none",
      options   = list(
        pageLength  = 60,
        dom         = 'Bfrtip',
        buttons     = list(),
        scrollX     = TRUE,
        autoWidth   = FALSE,
        columnDefs  = list(
          list(width = "140px", targets = 0),   # Package
          list(width = "90px",  targets = 1),   # Category
          list(width = "320px", targets = 2),   # Used For
          list(width = "150px", targets = 3),   # Status
          list(width = "80px",  targets = 4)    # Version
        ),
        order = list(list(1, "asc"), list(3, "asc")),  # sort: category then status
        language = list(
          search      = "Filter packages:",
          lengthMenu  = "Show _MENU_ packages",
          info        = "Showing _START_ to _END_ of _TOTAL_ packages"
        )
      ),
      class = "display nowrap stripe hover compact"
    )
  })

  # ── Install hint box (shows only when required packages are missing) ────────
  output$pkg_install_hint_box <- shiny::renderUI({
    df <- rv_pkg$df
    if (is.null(df)) return(NULL)

    miss_req <- df$Package[!df$Installed & df$Category %in% c("Required", "Core")]
    miss_opt <- df$Package[!df$Installed & df$Category == "Optional"]

    if (length(miss_req) == 0 && length(miss_opt) == 0) {
      return(shinydashboard::box(
        width = NULL, status = "success",
        shiny::tags$div(
          style = "text-align:center; padding:14px;",
          shiny::icon("check-circle", style = "color:#27ae60; font-size:28px;"),
          shiny::tags$h4("All packages installed!", style = "color:#1e8449; margin:8px 0 4px 0;"),
          shiny::tags$p("Every dependency is available. The full pipeline is ready to use.",
                        style = "color:#555; margin:0;")
        )
      ))
    }

    rows <- list()

    if (length(miss_req) > 0) {
      cmd_req <- paste0(
        'BiocManager::install(c(\n  ',
        paste0('"', miss_req, '"', collapse = ',\n  '),
        '\n))'
      )
      rows <- c(rows, list(
        shiny::tags$div(
          style = "margin-bottom:16px;",
          shiny::tags$h5(
            shiny::icon("exclamation-circle", style = "color:#e74c3c;"),
            paste0(" Required / Core packages missing (", length(miss_req), "):"),
            style = "color:#922b21; font-weight:700; margin:0 0 6px 0;"
          ),
          shiny::tags$pre(
            style = "background:#fff5f5; border:1.5px solid #f1948a; border-radius:8px;
                     padding:12px 16px; font-size:12.5px; color:#2c3e50; white-space:pre-wrap; word-break:break-all;",
            cmd_req
          ),
          shiny::tags$p(
            shiny::icon("wifi"), " Requires internet access. Run the command above in RStudio Console, or click ",
            shiny::tags$strong("Install / Fix Missing"), " above.",
            style = "font-size:12px; color:#666; margin:4px 0 0 0;"
          )
        )
      ))
    }

    if (length(miss_opt) > 0) {
      cmd_opt <- paste0(
        'BiocManager::install(c(\n  ',
        paste0('"', miss_opt, '"', collapse = ',\n  '),
        '\n))'
      )
      rows <- c(rows, list(
        shiny::tags$div(
          shiny::tags$h5(
            shiny::icon("info-circle", style = "color:#f39c12;"),
            paste0(" Optional packages not installed (", length(miss_opt), "):"),
            style = "color:#9a7d0a; font-weight:700; margin:0 0 6px 0;"
          ),
          shiny::tags$pre(
            style = "background:#fffde7; border:1.5px solid #f9e79f; border-radius:8px;
                     padding:12px 16px; font-size:12.5px; color:#2c3e50; white-space:pre-wrap; word-break:break-all;",
            cmd_opt
          ),
          shiny::tags$p(
            "Optional — the app works without these. Install for extra ML methods, guided tour, nomogram, and Ensembl mapping.",
            style = "font-size:12px; color:#666; margin:4px 0 0 0;"
          )
        )
      ))
    }

    shinydashboard::box(
      width = NULL, status = "warning",
      title = shiny::tagList(shiny::icon("tools"), " Installation Commands"),
      solidHeader = TRUE,
      shiny::tags$div(style = "padding:4px 0;", rows)
    )
  })
}
