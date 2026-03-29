gexp_app_attach_packages <- function() {
  # Called from gexp_app_ui(), gexp_app_server(), and tab/server loaders. Attach once
  # per app; runGExPipe() clears the flags so each new app object gets a fresh attach.
  if (isTRUE(getOption("gexpipe.attach.done", FALSE))) {
    return(invisible(NULL))
  }

  test_minimal <- isTRUE(getOption("shiny.testmode")) &&
    isTRUE(getOption("gexpipe.minimal_attach_in_testmode", TRUE))

  # Phase 1 (test mode only): attach UI stack so Shiny can finish session init quickly.
  # shinytest2 waits on window.shinytest2.ready; a full Bioconductor attach in the same
  # tick blocks the HTTP/WebSocket handshake for many minutes.
  if (test_minimal && !isTRUE(getOption("gexpipe.attach.shiny_stack_only_done", FALSE))) {
    req_shiny <- c("shiny", "shinydashboard", "shinyjs", "DT")
    miss <- character(0)
    for (p in req_shiny) {
      if (!requireNamespace(p, quietly = TRUE)) {
        miss <- c(miss, p)
        next
      }
      tryCatch({
        pkg_search <- paste0("package:", p)
        if (!pkg_search %in% search()) {
          base::attachNamespace(asNamespace(p))
        }
      }, error = function(e) NULL)
    }
    if (length(miss) > 0L) {
      stop(
        "Missing required packages for the Shiny app: ",
        paste(miss, collapse = ", "),
        ". Install them (e.g. via BiocManager::install()) and retry."
      )
    }
    options(gexpipe.attach.shiny_stack_only_done = TRUE)
    return(invisible(NULL))
  }

  # Phase 1 already ran from gexp_app_ui(); do not run full attach on the first gexp_app_server()
  # tick or shinytest2 will block on attachNamespace long enough to miss window.shinytest2.ready.
  # Full attach runs from session$onFlushed() after setting gexpipe.attach.allow_full_now, or
  # from tab loaders once allow_full_now is set / attach.done is TRUE.
  if (
    test_minimal &&
    isTRUE(getOption("gexpipe.attach.shiny_stack_only_done", FALSE)) &&
    !isTRUE(getOption("gexpipe.attach.allow_full_now", FALSE))
  ) {
    return(invisible(NULL))
  }

  required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  optional <- c(
    "cicerone",
    "dplyr", "data.table",
    "ggplot2", "gridExtra", "RColorBrewer", "pheatmap", "ggrepel",
    "VennDiagram", "UpSetR",
    "Biobase", "GEOquery", "limma", "edgeR", "sva", "DESeq2",
    "clusterProfiler", "enrichplot", "circlize",
    "STRINGdb", "igraph", "ggraph", "tidygraph", "tidyr",
    "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
    "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils",
    "dynamicTreeCut", "scales", "WGCNA"
  )

  pkgs <- unique(c(required, optional))

  missing <- character(0)
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      if (p %in% required) missing <- c(missing, p)
      next
    }
    tryCatch({
      pkg_search <- paste0("package:", p)
      if (!pkg_search %in% search()) {
        # Source-loaded Shiny modules rely on unqualified symbols; attach namespace.
        base::attachNamespace(asNamespace(p))
      }
    }, error = function(e) NULL)
  }

  if (length(missing) > 0L) {
    stop(
      "Missing required packages for the Shiny app: ",
      paste(missing, collapse = ", "),
      ". Install them (e.g. via BiocManager::install()) and retry."
    )
  }

  options(gexpipe.attach.done = TRUE)
  options(gexpipe.attach.shiny_stack_only_done = NULL)

  # Run once after namespaces exist (was in onStart). Keeping it here avoids calling
  # enableWGCNAThreads before WGCNA is attached.
  wgcna_opt <- getOption("gexpipe.wgcna_threads", NULL)
  if (!identical(wgcna_opt, 0L) && !identical(wgcna_opt, FALSE) && isNamespaceLoaded("WGCNA")) {
    tryCatch({
      if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA"))) {
        n_threads <- if (is.null(wgcna_opt)) {
          max(1L, parallel::detectCores() - 1L)
        } else {
          as.integer(wgcna_opt)
        }
        if (length(n_threads) == 1L && !is.na(n_threads) && n_threads > 0L) {
          WGCNA::enableWGCNAThreads(nThreads = n_threads)
        }
      }
    }, error = function(e) NULL)
  }

  invisible(NULL)
}

gexp_app_onStart <- function() {
  if (interactive()) {
    message("GExPipe: Shiny onStart (server wiring); next line from R is usually Listening on http://...")
  }
  # Do not attach analysis packages here: Shiny runs onStart before the HTTP listener is up,
  # and shinytest2 (and browsers) need a live session quickly. Attach runs from gexp_app_ui()
  # and gexp_app_server() on first session instead.
  options(stringsAsFactors = FALSE)
  options(shiny.maxRequestSize = 500 * 1024^2) # 500 MB
  options(timeout = 3600) # allow long GEO downloads
}

.gexp_inst_file <- function(rel_path) {
  # Prefer installed package location; fall back to source checkout.
  p <- system.file(rel_path, package = "GExPipe")
  if (nzchar(p) && file.exists(p)) return(p)

  # Compatibility: if we request R/shiny_src files, map to installed inst/shinyapp
  # copies (needed because installed packages do not expose R source files as paths).
  if (startsWith(rel_path, "shiny_src/")) {
    alt_rel <- sub("^shiny_src/", "shinyapp/", rel_path)
    p2 <- system.file(alt_rel, package = "GExPipe")
    if (nzchar(p2) && file.exists(p2)) return(p2)
  }

  candidates <- c(
    file.path(getwd(), "R", rel_path),
    file.path(getwd(), "..", "R", rel_path),
    file.path(getwd(), "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "..", "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "inst", rel_path),
    file.path(getwd(), "..", "inst", rel_path)
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) < 1L) {
    stop("GExPipe: could not locate inst file: ", rel_path,
         ". Install the package or run from package root.")
  }
  hits[1]
}

