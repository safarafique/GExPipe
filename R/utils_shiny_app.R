## Single source-of-truth for all GExPipe package names.
## Used by runGExPipe() (pre-launch install) and gexp_app_attach_packages() (load).
.gexpipe_all_pkgs <- function(include_optional = TRUE) {
  required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  core <- c(
    "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
    "dplyr", "data.table", "edgeR", "sva", "ggplot2", "gridExtra",
    "RColorBrewer", "pheatmap", "ggrepel", "VennDiagram", "UpSetR",
    "WGCNA", "clusterProfiler", "enrichplot", "circlize", "STRINGdb",
    "DESeq2", "igraph", "ggraph", "tidygraph", "tidyr",
    "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
    "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils",
    "dynamicTreeCut", "scales",
    "biomaRt", "Boruta", "cicerone", "mixOmics",
    "xgboost", "SHAPforxgboost", "rms", "rmda"
  )
  optional <- character(0)
  if (include_optional) c(required, core, optional) else c(required, core)
}

## Batch-install a vector of packages via BiocManager (handles both Bioc + CRAN).
##
## On Windows, R locks every package DLL that is loaded in the current session.
## BiocManager cannot overwrite a locked DLL, producing the infamous error:
##   "cannot remove earlier installation, is it in use?"
##
## Solution: run the installation in a FRESH Rscript subprocess.  That process
## starts with zero packages loaded, so no DLL is locked.  After it exits, the
## parent session can requireNamespace() the newly installed packages normally.
.gexpipe_batch_install <- function(pkgs) {

  # Initialise so the variable always exists even if an error occurs below.
  still_missing <- character(0)

  # ── 1. Install BiocManager if needed (rarely missing) ─────────────────────
  if (!requireNamespace("BiocManager", quietly = TRUE))
    utils::install.packages("BiocManager",
                             repos = "https://cloud.r-project.org", quiet = TRUE)

  # ── 2. Only install what is genuinely absent ───────────────────────────────
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing_pkgs) == 0L) return(invisible(character(0)))

  message(
    "\nGExPipe: installing ", length(missing_pkgs), " missing package(s).\n",
    "  Running in a background R process to avoid Windows DLL locks.\n",
    "  First-time install can take up to 40 minutes — please wait...\n"
  )

  # ── 3. Remove stale 00LOCK directories before installing ──────────────────
  # A previous failed install leaves a 00LOCK-<pkg> folder in the library.
  # R refuses to install that package again until the lock is removed.
  # This also happens when another RStudio session has the package loaded.
  lib_path <- .libPaths()[1L]
  lock_dirs <- list.files(lib_path, pattern = "^00LOCK-", full.names = TRUE)
  if (length(lock_dirs) > 0L) {
    message("GExPipe: removing ", length(lock_dirs),
            " stale lock director(y/ies): ",
            paste(basename(lock_dirs), collapse = ", "))
    unlink(lock_dirs, recursive = TRUE, force = TRUE)
  }

  # ── 4. Write a self-contained install script to a temp file ───────────────
  #   Using a temp file avoids all quoting/escaping problems on Windows.

  # Collect ALL parent library paths so the subprocess can see the same packages.
  # Rscript --vanilla strips .Renviron/.Rprofile, so the user library
  # (e.g. C:/Users/.../win-library/4.5) is NOT in the subprocess .libPaths()
  # by default. Without this, BiocManager refuses to reinstall (version matches).
  parent_libs <- paste0('"', gsub("\\\\", "/", .libPaths()), '"', collapse = ", ")
  pkg_vec     <- paste0('"', missing_pkgs, '"', collapse = ", ")

  # All packages in missing_pkgs failed requireNamespace() in the PARENT session.
  # They are NOT loaded, so their DLLs are NOT Windows-locked — force = TRUE is safe.
  # Let BiocManager choose the package type (its default "both" tries binary first,
  # falls back to source only when no binary is available).
  script <- c(
    paste0('.libPaths(c(', parent_libs, '))'),   # mirror parent library paths
    'options(',
    '  repos              = c(CRAN = "https://cloud.r-project.org"),',
    '  timeout            = 2400L,',   # 40-minute download timeout per file
    '  download.file.method = "libcurl"',
    ')',
    paste0('.lib  <- "', gsub("\\\\", "/", lib_path), '"'),
    paste0('.pkgs <- c(', pkg_vec, ')'),
    '',
    '# Remove any stale lock dirs inside the subprocess too',
    '.locks <- list.files(.lib, pattern = "^00LOCK-", full.names = TRUE)',
    'if (length(.locks) > 0L) unlink(.locks, recursive = TRUE, force = TRUE)',
    '',
    'if (!requireNamespace("BiocManager", quietly = TRUE))',
    '  install.packages("BiocManager", lib = .lib, quiet = FALSE)',
    '',
    'message("GExPipe subprocess: installing ", length(.pkgs),',
    '        " package(s): ", paste(.pkgs, collapse = ", "))',
    '',
    'BiocManager::install(',
    '  .pkgs,',
    '  lib    = .lib,',
    '  ask    = FALSE,',
    '  update = FALSE,',
    '  force  = TRUE,',
    '  quiet  = FALSE',
    ')'
  )

  tmp_script <- tempfile(pattern = "gexpipe_install_", fileext = ".R")
  on.exit(unlink(tmp_script), add = TRUE)
  writeLines(script, tmp_script)

  # ── 5. Run Rscript subprocess — timeout = 40 min (2400 s) ─────────────────
  rscript <- file.path(R.home("bin"), "Rscript")
  exit_code <- tryCatch(
    system2(rscript,
            args    = c("--vanilla", "--no-save", shQuote(tmp_script)),
            stdout  = "",       # print directly to user's console
            stderr  = "",       # print errors directly to user's console
            timeout = 2400L),   # 40 minutes — enough for a full cold install
    error = function(e) {
      message("GExPipe: could not launch Rscript subprocess: ", conditionMessage(e))
      1L
    }
  )

  if (!identical(exit_code, 0L)) {
    message(
      "GExPipe: subprocess exited with code ", exit_code,
      " — some packages may not have installed.\n",
      "  Run  gexpipe_setup()  for a full install log, or restart R and try again."
    )
  }

  # ── 5. Re-check: report anything still missing ────────────────────────────
  still_missing <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]

  if (length(still_missing) == 0L) {
    message("GExPipe: all packages installed successfully.\n")
  } else {
    message(
      "GExPipe: ", length(still_missing), " package(s) could not be installed: ",
      paste(still_missing, collapse = ", "), "\n",
      "  Try:  BiocManager::install(c(",
      paste0('"', still_missing, '"', collapse = ", "), "))\n",
      "  or restart R and run  shiny::runApp(GExPipe::runGExPipe())  again."
    )
  }

  invisible(still_missing)
}

## Single-package helper kept for backwards compatibility.
.gexpipe_ensure_pkg <- function(pkg) {
  if (requireNamespace(pkg, quietly = TRUE)) return(TRUE)
  .gexpipe_batch_install(pkg)
  requireNamespace(pkg, quietly = TRUE)
}

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
      tryCatch(
        {
          pkg_search <- paste0("package:", p)
          if (!pkg_search %in% search()) {
            base::attachNamespace(asNamespace(p))
          }
        },
        error = function(e) NULL
      )
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

  # batch-install any remaining missing packages.
  # runGExPipe() already ran .gexpipe_batch_install() before the app started,
  # so this is normally a no-op (all packages already present). It only does
  # real work if the user called shiny::runApp() without going through
  # runGExPipe() first, or if the pre-launch install partially failed.
  if (!isTRUE(getOption("gexpipe.prelaunch_install_done", FALSE))) {
    .gexpipe_batch_install(.gexpipe_all_pkgs(include_optional = TRUE))
  }

  pkgs <- unique(.gexpipe_all_pkgs(include_optional = TRUE))

  # Packages that must be present for the app to start at all (Shiny UI stack).
  hard_required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  missing <- character(0)
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      if (p %in% hard_required) missing <- c(missing, p)
      next
    }
    tryCatch(
      {
        pkg_search <- paste0("package:", p)
        if (!pkg_search %in% search()) {
          # Source-loaded Shiny modules rely on unqualified symbols; attach namespace.
          base::attachNamespace(asNamespace(p))
        }
      },
      error = function(e) NULL
    )
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
    tryCatch(
      {
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
      },
      error = function(e) NULL
    )
  }

  invisible(NULL)
}

## Try multiple STRING data versions; stringdb-downloads.org retires older releases over time.
## Override order: options(gexpipe.stringdb_try_versions = c("11.5", "11", "12"))
#' @keywords internal
gexp_stringdb_new_safe <- function(score_threshold, input_directory = "") {
  versions <- getOption("gexpipe.stringdb_try_versions", NULL)
  if (!is.character(versions) || length(versions) < 1L) {
    versions <- c("11.5", "11", "12")
  }
  errs <- character(0)
  for (sv in versions) {
    res <- tryCatch(
      STRINGdb::STRINGdb$new(
        version = sv,
        species = 9606L,
        score_threshold = score_threshold,
        input_directory = input_directory
      ),
      error = function(e) {
        list(db = NULL, err = paste0("STRING v", sv, ": ", conditionMessage(e)))
      }
    )
    if (is.list(res) && !is.null(res$err)) {
      errs <- c(errs, res$err)
      next
    }
    if (!is.null(res)) {
      return(list(db = res, version_used = sv, try_errors = errs))
    }
  }
  list(db = NULL, version_used = NA_character_, try_errors = errs)
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
  # Prefer local checkout (so `runGExPipe()` from source uses the latest edits),
  # then fall back to installed package files.
  candidates <- c(
    file.path(getwd(), "R", rel_path),
    file.path(getwd(), "..", "R", rel_path),
    file.path(getwd(), "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "..", "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "inst", rel_path),
    file.path(getwd(), "..", "inst", rel_path)
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) >= 1L) {
    return(hits[1])
  }

  # Fall back to installed package location.
  p <- system.file(rel_path, package = "GExPipe")
  if (nzchar(p) && file.exists(p)) {
    return(p)
  }

  if (startsWith(rel_path, "shiny_src/")) {
    alt_rel <- sub("^shiny_src/", "shinyapp/", rel_path)
    p2 <- system.file(alt_rel, package = "GExPipe")
    if (nzchar(p2) && file.exists(p2)) {
      return(p2)
    }
  }

  stop(
    "GExPipe: could not locate inst file: ",
    rel_path,
    ". Run from package root or install the package."
  )
}
