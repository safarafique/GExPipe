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
    "biomaRt", "Boruta", "car", "cicerone", "mixOmics",
    "xgboost", "SHAPforxgboost", "rms", "rmda"
  )
  optional <- character(0)
  if (include_optional) c(required, core, optional) else c(required, core)
}

## Minimum versions for packages that frequently cause version-conflict errors.
## Any package below its floor is treated as "needs update" even when installed.
.gexpipe_min_versions <- c(
  rlang     = "1.1.0",
  cli       = "3.4.0",
  vctrs     = "0.6.0",
  lifecycle = "1.0.3",
  glue      = "1.6.0",
  Matrix    = "1.5.0",
  Rcpp      = "1.0.10"
)

## Dedicated GExPipe library (created once per R version).
## Uses AppData\Local\GExPipe\ on Windows — never synced by OneDrive.
## Using Documents (path.expand("~")) causes "moving to final location failed"
## errors because OneDrive holds background file locks on that folder.
.gexpipe_get_lib <- function() {
  rv <- paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor))
  base <- if (.Platform$OS.type == "windows") {
    la <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(la) && dir.exists(dirname(la))) la else path.expand("~")
  } else {
    path.expand("~")
  }
  d <- file.path(base, "GExPipe", rv)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  # Keep old Documents-based path in .libPaths() so previously installed
  # packages there still load during the transition.
  old_d <- file.path(path.expand("~"), ".gexpipe_packages", rv)
  if (dir.exists(old_d) && old_d != d && !old_d %in% .libPaths())
    .libPaths(c(.libPaths(), old_d))
  d
}

## Best version of pkg available across all libPaths (not just what is loaded).
.gexpipe_best_version <- function(pkg) {
  vers <- vapply(.libPaths(), function(lib) {
    tryCatch(as.character(utils::packageVersion(pkg, lib.loc = lib)),
             error = function(e) "0.0.0")
  }, character(1L))
  vers <- vers[vers != "0.0.0"]
  if (length(vers) == 0L) return(package_version("0.0.0"))
  max(lapply(vers, package_version))
}

## Batch-install a vector of packages via BiocManager (handles both Bioc + CRAN).
##
## Four layers of protection:
##  1. Dedicated ~/.gexpipe_packages/R-x.y/ library — new versions shadow old
##     system ones via .libPaths() priority; subprocess writes there freely.
##  2. Missing-package detection — requireNamespace() check.
##  3. Version-conflict detection — compares best available version against
##     .gexpipe_min_versions; outdated packages are added to the install list.
##  4. Force-reload attempt — after subprocess, tries to unload + reload from
##     the dedicated lib; if still conflicted, shows a clear restart message.
.gexpipe_batch_install <- function(pkgs) {

  # ── 0. Dedicated library ───────────────────────────────────────────────────
  gexpipe_lib <- .gexpipe_get_lib()
  if (!gexpipe_lib %in% .libPaths())
    .libPaths(c(gexpipe_lib, unique(.libPaths())))

  # ── 1. BiocManager ────────────────────────────────────────────────────────
  if (!requireNamespace("BiocManager", quietly = TRUE))
    utils::install.packages("BiocManager",
                             lib   = gexpipe_lib,
                             repos = "https://cloud.r-project.org",
                             quiet = TRUE)

  # ── 2. Detect missing packages ────────────────────────────────────────────
  missing_pkgs <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]

  # ── 3. Detect version-conflicted packages ─────────────────────────────────
  version_conflict_pkgs <- names(.gexpipe_min_versions)[
    vapply(names(.gexpipe_min_versions), function(pkg) {
      tryCatch(
        .gexpipe_best_version(pkg) < package_version(.gexpipe_min_versions[[pkg]]),
        error = function(e) FALSE
      )
    }, logical(1L))
  ]

  # ── 3b. Detect outdated packages in dedicated lib ─────────────────────────
  outdated_pkgs <- tryCatch({
    old <- utils::old.packages(lib.loc = gexpipe_lib)
    if (!is.null(old) && nrow(old) > 0L) intersect(pkgs, rownames(old))
    else character(0L)
  }, error = function(e) character(0L), warning = function(w) character(0L))

  to_install <- unique(c(missing_pkgs, version_conflict_pkgs, outdated_pkgs))

  if (length(to_install) == 0L) return(invisible(character(0L)))

  if (length(missing_pkgs) > 0L)
    message("GExPipe: ", length(missing_pkgs), " missing package(s): ",
            paste(head(missing_pkgs, 8L), collapse = ", "),
            if (length(missing_pkgs) > 8L) " ..." else "")
  if (length(version_conflict_pkgs) > 0L)
    message("GExPipe: ", length(version_conflict_pkgs), " version-conflict package(s): ",
            paste(version_conflict_pkgs, collapse = ", "))
  if (length(outdated_pkgs) > 0L)
    message("GExPipe: ", length(outdated_pkgs), " outdated package(s): ",
            paste(head(outdated_pkgs, 8L), collapse = ", "),
            if (length(outdated_pkgs) > 8L) " ..." else "")
  message(
    "GExPipe: installing/updating ", length(to_install), " package(s).\n",
    "  Background Rscript subprocess (no DLL locks).\n",
    "  First-time install: up to 40 min. Subsequent runs: seconds."
  )

  # ── 4. Remove stale 00LOCK dirs from EVERY library path ───────────────────
  # A failed install leaves 00LOCK-<pkg> in the user's system library and makes
  # ALL subsequent downloads fail (e.g. curl.dll locked → no downloads possible).
  for (.ld in unique(c(gexpipe_lib, .libPaths()))) {
    .lk <- list.files(.ld, pattern = "^00LOCK-", full.names = TRUE)
    if (length(.lk) > 0L) {
      message("GExPipe: removing ", length(.lk), " stale lock dir(s) from ", .ld)
      unlink(.lk, recursive = TRUE, force = TRUE)
    }
  }

  # ── 5. Write and run install subprocess ───────────────────────────────────
  # Detect packages loaded from gexpipe_lib whose DLL the parent holds.
  # Windows locks loaded DLLs — the subprocess cannot overwrite them even
  # though it starts fresh. Skip those; they update on next R restart.
  .dll_locked_in_parent <- function(pkg) {
    if (!isNamespaceLoaded(pkg)) return(FALSE)
    pkg_path <- tryCatch(find.package(pkg), error = function(e) "")
    nzchar(pkg_path) &&
      startsWith(normalizePath(pkg_path,   winslash = "/", mustWork = FALSE),
                 normalizePath(gexpipe_lib, winslash = "/", mustWork = FALSE))
  }
  dll_locked <- to_install[vapply(to_install, .dll_locked_in_parent, logical(1L))]
  if (length(dll_locked) > 0L)
    message("GExPipe: DLL locked in parent (deferred to next restart): ",
            paste(dll_locked, collapse = ", "))
  subprocess_pkgs <- setdiff(to_install, dll_locked)

  if (length(subprocess_pkgs) == 0L) {
    message("GExPipe: all remaining packages are DLL-locked — restart R to apply updates.")
    return(invisible(dll_locked))
  }

  all_libs    <- unique(c(gexpipe_lib, .libPaths()))
  parent_libs <- paste0('"', gsub("\\\\", "/", all_libs), '"', collapse = ", ")
  lib_fwd     <- gsub("\\\\", "/", gexpipe_lib)
  pkg_vec     <- paste0('"', subprocess_pkgs, '"', collapse = ", ")

  script <- c(
    paste0('.libPaths(c(', parent_libs, '))'),
    # Clean locks from all lib paths inside subprocess
    'for (.d in .libPaths()) {',
    '  .lk <- list.files(.d, pattern = "^00LOCK-", full.names = TRUE)',
    '  if (length(.lk)) unlink(.lk, recursive = TRUE, force = TRUE)',
    '}',
    # Download method: libcurl by default; wininet fallback when curl.dll is locked
    'options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 2400L)',
    'if (.Platform$OS.type == "windows") {',
    '  .ok <- tryCatch({',
    '    utils::download.file("https://cloud.r-project.org", tempfile(),',
    '                         quiet = TRUE, method = "libcurl"); TRUE',
    '  }, error = function(e) FALSE, warning = function(w) FALSE)',
    '  options(download.file.method = if (.ok) "libcurl" else "wininet")',
    '  if (!.ok) message("GExPipe subprocess: libcurl unavailable, using wininet")',
    '} else {',
    '  options(download.file.method = "libcurl")',
    '}',
    paste0('.lib  <- "', lib_fwd, '"'),
    paste0('.pkgs <- c(', pkg_vec, ')'),
    'if (!requireNamespace("BiocManager", quietly = TRUE))',
    '  install.packages("BiocManager", lib = .lib,',
    '                   repos = "https://cloud.r-project.org", quiet = FALSE)',
    'message("GExPipe subprocess: installing/updating ", length(.pkgs), " package(s)")',
    # --no-staged-install: installs directly to final location, skipping the
    #   00LOCK-*/00new/* staging dir whose rename step fails when OneDrive or
    #   another process holds a lock on the target folder.
    # --no-lock: skips 00LOCK-* directory creation — safe because only this
    #   subprocess is writing to .lib at this moment.
    'BiocManager::install(.pkgs, lib = .lib, ask = FALSE,',
    '                     update = TRUE, force = TRUE, quiet = FALSE,',
    '                     INSTALL_opts = c("--no-staged-install", "--no-lock"))',
    # Second pass: fix transitive outdated deps
    '.old <- tryCatch(',
    '  utils::old.packages(lib.loc = .lib, repos = BiocManager::repositories()),',
    '  error = function(e) NULL, warning = function(w) NULL',
    ')',
    'if (!is.null(.old) && nrow(.old) > 0L) {',
    '  message("GExPipe subprocess: fixing ", nrow(.old), " transitive dep(s)")',
    '  BiocManager::install(rownames(.old), lib = .lib, ask = FALSE,',
    '                       update = TRUE, force = TRUE, quiet = FALSE,',
    '                       INSTALL_opts = c("--no-staged-install", "--no-lock"))',
    '}'
  )

  tmp_script <- tempfile(pattern = "gexpipe_install_", fileext = ".R")
  on.exit(unlink(tmp_script), add = TRUE)
  writeLines(script, tmp_script)

  rscript   <- file.path(R.home("bin"), "Rscript")
  exit_code <- tryCatch(
    system2(rscript,
            args    = c("--vanilla", "--no-save", shQuote(tmp_script)),
            stdout  = "", stderr  = "",
            timeout = 2400L),
    error = function(e) {
      message("GExPipe: could not launch subprocess: ", conditionMessage(e))
      1L
    }
  )
  if (!identical(exit_code, 0L))
    message("GExPipe: subprocess exit code ", exit_code,
            " — restart R and try again if packages are still missing.")

  # ── 6. Version-conflict recovery for already-loaded packages ──────────────
  # Packages updated in gexpipe_lib shadow old system versions on next load.
  # For packages already IN MEMORY with an old version, try unload + reload.
  .try_reload <- function(pkg, min_ver) {
    if (!isNamespaceLoaded(pkg)) return(TRUE)
    cur <- tryCatch(utils::packageVersion(pkg), error = function(e) package_version("0.0.0"))
    if (cur >= package_version(min_ver)) return(TRUE)
    new_v <- tryCatch(utils::packageVersion(pkg, lib.loc = gexpipe_lib),
                      error = function(e) package_version("0.0.0"))
    if (new_v < package_version(min_ver)) return(FALSE)
    tryCatch({
      suppressWarnings(unloadNamespace(pkg))
      loadNamespace(pkg, lib.loc = gexpipe_lib)
      utils::packageVersion(pkg) >= package_version(min_ver)
    }, error = function(e) FALSE)
  }
  still_conflicted <- names(.gexpipe_min_versions)[
    !mapply(.try_reload, names(.gexpipe_min_versions), .gexpipe_min_versions,
            SIMPLIFY = TRUE)
  ]
  if (length(still_conflicted) > 0L) {
    conflict_detail <- vapply(still_conflicted, function(pkg) {
      cur <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "?")
      new <- tryCatch(as.character(utils::packageVersion(pkg, lib.loc = gexpipe_lib)),
                      error = function(e) "updated")
      paste0(pkg, " (loaded=", cur, ", updated=", new, ")")
    }, character(1L))
    for (d in conflict_detail) message("GExPipe: ", d, " — requires restart to apply")
    message(
      "\nGExPipe: RESTART R to apply package updates, then run again.\n",
      "  RStudio: Ctrl+Shift+F10, then re-run GExPipe::runGExPipe()\n",
      "  The next run will open the app immediately (packages already updated)."
    )
    # Store state so the Shiny server can show an in-app restart notification.
    options(gexpipe.restart_required  = TRUE)
    options(gexpipe.still_conflicted  = still_conflicted)
  } else {
    # Clear any stale restart flag from a previous run.
    options(gexpipe.restart_required  = NULL)
    options(gexpipe.still_conflicted  = NULL)
  }

  # ── 7. Re-check: report still-missing packages ────────────────────────────
  still_missing <- missing_pkgs[
    !vapply(missing_pkgs, requireNamespace, logical(1L), quietly = TRUE)
  ]
  if (length(still_missing) > 0L)
    message("GExPipe: still missing after install: ",
            paste(still_missing, collapse = ", "),
            "\n  Try: BiocManager::install(c(",
            paste0('"', still_missing, '"', collapse = ", "), "))")
  else if (length(to_install) > 0L)
    message("GExPipe: all packages installed successfully.")

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

  pkgs    <- unique(.gexpipe_all_pkgs(include_optional = TRUE))
  n_total <- length(pkgs)

  hard_required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  missing  <- character(0)
  loaded   <- character(0)
  failed   <- character(0)

  message("\n======================================================================")
  message("  GExPipe \u2014 loading ", n_total, " packages...\n")

  for (i in seq_along(pkgs)) {
    p     <- pkgs[i]
    label <- formatC(p, width = -18L, flag = "-")
    idx   <- sprintf("  [%2d/%2d]", i, n_total)

    if (!requireNamespace(p, quietly = TRUE)) {
      message(idx, " ", label, "... \u2717 MISSING")
      if (p %in% hard_required) missing <- c(missing, p)
      failed <- c(failed, p)
      next
    }

    tryCatch(
      {
        pkg_search <- paste0("package:", p)
        if (!pkg_search %in% search())
          base::attachNamespace(asNamespace(p))
        ver <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
        message(idx, " ", label, "... \u2713 ", ver)
        loaded <- c(loaded, p)
      },
      error = function(e) {
        message(idx, " ", label, "... \u2717 load error")
        failed <<- c(failed, p)
      }
    )
  }

  # Store failed packages under the option that server_app.R reads for notifications.
  options(gexpipe.failed_pkgs = if (length(failed) > 0L) failed else NULL)

  message("\n----------------------------------------------------------------------")
  message("  \u2713 Loaded  : ", length(loaded), " / ", n_total, " packages")
  if (length(failed) > 0L)
    message("  \u2717 Missing : ", paste(failed, collapse = ", "))
  else
    message("  \u2713 Status  : All packages ready \u2014 opening app...")
  message("======================================================================\n")

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
