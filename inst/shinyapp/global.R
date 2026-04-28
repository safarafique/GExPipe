# ==============================================================================
# GLOBAL.R  —  GExPipe auto-setup: installs all packages, loads libraries
# ==============================================================================
# Users never need to manually install anything.
# Just run: shiny::runGitHub("GExPipe", "safarafique",
#                             ref = "main", subdir = "inst/shinyapp",
#                             destdir = tempfile())
# ==============================================================================

cat("\n")
cat("======================================================================\n")
cat("  GExPipe \u2014 Gene Expression Pipeline\n")
cat("  R", R.version.string, "\n")
cat("======================================================================\n")

options(timeout = 3600)   # 1 hour — covers slow connections and large Bioc package downloads

# ==============================================================================
# STEP 0  \u2014  Universal library setup (OS-aware, cloud-safe, two-lib design)
# ==============================================================================
# Path strategy — never in a cloud-synced folder:
#   Windows : %LOCALAPPDATA%\GExPipe\<R-ver>\        (AppData\Local, not Documents)
#   macOS   : ~/Library/Application Support/GExPipe/ (not iCloud Drive)
#   Linux   : $XDG_DATA_HOME/GExPipe/  (defaults to ~/.local/share/)
#
# Two-library design — eliminates the need for manual R restarts after updates:
#   main_lib    : GExPipe/<ver>/          <- parent session loads from here
#   pending_lib : GExPipe/<ver>-pending/  <- subprocess always writes here
#
# On each startup pending packages are promoted → main before ANY package
# loads, so no DLL is locked and the rename always succeeds.
# DLL-locked updates (packages already in memory) go to pending this run
# and are active on the very next startup automatically.

.gexpipe_rv <- paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor))

.gexpipe_lib_base <- {
  sysname <- Sys.info()[["sysname"]]
  if (.Platform$OS.type == "windows") {
    la <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(la)) la
    else { ap <- Sys.getenv("APPDATA", unset = ""); if (nzchar(ap)) ap else path.expand("~") }
  } else if (sysname == "Darwin") {
    file.path(path.expand("~"), "Library", "Application Support")
  } else {
    xdg <- Sys.getenv("XDG_DATA_HOME", unset = "")
    if (nzchar(xdg)) xdg else file.path(path.expand("~"), ".local", "share")
  }
}

.gexpipe_lib         <- file.path(.gexpipe_lib_base, "GExPipe", .gexpipe_rv)
.gexpipe_pending_lib <- file.path(.gexpipe_lib_base, "GExPipe",
                                   paste0(.gexpipe_rv, "-pending"))
dir.create(.gexpipe_lib, recursive = TRUE, showWarnings = FALSE)

# Warn if library is still inside a cloud-sync folder (edge case on some configs)
local({
  p <- normalizePath(.gexpipe_lib, winslash = "/", mustWork = FALSE)
  for (pat in c("OneDrive", "Dropbox", "Google Drive", "iCloud", "Box Sync", "Mega")) {
    if (grepl(pat, p, ignore.case = TRUE)) {
      cat("  WARNING: library path is inside a cloud-sync folder (", pat, ").\n",
          "  This causes 'moving to final location failed' during install.\n",
          "  Set the LOCALAPPDATA environment variable to a non-synced path.\n", sep = "")
      break
    }
  }
})

# Promote pending → main BEFORE loading anything (no DLL locks at this point).
# file.rename is atomic on same filesystem; falls back to copy+delete otherwise.
local({
  if (!dir.exists(.gexpipe_pending_lib)) return(invisible(NULL))
  items <- list.dirs(.gexpipe_pending_lib, recursive = FALSE, full.names = TRUE)
  if (length(items) == 0L) { unlink(.gexpipe_pending_lib, recursive = TRUE); return(invisible(NULL)) }
  n_ok <- 0L
  for (src in items) {
    dst <- file.path(.gexpipe_lib, basename(src))
    tryCatch(if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE), error = function(e) NULL)
    ok <- tryCatch(isTRUE(file.rename(src, dst)), error = function(e) FALSE)
    if (!ok) ok <- tryCatch({ file.copy(src, .gexpipe_lib, recursive = TRUE, overwrite = TRUE); unlink(src, recursive = TRUE); TRUE }, error = function(e) FALSE)
    if (ok) n_ok <- n_ok + 1L
  }
  if (n_ok > 0L) cat("  Promoted", n_ok, "pending update(s) to active library.\n")
  if (length(list.files(.gexpipe_pending_lib, all.files = TRUE, no.. = TRUE)) == 0L)
    unlink(.gexpipe_pending_lib, recursive = TRUE)
})

.libPaths(c(.gexpipe_lib, unique(.libPaths())))
# Backward-compat: keep old Documents-based path so packages installed before this fix load
local({ old <- file.path(path.expand("~"), ".gexpipe_packages", .gexpipe_rv)
        if (dir.exists(old) && !old %in% .libPaths()) .libPaths(c(.libPaths(), old)) })

cat("  Library      :", .gexpipe_lib, "\n")

# ==============================================================================
# STEP 1  \u2014  Ensure BiocManager is present
# ==============================================================================
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  cat("  Installing BiocManager...\n")
  install.packages("BiocManager",
                   lib   = .gexpipe_lib,
                   repos = "https://cloud.r-project.org",
                   quiet = TRUE)
}
# Determine the correct Bioconductor release for the running R version.
# Bioc 3.22 → R ≥ 4.6  |  Bioc 3.21 → R ≥ 4.5  |  Bioc 3.20 → R ≥ 4.4
# Forcing a HIGHER release than R supports causes every BiocManager::install() to fail.
.r_numeric <- tryCatch(
  numeric_version(paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor))),
  error = function(e) numeric_version("4.4")
)
.target_bioc <- if      (.r_numeric >= "4.6") "3.22"
                else if (.r_numeric >= "4.5") "3.21"
                else if (.r_numeric >= "4.4") "3.20"
                else                          "3.19"

bioc_ver <- tryCatch(as.character(BiocManager::version()), error = function(e) "?")
if (!identical(bioc_ver, .target_bioc)) {
  cat("  Upgrading Bioconductor to", .target_bioc, "(current:", bioc_ver, ")...\n")
  tryCatch(
    BiocManager::install(version = .target_bioc, ask = FALSE, quiet = TRUE),
    error   = function(e) cat("  BiocManager upgrade note:", conditionMessage(e), "\n"),
    warning = function(w) NULL
  )
  bioc_ver <- tryCatch(as.character(BiocManager::version()), error = function(e) "?")
}
cat("  Bioconductor :", bioc_ver, "(R", as.character(.r_numeric), ")\n\n")

# ==============================================================================
# STEP 2  \u2014  Package lists + minimum version requirements
# ==============================================================================

.gexpipe_all_required <- c(
  # Shiny UI
  "shiny", "shinydashboard", "shinyjs", "DT",
  # Bioconductor core
  "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
  "edgeR", "DESeq2", "sva", "clusterProfiler", "enrichplot", "STRINGdb", "WGCNA",
  # CRAN analysis
  "dplyr", "data.table", "tibble", "tidyr", "tidygraph", "reshape2",
  "ggplot2", "ggpubr", "ggrepel", "ggraph", "gridExtra", "RColorBrewer",
  "pheatmap", "circlize", "corrplot", "scales",
  "VennDiagram", "UpSetR", "igraph",
  "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
  "msigdbr", "R.utils", "dynamicTreeCut", "parallel",
  # Feature packages
  "biomaRt", "Boruta", "car", "cicerone", "mixOmics",
  "xgboost", "SHAPforxgboost", "rms", "rmda"
)

.gexpipe_all_optional <- character(0)

# Minimum versions for packages that frequently cause version-conflict errors
# (e.g. "namespace rlang 1.1.7 is loaded but >= 1.2.0 is required").
# Any package below these floors is treated as "needs update" even if present.
# Minimum versions — must stay in sync with DESCRIPTION Imports section.
# Any package below its floor is treated as "needs update" even when installed.
# Direct version constraints copied from DESCRIPTION:
.gexpipe_min_versions <- c(
  rlang     = "1.2.0",   # DESCRIPTION: rlang (>= 1.2.0)
  cli       = "3.6.0",   # DESCRIPTION: cli   (>= 3.6.0)
  glue      = "1.7.0",   # DESCRIPTION: glue  (>= 1.7.0)
  lifecycle = "1.0.4",   # DESCRIPTION: lifecycle (>= 1.0.4)
  vctrs     = "0.6.5",   # DESCRIPTION: vctrs (>= 0.6.5)
  # Indirect deps that frequently trigger runtime version-conflict errors:
  Matrix    = "1.6.0",
  Rcpp      = "1.0.12",
  withr     = "2.5.0",
  pillar    = "1.9.0"
)

# ==============================================================================
# STEP 3  \u2014  Detect missing, version-conflicted, and outdated packages
# ==============================================================================

# Best version available across ALL libPaths (not just what is loaded in memory)
.gexpipe_best_version <- function(pkg) {
  vers <- vapply(.libPaths(), function(lib) {
    tryCatch(as.character(utils::packageVersion(pkg, lib.loc = lib)),
             error = function(e) "0.0.0")
  }, character(1L))
  vers <- vers[vers != "0.0.0"]
  if (length(vers) == 0L) return(package_version("0.0.0"))
  max(lapply(vers, package_version))
}

.gexpipe_version_ok <- function(pkg, min_ver) {
  tryCatch(.gexpipe_best_version(pkg) >= package_version(min_ver),
           error = function(e) FALSE)
}

# 3a. Missing FROM GExPipe library specifically (even if present in main library).
# This guarantees every package is installed into the isolated GExPipe library
# so the app never loads from the user's main R library.
.missing_pkgs <- .gexpipe_all_required[
  vapply(.gexpipe_all_required, function(pkg) {
    if (pkg == "parallel") return(FALSE)          # base package, always available
    tryCatch(
      { utils::packageVersion(pkg, lib.loc = .gexpipe_lib); FALSE },
      error = function(e) TRUE
    )
  }, logical(1L))
]

# 3b. Present in GExPipe library but below minimum required version
.version_conflict_pkgs <- names(.gexpipe_min_versions)[
  vapply(names(.gexpipe_min_versions), function(pkg) {
    tryCatch({
      ver <- utils::packageVersion(pkg, lib.loc = .gexpipe_lib)
      ver < package_version(.gexpipe_min_versions[[pkg]])
    }, error = function(e) FALSE)
  }, logical(1L))
]

# 3c. Present and above minimum but still outdated in the dedicated lib
.outdated_in_lib <- tryCatch({
  old <- utils::old.packages(lib.loc = .gexpipe_lib)
  if (!is.null(old) && nrow(old) > 0L)
    rownames(old)[rownames(old) %in% .gexpipe_all_required]
  else character(0L)
}, error = function(e) character(0L), warning = function(w) character(0L))

.to_install <- unique(c(.missing_pkgs, .version_conflict_pkgs, .outdated_in_lib))

# ==============================================================================
# STEP 4  \u2014  Install / update packages with per-package visible progress
# ==============================================================================
# Logic per package:
#   NOT installed at all  → install directly (no DLL possible on a missing pkg)
#   Installed, old version, NOT loaded → update directly
#   Installed, old version, DLL-locked in this session → subprocess → pending lib
#                                                          (active on NEXT startup)
#   Installed, version OK → skip (nothing to do)

# ── Helpers ──────────────────────────────────────────────────────────────────
.dll_locked_in_parent <- function(pkg) {
  # TRUE when pkg is both: (a) loaded in this R session AND
  # (b) loaded from .gexpipe_lib (our managed library, not a system copy)
  if (!isNamespaceLoaded(pkg)) return(FALSE)
  pp <- tryCatch(find.package(pkg), error = function(e) "")
  nzchar(pp) &&
    startsWith(normalizePath(pp,            winslash = "/", mustWork = FALSE),
               normalizePath(.gexpipe_lib,  winslash = "/", mustWork = FALSE))
}

.ncpus_install <- max(1L, tryCatch(parallel::detectCores() - 1L, error = function(e) 1L))
.inst_opts     <- c("--no-staged-install", "--no-lock")

# ── Remove stale lock dirs first ─────────────────────────────────────────────
for (.ld in unique(c(.gexpipe_lib, .libPaths()))) {
  .lk <- list.files(.ld, pattern = "^00LOCK-", full.names = TRUE)
  if (length(.lk) > 0L) unlink(.lk, recursive = TRUE, force = TRUE)
}

# ── Classify ──────────────────────────────────────────────────────────────────
.pending_pkgs <- if (length(.to_install) > 0L)
  .to_install[vapply(.to_install, .dll_locked_in_parent, logical(1L))]
else character(0L)
.direct_pkgs  <- setdiff(.to_install, .pending_pkgs)

# ── Phase 1: direct installs (missing + outdated non-locked packages) ─────────
if (length(.to_install) == 0L) {
  cat("  \u2713 All", length(.gexpipe_all_required),
      "required packages present and up to date \u2014 skipping install.\n\n")

} else {
  n_miss  <- length(.missing_pkgs)
  n_upd   <- length(.version_conflict_pkgs) + length(.outdated_in_lib)
  n_pend  <- length(.pending_pkgs)
  n_dir   <- length(.direct_pkgs)

  cat("  Need to install :", n_miss, "package(s)\n")
  cat("  Need to update  :", n_upd,  "package(s)\n")
  if (n_pend > 0L)
    cat("  DLL-locked      :", n_pend, "(updated via subprocess, active next startup)\n")
  cat("\n")

  if (n_dir > 0L) {
    cat("  Installing/updating", n_dir, "package(s)...\n")
    cat("  First run: 10-40 min depending on internet speed.\n\n")
    n <- n_dir
    for (i in seq_len(n)) {
      pkg    <- .direct_pkgs[i]
      action <- if (pkg %in% .missing_pkgs) "INSTALL" else "UPDATE "
      cat(sprintf("  [%2d/%d] %s  %-24s", i, n, action, pkg))
      tryCatch({
        suppressMessages(suppressWarnings(
          BiocManager::install(pkg,
                               lib          = .gexpipe_lib,
                               ask          = FALSE,
                               update       = TRUE,
                               force        = TRUE,
                               Ncpus        = .ncpus_install,
                               INSTALL_opts = .inst_opts)
        ))
        ver <- tryCatch(
          as.character(utils::packageVersion(pkg, lib.loc = .gexpipe_lib)),
          error = function(e) "?")
        cat(" \u2713", ver, "\n")
      },
      error   = function(e) cat(" \u2717 FAILED:", conditionMessage(e), "\n"),
      warning = function(w) NULL)
    }
    cat("\n")
  }

  # ── Phase 2: DLL-locked packages → pending lib via subprocess ─────────────
  if (length(.pending_pkgs) > 0L) {
    cat("  Updating DLL-locked package(s) via subprocess (active on next startup):\n")
    cat("  ", paste(.pending_pkgs, collapse = ", "), "\n\n")

    .lib_fwd  <- gsub("\\\\", "/", .gexpipe_lib)
    .pend_fwd <- gsub("\\\\", "/", .gexpipe_pending_lib)
    .pv       <- paste0('"', .pending_pkgs, '"', collapse = ", ")
    .all_libs <- paste0('"', gsub("\\\\", "/", unique(c(.gexpipe_lib, .libPaths()))), '"',
                        collapse = ", ")

    .sub_script <- c(
      paste0('.libPaths(c(', .all_libs, '))'),
      paste0('.pend_lib <- "', .pend_fwd, '"'),
      paste0('.pkgs     <- c(', .pv, ')'),
      paste0('.ncpus    <- ', .ncpus_install, 'L'),
      paste0('.bver     <- "', .target_bioc, '"'),
      'options(repos = c(CRAN="https://cloud.r-project.org"), timeout=2400L)',
      'if (.Platform$OS.type=="windows"){',
      '  .ok <- tryCatch({utils::download.file("https://cloud.r-project.org",tempfile(),quiet=TRUE,method="libcurl");TRUE},error=function(e)FALSE,warning=function(w)FALSE)',
      '  options(download.file.method=if(.ok)"libcurl" else "wininet")',
      '} else options(download.file.method="libcurl")',
      'if (!requireNamespace("BiocManager",quietly=TRUE))',
      '  install.packages("BiocManager",repos="https://cloud.r-project.org")',
      'if (!identical(as.character(BiocManager::version()),.bver))',
      '  tryCatch(BiocManager::install(version=.bver,ask=FALSE),error=function(e)NULL,warning=function(w)NULL)',
      'dir.create(.pend_lib, recursive=TRUE, showWarnings=FALSE)',
      'BiocManager::install(.pkgs, lib=.pend_lib, ask=FALSE, update=TRUE, force=TRUE,',
      '  Ncpus=.ncpus, INSTALL_opts=c("--no-staged-install","--no-lock"))',
      'cat("Pending install done\\n")'
    )
    .tmp_sub  <- tempfile(pattern = "gexpipe_pending_", fileext = ".R")
    .log_sub  <- tempfile(pattern = "gexpipe_pending_log_", fileext = ".txt")
    on.exit({ unlink(.tmp_sub); unlink(.log_sub) }, add = TRUE)
    writeLines(.sub_script, .tmp_sub)
    .rscript  <- file.path(R.home("bin"), "Rscript")
    .ec       <- tryCatch(
      system2(.rscript, args = c("--vanilla","--no-save", shQuote(.tmp_sub)),
              stdout = .log_sub, stderr = .log_sub, timeout = 2400L),
      error = function(e) { cat("  Could not launch subprocess:", conditionMessage(e), "\n"); 1L })
    if (!identical(.ec, 0L)) {
      cat("  Subprocess exit code:", .ec, "— last log lines:\n")
      .ll <- tryCatch(readLines(.log_sub, warn=FALSE), error=function(e) character(0))
      if (length(.ll) > 0L) cat(paste(utils::tail(.ll, 20L), collapse="\n"), "\n")
    }
    cat("  Restart R and run again to apply these updates.\n\n")
  }
}

cat("  [3/4] Checking optional packages...\n")
if (length(.gexpipe_all_optional) == 0L) {
  cat("  No optional packages defined.\n\n")
}

# ==============================================================================
# STEP 4a  \u2014  Version-conflict recovery for already-loaded packages
# ==============================================================================
# After the subprocess, new versions sit in .gexpipe_lib (highest .libPaths()
# priority). For packages that are ALREADY LOADED in this R session with an
# old version, the namespace cache holds the old version. We try to unload
# and reload them from .gexpipe_lib. This works for leaf packages (rlang,
# cli, etc.) that have no running dependents at this stage of global.R.

.gexpipe_try_reload <- function(pkg, min_ver) {
  if (!isNamespaceLoaded(pkg)) return(TRUE)      # not loaded, no issue
  cur_ver <- tryCatch(utils::packageVersion(pkg), error = function(e) package_version("0.0.0"))
  if (cur_ver >= package_version(min_ver)) return(TRUE)  # already fine

  # Check that a good version is available in .gexpipe_lib
  new_ver <- tryCatch(
    utils::packageVersion(pkg, lib.loc = .gexpipe_lib),
    error = function(e) package_version("0.0.0")
  )
  if (new_ver < package_version(min_ver)) return(FALSE)  # not there yet

  # Attempt graceful unload + reload from .gexpipe_lib
  tryCatch({
    suppressWarnings(unloadNamespace(pkg))
    loadNamespace(pkg, lib.loc = .gexpipe_lib)
    reloaded_ver <- utils::packageVersion(pkg)
    reloaded_ver >= package_version(min_ver)
  }, error = function(e) FALSE)
}

.reload_results <- mapply(
  .gexpipe_try_reload,
  names(.gexpipe_min_versions),
  .gexpipe_min_versions,
  SIMPLIFY = TRUE
)
.still_conflicted <- names(.gexpipe_min_versions)[!.reload_results]

if (length(.still_conflicted) > 0L) {
  cat("\n")
  cat("\u2554", strrep("\u2550", 62L), "\u2557\n", sep = "")
  cat("\u2551  PACKAGES UPDATED \u2014 RESTART R TO APPLY              ",
      strrep(" ", max(0L, 14L - nchar(paste(.still_conflicted, collapse=", ")))),
      "     \u2551\n", sep = "")
  cat("\u2551                                                              \u2551\n")
  cat("\u2551  These packages were updated in .gexpipe_packages/ but are  \u2551\n")
  cat("\u2551  still loaded in this session with older versions:           \u2551\n")
  for (pkg in .still_conflicted) {
    cur <- tryCatch(as.character(utils::packageVersion(pkg)), error = function(e) "?")
    new <- tryCatch(
      as.character(utils::packageVersion(pkg, lib.loc = .gexpipe_lib)),
      error = function(e) "updated"
    )
    cat(sprintf("\u2551    %-12s  loaded: %-10s  updated to: %-10s  \u2551\n",
                pkg, cur, new))
  }
  cat("\u2551                                                              \u2551\n")
  cat("\u2551  To fix:                                                     \u2551\n")
  cat("\u2551  1. Press Ctrl+C  (or RStudio Stop \u25a0)                       \u2551\n")
  cat("\u2551  2. Restart R     (RStudio: Ctrl+Shift+F10)                 \u2551\n")
  cat("\u2551  3. Run the same runGitHub() line again                     \u2551\n")
  cat("\u2551     The next run opens the app in seconds.                  \u2551\n")
  cat("\u255a", strrep("\u2550", 62L), "\u255d\n\n", sep = "")
  stop("GExPipe: restart R to apply package updates (see box above).",
       call. = FALSE)
}

# ==============================================================================
# STEP 4b  \u2014  Re-check ALL required packages after install
# ==============================================================================
# Re-check the full required list (not just .missing_pkgs) so packages that
# were installed this run are now properly counted as available.
failed_required <- .gexpipe_all_required[
  vapply(.gexpipe_all_required, function(pkg) {
    if (pkg == "parallel") return(FALSE)
    tryCatch(
      { utils::packageVersion(pkg, lib.loc = .gexpipe_lib); FALSE },
      error = function(e) TRUE
    )
  }, logical(1L))
]

# ==============================================================================
# STEP 5  \u2014  Load all libraries and show full package status table
# ==============================================================================
.gexpipe_load_quietly <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(FALSE)
  if (pkg == "parallel") {
    suppressPackageStartupMessages(library(parallel, quietly = TRUE))
    return(TRUE)
  }
  tryCatch({
    suppressPackageStartupMessages(
      library(pkg, character.only = TRUE, quietly = TRUE)
    )
    TRUE
  }, error = function(e) FALSE)
}

all_pkgs    <- c(.gexpipe_all_required, .gexpipe_all_optional)
n_total     <- length(all_pkgs)
loaded      <- character(0)
failed_load <- character(0)

cat("\n  [4/4] Loading", n_total, "libraries...\n")
cat("  ---------------------------------------------------------------\n")
cat(sprintf("  %-4s  %-24s  %-10s  %s\n", "No.", "Package", "Version", "Status"))
cat("  ---------------------------------------------------------------\n")

for (i in seq_along(all_pkgs)) {
  p <- all_pkgs[i]

  # Determine what action was taken this run
  action <- if      (p %in% .missing_pkgs)          "Installed"
            else if (p %in% .version_conflict_pkgs)  "Updated  "
            else if (p %in% .outdated_in_lib)         "Updated  "
            else if (p %in% .pending_pkgs)            "Pending  "
            else                                      "Ready    "

  if (.gexpipe_load_quietly(p)) {
    ver <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) "?")
    cat(sprintf("  [%2d] %-24s  %-10s  \u2713 %s\n", i, p, ver, action))
    loaded <- c(loaded, p)
  } else {
    cat(sprintf("  [%2d] %-24s  %-10s  \u2717 MISSING\n", i, p, "---"))
    failed_load <- c(failed_load, p)
  }
}
cat("  ---------------------------------------------------------------\n")

# ==============================================================================
# Runtime options
# ==============================================================================
options(shiny.maxRequestSize = 500 * 1024^2)
options(timeout = 3600)
options(stringsAsFactors = FALSE)

if (isNamespaceLoaded("WGCNA")) {
  tryCatch({
    if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA")))
      WGCNA::enableWGCNAThreads(nThreads = max(1L, parallel::detectCores() - 1L))
  }, error = function(e) NULL)
}

# ==============================================================================
# Summary
# ==============================================================================
cat("\n")
cat("  ===============================================================\n")
cat("  Loaded       :", length(loaded), "/", n_total, "packages\n")

n_installed_this_run <- length(intersect(loaded,  .missing_pkgs))
n_updated_this_run   <- length(intersect(loaded,  c(.version_conflict_pkgs, .outdated_in_lib)))
n_ready              <- length(loaded) - n_installed_this_run - n_updated_this_run

if (n_installed_this_run > 0L)
  cat("  \u2713 Installed    :", n_installed_this_run, "new package(s) this run\n")
if (n_updated_this_run > 0L)
  cat("  \u2713 Updated      :", n_updated_this_run, "package(s) this run\n")
if (n_ready > 0L)
  cat("  \u2713 Already OK   :", n_ready, "package(s) (skipped install)\n")
if (length(.pending_pkgs) > 0L)
  cat("  \u23f3 Pending      :", length(.pending_pkgs),
      "DLL-locked (restart R to apply:", paste(.pending_pkgs, collapse=", "), ")\n")

if (length(failed_required) > 0L) {
  cat("\n  \u2717 Still missing:", length(failed_required), "required package(s):\n")
  cat("    ", paste(failed_required, collapse = ", "), "\n\n")
  cat("  \u2192 Run again (network may have been interrupted), or paste this\n")
  cat("    in the R console to install manually:\n\n")
  cat('    BiocManager::install(c(\n')
  cat('      ', paste0('"', failed_required, '"', collapse = ",\n      "), '\n')
  cat('    ))\n\n')
  cat("  Some features may be unavailable until these are installed.\n")
} else {
  cat("\n  \u2713 Status       : All packages ready \u2014 opening app...\n")
}
cat("  ===============================================================\n\n")

# ==============================================================================
# HELPER LOADING (prefer package namespace; fallback to R/ source checkout)
# ==============================================================================
if (!requireNamespace("GExPipe", quietly = TRUE)) {
  helper_candidates <- c(
    file.path(getwd(), "..", "..", "R", "gexpipe_shiny_helpers.R"),
    file.path(getwd(), "R", "gexpipe_shiny_helpers.R")
  )
  helper_file <- helper_candidates[file.exists(helper_candidates)][1]
  if (is.na(helper_file) || !nzchar(helper_file)) {
    stop("GExPipe helper code not found. Run via GExPipe::runGExPipe() or from package root.")
  }
  source(helper_file, local = TRUE)
}

tryCatch({
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    to_pull <- c(
      "theme_publication", "palette_primary", ".gexpipe_log_file", "app_log", "safe_run",
      "IMAGE_DPI", "CSV_EXPORT_DIR",
      "detect_gene_id_format",
      "probe_ids_to_symbol_hugene_db", "probe_ids_to_symbol_gpl", "probe_ids_to_symbol_biomart",
      "map_microarray_ids", "entrez_to_symbol_biomart", "any_id_to_symbol",
      "get_platform_for_gse", "run_gse_annotation_and_download",
      "convert_ids_to_symbols_simple", "convert_rnaseq_ids",
      "download_ncbi_raw_counts", "download_ncbi_raw_counts_best",
      "read_count_matrix", "classify_groups",
      "normalize_microarray", "GPL_USE_OLIGO", "normalize_microarray_rma", "normalize_rnaseq",
      "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
      "gexp_fetch_geo_series_matrix_metadata", "gexp_no_common_genes_diagnostic_log",
      "gexp_rebuild_all_genes_list", "gexp_download_normalize_ids_for_overlap",
      "gexp_download_one_microarray_gse", "gexp_download_one_rnaseq_gse",
      "gexp_qc_detect_outliers", "gexp_qc_exclude_samples", "gexp_qc_gene_overlap_summary",
      "gexp_qc_prepare_venn_sets", "gexp_qc_prepare_upset_data",
      "gexp_qc_prepare_boxplot_data", "gexp_qc_prepare_density_data",
      "gexp_register_pipeline_observers",
      "gexp_register_navigation_observers",
      "gexp_register_workspace_observers",
      "gexp_register_help_observers",
      "gexp_user_guideline_modal_ui"
    )
    for (nm in to_pull) {
      if (exists(nm, envir = ns, inherits = FALSE))
        assign(nm, get(nm, envir = ns, inherits = FALSE), envir = environment())
    }
  }
}, error = function(e) NULL)

# Source any local R/ pipeline files (covers runGitHub checkout path)
local_r_files <- c(
  file.path(getwd(), "..", "..", "R", "gexpipe_shiny_helpers.R"),
  file.path(getwd(), "..", "..", "R", "utils_shiny_app.R"),
  file.path(getwd(), "..", "..", "R", "gexp_download_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_qc_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_normalize_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_batch_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_de_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "gexp_wgcna_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "observers_pipeline.R"),
  file.path(getwd(), "..", "..", "R", "observers_navigation.R"),
  file.path(getwd(), "..", "..", "R", "observers_workspace.R"),
  file.path(getwd(), "..", "..", "R", "observers_help.R"),
  file.path(getwd(), "R", "gexpipe_shiny_helpers.R"),
  file.path(getwd(), "R", "utils_shiny_app.R"),
  file.path(getwd(), "R", "gexp_download_pipeline.R"),
  file.path(getwd(), "R", "gexp_qc_pipeline.R"),
  file.path(getwd(), "R", "gexp_normalize_pipeline.R"),
  file.path(getwd(), "R", "gexp_batch_pipeline.R"),
  file.path(getwd(), "R", "gexp_de_pipeline.R"),
  file.path(getwd(), "R", "gexp_wgcna_pipeline.R"),
  file.path(getwd(), "R", "observers_pipeline.R"),
  file.path(getwd(), "R", "observers_navigation.R"),
  file.path(getwd(), "R", "observers_workspace.R"),
  file.path(getwd(), "R", "observers_help.R")
)
for (rf in unique(local_r_files[file.exists(local_r_files)]))
  tryCatch(source(rf, local = TRUE), error = function(e) NULL)

# Verify core functions are present
.gexpipe_has_core_fn <- function(nm) {
  if (exists(nm, mode = "function", inherits = TRUE)) return(TRUE)
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    if (exists(nm, envir = ns, inherits = FALSE, mode = "function")) return(TRUE)
  }
  FALSE
}

core_needed <- c(
  "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
  "gexp_download_normalize_ids_for_overlap", "gexp_download_one_microarray_gse",
  "gexp_download_one_rnaseq_gse", "gexp_qc_detect_outliers", "gexp_qc_exclude_samples",
  "gexp_register_pipeline_observers", "gexp_register_navigation_observers",
  "gexp_register_workspace_observers", "gexp_register_help_observers"
)
missing_core <- core_needed[!vapply(core_needed, .gexpipe_has_core_fn, logical(1))]
if (length(missing_core) > 0L) {
  stop(
    "Missing core GExPipe functions: ", paste(missing_core, collapse = ", "),
    ". Reinstall/update the package or run from the package root."
  )
}
