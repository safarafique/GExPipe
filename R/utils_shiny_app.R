## Single source-of-truth for all GExPipe package names.
## Used by runGExPipe() (pre-launch install) and gexp_app_attach_packages() (load).
.gexpipe_all_pkgs <- function(include_optional = TRUE) {
  required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  core <- c(
    "affy", "oligo",
    "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
    "dplyr", "data.table", "edgeR", "sva", "ggplot2", "gridExtra",
    "RColorBrewer", "pheatmap", "ggrepel", "VennDiagram", "UpSetR",
    "WGCNA", "clusterProfiler", "enrichplot", "circlize", "STRINGdb",
    "DESeq2", "igraph", "ggraph", "tidygraph", "tidyr",
    "randomForest", "caret", "glmnet", "pROC", "kernlab",
    "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils",
    "dynamicTreeCut", "scales",
    "biomaRt", "Boruta", "car", "cicerone", "mixOmics",
    "xgboost", "SHAPforxgboost", "rms", "dcurves",
    "cli", "glue", "lifecycle", "rlang", "vctrs",
    "Matrix", "Rcpp", "withr", "pillar"
  )
  optional <- character(0)
  if (include_optional) c(required, core, optional) else c(required, core)
}

## Resolve and call an exported GExPipe helper (works from Shiny server modules and global.R).
.gexpipe_call <- function(name, ...) {
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    ns <- asNamespace("GExPipe")
    if (exists(name, envir = ns, inherits = FALSE, mode = "function")) {
      return(get(name, envir = ns, mode = "function")(...))
    }
  }
  if (exists(name, mode = "function", inherits = TRUE)) {
    return(get(name, mode = "function", inherits = TRUE)(...))
  }
  stop(
    "Function ", name, " is not available. Update GExPipe:\n",
    "  remotes::install_github('safarafique/GExPipe')\n",
    "Then restart R and run GExPipe::runGExPipe() again.",
    call. = FALSE
  )
}

## Minimum versions for packages that frequently cause version-conflict errors.
## Any package below its floor is treated as "needs update" even when installed.
# Minimum versions — must stay in sync with DESCRIPTION Imports section.
# Any package below its floor is treated as "needs update" even when installed.
# Direct version constraints copied from DESCRIPTION:
.gexpipe_min_versions <- c(
  rlang     = "1.1.0",
  cli       = "3.6.0",
  glue      = "1.6.0",
  lifecycle = "1.0.0",
  vctrs     = "0.6.0",
  Matrix    = "1.6.0",
  Rcpp      = "1.0.12",
  withr     = "2.5.0",
  pillar    = "1.9.0"
)

## Universal library helpers — same OS-aware, cloud-safe, two-lib logic as global.R.

## Return the R version string used as the library subdirectory name.
.gexpipe_rv_str <- function()
  paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor))

## OS-aware base directory (never inside a cloud-sync folder).
.gexpipe_lib_base <- function() {
  sysname <- Sys.info()[["sysname"]]
  if (.Platform$OS.type == "windows") {
    la <- Sys.getenv("LOCALAPPDATA", unset = "")
    if (nzchar(la)) return(la)
    ap <- Sys.getenv("APPDATA", unset = "")
    if (nzchar(ap)) return(ap)
    return(path.expand("~"))
  }
  if (sysname == "Darwin")
    return(file.path(path.expand("~"), "Library", "Application Support"))
  xdg <- Sys.getenv("XDG_DATA_HOME", unset = "")
  if (nzchar(xdg)) xdg else file.path(path.expand("~"), ".local", "share")
}

## Main library (parent session loads from here).
.gexpipe_get_lib <- function() {
  rv   <- .gexpipe_rv_str()
  base <- .gexpipe_lib_base()
  d    <- file.path(base, "GExPipe", rv)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  old_d <- file.path(path.expand("~"), ".gexpipe_packages", rv)
  if (dir.exists(old_d) && old_d != d && !old_d %in% .libPaths())
    .libPaths(c(.libPaths(), old_d))
  d
}

## Pending library (subprocess writes here; promoted → main on next startup).
.gexpipe_get_pending_lib <- function() {
  rv   <- .gexpipe_rv_str()
  base <- .gexpipe_lib_base()
  file.path(base, "GExPipe", paste0(rv, "-pending"))
}

## TRUE when launch-time package installation is allowed (GitHub / runGitHub workflow).
## FALSE when GExPipe is installed in a normal R library (Bioconductor / CRAN path).
.gexpipe_runtime_install_enabled <- function() {
  if (!interactive()) return(FALSE)
  if (isTRUE(getOption("shiny.testmode"))) return(FALSE)
  explicit <- getOption("gexpipe.auto_install", NULL)
  if (!is.null(explicit)) return(isTRUE(explicit))

  pkg_path <- tryCatch(
    normalizePath(system.file(package = "GExPipe"), winslash = "/", mustWork = FALSE),
    error = function(e) ""
  )
  if (!nzchar(pkg_path) || !dir.exists(pkg_path)) return(TRUE)

  isolated_root <- normalizePath(
    file.path(.gexpipe_lib_base(), "GExPipe"),
    winslash = "/", mustWork = FALSE
  )
  pkg_norm <- normalizePath(pkg_path, winslash = "/", mustWork = FALSE)
  if (startsWith(pkg_norm, paste0(isolated_root, "/"))) return(TRUE)
  FALSE
}

## Report missing Imports without installing (Bioconductor default).
.gexpipe_verify_imports <- function(
    pkgs = .gexpipe_all_pkgs(include_optional = TRUE),
    quiet = FALSE) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1L), quietly = TRUE)]
  if (length(missing) == 0L) return(invisible(character(0L)))
  msg <- paste0(
    "GExPipe: missing required package(s): ",
    paste(missing, collapse = ", "),
    "\n  Install with full dependencies:\n",
    "    BiocManager::install(\"GExPipe\", dependencies = TRUE)\n",
    "  GitHub auto-install: options(gexpipe.auto_install = TRUE) before runGExPipe()."
  )
  if (isTRUE(quiet)) message(msg) else warning(msg, call. = FALSE)
  invisible(missing)
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
  if (!.gexpipe_runtime_install_enabled()) {
    return(invisible(.gexpipe_verify_imports(pkgs, quiet = TRUE)))
  }

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
  # Set the Bioc release that matches the running R version (never force a higher
  # release — e.g. 3.22 on R 4.5 would break every BiocManager::install() call).
  .target_bioc_r <- local({
    rv <- tryCatch(
      numeric_version(paste0(R.Version()$major, ".",
                             sub("\\..*", "", R.Version()$minor))),
      error = function(e) numeric_version("4.4"))
    if      (rv >= "4.6") "3.22"
    else if (rv >= "4.5") "3.21"
    else if (rv >= "4.4") "3.20"
    else                  "3.19"
  })
  .bver <- tryCatch(as.character(BiocManager::version()), error = function(e) "?")
  if (!identical(.bver, .target_bioc_r)) {
    message("GExPipe: setting Bioconductor to ", .target_bioc_r,
            " (current: ", .bver, ")...")
    tryCatch(BiocManager::install(version = .target_bioc_r, ask = FALSE),
             error = function(e) NULL, warning = function(w) NULL)
  }

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
  # Two-library design: pending lib receives DLL-locked updates so they become
  # active on the NEXT startup without any manual restart. Safe packages go
  # directly to main lib and are immediately usable in this run.
  pending_lib <- .gexpipe_get_pending_lib()

  .dll_locked_in_parent <- function(pkg) {
    if (!isNamespaceLoaded(pkg)) return(FALSE)
    pkg_path <- tryCatch(find.package(pkg), error = function(e) "")
    nzchar(pkg_path) &&
      startsWith(normalizePath(pkg_path,   winslash = "/", mustWork = FALSE),
                 normalizePath(gexpipe_lib, winslash = "/", mustWork = FALSE))
  }
  pending_pkgs <- to_install[vapply(to_install, .dll_locked_in_parent, logical(1L))]
  safe_pkgs    <- setdiff(to_install, pending_pkgs)

  if (length(pending_pkgs) > 0L)
    message("GExPipe: DLL-locked (active on next startup via pending lib): ",
            paste(pending_pkgs, collapse = ", "))

  all_libs    <- unique(c(gexpipe_lib, .libPaths()))
  parent_libs <- paste0('"', gsub("\\\\", "/", all_libs), '"', collapse = ", ")
  lib_fwd     <- gsub("\\\\", "/", gexpipe_lib)
  pend_fwd    <- gsub("\\\\", "/", pending_lib)
  safe_vec    <- if (length(safe_pkgs)    > 0L) paste0('"', safe_pkgs,    '"', collapse=", ") else 'character(0)'
  pend_vec    <- if (length(pending_pkgs) > 0L) paste0('"', pending_pkgs, '"', collapse=", ") else 'character(0)'
  ncpus_val   <- max(1L, tryCatch(parallel::detectCores() - 1L, error = function(e) 1L))
  inst_opts   <- 'c("--no-staged-install", "--no-lock")'

  script <- c(
    paste0('.libPaths(c(', parent_libs, '))'),
    'for (.d in .libPaths()) {',
    '  .lk <- list.files(.d, pattern = "^00LOCK-", full.names = TRUE)',
    '  if (length(.lk)) unlink(.lk, recursive = TRUE, force = TRUE)',
    '}',
    'options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 2400L)',
    'if (.Platform$OS.type == "windows") {',
    '  .ok <- tryCatch({ utils::download.file("https://cloud.r-project.org",',
    '    tempfile(), quiet = TRUE, method = "libcurl"); TRUE },',
    '    error = function(e) FALSE, warning = function(w) FALSE)',
    '  options(download.file.method = if (.ok) "libcurl" else "wininet")',
    '} else { options(download.file.method = "libcurl") }',
    paste0('.main_lib    <- "', lib_fwd,     '"'),
    paste0('.pending_lib <- "', pend_fwd,    '"'),
    paste0('.safe_pkgs    <- c(', safe_vec,   ')'),
    paste0('.pending_pkgs <- c(', pend_vec,   ')'),
    paste0('.ncpus        <- ',   ncpus_val,  'L'),
    paste0('.bioc_ver     <- "',  .target_bioc_r, '"'),
    'if (!requireNamespace("BiocManager", quietly = TRUE))',
    '  install.packages("BiocManager", lib = .main_lib,',
    '                   repos = "https://cloud.r-project.org")',
    # Set correct Bioc release for this R version inside subprocess
    'if (!identical(as.character(BiocManager::version()), .bioc_ver))',
    '  tryCatch(BiocManager::install(version = .bioc_ver, ask = FALSE),',
    '           error = function(e) NULL, warning = function(w) NULL)',
    'if (length(.safe_pkgs) > 0L) {',
    '  message("GExPipe subprocess: ", length(.safe_pkgs), " pkg(s) → main lib")',
    '  BiocManager::install(.safe_pkgs, lib = .main_lib, ask = FALSE,',
    '    update = TRUE, force = TRUE, Ncpus = .ncpus,',
    paste0('    INSTALL_opts = ', inst_opts, ')'),
    '  .failed <- .safe_pkgs[!vapply(.safe_pkgs, function(p)',
    '    requireNamespace(p, lib.loc = .main_lib, quietly = TRUE), logical(1L))]',
    '  for (.p in .failed) {',
    '    message("GExPipe subprocess: retrying ", .p)',
    '    tryCatch(BiocManager::install(.p, lib = .main_lib, ask = FALSE,',
    '      force = TRUE, Ncpus = 1L,',
    paste0('      INSTALL_opts = ', inst_opts, '),'),
    '      error = function(e) message("  retry failed: ", conditionMessage(e)))',
    '  }',
    '}',
    'if (length(.pending_pkgs) > 0L) {',
    '  dir.create(.pending_lib, recursive = TRUE, showWarnings = FALSE)',
    '  message("GExPipe subprocess: ", length(.pending_pkgs),',
    '          " DLL-locked pkg(s) → pending lib (active next startup)")',
    '  BiocManager::install(.pending_pkgs, lib = .pending_lib, ask = FALSE,',
    '    update = TRUE, force = TRUE, Ncpus = .ncpus,',
    paste0('    INSTALL_opts = ', inst_opts, ')'),
    '}',
    '.old <- tryCatch(utils::old.packages(lib.loc = .main_lib,',
    '  repos = BiocManager::repositories()),',
    '  error = function(e) NULL, warning = function(w) NULL)',
    'if (!is.null(.old) && nrow(.old) > 0L) {',
    '  message("GExPipe subprocess: fixing ", nrow(.old), " transitive dep(s)")',
    '  BiocManager::install(rownames(.old), lib = .main_lib, ask = FALSE,',
    '    update = TRUE, force = TRUE, Ncpus = .ncpus,',
    paste0('    INSTALL_opts = ', inst_opts, ')'),
    '}'
  )

  tmp_script <- tempfile(pattern = "gexpipe_install_", fileext = ".R")
  log_file   <- tempfile(pattern = "gexpipe_install_log_", fileext = ".txt")
  on.exit({ unlink(tmp_script); unlink(log_file) }, add = TRUE)
  writeLines(script, tmp_script)

  rscript   <- file.path(R.home("bin"), "Rscript")
  exit_code <- tryCatch(
    system2(rscript,
            args    = c("--vanilla", "--no-save", shQuote(tmp_script)),
            stdout  = log_file, stderr = log_file,
            timeout = 2400L),
    error = function(e) { message("GExPipe: could not launch subprocess: ", conditionMessage(e)); 1L }
  )
  if (!identical(exit_code, 0L)) {
    message("GExPipe: subprocess exit code ", exit_code, " — last install log:")
    log_lines <- tryCatch(readLines(log_file, warn = FALSE), error = function(e) character(0))
    if (length(log_lines) > 0L)
      message(paste(utils::tail(log_lines, 30L), collapse = "\n"))
  }

  # ── Direct fallback for still-missing packages ─────────────────────────────
  # Missing packages have NO DLL lock (never loaded) → safe to install directly.
  .still_missing_u <- to_install[!vapply(to_install,
    function(p) requireNamespace(p, quietly = TRUE), logical(1L))]
  .direct_fb <- .still_missing_u[!vapply(.still_missing_u,
    .dll_locked_in_parent, logical(1L))]

  if (length(.direct_fb) > 0L) {
    message("GExPipe: fallback — direct install for ", length(.direct_fb),
            " still-missing package(s): ", paste(.direct_fb, collapse = ", "))
    for (.p in .direct_fb) {
      message("  → ", .p, " ...")
      tryCatch(
        BiocManager::install(.p, lib = gexpipe_lib, ask = FALSE,
                             update = FALSE, force = TRUE, Ncpus = 1L,
                             INSTALL_opts = c("--no-staged-install", "--no-lock")),
        error   = function(e) message("    failed: ", conditionMessage(e)),
        warning = function(w) NULL
      )
    }
  }

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
  if (!.gexpipe_runtime_install_enabled()) {
    .gexpipe_verify_imports(pkg, quiet = TRUE)
    return(FALSE)
  }
  .gexpipe_batch_install(pkg)
  requireNamespace(pkg, quietly = TRUE)
}

## Packages with compiled native code that must match the running R version.
.gexpipe_native_pkgs <- function() c("glmnet", "xgboost", "Matrix", "Rcpp")

## Return lib paths where pkg is installed (searches all .libPaths()).
.gexpipe_pkg_lib_paths <- function(pkg) {
  libs <- unique(c(.gexpipe_get_lib(), .libPaths()))
  libs[vapply(libs, function(lib) {
    dir.exists(file.path(lib, pkg))
  }, logical(1L))]
}

## TRUE if DESCRIPTION Built: field matches current R major.minor.
.gexpipe_pkg_built_for_current_r <- function(pkg, lib) {
  built <- tryCatch({
    desc <- utils::packageDescription(pkg, lib.loc = lib)
    if (is.null(desc)) return(NA)
    desc[["Built"]]
  }, error = function(e) NULL)
  if (is.null(built) || !nzchar(built)) return(NA)
  m <- regexpr("R [0-9]+\\.[0-9]+", built, perl = TRUE)
  if (m < 0L) return(NA)
  built_r <- numeric_version(sub("^R ", "", regmatches(built, m)))
  cur_r <- numeric_version(paste(R.Version()$major, R.Version()$minor, sep = "."))
  identical(built_r[1, 1:2], cur_r[1, 1:2])
}

## Lightweight glmnet native-code check (glmnet 4.x: glmnet_control; 5.x: glmnet.control).
.gexpipe_glmnet_smoke <- function() {
  if (!requireNamespace("glmnet", quietly = TRUE)) {
    return(FALSE)
  }
  ns <- asNamespace("glmnet")
  tryCatch({
    if (exists("glmnet.control", envir = ns, inherits = FALSE, mode = "function")) {
      get("glmnet.control", envir = ns, mode = "function")()
    } else if (exists("glmnet_control", envir = ns, inherits = FALSE, mode = "function")) {
      get("glmnet_control", envir = ns, mode = "function")()
    } else {
      x <- matrix(stats::rnorm(20), nrow = 2)
      y <- rep(0:1, each = 10)
      glmnet::cv.glmnet(x, y, family = "binomial", nfolds = 3)
    }
    TRUE
  }, error = function(e) FALSE)
}

## Smoke-test native code for a package loaded from a specific library.
.gexpipe_native_smoke_test <- function(pkg) {
  switch(pkg,
    glmnet  = .gexpipe_glmnet_smoke(),
    xgboost = { xgboost::xgb.DMatrix(matrix(0, 1, 1)); TRUE },
    Matrix  = { Matrix::Matrix(1); TRUE },
    Rcpp    = TRUE,
    TRUE
  )
}

## Detach a package from the search path and unload its namespace.
.gexpipe_detach_pkg <- function(pkg) {
  while (paste0("package:", pkg) %in% search()) {
    ok <- tryCatch(
      detach(paste0("package:", pkg), unload = TRUE, character.only = TRUE),
      error = function(e) FALSE
    )
    if (!isTRUE(ok)) break
  }
  if (isNamespaceLoaded(pkg)) {
    tryCatch(suppressWarnings(unloadNamespace(pkg)), error = function(e) NULL)
  }
  invisible(TRUE)
}

## Return TRUE when native code for pkg works in this R session (optional repair).
.gexpipe_native_session_ok <- function(pkg, lib = .gexpipe_get_lib(), try_repair = FALSE) {
  if (identical(pkg, "glmnet")) {
    ok <- .gexpipe_glmnet_smoke()
    if (ok || !try_repair) return(isTRUE(ok))
    .gexpipe_detach_pkg(pkg)
    tryCatch(.gexpipe_ensure_native_pkg(pkg, lib = lib, quiet = TRUE), error = function(e) NULL)
    return(isTRUE(.gexpipe_glmnet_smoke()))
  }
  ok <- tryCatch({
    if (!requireNamespace(pkg, lib.loc = lib, quietly = TRUE)) return(FALSE)
    if (!isNamespaceLoaded(pkg)) loadNamespace(pkg, lib.loc = lib)
    .gexpipe_native_smoke_test(pkg)
  }, error = function(e) FALSE)
  if (isTRUE(ok) || !isTRUE(try_repair)) return(isTRUE(ok))

  .gexpipe_detach_pkg(pkg)
  if (pkg %in% .gexpipe_native_pkgs()) {
    tryCatch(.gexpipe_ensure_native_pkg(pkg, lib = lib, quiet = TRUE), error = function(e) NULL)
  }
  tryCatch({
    if (!isNamespaceLoaded(pkg)) loadNamespace(pkg, lib.loc = lib)
    .gexpipe_native_smoke_test(pkg)
  }, error = function(e) FALSE)
}

## Run a short Rscript in a fresh R process (avoids Windows DLL locks in the parent).
.gexpipe_rscript_eval <- function(lines, timeout = 600L) {
  tmp_script <- tempfile(pattern = "gexpipe_", fileext = ".R")
  tmp_log <- tempfile(pattern = "gexpipe_log_", fileext = ".txt")
  on.exit(unlink(c(tmp_script, tmp_log)), add = TRUE)
  writeLines(lines, tmp_script)
  ec <- system2(
    file.path(R.home("bin"), "Rscript"),
    args = c("--vanilla", "--no-save", shQuote(normalizePath(tmp_script, winslash = "/"))),
    stdout = tmp_log,
    stderr = tmp_log,
    timeout = timeout
  )
  list(
    ok = identical(ec, 0L),
    exit = ec,
    log = tryCatch(readLines(tmp_log, warn = FALSE), error = function(e) character(0))
  )
}

## glmnet smoke test in a child R process (works when parent DLL is locked).
.gexpipe_glmnet_smoke_subprocess <- function(lib = .gexpipe_get_lib()) {
  libs_fwd <- vapply(unique(c(lib, .libPaths())), function(lp) {
    gsub("\\\\", "/", normalizePath(lp, winslash = "/", mustWork = FALSE))
  }, character(1L))
  libs_expr <- paste0('"', libs_fwd, '"', collapse = ", ")
  res <- .gexpipe_rscript_eval(c(
    paste0(".libPaths(c(", libs_expr, "))"),
    "suppressPackageStartupMessages(library(glmnet))",
    "if (exists('glmnet.control', mode='function')) glmnet::glmnet.control() else if (exists('glmnet_control', mode='function')) glmnet::glmnet_control() else stop('no glmnet control')",
    'cat("OK\\n")'
  ), timeout = 120L)
  isTRUE(res$ok) && any(grepl("^OK$", res$log))
}

## cv.glmnet in a child R process when the parent session cannot load glmnet's DLL.
.gexpipe_glmnet_cv_subprocess <- function(x, y, alpha = 1, family = "binomial",
                                          lib = .gexpipe_get_lib()) {
  tmp_x <- normalizePath(tempfile(pattern = "gexp_x_", fileext = ".rds"), winslash = "/", mustWork = FALSE)
  tmp_y <- normalizePath(tempfile(pattern = "gexp_y_", fileext = ".rds"), winslash = "/", mustWork = FALSE)
  tmp_out <- normalizePath(tempfile(pattern = "gexp_fit_", fileext = ".rds"), winslash = "/", mustWork = FALSE)
  on.exit(unlink(c(tmp_x, tmp_y, tmp_out)), add = TRUE)
  saveRDS(x, tmp_x)
  saveRDS(y, tmp_y)
  lib_fwd <- gsub("\\\\", "/", normalizePath(lib, winslash = "/", mustWork = FALSE))
  libs_fwd <- vapply(unique(c(lib, .libPaths())), function(lp) {
    gsub("\\\\", "/", normalizePath(lp, winslash = "/", mustWork = FALSE))
  }, character(1L))
  libs_expr <- paste0('"', libs_fwd, '"', collapse = ", ")
  res <- .gexpipe_rscript_eval(c(
    paste0(".libPaths(c(", libs_expr, "))"),
    "suppressPackageStartupMessages(library(glmnet))",
    paste0('x <- readRDS("', tmp_x, '")'),
    paste0('y <- readRDS("', tmp_y, '")'),
    paste0('fit <- glmnet::cv.glmnet(x, y, alpha = ', as.numeric(alpha),
           ', family = "', family, '")'),
    paste0('saveRDS(fit, "', tmp_out, '")')
  ), timeout = 1800L)
  if (!isTRUE(res$ok) || !file.exists(tmp_out)) {
    log_tail <- paste(utils::tail(res$log, 8L), collapse = "\n")
    stop("glmnet could not run in an isolated R process. ", log_tail, call. = FALSE)
  }
  readRDS(tmp_out)
}

## cv.glmnet in the current session, or subprocess fallback — no R restart required.
.gexpipe_glmnet_cv_fit <- function(x, y, alpha = 1, family = "binomial",
                                   lib = .gexpipe_get_lib()) {
  in_session <- tryCatch({
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("glmnet is not installed")
    }
    if (!isNamespaceLoaded("glmnet")) {
      loadNamespace("glmnet")
    }
    if (!isTRUE(.gexpipe_glmnet_smoke())) {
      stop("glmnet smoke test failed")
    }
    glmnet::cv.glmnet(x, y, alpha = alpha, family = family)
  }, error = function(e) NULL)
  if (inherits(in_session, "cv.glmnet")) {
    options(gexpipe.glmnet_subprocess = FALSE)
    return(in_session)
  }

  if (!isTRUE(.gexpipe_glmnet_smoke_subprocess(lib = lib))) {
    stop(
      "glmnet is not available in this R session or in an isolated process. ",
      "Restart R (Ctrl+Shift+F10), then run GExPipe::runGExPipe() again.",
      call. = FALSE
    )
  }
  options(gexpipe.glmnet_subprocess = TRUE)
  .gexpipe_glmnet_cv_subprocess(x, y, alpha = alpha, family = family, lib = lib)
}

## Unload pkg and remove it from every library on .libPaths().
.gexpipe_remove_pkg_all_libs <- function(pkg) {
  if (isNamespaceLoaded(pkg)) {
    tryCatch(suppressWarnings(unloadNamespace(pkg)), error = function(e) NULL)
  }
  for (lib in rev(unique(c(.gexpipe_get_lib(), .libPaths())))) {
    pkg_dir <- file.path(lib, pkg)
    if (!dir.exists(pkg_dir)) next
    tryCatch(utils::remove.packages(pkg, lib = lib), error = function(e) {
      unlink(pkg_dir, recursive = TRUE, force = TRUE)
    })
  }
  invisible(TRUE)
}

## Install a native package into gexpipe_lib (binary on Windows for current R).
.gexpipe_install_native_pkg <- function(pkg, lib) {
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  inst_type <- if (.Platform$OS.type == "windows") "binary" else "source"
  tryCatch(
    utils::install.packages(
      pkg,
      lib = lib,
      repos = "https://cloud.r-project.org",
      type = inst_type,
      quiet = TRUE
    ),
    error = function(e) NULL
  )
  if (!requireNamespace(pkg, lib.loc = lib, quietly = TRUE)) {
    if (!requireNamespace("BiocManager", quietly = TRUE)) {
      utils::install.packages(
        "BiocManager", lib = lib, repos = "https://cloud.r-project.org", quiet = TRUE
      )
    }
    BiocManager::install(
      pkg,
      lib = lib,
      ask = FALSE,
      force = TRUE,
      update = TRUE,
      INSTALL_opts = c("--no-staged-install", "--no-lock")
    )
  }
  requireNamespace(pkg, lib.loc = lib, quietly = TRUE)
}

## Ensure pkg works from gexpipe_lib: remove stale copies, reinstall, load, test.
## Returns TRUE when native code is usable in this session.
.gexpipe_ensure_native_pkg <- function(pkg, lib = .gexpipe_get_lib(), quiet = FALSE) {
  if (!quiet) {
    message("GExPipe: verifying native package ", pkg, " for R ",
            paste(R.Version()$major, R.Version()$minor, sep = "."), " ...")
  }

  .works <- function() {
    if (!requireNamespace(pkg, lib.loc = lib, quietly = TRUE)) return(FALSE)
    was_loaded <- isNamespaceLoaded(pkg)
    ok <- tryCatch({
      if (!was_loaded) loadNamespace(pkg, lib.loc = lib)
      .gexpipe_native_smoke_test(pkg)
    }, error = function(e) FALSE)
    if (!was_loaded && isNamespaceLoaded(pkg)) {
      tryCatch(suppressWarnings(unloadNamespace(pkg)), error = function(e) NULL)
    }
    isTRUE(ok)
  }

  # Reinstall before first load: avoids Windows DLL-lock requiring a restart.
  if (.gexpipe_runtime_install_enabled() &&
      !isNamespaceLoaded(pkg) &&
      requireNamespace(pkg, lib.loc = lib, quietly = TRUE) &&
      isFALSE(.gexpipe_pkg_built_for_current_r(pkg, lib))) {
    if (!quiet) {
      message("GExPipe: ", pkg, " was built for a different R version — ",
              "rebuilding before first load (no restart needed).")
    }
    .gexpipe_remove_pkg_all_libs(pkg)  # safe: not loaded, so the DLL isn't locked
    if (.gexpipe_install_native_pkg(pkg, lib) && .works()) {
      if (!quiet) message("GExPipe: ", pkg, " OK.")
      return(TRUE)
    }
  }

  if (.works()) return(TRUE)

  if (!.gexpipe_runtime_install_enabled()) {
    if (!quiet && !.works()) {
      message("GExPipe: ", pkg, " native code is not usable. Restart R or reinstall ",
              pkg, " for this R version.")
    }
    return(.works())
  }

  built_ok <- .gexpipe_pkg_built_for_current_r(pkg, lib)
  if (isFALSE(built_ok) || is.na(built_ok)) {
    if (!quiet) message("GExPipe: ", pkg, " was built for a different R version — reinstalling.")
  } else if (!quiet) {
    message("GExPipe: ", pkg, " native code failed smoke test — reinstalling.")
  }

  if (isNamespaceLoaded(pkg)) {
    # DLL locked in this session — reinstall, then try detach + reload once.
    if (!quiet) {
      message("GExPipe: ", pkg, " DLL is locked; reinstalling and retrying reload...")
    }
    .gexpipe_batch_install(pkg)
    .gexpipe_detach_pkg(pkg)
    if (.gexpipe_install_native_pkg(pkg, lib) && .works()) {
      if (!quiet) message("GExPipe: ", pkg, " OK after reload.")
      return(TRUE)
    }
    if (!quiet) {
      message("GExPipe: ", pkg, " reinstalled; parent session will use subprocess fallback where supported.")
    }
    return(FALSE)
  }

  .gexpipe_remove_pkg_all_libs(pkg)
  if (!.gexpipe_install_native_pkg(pkg, lib)) {
    if (!quiet) message("GExPipe: could not install ", pkg, " into ", lib)
    return(FALSE)
  }

  if (!.works()) {
    if (!quiet) message("GExPipe: ", pkg, " still broken after reinstall — restart R once.")
    return(FALSE)
  }
  if (!quiet) message("GExPipe: ", pkg, " OK.")
  TRUE
}

.gexpipe_ensure_all_native_pkgs <- function(quiet = FALSE) {
  all_ok <- TRUE
  for (pkg in .gexpipe_native_pkgs()) {
    ok <- tryCatch(
      .gexpipe_ensure_native_pkg(pkg, quiet = quiet),
      error = function(e) {
        if (!quiet) message("GExPipe: ", pkg, " check failed: ", conditionMessage(e))
        FALSE
      }
    )
    all_ok <- all_ok && isTRUE(ok)
  }
  all_ok
}

## Detect and repair a corrupted GExPipe lazy-load database (common after in-session reinstall).
.gexpipe_ensure_self <- function(quiet = FALSE) {
  if (!.gexpipe_runtime_install_enabled()) {
    return(invisible(requireNamespace("GExPipe", quietly = TRUE)))
  }
  if (!requireNamespace("GExPipe", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  smoke_ok <- tryCatch({
    utils::getFromNamespace("gexpipe_wgcna_heatmap_cor", "GExPipe")
    TRUE
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("corrupt|lazy-load", msg, ignore.case = TRUE)) {
      return(FALSE)
    }
    TRUE
  })
  if (isTRUE(smoke_ok)) {
    return(invisible(TRUE))
  }

  lib <- dirname(utils::find.package("GExPipe", quiet = TRUE))
  if (!nzchar(lib)) {
    return(invisible(FALSE))
  }
  if (!quiet) {
    message("GExPipe: corrupted install detected — reinstalling package into ", lib, " ...")
  }
  if (isNamespaceLoaded("GExPipe")) {
    tryCatch(suppressWarnings(unloadNamespace("GExPipe")), error = function(e) NULL)
  }
  tryCatch(utils::remove.packages("GExPipe", lib = lib), error = function(e) NULL)
  ok <- tryCatch({
    if (requireNamespace("remotes", quietly = TRUE)) {
      remotes::install_github(
        "safarafique/GExPipe",
        lib = lib,
        upgrade = "never",
        quiet = TRUE,
        force = TRUE
      )
    } else {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        utils::install.packages("BiocManager", repos = "https://cloud.r-project.org")
      }
      BiocManager::install(
        "safarafique/GExPipe",
        lib = lib,
        ask = FALSE,
        update = FALSE,
        quiet = TRUE
      )
    }
    requireNamespace("GExPipe", lib.loc = lib, quietly = TRUE)
  }, error = function(e) {
    if (!quiet) message("GExPipe: self-repair failed: ", conditionMessage(e))
    FALSE
  })
  if (!quiet && isTRUE(ok)) {
    message("GExPipe: package reinstalled. Restart R if errors persist.")
  }
  invisible(isTRUE(ok))
}

## Detect packages that are installed but whose compiled DLL is broken (e.g. after
## an R version upgrade).  Returns a character vector of broken package names.
## Only tests packages that are installed; missing packages are not included.
.gexpipe_detect_broken_pkgs <- function(pkgs) {
  # Per-package lightweight DLL smoke-tests.
  # Each entry is a zero-argument function that exercises native code.
  .dll_tests <- list(
    glmnet   = function() .gexpipe_glmnet_smoke(),
    Matrix   = function() Matrix::Matrix(1),
    Rcpp     = function() Rcpp::evalCpp("1 + 1"),
    xgboost  = function() xgboost::xgb.DMatrix(matrix(0, 1, 1)),
    glue     = function() glue::glue("x"),
    stringi  = function() stringi::stri_length("x")
  )
  broken <- character(0)
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) next  # missing, not broken
    # Try to load the namespace; catch DLL-load errors
    ns_err <- tryCatch({
      loadNamespace(pkg)
      NULL
    }, error = function(e) conditionMessage(e))
    dll_load_broken <- !is.null(ns_err) &&
      grepl("not available for .Call|LoadLibrary|shared object|compiled for a different|undefined symbol|DLL",
            ns_err, ignore.case = TRUE)
    if (dll_load_broken) {
      broken <- c(broken, pkg)
      next
    }
    # Additional smoke-test for packages with known DLL entry points
    if (pkg %in% names(.dll_tests)) {
      smoke_ok <- tryCatch({ .dll_tests[[pkg]](); TRUE }, error = function(e) {
        msg <- conditionMessage(e)
        !grepl("not available for .Call|not available|DLL|undefined symbol", msg, ignore.case = TRUE)
      })
      if (!isTRUE(smoke_ok)) broken <- c(broken, pkg)
    }
  }
  broken
}

## Auto-reinstall broken (DLL-mismatch) packages via the subprocess.
## Returns TRUE if all were fixed, FALSE if a restart is still required.
.gexpipe_fix_broken_pkgs <- function(broken_pkgs) {
  if (length(broken_pkgs) == 0L) return(TRUE)
  if (!.gexpipe_runtime_install_enabled()) {
    warning(
      "GExPipe: package(s) with broken native code detected: ",
      paste(broken_pkgs, collapse = ", "),
      ". Restart R and reinstall GExPipe dependencies.",
      call. = FALSE
    )
    return(FALSE)
  }
  message("GExPipe: ", length(broken_pkgs),
          " package(s) have broken native code (compiled for a different R version): ",
          paste(broken_pkgs, collapse = ", "))
  message("GExPipe: auto-reinstalling broken package(s)...")
  still_broken <- character(0)
  gexpipe_lib <- .gexpipe_get_lib()
  for (pkg in broken_pkgs) {
    if (pkg %in% .gexpipe_native_pkgs()) {
      fixed <- tryCatch(.gexpipe_ensure_native_pkg(pkg, lib = gexpipe_lib, quiet = TRUE),
                        error = function(e) FALSE)
    } else {
      .gexpipe_batch_install(pkg)
      fixed <- tryCatch({
        if (isNamespaceLoaded(pkg))
          suppressWarnings(unloadNamespace(pkg))
        loadNamespace(pkg, lib.loc = gexpipe_lib)
        TRUE
      }, error = function(e) FALSE)
    }
    if (!fixed) still_broken <- c(still_broken, pkg)
  }
  if (length(still_broken) > 0L) {
    message(
      "GExPipe: ", paste(still_broken, collapse = ", "),
      " reinstalled but DLL is locked — restart R once to apply.\n",
      "  (RStudio: Ctrl+Shift+F10, then re-run GExPipe::runGExPipe())"
    )
    options(gexpipe.restart_required = TRUE)
    options(gexpipe.still_conflicted = c(
      getOption("gexpipe.still_conflicted", character(0)),
      still_broken
    ))
    return(FALSE)
  }
  message("GExPipe: broken package(s) fixed — proceeding.")
  TRUE
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

  # ── Native packages (glmnet, xgboost, …) must match this R version ─────────
  tryCatch(.gexpipe_ensure_all_native_pkgs(quiet = TRUE), error = function(e) NULL)

  # ── Detect and auto-fix any other broken DLL packages before attach ─────────
  broken_pre <- .gexpipe_detect_broken_pkgs(pkgs)
  if (length(broken_pre) > 0L) {
    .gexpipe_fix_broken_pkgs(broken_pre)
  }

  hard_required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  missing  <- character(0)
  loaded   <- character(0)
  failed   <- character(0)
  broken   <- character(0)

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
        err_msg <- conditionMessage(e)
        is_dll_err <- grepl(
          "not available for .Call|LoadLibrary|shared object|compiled for a different|undefined symbol|DLL",
          err_msg, ignore.case = TRUE)
        if (is_dll_err) {
          message(idx, " ", label, "... \u26A0 broken DLL (auto-fixing...)")
          broken <<- c(broken, p)
        } else {
          message(idx, " ", label, "... \u2717 load error")
        }
        failed <<- c(failed, p)
      }
    )
  }

  # Auto-fix any broken packages detected during attach
  if (length(broken) > 0L) {
    fixed <- .gexpipe_fix_broken_pkgs(broken)
    if (fixed) {
      # Try to attach newly fixed packages
      for (p in broken) {
        tryCatch({
          pkg_search <- paste0("package:", p)
          if (!pkg_search %in% search())
            base::attachNamespace(asNamespace(p))
          failed <<- setdiff(failed, p)
          loaded  <<- c(loaded, p)
          message("  [fix] ", p, " \u2713 reloaded successfully")
        }, error = function(e) NULL)
      }
    }
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
  # Only use getwd()-relative paths when we are genuinely running from the
  # GExPipe *source* root (development mode).  When running from a GitHub
  # install the working directory may be any folder, and checking it first
  # would pick up stale/old UI files from old local copies (e.g. GExpipe_1/).
  #
  # Dev-mode detection: getwd() must have a DESCRIPTION file that says
  # "Package: GExPipe".  Any other directory (user's home, Documents, an old
  # project copy, etc.) skips local-file lookup and goes straight to the
  # installed package files via system.file().
  .is_gexpipe_src_root <- function(dir) {
    desc <- file.path(dir, "DESCRIPTION")
    if (!file.exists(desc)) return(FALSE)
    lines <- tryCatch(readLines(desc, n = 10L, warn = FALSE), error = function(e) character(0))
    any(grepl("^Package:\\s*GExPipe\\s*$", lines))
  }

  dev_mode <- .is_gexpipe_src_root(getwd()) ||
              .is_gexpipe_src_root(file.path(getwd(), ".."))

  if (dev_mode) {
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
  }

  # Installed package location (primary path when not in dev mode).
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
