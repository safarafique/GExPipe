## GitHub / runGitHub bootstrap for inst/shinyapp (no runtime source() of R/ modules).
## Called from inst/shinyapp/global.R after the package namespace is available.

.gexpipe_core_namespace_fns <- function() {
  c(
    "gexp_parse_gse_inputs", "gexp_prepare_download_dirs", "gexp_download_finalize_common_genes",
    "gexp_download_normalize_ids_for_overlap", "gexp_download_one_microarray_gse",
    "gexp_download_one_rnaseq_gse", "gexp_qc_detect_outliers", "gexp_qc_exclude_samples",
    "gexp_register_pipeline_observers", "gexp_register_navigation_observers",
    "gexp_register_workspace_observers", "gexp_register_help_observers"
  )
}

.gexpipe_installed_in_system_library <- function() {
  if (!requireNamespace("GExPipe", quietly = TRUE)) {
    return(FALSE)
  }
  pkg_path <- tryCatch(
    normalizePath(system.file(package = "GExPipe"), winslash = "/", mustWork = FALSE),
    error = function(e) ""
  )
  if (!nzchar(pkg_path) || !dir.exists(pkg_path)) {
    return(FALSE)
  }
  isolated_root <- normalizePath(
    file.path(.gexpipe_lib_base_dir(), "GExPipe"),
    winslash = "/", mustWork = FALSE
  )
  !startsWith(
    normalizePath(pkg_path, winslash = "/", mustWork = FALSE),
    paste0(isolated_root, "/")
  )
}

.gexpipe_verify_core_namespace <- function() {
  ns <- asNamespace("GExPipe")
  missing_core <- .gexpipe_core_namespace_fns()[
    !vapply(.gexpipe_core_namespace_fns(), function(nm) {
      exists(nm, envir = ns, inherits = FALSE, mode = "function")
    }, logical(1L))
  ]
  if (length(missing_core) > 0L) {
    stop(
      "Missing core GExPipe functions: ", paste(missing_core, collapse = ", "),
      ". Reinstall/update the package or run GExPipe::runGExPipe().",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.gexpipe_find_clone_src <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  for (d in unique(c(
    normalizePath(file.path(wd, "..", ".."), winslash = "/", mustWork = FALSE),
    wd
  ))) {
    desc <- file.path(d, "DESCRIPTION")
    if (!file.exists(desc)) {
      next
    }
    hdr <- tryCatch(readLines(desc, n = 12L, warn = FALSE), error = function(e) character(0))
    if (any(grepl("^Package:\\s*GExPipe\\s*$", hdr))) {
      return(d)
    }
  }
  NULL
}

#' Load GExPipe from a GitHub clone (pkgload or source install into isolated lib).
#' @keywords internal
gexpipe_shinyapp_ensure_package <- function() {
  if (requireNamespace("GExPipe", quietly = TRUE)) {
    return(invisible(TRUE))
  }
  src <- .gexpipe_find_clone_src()
  if (is.null(src)) {
    return(invisible(FALSE))
  }
  if (requireNamespace("pkgload", quietly = TRUE)) {
    ok <- tryCatch({
      pkgload::load_all(src, quiet = TRUE, export_all = FALSE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok) && requireNamespace("GExPipe", quietly = TRUE)) {
      return(invisible(TRUE))
    }
  }
  if (requireNamespace("devtools", quietly = TRUE)) {
    ok <- tryCatch({
      devtools::load_all(src, quiet = TRUE)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok) && requireNamespace("GExPipe", quietly = TRUE)) {
      return(invisible(TRUE))
    }
  }
  invisible(FALSE)
}

.gexpipe_promote_pending_lib <- function() {
  pending_lib <- .gexpipe_get_pending_lib()
  main_lib <- .gexpipe_get_lib()
  if (!dir.exists(pending_lib)) {
    return(invisible(NULL))
  }
  items <- list.dirs(pending_lib, recursive = FALSE, full.names = TRUE)
  if (length(items) == 0L) {
    unlink(pending_lib, recursive = TRUE)
    return(invisible(NULL))
  }
  n_ok <- 0L
  for (src in items) {
    dst <- file.path(main_lib, basename(src))
    tryCatch(if (dir.exists(dst)) unlink(dst, recursive = TRUE, force = TRUE), error = function(e) NULL)
    ok <- tryCatch(isTRUE(file.rename(src, dst)), error = function(e) FALSE)
    if (!ok) {
      ok <- tryCatch({
        file.copy(src, main_lib, recursive = TRUE, overwrite = TRUE)
        unlink(src, recursive = TRUE)
        TRUE
      }, error = function(e) FALSE)
    }
    if (ok) {
      n_ok <- n_ok + 1L
    }
  }
  if (n_ok > 0L) {
    message("GExPipe: promoted ", n_ok, " pending package update(s) to active library.")
  }
  if (length(list.files(pending_lib, all.files = TRUE, no.. = TRUE)) == 0L) {
    unlink(pending_lib, recursive = TRUE)
  }
  invisible(NULL)
}

.gexpipe_warn_cloud_sync_lib <- function(lib) {
  p <- normalizePath(lib, winslash = "/", mustWork = FALSE)
  for (pat in c("OneDrive", "Dropbox", "Google Drive", "iCloud", "Box Sync", "Mega")) {
    if (grepl(pat, p, ignore.case = TRUE)) {
      message(
        "GExPipe WARNING: library path is inside a cloud-sync folder (", pat, ").\n",
        "  Set LOCALAPPDATA to a non-synced path if installs fail."
      )
      break
    }
  }
  invisible(NULL)
}

.gexpipe_shinyapp_attach_ready <- function(run_source = c("github-clone", "installed", "bootstrap")) {
  run_source <- match.arg(run_source)
  options(gexpipe.run_source = run_source)
  tryCatch(gexp_app_attach_packages(), error = function(e) {
    warning("GExPipe attach: ", conditionMessage(e), call. = FALSE)
  })
  .gexpipe_verify_core_namespace()
  invisible(TRUE)
}

.gexpipe_shinyapp_blocked_app <- function(failed_required) {
  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(
      "body{font-family:monospace;background:#1e1e1e;color:#f8f8f8;padding:40px;}
       h2{color:#ff6b6b;} code{background:#333;padding:2px 6px;border-radius:3px;}
       .pkg{color:#ff9800;font-weight:bold;} .fix{color:#69db7c;}"
    )),
    shiny::h2("GExPipe could not start — missing packages"),
    shiny::p("The following required packages are not installed:"),
    shiny::tags$ul(lapply(failed_required, function(p) {
      shiny::tags$li(shiny::tags$span(class = "pkg", p))
    })),
    shiny::hr(),
    shiny::p(shiny::tags$strong("Fix — run in R:")),
    shiny::tags$pre(class = "fix", paste0(
      'options(timeout = 3600)\n',
      'BiocManager::install(c(\n  ',
      paste0('"', failed_required, '"', collapse = ',\n  '),
      '\n))\nGExPipe::runGExPipe()'
    ))
  )
  server <- function(input, output, session) {}
  list(status = "blocked", ui = ui, server = server)
}

.gexpipe_missing_required_pkgs <- function(pkgs, lib) {
  pkgs[vapply(pkgs, function(pkg) {
    if (identical(pkg, "parallel")) {
      return(FALSE)
    }
    tryCatch({
      utils::packageVersion(pkg, lib.loc = lib)
      FALSE
    }, error = function(e) TRUE)
  }, logical(1L))]
}

#' Bootstrap dependencies for inst/shinyapp (runGitHub / first-run workflow).
#'
#' Returns a list with `status` of `"ready"` or `"blocked"`. When blocked,
#' `ui` and `server` hold a minimal error Shiny app.
#'
#' @param verbose Logical; print progress messages.
#' @keywords internal
gexpipe_shinyapp_bootstrap <- function(verbose = TRUE) {
  if (verbose) {
    message("\n======================================================================")
    message("  GExPipe — Gene Expression Pipeline")
    message("  R ", R.version.string)
    message("======================================================================")
  }

  options(timeout = 3600)
  options(
    BiocManager.ask = FALSE,
    install.packages.check.source = "no"
  )

  clone_src <- .gexpipe_find_clone_src()
  loaded_clone <- FALSE
  if (!is.null(clone_src) && requireNamespace("GExPipe", quietly = TRUE)) {
    pkg_path <- tryCatch(
      normalizePath(system.file(package = "GExPipe"), winslash = "/", mustWork = FALSE),
      error = function(e) ""
    )
    loaded_clone <- nzchar(pkg_path) && startsWith(
      normalizePath(pkg_path, winslash = "/", mustWork = FALSE),
      normalizePath(clone_src, winslash = "/", mustWork = FALSE)
    )
  }

  if (isTRUE(loaded_clone)) {
    if (verbose) {
      message("  Mode         : GitHub checkout (loaded from clone, not old install)")
      message("  Source       : ", clone_src)
    }
    .gexpipe_shinyapp_attach_ready("github-clone")
    if (verbose) {
      message("  Status       : ready\n")
    }
    return(list(status = "ready", ui = NULL, server = NULL))
  }

  if (.gexpipe_installed_in_system_library()) {
    if (verbose) {
      message("  Mode         : installed GExPipe package")
    }
    .gexpipe_shinyapp_attach_ready("installed")
    if (verbose) {
      message("  Status       : ready (prefer GExPipe::runGExPipe() for future runs)\n")
    }
    return(list(status = "ready", ui = NULL, server = NULL))
  }

  gexpipe_lib <- .gexpipe_get_lib()
  .gexpipe_promote_pending_lib()
  if (!gexpipe_lib %in% .libPaths()) {
    .libPaths(c(gexpipe_lib, unique(.libPaths())))
  }
  options(gexpipe.lib = gexpipe_lib)
  .gexpipe_warn_cloud_sync_lib(gexpipe_lib)

  if (verbose) {
    message("  Library      : ", gexpipe_lib)
    message("  Mode         : GitHub first-run bootstrap")
  }

  if (!requireNamespace("GExPipe", quietly = TRUE)) {
    stop(
      "GExPipe is not installed. Install pkgload and re-run runGitHub(), or use:\n",
      "  BiocManager::install('GExPipe', dependencies = TRUE)\n",
      "  GExPipe::runGExPipe()",
      call. = FALSE
    )
  }

  if (isTRUE(.gexpipe_runtime_install_enabled())) {
    if (verbose) {
      message("  Installing/updating dependencies (first run may take 10–40 min)...")
    }
    options(gexpipe.prelaunch_install_done = NULL)
    still_missing <- .gexpipe_batch_install(.gexpipe_all_pkgs(include_optional = TRUE))
    if (isTRUE(getOption("gexpipe.restart_required", FALSE))) {
      stop(
        "GExPipe: restart R to apply package updates, then run again.\n",
        "  RStudio: Ctrl+Shift+F10, then re-run shiny::runGitHub(...) or GExPipe::runGExPipe().",
        call. = FALSE
      )
    }
    if (length(still_missing) > 0L && verbose) {
      message("  Retrying: ", paste(still_missing, collapse = ", "))
      for (pkg in still_missing) {
        tryCatch(
          .gexpipe_bioc_install(pkg, lib = gexpipe_lib, ask = FALSE, update = FALSE, quiet = TRUE),
          error = function(e) NULL
        )
      }
    }
  } else if (verbose) {
    message("  Auto-install skipped (use BiocManager::install('GExPipe', dependencies = TRUE)).")
  }

  failed_required <- .gexpipe_missing_required_pkgs(.gexpipe_all_pkgs(include_optional = TRUE), gexpipe_lib)
  if (length(failed_required) > 0L) {
    if (verbose) {
      message("  Status       : blocked — missing ", length(failed_required), " required package(s)\n")
    }
    return(.gexpipe_shinyapp_blocked_app(failed_required))
  }

  options(gexpipe.run_source = "bootstrap")
  tryCatch(gexp_app_attach_packages(), error = function(e) {
    warning("GExPipe attach: ", conditionMessage(e), call. = FALSE)
  })
  .gexpipe_verify_core_namespace()

  options(shiny.maxRequestSize = 500 * 1024^2)
  options(stringsAsFactors = FALSE)

  if (verbose) {
    message("  Status       : ready — opening app...\n")
  }
  list(status = "ready", ui = NULL, server = NULL)
}
