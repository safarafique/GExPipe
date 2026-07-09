#' Run the GExPipe Shiny application
#'
#' Launches the GExPipe Shiny app for multi-omics RNA analysis (bulk RNA-seq,
#' microarray, GEO download, QC, normalization, differential expression,
#' WGCNA, pathway enrichment, PPI, machine learning, and more).
#'
#' @param launch.browser If TRUE, open the app in the default browser (default).
#'   Set to FALSE in Google Colab or headless servers (no local browser).
#' @param port TCP port for Shiny. Use a **positive** port (e.g. `3838`) when you will open
#'   the app manually in a browser. **`0`** means "pick a free port" (fine for automated
#'   tests); some setups print `http://127.0.0.1:0`, which **cannot** be pasted into a
#'   browser - then use `launch.browser = TRUE` or `port = 3838`.
#' @param host The host to bind. Use "0.0.0.0" in Google Colab so port forwarding works.
#' @section Startup time:
#' When `GExPipe` is installed from Bioconductor (or any normal R library),
#' dependencies must be installed at package install time via
#' `BiocManager::install("GExPipe", dependencies = TRUE)`. `runGExPipe()`
#' then verifies imports and opens the app without downloading packages.
#'
#' For the GitHub / first-run workflow, set
#' `options(gexpipe.auto_install = TRUE)` before `runGExPipe()` to enable
#' background dependency installation (10-30 minutes on a fresh machine).
#'
#' The welcome screen loads first; the full 15-tab dashboard is built only
#' after the user clicks **Go to Analysis**, so the initial browser response is
#' fast even on slow networks.  Prefer `GExPipe::runGExPipe()` from an
#' **installed** package rather than opening `inst/shinyapp/` in RStudio
#' directly (that path runs `global.R`, which duplicates the install logic).
#'
#' Optional tuning before `runGExPipe()`:
#' * `options(gexpipe.wgcna_threads = 1L)` - fewer WGCNA threads (less parallel setup noise).
#' * `options(gexpipe.wgcna_threads = 0L)` or `FALSE` - skip [WGCNA::enableWGCNAThreads()] at app start.
#' * `options(shiny.testmode = TRUE)` with `options(gexpipe.minimal_attach_in_testmode = TRUE)` (default
#'   when unset) attaches only the Shiny stack first, then the rest after the first flush, so
#'   automated tests can connect quickly. Set `gexpipe.minimal_attach_in_testmode = FALSE` for a
#'   full attach on the first tick (e.g. deep `shinytest2` scenarios).
#' @section PPI (STRINGdb):
#' Install with full dependencies so [STRINGdb] support packages resolve:
#' `BiocManager::install("GExPipe", dependencies = TRUE)`. STRINGdb is installed as a dependency.
#' The first PPI run may download large STRING files (requires network). If initialization fails,
#' try `options(gexpipe.stringdb_try_versions = c("11.5", "11", "12"))` or update [STRINGdb].
#' @return A Shiny app object. Users should run it with [shiny::runApp()], which
#'   starts the server and **blocks** the R session until the app is stopped
#'   (e.g. RStudio Stop button).
#' @export
#' @examples
#' # Non-interactive (R CMD check / example()): build an app object only.
#' # Dependency auto-install is skipped when interactive() is FALSE, so this
#' # runs quickly without touching the network.
#' app <- runGExPipe(launch.browser = FALSE, port = 0L)
#' stopifnot(inherits(app, "shiny.appobj"))
#'
#' # Interactive: two-step launch. runGExPipe() builds the app object
#' # (installing any missing packages on first run); shiny::runApp() starts it.
#' if (interactive()) {
#'   app <- GExPipe::runGExPipe()      # Step 1: build the app object
#'   shiny::runApp(app, port = 3838L)  # Step 2: start the server
#' }
runGExPipe <- function(launch.browser = TRUE, port = getOption("shiny.port", 3838), host = getOption("shiny.host", "127.0.0.1")) {
  # When running from the package source tree, load latest R/ code (not stale install).
  if (interactive() && !isTRUE(getOption("gexpipe.no_auto_dev_load"))) {
    desc <- file.path(getwd(), "DESCRIPTION")
    if (file.exists(desc)) {
      hdr <- tryCatch(readLines(desc, n = 12L, warn = FALSE), error = function(e) character(0))
      if (any(grepl("^Package:\\s*GExPipe\\s*$", hdr))) {
        if (requireNamespace("pkgload", quietly = TRUE)) {
          message("GExPipe: loading latest source from ", normalizePath(getwd(), winslash = "/"))
          pkgload::load_all(getwd(), quiet = TRUE, export_all = FALSE)
          options(gexpipe.run_source = "source-tree")
        } else if (requireNamespace("devtools", quietly = TRUE)) {
          message("GExPipe: loading latest source from ", normalizePath(getwd(), winslash = "/"))
          devtools::load_all(getwd(), quiet = TRUE)
          options(gexpipe.run_source = "source-tree")
        }
      }
    }
  }
  # Bioconductor Shiny guidance: do not launch the app inside the package.
  # This function must return a Shiny app object.
  port <- as.integer(port)
  options(shiny.launch.browser = isTRUE(launch.browser))
  options(shiny.host = host)
  options(shiny.port = port)
  options(gexpipe.attach.done = NULL)
  options(gexpipe.attach.shiny_stack_only_done = NULL)
  options(gexpipe.attach.allow_full_now = NULL)
  options(gexpipe.prelaunch_install_done = NULL)

  # -- Auto-install all missing dependencies BEFORE the app opens --------------
  # This runs only in interactive sessions (not during R CMD check or vignette
  # build, where interactive() is FALSE). The user sees a progress log in the
  # console; the browser only opens once everything is ready.
  if (interactive() && !isTRUE(getOption("shiny.testmode"))) {
    all_pkgs <- .gexpipe_all_pkgs(include_optional = TRUE)

    if (.gexpipe_runtime_install_enabled()) {
      # -- GitHub / auto-install workflow ---------------------------------------
      missing_now <- all_pkgs[
        !vapply(all_pkgs, requireNamespace, logical(1L), quietly = TRUE)
      ]

      version_conflict_now <- names(.gexpipe_min_versions)[
        vapply(names(.gexpipe_min_versions), function(pkg) {
          tryCatch(
            .gexpipe_best_version(pkg) < package_version(.gexpipe_min_versions[[pkg]]),
            error = function(e) FALSE
          )
        }, logical(1L))
      ]

      needs_install <- length(missing_now) > 0L || length(version_conflict_now) > 0L

      tryCatch(.gexpipe_ensure_all_native_pkgs(quiet = FALSE), error = function(e) {
        message("GExPipe: native package check note: ", conditionMessage(e))
      })
      tryCatch(.gexpipe_ensure_self(quiet = FALSE), error = function(e) {
        message("GExPipe: package integrity check note: ", conditionMessage(e))
      })

      if (needs_install) {
        if (length(missing_now) > 0L)
          message("\nGExPipe: ", length(missing_now), " missing package(s):\n",
                  "  ", paste(missing_now, collapse = ", "))
        if (length(version_conflict_now) > 0L)
          message("GExPipe: ", length(version_conflict_now),
                  " version-conflict(s) (below minimum required version):\n",
                  "  ", paste(version_conflict_now, collapse = ", "))
        message(
          "\nGExPipe: updating via background subprocess ",
          "(bypasses DLL locks - no session restart needed).\n",
          "  First run: up to 40 min.  Subsequent runs: seconds.\n"
        )
        .gexpipe_batch_install(all_pkgs)
        still_missing <- all_pkgs[
          !vapply(all_pkgs, requireNamespace, logical(1L), quietly = TRUE)
        ]
        if (length(still_missing) > 0L) {
          message("GExPipe: could not install: ", paste(still_missing, collapse = ", "),
                  "\n  Run  gexpipe_setup()  for a detailed install log.")
        } else {
          message("GExPipe: all packages ready.\n")
        }
      }
    } else {
      .gexpipe_verify_imports(all_pkgs, quiet = FALSE)
    }

    glm_ok <- tryCatch({
      if (exists(".gexpipe_native_session_ok", mode = "function")) {
        isTRUE(.gexpipe_native_session_ok("glmnet", try_repair = TRUE))
      } else if (exists(".gexpipe_glmnet_smoke", mode = "function")) {
        isTRUE(.gexpipe_glmnet_smoke())
      } else {
        FALSE
      }
    }, error = function(e) FALSE)
    if (isTRUE(glm_ok)) {
      message("GExPipe: glmnet OK for LASSO / Elastic Net / Ridge in this session.")
    } else if (exists(".gexpipe_glmnet_smoke_subprocess", mode = "function") &&
               isTRUE(.gexpipe_glmnet_smoke_subprocess())) {
      message("GExPipe: glmnet will use an isolated R process for LASSO / Elastic Net / Ridge (no restart needed).")
    } else {
      message("GExPipe: glmnet not ready - restart R (Ctrl+Shift+F10), then run GExPipe::runGExPipe() before ML.")
    }
  }
  # Signal to gexp_app_attach_packages() that install already ran - skip repeat.
  options(gexpipe.prelaunch_install_done = TRUE)
  # ----------------------------------------------------------------------------

  if (interactive() && identical(port, 0L) && !isTRUE(launch.browser)) {
    message(
      "GExPipe: port = 0 with launch.browser = FALSE - if the console later shows http://127.0.0.1:0, ",
      "that address will not work in a browser. Prefer launch.browser = TRUE, or port = 3838, ",
      "and pass the same port to shiny::runApp(app, port = ...)."
    )
  }

  if (interactive()) {
    message(
      "GExPipe ", utils::packageVersion("GExPipe"),
      " - app object ready (UI builds on first browser load; first load can take several minutes)."
    )
  }
  app <- shiny::shinyApp(
    ui = function() {
      if (interactive()) {
        message("GExPipe: building Shiny UI (first session)...")
      }
      gexp_app_ui()
    },
    server = gexp_app_server,
    onStart = gexp_app_onStart
  )
  if (interactive()) {
    message("GExPipe: call shiny::runApp(app) to start the server (Listening on http://...).")
  }
  if (isTRUE(launch.browser)) {
    url <- sprintf("http://%s:%s", if (host == "0.0.0.0") "127.0.0.1" else host, port)
    message("GExPipe: app ready at ", url, " (run with runApp(app, host = host, port = port)).")
  }
  app
}
