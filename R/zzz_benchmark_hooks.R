# ==============================================================================
# TEMPORARY - case-study Table S3 (env/runtime/memory) instrumentation.
#
# Adds NO behavior to the shipped app by default: everything here is a no-op
# unless the user explicitly opts in with
#   options(gexpipe.benchmark = TRUE)
# before calling GExPipe::runGExPipe(). It logs a "start"/"end" line (with
# wall-clock time and the OS's true peak-working-set for this R process) for
# every "Run"-type button across Steps 1-16, grouped into the 5 buckets used
# in the manuscript's Table S3 - by watching the SAME actionButtons the user
# already clicks, via companion observers at priority 1000 (START) and -1000 (END)
# (END fires right after the real handler, priority 0, has finished,
# i.e. right after the real handler's computation completes). No existing
# server_*.R file is modified.
#
# TO REMOVE once the case-study numbers are collected: delete this file (and
# the one line calling gexpipe_install_benchmark_hooks() in server_app.R) and
# reinstall - nothing else references it.
# ==============================================================================

.gexpipe_bench_log_path <- function() {
  getOption("gexpipe.benchmark_log", file.path(getwd(), "gexpipe_benchmark_log.csv"))
}

.gexpipe_bench_peak_mem_mb <- function() {
  pid <- Sys.getpid()
  if (.Platform$OS.type == "windows") {
    out <- tryCatch(
      system2("powershell", c(
        "-NoProfile", "-Command",
        sprintf("(Get-Process -Id %d).PeakWorkingSet64", pid)
      ), stdout = TRUE, stderr = FALSE),
      error = function(e) character(0)
    )
    bytes <- suppressWarnings(as.numeric(trimws(out[length(out)])))
    if (length(bytes) == 0L || is.na(bytes)) return(NA_real_)
    return(round(bytes / 1024^2, 1))
  }
  status_file <- "/proc/self/status"
  if (file.exists(status_file)) {
    ln <- grep("VmHWM:", readLines(status_file), value = TRUE)
    if (length(ln) == 1L) return(round(as.numeric(regmatches(ln, regexpr("[0-9]+", ln))) / 1024, 1))
  }
  NA_real_
}

.gexpipe_bench_mark <- function(event) {
  path <- .gexpipe_bench_log_path()
  if (!file.exists(path)) cat("timestamp,event,peak_mem_mb\n", file = path)
  line <- sprintf(
    "%s,%s,%s\n",
    format(Sys.time(), "%Y-%m-%d %H:%M:%OS3"), event, .gexpipe_bench_peak_mem_mb()
  )
  cat(line, file = path, append = TRUE)
  cat(line)
}

#' Install temporary Step 1-16 timing/memory hooks (opt-in; see file header)
#' @keywords internal
gexpipe_install_benchmark_hooks <- function(input, output, session, rv) {
  if (!isTRUE(getOption("gexpipe.benchmark", FALSE))) return(invisible(NULL))

  .gexpipe_bench_mark("SESSION_START")

  step_buttons <- list(
    "Steps 1-2 (download, normalisation)" = c(
      "start_processing", "apply_normalization", "apply_normalization_parallel"
    ),
    "Steps 3-7 (QC, groups, batch, DE, consensus)" = c(
      "run_outlier_detection", "exclude_outliers_btn", "apply_groups_btn",
      "apply_batch", "single_ds_apply_batch_covariate", "single_ds_confirm_skip",
      "run_de", "run_de_parallel", "compute_common_genes"
    ),
    "Step 8 (WGCNA)" = c(
      "prepare_wgcna", "wgcna_detect_outliers", "wgcna_apply_exclude", "wgcna_skip_outliers",
      "pick_soft_threshold", "run_wgcna", "calculate_module_trait", "calculate_me_relationships",
      "identify_significant_modules", "generate_gs_mm_all_modules"
    ),
    "Steps 9-11 (enrichment, PPI, ML)" = c(
      "run_go_enrichment", "run_kegg_enrichment", "run_ppi", "ppi_apply_gene_set", "run_ml"
    ),
    "Steps 12-16 (validation, ROC, nomogram, GSEA, report)" = c(
      "ext_val_download_btn", "ext_val_run_btn",
      "roc_confirm_gene_selection", "run_nomogram", "run_gsea"
    )
  )

  for (grp in names(step_buttons)) {
    for (btn in step_buttons[[grp]]) {
      local({
        grp_local <- grp
        btn_local <- btn
        # START runs before the real handler (priority 1000); END runs after it
        # and after any default-priority observers it triggers (priority -1000).
        # (session$onFlushed was unreliable: Shiny skips it while busy/closed.)
        shiny::observeEvent(input[[btn_local]], {
          .gexpipe_bench_mark(paste0("START | ", grp_local, " | ", btn_local))
        }, priority = 1000, ignoreInit = TRUE)
        shiny::observeEvent(input[[btn_local]], {
          .gexpipe_bench_mark(paste0("END   | ", grp_local, " | ", btn_local))
        }, priority = -1000, ignoreInit = TRUE)
      })
    }
  }

  invisible(NULL)
}
