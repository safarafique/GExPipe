#!/usr/bin/env Rscript
# ==============================================================================
# Turn the Shiny benchmark log(s) written by R/zzz_benchmark_hooks.R into the
# case-study "software environment, run time and memory" table.
#
# Usage (from R, after running the app once per mode - see the steps below):
#   source(system.file("scripts", "summarise-benchmark-log.R", package = "GExPipe"))
#   tab <- gexpipe_summarise_bench(c(
#     "Merged mode"   = "bench_merged.csv",
#     "Parallel mode" = "bench_parallel.csv"
#   ))
#   print(tab)
#   write.csv(tab, "case_study_runtime_memory_table.csv", row.names = FALSE)
#
# Minutes = sum of (END - START) for every "Run" button in that step group, so
# it measures compute time only - time you spend reading plots or choosing
# options between clicks is NOT counted. Peak memory = highest OS peak working
# set (Windows PeakWorkingSet64 / Linux VmHWM) seen in that log.
# ==============================================================================

.bench_groups <- c(
  "Steps 1-2 (download, normalisation)",
  "Steps 3-7 (QC, groups, batch, DE, consensus)",
  "Step 8 (WGCNA)",
  "Steps 9-11 (enrichment, PPI, ML)",
  "Steps 12-16 (validation, ROC, nomogram, GSEA, report)"
)

gexpipe_bench_minutes <- function(log_file) {
  # Event names contain unquoted commas, so split manually:
  # first field = timestamp, last field = peak MB, everything between = event
  ln <- readLines(log_file)[-1]
  ln <- ln[nzchar(ln)]
  x <- data.frame(
    timestamp = sub(",.*$", "", ln),
    event = sub("^[^,]*,(.*),[^,]*$", "\\1", ln),
    peak_mem_mb = suppressWarnings(as.numeric(sub("^.*,", "", ln))),
    stringsAsFactors = FALSE
  )
  x$time <- as.POSIXct(strptime(x$timestamp, "%Y-%m-%d %H:%M:%OS"))
  ev <- x[grepl("^(START|END)", x$event), , drop = FALSE]
  parts <- strsplit(ev$event, " \\| ")
  ev$kind <- trimws(vapply(parts, `[`, "", 1))
  ev$group <- trimws(vapply(parts, `[`, "", 2))
  ev$button <- trimws(vapply(parts, `[`, "", 3))

  # Pair each START with the next END of the same button
  secs <- stats::setNames(numeric(length(.bench_groups)), .bench_groups)
  open <- list()
  for (i in seq_len(nrow(ev))) {
    key <- ev$button[i]
    if (ev$kind[i] == "START") {
      open[[key]] <- ev$time[i]
    } else if (!is.null(open[[key]])) {
      g <- ev$group[i]
      if (g %in% names(secs)) {
        secs[[g]] <- secs[[g]] + as.numeric(difftime(ev$time[i], open[[key]], units = "secs"))
      }
      open[[key]] <- NULL
    }
  }
  if (length(open)) {
    warning(log_file, ": START without END for ", paste(names(open), collapse = ", "),
            " (app closed mid-run?) - those clicks are ignored.")
  }
  list(minutes = secs / 60, peak_gb = max(x$peak_mem_mb, na.rm = TRUE) / 1024)
}

gexpipe_summarise_bench <- function(log_files) {
  cols <- lapply(log_files, gexpipe_bench_minutes)
  row_names <- c(
    "Steps 1-2 (download, normalisation), min",
    "Steps 3-7 (QC, groups, batch, DE, consensus), min",
    "Step 8 (WGCNA), min",
    "Steps 9-11 (enrichment, PPI, ML), min",
    "Steps 12-16 (validation, ROC, nomogram, GSEA, report), min",
    "Total, min",
    "Peak memory, GB"
  )
  out <- data.frame(Item = row_names, stringsAsFactors = FALSE)
  for (mode in names(cols)) {
    m <- cols[[mode]]$minutes
    out[[mode]] <- c(round(m, 1), round(sum(m), 1), round(cols[[mode]]$peak_gb, 2))
  }
  out
}
