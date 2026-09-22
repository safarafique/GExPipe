#!/usr/bin/env Rscript
# ==============================================================================
# Case-study "software environment, runtime, and memory" benchmark harness.
#
# Fills in the manuscript table:
#   Computer (CPU/cores/RAM/OS), R/Bioconductor version,
#   Steps 1-2 / 3-7 / 8 / 9-11 / 12-16 wall-clock minutes, Total, Peak memory.
#
# Accuracy notes (read before running):
#   - Peak memory uses the OS's own historical high-water mark for THIS
#     process (Windows: PeakWorkingSet64; Linux: /proc/self/status VmHWM).
#     That counter only grows for the life of the process, so it can be read
#     at any point (even once, at the very end) and still reflect the true
#     peak - unlike gc()/memory.size() snapshots, which can miss a spike that
#     happened between two calls.
#   - Run "Merged mode" and "Parallel mode" as TWO SEPARATE Rscript processes
#     (not one after another in the same session). The OS peak counter never
#     resets, so running both in one session would let mode 1's peak leak
#     into mode 2's number.
#   - Run on an otherwise idle machine; consider discarding a warm-up run
#     (package/JIT loading, first GEO download caching) and reporting the
#     timed run, or the mean of >=3 runs, and say so in the Methods text.
# ==============================================================================

suppressPackageStartupMessages(library(GExPipe))

# ------------------------------------------------------------------------
# 1) True OS-level peak memory (MB) for this R process, no polling needed.
# ------------------------------------------------------------------------
gexpipe_peak_mem_mb <- function() {
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
    return(bytes / 1024^2)
  }
  status_file <- "/proc/self/status"
  if (file.exists(status_file)) {   # Linux: VmHWM = "high water mark" RSS
    ln <- grep("VmHWM:", readLines(status_file), value = TRUE)
    if (length(ln) == 1L) {
      kb <- as.numeric(regmatches(ln, regexpr("[0-9]+", ln)))
      return(kb / 1024)
    }
  }
  # macOS / last-resort fallback: current RSS only (not a true historical
  # peak) - sample this frequently if you rely on this branch.
  out <- tryCatch(system2("ps", c("-o", "rss=", "-p", pid), stdout = TRUE),
                   error = function(e) character(0))
  kb <- suppressWarnings(as.numeric(trimws(out)))
  if (length(kb) == 0L || is.na(kb)) NA_real_ else kb / 1024
}

# ------------------------------------------------------------------------
# 2) Timing harness - wrap each report step-group in step_timer(label, {...})
# ------------------------------------------------------------------------
.bench_log <- new.env(parent = emptyenv())
.bench_log$steps <- list()

step_timer <- function(step_label, expr) {
  gc(FALSE)  # reduce carry-over noise from the previous step's garbage
  t0 <- proc.time()[["elapsed"]]
  result <- force(expr)
  elapsed_min <- (proc.time()[["elapsed"]] - t0) / 60
  peak_mb <- gexpipe_peak_mem_mb()
  .bench_log$steps[[step_label]] <- list(minutes = elapsed_min, peak_mb = peak_mb)
  cat(sprintf("[%s] %.2f min | OS peak-so-far: %.0f MB\n", step_label, elapsed_min, peak_mb))
  invisible(result)
}

# ------------------------------------------------------------------------
# 3) One-time environment info (CPU, cores, RAM, OS, R/Bioc/GExPipe version)
# ------------------------------------------------------------------------
gexpipe_env_info <- function() {
  os <- if (.Platform$OS.type == "windows") {
    tryCatch({
      v <- system2("wmic", c("os", "get", "Caption"), stdout = TRUE)
      trimws(v[nzchar(trimws(v))][2])
    }, error = function(e) paste(Sys.info()[["sysname"]], Sys.info()[["release"]]))
  } else {
    paste(Sys.info()[["sysname"]], Sys.info()[["release"]])
  }

  cpu <- if (.Platform$OS.type == "windows") {
    tryCatch({
      v <- system2("wmic", c("cpu", "get", "Name"), stdout = TRUE)
      trimws(v[nzchar(trimws(v))][2])
    }, error = function(e) NA_character_)
  } else if (file.exists("/proc/cpuinfo")) {
    ln <- grep("model name", readLines("/proc/cpuinfo"), value = TRUE)
    if (length(ln)) trimws(sub(".*:", "", ln[1])) else NA_character_
  } else if (Sys.info()[["sysname"]] == "Darwin") {
    tryCatch(trimws(system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE)),
             error = function(e) NA_character_)
  } else NA_character_

  ram_gb <- if (.Platform$OS.type == "windows") {
    tryCatch({
      v <- system2("wmic", c("computersystem", "get", "TotalPhysicalMemory"), stdout = TRUE)
      b <- as.numeric(trimws(v[nzchar(trimws(v))][2]))
      round(b / 1024^3, 1)
    }, error = function(e) NA_real_)
  } else if (file.exists("/proc/meminfo")) {
    ln <- grep("MemTotal:", readLines("/proc/meminfo"), value = TRUE)
    round(as.numeric(regmatches(ln, regexpr("[0-9]+", ln))) / 1024^2, 1)
  } else NA_real_

  bioc_ver <- tryCatch(as.character(BiocManager::version()), error = function(e) NA_character_)

  data.frame(
    CPU = cpu,
    Cores_logical = parallel::detectCores(logical = TRUE),
    Cores_physical = parallel::detectCores(logical = FALSE),
    RAM_GB = ram_gb,
    OS = os,
    R_version = R.version.string,
    Bioconductor_version = bioc_ver,
    GExPipe_version = as.character(tryCatch(utils::packageVersion("GExPipe"), error = function(e) NA)),
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------------------
# 4) Build the manuscript-table row for one mode, after all step_timer()
#    calls for that run have completed.
# ------------------------------------------------------------------------
gexpipe_build_report_row <- function(mode_label) {
  s <- .bench_log$steps
  need <- c(
    "Steps 1-2 (download, normalisation)",
    "Steps 3-7 (QC, groups, batch, DE, consensus)",
    "Step 8 (WGCNA)",
    "Steps 9-11 (enrichment, PPI, ML)",
    "Steps 12-16 (validation, ROC, nomogram, GSEA, report)"
  )
  missing <- setdiff(need, names(s))
  if (length(missing)) {
    stop("Missing step_timer() calls for: ", paste(missing, collapse = ", "))
  }
  mins <- vapply(s[need], function(x) x$minutes, numeric(1))
  peaks <- vapply(s[need], function(x) x$peak_mb, numeric(1))
  data.frame(
    Mode = mode_label,
    `Steps 1-2, min` = round(mins[[1]], 2),
    `Steps 3-7, min` = round(mins[[2]], 2),
    `Step 8, min` = round(mins[[3]], 2),
    `Steps 9-11, min` = round(mins[[4]], 2),
    `Steps 12-16, min` = round(mins[[5]], 2),
    `Total, min` = round(sum(mins), 2),
    `Peak memory, GB` = round(max(peaks, na.rm = TRUE) / 1024, 2),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# ==============================================================================
# USAGE (run each mode as its own `Rscript` invocation - see notes above):
#
#   mode      <- "merged"    # or "parallel"
#   keep_sep  <- identical(mode, "parallel")
#   work_dir  <- file.path(getwd(), "case_study_data")
#   dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)
#
#   step_timer("Steps 1-2 (download, normalisation)", {
#     ... your existing download + gexp_normalize_and_intersect(...,
#         keep_platforms_separate = keep_sep) code ...
#   })
#
#   step_timer("Steps 3-7 (QC, groups, batch, DE, consensus)", {
#     ... your existing QC / group assignment / batch correction / DE /
#         consensus code ...
#   })
#
#   step_timer("Step 8 (WGCNA)", {
#     ... your existing WGCNA code (e.g. top 5000 genes, as already drafted) ...
#   })
#
#   step_timer("Steps 9-11 (enrichment, PPI, ML)", {
#     ... your existing GO/KEGG, STRINGdb PPI, and ML feature-selection code ...
#   })
#
#   step_timer("Steps 12-16 (validation, ROC, nomogram, GSEA, report)", {
#     ... your existing external validation, ROC, nomogram, GSEA, report code ...
#   })
#
#   env_info <- gexpipe_env_info()
#   row      <- gexpipe_build_report_row(if (keep_sep) "Parallel mode" else "Merged mode")
#
#   write.csv(env_info, sprintf("env_info_%s.csv", mode), row.names = FALSE)
#   write.csv(row,      sprintf("timing_memory_%s.csv", mode), row.names = FALSE)
#
# Then, after running BOTH modes (two separate Rscript calls), stitch the
# two CSVs together into the final table:
#
#   merged   <- read.csv("timing_memory_merged.csv",   check.names = FALSE)
#   parallel <- read.csv("timing_memory_parallel.csv", check.names = FALSE)
#   final_table <- rbind(merged, parallel)
#   print(final_table)
#   write.csv(final_table, "case_study_runtime_memory_table.csv", row.names = FALSE)
# ==============================================================================
