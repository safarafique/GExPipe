suppressMessages(pkgload::load_all(".", quiet = TRUE))

destdir <- file.path(tempdir(), "new_behavior_test")
dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
gse_id <- "GSE13159"

# Deliberately tiny budget so this test finishes fast: 2 attempts x 5s each,
# nowhere near enough to complete an 886MB file - simulates hitting the
# retry limit, to verify the partial file is KEPT (not deleted) and the
# caller gets a clear, informative stop instead of a crash.
res <- .gexpipe_resumable_prefetch_series_matrix(gse_id, destdir, max_attempts = 2L, per_attempt_timeout = 5L)
cat("prefetch result:\n"); str(res)

f <- file.path(destdir, paste0(gse_id, "_series_matrix.txt.gz"))
cat("\npartial file KEPT on disk:", file.exists(f), " size:", if (file.exists(f)) file.info(f)$size else 0, "\n")

cat("\n=== Now call .gexpipe_getgeo_series() with the SAME destdir - should stop with a clear message, not crash ===\n")
out <- tryCatch(
  .gexpipe_getgeo_series(gse_id, destdir = destdir),
  error = function(e) { cat("STOPPED WITH:", conditionMessage(e), "\n"); "caught cleanly" }
)
cat("\nresult class:", class(out)[1], "\n")

cat("\nfile still present after that call (not silently deleted):", file.exists(f), "\n")
