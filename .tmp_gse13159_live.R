suppressMessages(pkgload::load_all(".", quiet = TRUE))
destdir <- "D:/GExPipe/inst/shinyapp/micro_data"
dir.create(destdir, showWarnings = FALSE, recursive = TRUE)
f <- file.path(destdir, "GSE13159_series_matrix.txt.gz")

url <- "https://ftp.ncbi.nlm.nih.gov/geo/series/GSE13nnn/GSE13159/matrix/GSE13159_series_matrix.txt.gz"
cl <- .gexpipe_get_content_length(url)
cat("expected total size:", round(cl/1e6, 1), "MB\n")

t0 <- Sys.time()
ok <- .gexpipe_resumable_prefetch_series_matrix("GSE13159", destdir)
elapsed <- round(as.numeric(Sys.time() - t0), 1)
cat("prefetch result:", ok, " elapsed sec:", elapsed, "\n")
cat("final file exists:", file.exists(f), " size:", if (file.exists(f)) file.info(f)$size else 0, "\n")
