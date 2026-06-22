pkg_root <- Sys.getenv("GEXPIPE_ROOT", "e:/GExPipe")
desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"))
imp <- desc[1, "Imports"]
imp <- gsub("\n", " ", imp)
parts <- strsplit(imp, ",\\s*")[[1]]
pkgs <- vapply(parts, function(p) sub("\\s*\\(.*", "", p), character(1))
pkgs <- pkgs[pkgs != "parallel"]

lib <- file.path(Sys.getenv("LOCALAPPDATA"), "GExPipe",
                  paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor)))
out <- file.path(pkg_root, "inst", "pkg_versions_installed.local.txt")
rows <- lapply(sort(pkgs), function(p) {
  v <- tryCatch(
    as.character(utils::packageVersion(p, lib.loc = lib)),
    error = function(e) {
      tryCatch(as.character(utils::packageVersion(p)), error = function(e2) NA_character_)
    }
  )
  data.frame(package = p, installed_version = ifelse(is.na(v), "NOT_INSTALLED", v),
             stringsAsFactors = FALSE)
})
tab <- do.call(rbind, rows)
write.table(tab, out, sep = "\t", row.names = FALSE, quote = FALSE)
cat("Wrote", nrow(tab), "rows to", out, "\n")
