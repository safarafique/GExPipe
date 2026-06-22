# Internal: verify DESCRIPTION, pkg_versions.txt, and install lists match.
pkg_root <- if (nzchar(Sys.getenv("GEXPIPE_ROOT"))) Sys.getenv("GEXPIPE_ROOT") else {
  cand <- normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(cand, "DESCRIPTION"))) cand else getwd()
}
desc_path <- file.path(pkg_root, "DESCRIPTION")
pkg_ver_path <- file.path(pkg_root, "inst", "pkg_versions.txt")

parse_desc_imports <- function(path) {
  lines <- readLines(path, warn = FALSE)
  start <- which(grepl("^Imports:", lines))[1]
  stopifnot(!is.na(start))
  end <- which(grepl("^[A-Za-z]+:", lines) & seq_along(lines) > start)[1] - 1L
  block <- paste(gsub("^\\s+", "", lines[(start + 1L):end]), collapse = " ")
  parts <- strsplit(block, ",\\s*")[[1]]
  out <- setNames(character(length(parts)), nm <- character(length(parts)))
  for (i in seq_along(parts)) {
    p <- parts[i]
    m <- regexec("^([A-Za-z][A-Za-z0-9.]*)\\s*(?:\\(>=\\s*([^)]+)\\))?", p)
    g <- regmatches(p, m)[[1]]
    out[i] <- if (length(g) >= 3 && nzchar(g[3])) g[3] else NA_character_
    names(out)[i] <- g[2]
  }
  out[!names(out) %in% c("parallel")]
}

desc <- parse_desc_imports(desc_path)
if (!file.exists(pkg_ver_path)) stop("Missing ", pkg_ver_path)
pv <- read.delim(pkg_ver_path, stringsAsFactors = FALSE, check.names = FALSE)
names(pv) <- tolower(names(pv))
if (!all(c("package", "min_version") %in% names(pv))) {
  stop("pkg_versions.txt must have columns: package, min_version")
}
pv_vec <- setNames(pv$min_version, pv$package)

# Load install lists from source (without full package load)
utils_path <- file.path(pkg_root, "R", "utils_shiny_app.R")
u_lines <- readLines(utils_path, warn = FALSE)
core_start <- grep("core <- c\\(", u_lines)[1]
core_end <- grep("^  \\)$", u_lines)
core_end <- core_end[core_end > core_start][1]
core_block <- paste(u_lines[(core_start + 1):(core_end - 1)], collapse = "\n")
core_pkgs <- gsub('.*"([^"]+)".*', "\\1", strsplit(core_block, "\n")[[1]])
core_pkgs <- core_pkgs[nzchar(core_pkgs)]
req_start <- grep(".gexpipe_all_required <- c\\(", readLines(file.path(pkg_root, "inst", "shinyapp", "global.R"), warn = FALSE))[1]
# simpler: eval from utils
source(utils_path, local = env <- new.env())
all_pkgs <- env$.gexpipe_all_pkgs(TRUE)

desc_names <- sort(names(desc))
all_names <- sort(setdiff(all_pkgs, "parallel"))
pv_names <- sort(pv$package)

cat("=== GExPipe package alignment report ===\n")
cat("Package root:", pkg_root, "\n")
cat("R:", R.version.string, "\n\n")

miss_desc <- setdiff(all_names, desc_names)
extra_desc <- setdiff(desc_names, all_names)
miss_pv <- setdiff(desc_names, pv_names)
extra_pv <- setdiff(pv_names, desc_names)
ver_mismatch <- character(0)
for (p in intersect(desc_names, pv_names)) {
  if (!identical(as.character(desc[p]), as.character(pv_vec[p])))
    ver_mismatch <- c(ver_mismatch, sprintf("%s: DESCRIPTION>=%s vs pkg_versions=%s", p, desc[p], pv_vec[p]))
}

cat("DESCRIPTION Imports:", length(desc_names), "\n")
cat("pkg_versions.txt:", length(pv_names), "\n")
cat(".gexpipe_all_pkgs():", length(all_names), "(excl. parallel)\n\n")

if (length(miss_desc)) cat("In install list but NOT in DESCRIPTION:\n ", paste(miss_desc, collapse = ", "), "\n\n")
if (length(extra_desc)) cat("In DESCRIPTION but NOT in install list:\n ", paste(extra_desc, collapse = ", "), "\n\n")
if (length(miss_pv)) cat("In DESCRIPTION but NOT in pkg_versions.txt:\n ", paste(miss_pv, collapse = ", "), "\n\n")
if (length(extra_pv)) cat("In pkg_versions.txt but NOT in DESCRIPTION:\n ", paste(extra_pv, collapse = ", "), "\n\n")
if (length(ver_mismatch)) {
  cat("Version floor mismatches:\n")
  cat(paste0("  ", ver_mismatch, collapse = "\n"), "\n\n")
}

aligned <- !length(miss_desc) && !length(extra_desc) && !length(miss_pv) && !length(extra_pv) && !length(ver_mismatch)
cat(if (aligned) "METADATA: ALIGNED\n" else "METADATA: MISALIGNED\n")

# Installed versions
gexpipe_lib <- file.path(Sys.getenv("LOCALAPPDATA"), "GExPipe", paste0(R.Version()$major, ".", sub("\\..*", "", R.Version()$minor)))
libs <- unique(c(gexpipe_lib, .libPaths()))
cat("\nGExPipe library:", gexpipe_lib, "exists:", dir.exists(gexpipe_lib), "\n\n")

inst_ver <- function(pkg) {
  for (lib in libs) {
    v <- tryCatch(as.character(utils::packageVersion(pkg, lib.loc = lib)), error = function(e) NA_character_)
    if (!is.na(v)) return(v)
  }
  NA_character_
}

below <- missing <- character(0)
for (p in desc_names) {
  v <- inst_ver(p)
  if (is.na(v)) missing <- c(missing, p)
  else if (!is.na(desc[p]) && package_version(v) < package_version(desc[p]))
    below <- c(below, sprintf("%s installed %s < required %s", p, v, desc[p]))
}

if (length(missing)) {
  cat("NOT INSTALLED (", length(missing), "):\n  ", paste(head(missing, 20), collapse = ", "),
      if (length(missing) > 20) " ..." else "", "\n\n", sep = "")
} else cat("All DESCRIPTION Imports are installed.\n\n")

if (length(below)) {
  cat("BELOW minimum (", length(below), "):\n  ", paste(head(below, 15), collapse = "\n  "), "\n\n", sep = "")
} else if (!length(missing)) cat("All installed packages meet DESCRIPTION minimums.\n\n")

invisible(list(aligned = aligned, missing = missing, below = below))
