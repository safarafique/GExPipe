gexp_app_attach_packages <- function() {
  required <- c("shiny", "shinydashboard", "shinyjs", "DT")
  optional <- c(
    "cicerone",
    "dplyr", "data.table",
    "ggplot2", "gridExtra", "RColorBrewer", "pheatmap", "ggrepel",
    "VennDiagram", "UpSetR",
    "Biobase", "GEOquery", "limma", "edgeR", "sva", "DESeq2",
    "clusterProfiler", "enrichplot", "circlize",
    "STRINGdb", "igraph", "ggraph", "tidygraph", "tidyr",
    "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
    "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils",
    "dynamicTreeCut", "scales", "WGCNA"
  )

  pkgs <- unique(c(required, optional))

  missing <- character(0)
  for (p in pkgs) {
    if (!requireNamespace(p, quietly = TRUE)) {
      if (p %in% required) missing <- c(missing, p)
      next
    }
    tryCatch({
      pkg_search <- paste0("package:", p)
      if (!pkg_search %in% search()) {
        # Source-loaded Shiny modules rely on unqualified symbols; attach namespace.
        base::attachNamespace(asNamespace(p))
      }
    }, error = function(e) NULL)
  }

  if (length(missing) > 0L) {
    stop(
      "Missing required packages for the Shiny app: ",
      paste(missing, collapse = ", "),
      ". Install them (e.g. via BiocManager::install()) and retry."
    )
  }
}

gexp_app_onStart <- function() {
  gexp_app_attach_packages()
  options(stringsAsFactors = FALSE)
  options(shiny.maxRequestSize = 500 * 1024^2) # 500 MB
  options(timeout = 3600) # allow long GEO downloads

  if (isNamespaceLoaded("WGCNA")) {
    tryCatch({
      if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA"))) {
        WGCNA::enableWGCNAThreads(nThreads = max(1L, parallel::detectCores() - 1L))
      }
    }, error = function(e) NULL)
  }
}

.gexp_inst_file <- function(rel_path) {
  # Prefer installed package location; fall back to source checkout.
  p <- system.file(rel_path, package = "GExPipe")
  if (nzchar(p) && file.exists(p)) return(p)

  # Compatibility: if we request R/shiny_src files, map to installed inst/shinyapp
  # copies (needed because installed packages do not expose R source files as paths).
  if (startsWith(rel_path, "shiny_src/")) {
    alt_rel <- sub("^shiny_src/", "shinyapp/", rel_path)
    p2 <- system.file(alt_rel, package = "GExPipe")
    if (nzchar(p2) && file.exists(p2)) return(p2)
  }

  candidates <- c(
    file.path(getwd(), "R", rel_path),
    file.path(getwd(), "..", "R", rel_path),
    file.path(getwd(), "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "..", "inst", sub("^shiny_src/", "shinyapp/", rel_path)),
    file.path(getwd(), "inst", rel_path),
    file.path(getwd(), "..", "inst", rel_path)
  )
  hits <- candidates[file.exists(candidates)]
  if (length(hits) < 1L) {
    stop("GExPipe: could not locate inst file: ", rel_path,
         ". Install the package or run from package root.")
  }
  hits[1]
}

