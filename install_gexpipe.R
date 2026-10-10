# ==============================================================================
# GExPipe one-line installer + launcher
#
#   source("https://raw.githubusercontent.com/safarafique/GExPipe/main/install_gexpipe.R")
#
# Run it in a FRESH R session (RStudio: Ctrl+Shift+F10 first). Every time, it:
#   1. sets up BiocManager and the CRAN + Bioconductor repositories,
#   2. installs/updates GExPipe from GitHub when it is missing, older than the
#      GitHub version, missing a dependency, or fails to load (outdated deps),
#      updating out-of-date packages so CRAN/Bioconductor versions match,
#   3. opens the Shiny app.
# Already up to date? Step 2 is skipped and the app opens straight away.
# ==============================================================================

local({
  repo <- "safarafique/GExPipe"
  ref  <- getOption("gexpipe.ref", "main")
  port <- getOption("gexpipe.port", 3838L)

  options(timeout = max(600, getOption("timeout")))
  if (.Platform$OS.type == "windows") {
    # Use ready-made Windows binaries; building from source needs Rtools.
    options(install.packages.compile.from.source = "never")
  }
  say <- function(...) message("\n[GExPipe] ", ...)

  if (getRversion() < "4.6.0") {
    stop("GExPipe needs R >= 4.6.0 (you have ", getRversion(), "). ",
         "Install the latest R from https://cran.r-project.org and run this again.",
         call. = FALSE)
  }

  # -- 1. BiocManager + repositories -----------------------------------------
  for (p in c("BiocManager", "remotes")) {
    if (!nzchar(system.file(package = p))) {
      utils::install.packages(p, repos = "https://cloud.r-project.org")
    }
  }
  options(repos = suppressMessages(BiocManager::repositories()))

  # -- 2. What does GitHub have, and what is installed? ----------------------
  # (checked WITHOUT loading packages, so nothing gets locked before updating)
  desc <- tryCatch(read.dcf(url(sprintf(
    "https://raw.githubusercontent.com/%s/%s/DESCRIPTION", repo, ref))),
    error = function(e) NULL)
  gh_ver <- if (!is.null(desc)) desc[1, "Version"] else NA_character_
  deps <- if (!is.null(desc)) {
    x <- trimws(sub("\\(.*", "", unlist(strsplit(desc[1, "Imports"], ","))))
    setdiff(x[nzchar(x)], rownames(utils::installed.packages(priority = "base")))
  } else character(0)
  deps <- unique(c(deps, "shiny"))

  installed <- function() rownames(utils::installed.packages())
  inst_ver  <- function() {
    v <- system.file("DESCRIPTION", package = "GExPipe")
    if (nzchar(v)) read.dcf(v, fields = "Version")[1, 1] else NA_character_
  }
  # Test-load GExPipe in a separate R process (keeps this session unlocked)
  loads_ok <- function() {
    rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    suppressWarnings(system2(rs, c("-e", shQuote("loadNamespace('GExPipe')")),
                             stdout = FALSE, stderr = FALSE)) == 0
  }

  v <- inst_ver()
  reason <- if (is.na(v)) "not installed"
    else if (!is.na(gh_ver) && utils::compareVersion(gh_ver, v) > 0) paste(v, "->", gh_ver)
    else if (length(setdiff(deps, installed()))) "missing dependencies"
    else if (!loads_ok()) "outdated dependencies"
    else NULL

  if (!is.null(reason)) {
    if ("GExPipe" %in% loadedNamespaces()) {
      stop("GExPipe is already loaded. Restart R (RStudio: Ctrl+Shift+F10) and run this again.",
           call. = FALSE)
    }
    say("Installing/updating GExPipe (", reason, "). First time takes 15-30 min ...")

    # -- 3. Dependencies: missing ones + outdated ones in GExPipe's own
    #       dependency tree (other packages on the computer are left alone)
    ap   <- utils::available.packages()
    tree <- unique(c(deps, unlist(tools::package_dependencies(
      deps, db = ap, which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE))))
    old  <- tryCatch(rownames(utils::old.packages(available = ap, checkBuilt = TRUE)),
                     error = function(e) character(0))
    todo <- unique(c(setdiff(deps, installed()), intersect(old, tree)))
    if (length(todo)) try(BiocManager::install(todo, ask = FALSE, update = FALSE))
    for (p in setdiff(deps, installed())) {
      say("Retrying ", p, " ...")
      try(BiocManager::install(p, ask = FALSE, update = FALSE))
    }
    left <- setdiff(deps, installed())
    if (length(left)) {
      stop("These packages could not be installed: ", paste(left, collapse = ", "),
           "\nScroll up for the error. Usual fixes: close all other R/RStudio windows, ",
           "check the internet, restart R and run this line again.", call. = FALSE)
    }

    # -- 4. GExPipe itself ---------------------------------------------------
    # (retry: a brief internet/DNS drop makes the GitHub download fail)
    for (attempt in 1:3) {
      ok <- tryCatch({
        remotes::install_github(repo, ref = ref, dependencies = FALSE, upgrade = "never",
                                force = TRUE, INSTALL_opts = "--no-staged-install")
        TRUE
      }, error = function(e) { say("GitHub download failed: ", conditionMessage(e)); FALSE })
      if (ok) break
      if (attempt < 3) { say("Retrying in 15 s (", attempt, "/3) ..."); Sys.sleep(15) }
    }
    if (is.na(inst_ver())) stop("GExPipe did not install - scroll up for the error.", call. = FALSE)
    if (!loads_ok()) {
      stop("GExPipe installed but does not load. Restart R and run this line again; ",
           "if it still fails, run  BiocManager::valid()  to see which packages are outdated.",
           call. = FALSE)
    }
  }

  # -- 5. Launch ---------------------------------------------------------------
  say("GExPipe ", inst_ver(), " is ready. Opening the app ...")
  app <- GExPipe::runGExPipe()
  shiny::runApp(app, port = port, launch.browser = TRUE)
})
