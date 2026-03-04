# ==============================================================================
# GLOBAL.R - Libraries, Functions, and Global Variables
# ==============================================================================

# Timeout for GEO/downloads (user data), not for package installation
options(timeout = 600)

# ==============================================================================
# LOAD PACKAGES (Bioconductor-compliant: no runtime install; use BiocManager::install("GExPipe"))
# ==============================================================================
cat("Loading packages (R ", R.version.string, ")...\n")

# Core UI packages (required for app to open) - stop if any fail
core_pkgs <- c("shiny", "shinydashboard", "shinyjs", "DT")
for (p in core_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    stop("GExPipe requires package '", p, "'. Install with: install.packages(\"", p, "\")")
  }
}
suppressPackageStartupMessages({
  library(shiny); library(shinydashboard); library(shinyjs); library(DT)
})

# Auto-install Bioconductor packages (e.g. DESeq2) if missing, so new users can run the app
bioc_imports <- c(
  "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
  "edgeR", "clusterProfiler", "enrichplot", "STRINGdb", "DESeq2", "sva"
)
bioc_missing <- bioc_imports[!sapply(bioc_imports, function(p) requireNamespace(p, quietly = TRUE))]
if (length(bioc_missing) > 0L) {
  cat("Installing missing Bioconductor packages (e.g. DESeq2): ", paste(bioc_missing, collapse = ", "), "\n")
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager", repos = "https://cloud.r-project.org")
    library(BiocManager)
  }
  tryCatch({
    BiocManager::install(bioc_missing, update = FALSE, ask = FALSE)
  }, error = function(e) {
    warning("Auto-install of Bioconductor packages failed: ", conditionMessage(e),
            ". Install manually with: BiocManager::install(c(\"", bioc_missing[1], "\", ...))")
  })
}

# All other Imports: load one-by-one so one failure does not stop the app
imports_ordered <- c(
  "Biobase", "GEOquery", "limma", "AnnotationDbi", "org.Hs.eg.db",
  "dplyr", "data.table", "edgeR", "sva", "ggplot2", "gridExtra", "RColorBrewer", "pheatmap", "ggrepel",
  "VennDiagram", "UpSetR", "WGCNA", "parallel", "clusterProfiler", "enrichplot", "circlize", "STRINGdb", "DESeq2",
  "igraph", "ggraph", "tidygraph", "tidyr", "randomForest", "caret", "e1071", "glmnet", "pROC", "kernlab",
  "tibble", "msigdbr", "ggpubr", "reshape2", "corrplot", "R.utils", "dynamicTreeCut", "scales"
)
omniVerse_missing_pkgs <- character(0)
for (p in imports_ordered) {
  if (isNamespaceLoaded(p)) next
  ok <- tryCatch(
    suppressPackageStartupMessages(library(p, character.only = TRUE, quietly = TRUE)),
    error = function(e) FALSE
  )
  if (identical(ok, FALSE) || !isNamespaceLoaded(p)) {
    omniVerse_missing_pkgs <- c(omniVerse_missing_pkgs, p)
  }
}
if (length(omniVerse_missing_pkgs) > 0L) {
  warning("GExPipe: some packages could not be loaded (", paste(omniVerse_missing_pkgs, collapse = ", "), "). ",
          "Install with: BiocManager::install(c(\"", omniVerse_missing_pkgs[1], "\", ...)) or BiocManager::install(\"GExPipe\"). ",
          "App will start but some steps may not work until these are installed.")
  options(omniVerse.missingPkgs = omniVerse_missing_pkgs)
}

# Optional Suggests (loaded only if present)
if (requireNamespace("Boruta", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(Boruta)), error = function(e) NULL)
if (requireNamespace("mixOmics", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(mixOmics)), error = function(e) NULL)
if (requireNamespace("xgboost", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(xgboost)), error = function(e) NULL)
if (requireNamespace("SHAPforxgboost", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(SHAPforxgboost)), error = function(e) NULL)
if (requireNamespace("immunedeconv", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(immunedeconv)), error = function(e) NULL)
# rms (nomogram) requires Hmisc >= 5.2.4; ensure it before loading rms
if (requireNamespace("rms", quietly = TRUE)) {
  need_hmisc <- !requireNamespace("Hmisc", quietly = TRUE) ||
    (packageVersion("Hmisc") < "5.2.4")
  if (need_hmisc) {
    tryCatch(install.packages("Hmisc", repos = "https://cloud.r-project.org"), error = function(e) NULL)
  }
  tryCatch(suppressPackageStartupMessages(library(rms)), error = function(e) NULL)
}
if (requireNamespace("rmda", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(rmda)), error = function(e) NULL)
if (requireNamespace("cicerone", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(cicerone)), error = function(e) NULL)
if (requireNamespace("biomaRt", quietly = TRUE)) tryCatch(suppressPackageStartupMessages(library(biomaRt)), error = function(e) NULL)

# Allow large workspace .rds uploads (default Shiny limit is 5 MB)
options(shiny.maxRequestSize = 500 * 1024^2)  # 500 MB

# Allow long-running downloads (GEO, NCBI, supplementary files); R default is 60s
options(timeout = 3600)  # 1 hour so large GEO/supplementary files can finish

# WGCNA-friendly options (match standalone script)
options(stringsAsFactors = FALSE)
if (isNamespaceLoaded("WGCNA")) {
  tryCatch({
    if (exists("enableWGCNAThreads", mode = "function", where = asNamespace("WGCNA")))
      WGCNA::enableWGCNAThreads(nThreads = max(1L, parallel::detectCores() - 1L))
  }, error = function(e) NULL)
}

cat("✓ All packages loaded\n")

# ==============================================================================
# GLOBAL PLOT THEME (publication-quality, international standard)
# ==============================================================================
theme_publication <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 2, hjust = 0.5, margin = ggplot2::margin(b = 10)),
      plot.subtitle = ggplot2::element_text(size = base_size - 1, hjust = 0.5, color = "gray40"),
      axis.title = ggplot2::element_text(face = "bold", size = base_size),
      axis.text = ggplot2::element_text(size = base_size - 1, color = "gray30"),
      legend.title = ggplot2::element_text(face = "bold", size = base_size - 1),
      legend.text = ggplot2::element_text(size = base_size - 2),
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color = "gray70", linewidth = 0.5),
      strip.background = ggplot2::element_rect(fill = "gray95", color = "gray70"),
      strip.text = ggplot2::element_text(face = "bold", size = base_size - 1)
    )
}

# Color palette for consistent, accessible plots (colorblind-friendly where possible)
palette_primary <- c(
  "#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#3B1F2B",
  "#95C623", "#4A90A4", "#E94F37", "#6A4C93", "#88D498"
)

# ==============================================================================
# GLOBAL LOGGING & ERROR HANDLING HELPERS
# ==============================================================================
# Persistent log file (rotated daily) in the Shiny app working directory
LOG_DIR <- getwd()
LOG_FILE <- file.path(LOG_DIR, paste0("omniverse_log_", format(Sys.Date(), "%Y%m%d"), ".txt"))

# Write a line to the log (and optionally to console)
app_log <- function(msg, level = "INFO") {
  line <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] [", level, "] ", msg)
  # Always try to append to log file; failures should not crash the app
  tryCatch({
    cat(line, "\n", file = LOG_FILE, append = TRUE)
  }, error = function(e) {
    message("Logging failed: ", conditionMessage(e))
  })
  message(line)
}

# Wrapper for robust error handling with logging + user-friendly notification
safe_run <- function(expr, step_name = "this step", session = NULL) {
  tryCatch(
    expr,
    error = function(e) {
      app_log(paste0("Error in ", step_name, ": ", conditionMessage(e)), level = "ERROR")
      showNotification(
        tags$div(
          tags$strong(paste0("Error in ", step_name, ":")),
          tags$p(conditionMessage(e)),
          tags$p(
            "Check your inputs and try again. A detailed message has been written to the log file in the app working directory.",
            style = "font-size: 12px; color: #555;"
          )
        ),
        type = "error",
        duration = 10
      )
      NULL
    }
  )
}

# ==============================================================================
# GLOBAL VARIABLES
# ==============================================================================
# Image export: all downloaded plot images use this DPI (publication quality)
IMAGE_DPI <- 300L

# CSV export directory: all CSV downloads are also written here (app working directory)
CSV_EXPORT_DIR <- function() {
  d <- file.path(getwd(), "csv_exports")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

# ==============================================================================
# PLATFORM IDS FOR GSE ANNOTATION - ADD ALL NEW PLATFORM IDs HERE
# ==============================================================================
# Purpose: Bulk RNA (bulk RNA-seq + bulk gene expression microarrays) for
#          Homo sapiens and multi-species studies. If platform matches this list,
#          annotation and analysis run. (1) Value = .db package name (e.g. hgu133plus2.db);
#          (2) Value = NA means no .db; annotation via biomaRt/GPL (display type: "Other microarray (biomaRt)").
#          platform_id_to_type (below) gives a proper type label for every GPL.
platform_to_annot <- list(
  "GPL96" = "hgu133a.db",   "GPL97" = "hgu133b.db",   "GPL201" = "hgu133a.db",   "GPL570" = "hgu133plus2.db",   "GPL6104" = "illuminahumanv2.db",   "GPL10558" = "illuminahumanht12v4.db",   "GPL13607" = NA,   "GPL13667" = NA,   "GPL15207" = "hgu133plus2.db",   "GPL6244" = "hugene10sttranscriptcluster.db",   "GPL14550" = NA,   "GPL24676" = "illuminahumanht12v3.db",
  "GPL23126" = "clariomdhuman.db",   "GPL23159" = "clariomdhuman.db",   "GPL16686" = "hugene21sttranscriptcluster.db",   "GPL17585" = "hta20transcriptcluster.db",   "GPL17586" = "hta20transcriptcluster.db",   "GPL16043" = "primeview.db",   "GPL571" = "hgu133a2.db",   "GPL11532" = "hugene11sttranscriptcluster.db",   "GPL17556" = "hugene11sttranscriptcluster.db",   "GPL26944" = "hugene21sttranscriptcluster.db",   "GPL24299" = "clarionshuman.db",   "GPL23270" = "clarionshuman.db",
  "GPL26525" = NA,   "GPL10904" = "illuminahumanht12v4.db",   "GPL18643" = NA,   "GPL13915" = "hugene10sttranscriptcluster.db",   "GPL8227" = NA,   "GPL9081" = NA,   "GPL24072" = NA,   "GPL18402" = NA,   "GPL21559" = "hugene21sttranscriptcluster.db",   "GPL21439" = NA,   "GPL22003" = NA,   "GPL8363" = NA,
  "GPL20204" = NA,   "GPL6359" = NA,   "GPL16020" = NA,   "GPL16956" = NA,   "GPL19826" = NA,   "GPL19828" = NA,   "GPL6947" = "illuminahumanht12v3.db",   "GPL6885" = "illuminaMousev2.db",   "GPL6883" = NA,   "GPL18281" = NA,   "GPL18451" = NA,   "GPL4133" = NA,
  "GPL21185" = NA,   "GPL19072" = NA,   "GPL23465" = NA,   "GPL20791" = NA,   "GPL19965" = NA,   "GPL27487" = NA,   "GPL24875" = NA,   "GPL13987" = NA,   "GPL17077" = NA,   "GPL26747" = NA,   "GPL19387" = NA,   "GPL26705" = NA,
  "GPL26683" = NA,   "GPL19251" = "hta20transcriptcluster.db",   "GPL15088" = "hugene20sttranscriptcluster.db",   "GPL25381" = "clarionshuman.db",   "GPL13158" = "hgu133plus2.db",   "GPL23961" = "clarionshuman.db",   "GPL23178" = NA,   "GPL16847" = NA,   "GPL21970" = "hgu133plus2.db",   "GPL17629" = NA,   "GPL25024" = NA,   "GPL21827" = NA,
  "GPL25700" = NA,   "GPL16311" = "hugene20sttranscriptcluster.db",   "GPL17810" = NA,   "GPL25635" = NA,   "GPL25480" = NA,   "GPL25672" = NA,   "GPL24532" = "hugene21sttranscriptcluster.db",   "GPL25484" = NA,   "GPL24605" = NA,   "GPL24606" = NA,   "GPL24607" = NA,   "GPL25336" = "hugene21sttranscriptcluster.db",
  "GPL23649" = NA,   "GPL23578" = NA,   "GPL25166" = NA,   "GPL24507" = "clarionshuman.db",   "GPL6480" = NA,   "GPL21061" = "hugene20sttranscriptcluster.db",   "GPL22516" = NA,   "GPL22517" = NA,   "GPL23432" = "hgu133plus2.db",   "GPL24904" = NA,   "GPL24879" = NA,   "GPL17107" = NA,
  "GPL19065" = NA,   "GPL16384" = NA,   "GPL26026" = NA,   "GPL29173" = NA,   "GPL27765" = NA,   "GPL27766" = NA,   "GPL24594" = NA,   "GPL29558" = NA,   "GPL16131" = NA,   "GPL6801" = NA,   "GPL8490" = NA,   "GPL5188" = "huex10sttranscriptcluster.db",
  "GPL24456" = NA,   "GPL28929" = NA,   "GPL27033" = NA,   "GPL27034" = NA,   "GPL27035" = NA,   "GPL27036" = NA,   "GPL28859" = NA,   "GPL10332" = NA,   "GPL19983" = "hgu133a2.db",   "GPL5175" = "huex10sttranscriptcluster.db",   "GPL19117" = NA,   "GPL19137" = NA,
  "GPL21825" = NA,   "GPL23259" = NA,   "GPL22722" = NA,   "GPL19859" = "hgu133plus2.db",   "GPL30304" = NA,   "GPL28885" = NA,   "GPL30862" = NA,   "GPL20822" = NA,   "GPL15314" = "hugene11sttranscriptcluster.db",   "GPL10656" = NA,   "GPL15491" = NA,   "GPL22120" = NA,
  "GPL13534" = NA,   "GPL24205" = NA,   "GPL2879" = NA,   "GPL9777" = NA,   "GPL4093" = NA,   "GPL19449" = NA,   "GPL33762" = NA,   "GPL33499" = NA,   "GPL34054" = NA,   "GPL33072" = NA,   "GPL32072" = NA,   "GPL32522" = NA,
  "GPL21986" = NA,   "GPL22163" = NA,   "GPL20115" = NA,   "GPL31056" = NA,   "GPL18058" = NA,   "GPL29703" = NA,   "GPL27490" = NA,   "GPL29441" = NA,   "GPL18990" = "hugene21sttranscriptcluster.db",   "GPL18991" = "hgu133a2.db",   "GPL25865" = NA,   "GPL17052" = NA,
  "GPL25249" = "clarionshuman.db",   "GPL17692" = "hugene21sttranscriptcluster.db",   "GPL30929" = NA,   "GPL20265" = "hugene21sttranscriptcluster.db",   "GPL21572" = NA,   "GPL30572" = "hugene21sttranscriptcluster.db",   "GPL30404" = NA,   "GPL26936" = NA,   "GPL29829" = "hgu133a.db",   "GPL23668" = NA,   "GPL24335" = NA,   "GPL14951" = "hugene10sttranscriptcluster.db",
  "GPL30379" = NA,   "GPL28718" = "hugene10sttranscriptcluster.db",   "GPL20748" = NA,   "GPL28902" = NA,   "GPL29006" = NA,   "GPL29292" = NA,   "GPL29320" = NA,   "GPL29454" = NA,   "GPL29433" = NA,   "GPL29549" = NA,   "GPL29595" = NA,   "GPL29607" = NA,
  "GPL29623" = NA,   "GPL29637" = NA,   "GPL29805" = NA,   "GPL29806" = NA,   "GPL31923" = NA,   "GPL31924" = NA,   "GPL31944" = NA,   "GPL31980" = NA,   "GPL31989" = NA,   "GPL31999" = NA,   "GPL32083" = NA,   "GPL32090" = NA,
  "GPL32495" = NA,   "GPL32568" = NA,   "GPL32794" = NA,   "GPL35272" = NA,   "GPL35292" = NA,   "GPL35691" = NA,   "GPL35894" = NA,   "GPL26780" = NA,   "GPL26863" = NA,   "GPL27452" = NA,   "GPL27644" = NA,   "GPL27648" = NA,
  "GPL27804" = NA,   "GPL28282" = NA,   "GPL28283" = NA,   "GPL28575" = NA,   "GPL25526" = NA,   "GPL26248" = NA,   "GPL29914" = NA,   "GPL29948" = NA,   "GPL29977" = NA,   "GPL30008" = NA,   "GPL30279" = NA,   "GPL30296" = NA,
  "GPL30425" = NA,   "GPL30426" = NA,   "GPL30446" = NA,   "GPL30626" = NA,   "GPL30982" = NA,   "GPL31063" = NA,   "GPL31988" = NA,   "GPL32123" = NA,   "GPL32125" = NA,   "GPL32126" = NA,   "GPL32226" = NA,   "GPL32306" = NA,
  "GPL32695" = NA,   "GPL35315" = NA,   "GPL35403" = NA,   "GPL35454" = NA,   "GPL35455" = NA,   "GPL35689" = NA,   "GPL35920" = NA,   "GPL35956" = NA,   "GPL35989" = NA,   "GPL36059" = NA,   "GPL36500" = NA,   "GPL29372" = NA,
  "GPL29808" = NA,   "GPL30202" = NA,   "GPL30209" = NA,   "GPL18573" = NA,   "GPL19415" = NA,   "GPL20051" = NA,   "GPL20276" = NA,   "GPL26866" = NA,   "GPL26921" = NA,   "GPL26922" = NA,   "GPL27004" = NA,   "GPL27005" = NA,
  "GPL27991" = NA,   "GPL28223" = NA,   "GPL28658" = NA,   "GPL28867" = NA,   "GPL24625" = NA,   "GPL24958" = NA,   "GPL25467" = NA,   "GPL25496" = NA,   "GPL25540" = NA,   "GPL25584" = NA,   "GPL26044" = NA,   "GPL26178" = NA,
  "GPL26236" = NA,   "GPL26306" = NA,   "GPL26509" = NA,   "GPL28886" = NA,   "GPL28896" = NA,   "GPL28989" = NA,   "GPL29057" = NA,   "GPL29105" = NA,   "GPL29160" = NA,   "GPL29219" = NA,   "GPL29228" = NA,   "GPL29321" = NA,
  "GPL29429" = NA,   "GPL29543" = NA,   "GPL29603" = NA,   "GPL29636" = NA,   "GPL29673" = NA,   "GPL31034" = NA,   "GPL31050" = NA,   "GPL31935" = NA,   "GPL32511" = NA,   "GPL35910" = NA,   "GPL36096" = NA,   "GPL36117" = NA,
  "GPL26639" = NA,   "GPL26693" = NA,   "GPL26733" = NA,   "GPL26864" = NA,   "GPL26865" = NA,   "GPL26867" = NA,   "GPL26957" = NA,   "GPL26958" = NA,   "GPL26959" = NA,   "GPL26995" = NA,   "GPL27024" = NA,   "GPL27025" = NA,
  "GPL27044" = NA,   "GPL27157" = NA,   "GPL27467" = NA,   "GPL27802" = NA,   "GPL27803" = NA,   "GPL28025" = NA,   "GPL28128" = NA,   "GPL28146" = NA,   "GPL28186" = NA,   "GPL28185" = NA,   "GPL28205" = NA,   "GPL28694" = NA,
  "GPL24432" = NA,   "GPL24496" = NA,   "GPL24632" = NA,   "GPL24703" = NA,   "GPL24850" = NA,   "GPL24878" = NA,   "GPL24896" = NA,   "GPL24907" = NA,   "GPL24974" = NA,   "GPL24975" = NA,   "GPL24976" = NA,   "GPL24977" = NA,
  "GPL25161" = NA,   "GPL25348" = NA,   "GPL25431" = NA,   "GPL25466" = NA,   "GPL25476" = NA,   "GPL25625" = NA,   "GPL25643" = NA,   "GPL25719" = NA,   "GPL25750" = NA,   "GPL25751" = NA,   "GPL25904" = NA,   "GPL26016" = NA,
  "GPL26132" = NA,   "GPL26134" = NA,   "GPL26226" = NA,   "GPL26291" = NA,   "GPL26303" = NA,   "GPL26363" = NA,   "GPL26619" = NA,   "GPL11154" = NA,   "GPL15433" = NA,   "GPL16512" = NA,   "GPL16791" = NA,   "GPL17553" = NA,
  "GPL18031" = NA,   "GPL18160" = NA,   "GPL18215" = NA,   "GPL18430" = NA,   "GPL18448" = NA,   "GPL18460" = NA,   "GPL19227" = NA,   "GPL19367" = NA,   "GPL19381" = NA,   "GPL20050" = NA,   "GPL20238" = NA,   "GPL20271" = NA,
  "GPL20301" = NA,   "GPL20607" = NA,   "GPL20795" = NA,   "GPL21219" = NA,   "GPL35694" = NA,   "GPL35732" = NA,   "GPL36010" = NA,   "GPL36168" = NA,   "GPL36169" = NA,   "GPL36170" = NA,   "GPL36176" = NA,   "GPL36331" = NA,
  # (GPL11154=Illumina HiSeq 2000, GPL15433=Illumina HiSeq 1000, GPL16512=Illumina HiSeq 2000 Multi-species, GPL16791=Illumina HiSeq 2500, GPL18448=Illumina HiSeq 2000, GPL18460=Illumina HiSeq 2500, GPL19227=Illumina NextSeq 500, GPL20050=Illumina HiSeq 2500 Dual RNA-Seq, GPL20301=Illumina HiSeq 4000, GPL21219=Illumina HiSeq 3000)
  "GPL36530" = NA,   "GPL28955" = NA,   "GPL29480" = NA,   "GPL30882" = NA,   "GPL32186" = NA,   "GPL35417" = NA,   "GPL35868" = NA,   "GPL28038" = NA,   "GPL28337" = NA,   "GPL29439" = NA,   "GPL29481" = NA,   "GPL29482" = NA,
  "GPL32283" = NA,   "GPL26167" = NA,   "GPL28975" = NA,   "GPL32242" = NA,   "GPL32271" = NA,   "GPL31265" = NA,   "GPL27444" = NA,   "GPL17301" = NA,   "GPL17303" = NA,   "GPL29544" = NA,   "GPL28352" = NA,   "GPL26180" = NA,
  "GPL19124" = NA,   "GPL36273" = NA,   "GPL29455" = NA,   "GPL29714" = NA,   "GPL29811" = NA,   "GPL29861" = NA,   "GPL26694" = NA,   "GPL24950" = NA,   "GPL25879" = NA,   "GPL15520" = NA,   "GPL17658" = NA,   "GPL19969" = NA,
  "GPL20062" = NA,   "GPL20172" = NA,   "GPL31277" = NA,   "GPL26996" = NA,   "GPL25943" = NA,   "GPL28191" = NA,   "GPL28817" = NA,   "GPL9115" = NA,   "GPL9138" = NA,   "GPL9178" = NA,   "GPL9186" = NA,   "GPL9442" = NA,
  "GPL9520" = NA,   "GPL10329" = NA,   "GPL10400" = NA,   "GPL10999" = NA,   "GPL13393" = NA,   "GPL14603" = NA,   "GPL14761" = NA,   "GPL15456" = NA,   "GPL16061" = NA,   "GPL16288" = NA,   "GPL16558" = NA,   "GPL17232" = NA,
  "GPL18723" = NA,   "GPL20148" = NA,   "GPL13497" = NA,   "GPL21145" = NA,   "GPL30035" = NA,   "GPL30065" = NA,   "GPL30173" = NA,   "GPL30181" = NA,   "GPL30223" = NA,   "GPL30234" = NA,   "GPL30319" = NA,   "GPL30320" = NA,
  "GPL30321" = NA,   "GPL30391" = NA,   "GPL30396" = NA,   "GPL30440" = NA,   "GPL31905" = NA,   "GPL31906" = NA,   "GPL31910" = NA,   "GPL31911" = NA,   "GPL32713" = NA,   "GPL33803" = NA,   "GPL33934" = NA,   "GPL33970" = NA,
  "GPL34281" = NA,   "GPL34284" = NA,   "GPL34295" = NA,   "GPL34330" = NA,   "GPL34340" = NA,   "GPL34362" = NA,   "GPL34372" = NA,   "GPL34374" = NA,   "GPL34382" = NA,   "GPL34474" = NA,   "GPL34539" = NA,   "GPL34618" = NA,
  "GPL34633" = NA,   "GPL34678" = NA,   "GPL34710" = NA,   "GPL34711" = NA,   "GPL34752" = NA,   "GPL34762" = NA,   "GPL34775" = NA,   "GPL34841" = NA,   "GPL34892" = NA,   "GPL34943" = NA,   "GPL34980" = NA,   "GPL35012" = NA,
  "GPL35048" = NA,   "GPL35050" = NA,   "GPL35059" = NA,   "GPL35109" = NA,   "GPL35146" = NA,   "GPL35167" = NA,   "GPL35169" = NA,   "GPL35233" = NA,   "GPL35238" = NA,   "GPL35245" = NA,   "GPL35254" = NA,   "GPL35268" = NA,
  "GPL35275" = NA,   "GPL16699" = NA,   "GPL21290" = NA,   "GPL21311" = NA,   "GPL21697" = NA,   "GPL23227" = NA,   "GPL23976" = NA,   "GPL24014" = NA,   "GPL24106" = NA,   "GPL26963" = NA,   "GPL33023" = NA,   "GPL33758" = NA,
  "GPL9052" = NA,   "GPL100" = NA,   "GPL10044" = NA,   "GPL10048" = NA,   "GPL10054" = NA,   "GPL10077" = NA,   "GPL10078" = NA,   "GPL10094" = NA,   "GPL10097" = NA,   "GPL101" = NA,   "GPL10102" = NA,   "GPL10106" = NA,
  "GPL10108" = NA,   "GPL10123" = NA,   "GPL10127" = NA,   "GPL10128" = NA,   "GPL10133" = NA,   "GPL10152" = NA,   "GPL10154" = NA,   "GPL10175" = NA,   "GPL10183" = NA,   "GPL10184" = NA,   "GPL10191" = NA,   "GPL10200" = NA,
  "GPL10231" = NA,   "GPL10233" = NA,   "GPL10239" = NA,   "GPL10262" = NA,   "GPL10264" = NA,   "GPL10270" = NA,   "GPL10274" = NA,   "GPL10289" = NA,   "GPL10295" = NA,   "GPL10301" = NA,   "GPL10305" = NA,   "GPL10319" = NA,
  "GPL10320" = NA,   "GPL10322" = NA,   "GPL10333" = NA,   "GPL10335" = NA,   "GPL10349" = NA,   "GPL10352" = NA,   "GPL10370" = NA,   "GPL10377" = NA,   "GPL10379" = NA,   "GPL10381" = NA,   "GPL10383" = NA,   "GPL10384" = NA,
  "GPL10399" = NA,   "GPL104" = NA,   "GPL10412" = NA,   "GPL10423" = NA,   "GPL10444" = NA,   "GPL10445" = NA,   "GPL10457" = NA,   "GPL10464" = NA,   "GPL10465" = NA,   "GPL10468" = NA,   "GPL10481" = NA,   "GPL1052" = NA,
  "GPL10520" = NA,   "GPL10523" = NA,   "GPL10525" = NA,   "GPL10526" = NA,   "GPL1053" = "hgu133b.db",   "GPL10531" = NA,   "GPL10553" = NA,   "GPL10555" = NA,   "GPL10562" = NA,   "GPL10567" = NA,   "GPL10585" = NA,   "GPL10588" = NA,
  "GPL10630" = NA,   "GPL10635" = NA,   "GPL10647" = NA,   "GPL10648" = NA,   "GPL10651" = NA,   "GPL10666" = NA,   "GPL10671" = NA,   "GPL10676" = NA,   "GPL10687" = NA,   "GPL10691" = NA,   "GPL10701" = NA,   "GPL10702" = NA,
  "GPL10703" = NA,   "GPL10714" = NA,   "GPL1073" = "hgu133a.db",   "GPL10739" = "hgu133plus2.db",   "GPL1074" = NA,   "GPL10749" = NA,   "GPL10756" = NA,   "GPL10760" = NA,   "GPL10773" = NA,   "GPL10778" = NA,   "GPL10787" = NA,   "GPL10796" = NA,
  "GPL10810" = NA,   "GPL10845" = NA,   "GPL10850" = NA,   "GPL10860" = NA,   "GPL1087" = NA,   "GPL10878" = NA,   "GPL10879" = NA,   "GPL10881" = NA,   "GPL10887" = NA,   "GPL10897" = NA,   "GPL10898" = NA,   "GPL1090" = NA,
  "GPL10907" = NA,   "GPL10912" = NA,   "GPL10913" = NA,   "GPL10914" = NA,   "GPL10925" = NA,   "GPL10931" = NA,   "GPL10936" = NA,   "GPL1098" = NA,   "GPL10982" = NA,   "GPL10993" = NA,   "GPL10994" = NA,   "GPL11010" = NA,
  "GPL11013" = NA,   "GPL11017" = NA,   "GPL11028" = NA,   "GPL11048" = NA,   "GPL1105" = NA,   "GPL11052" = NA,   "GPL11054" = NA,   "GPL11059" = NA,   "GPL1106" = NA,   "GPL11060" = NA,   "GPL1107" = NA,   "GPL11070" = NA,
  "GPL11071" = NA,   "GPL11084" = NA,   "GPL1109" = NA,   "GPL11093" = NA,   "GPL11094" = NA,   "GPL11097" = NA,   "GPL11118" = NA,   "GPL11123" = NA,   "GPL11157" = NA,   "GPL11162" = NA,   "GPL11179" = NA,   "GPL11180" = NA,
  "GPL11181" = NA,   "GPL11195" = NA,   "GPL1120" = NA,   "GPL11205" = NA,   "GPL11209" = NA,   "GPL11219" = NA,   "GPL11226" = NA,   "GPL11241" = NA,   "GPL11249" = NA,   "GPL1126" = NA,   "GPL11289" = NA,   "GPL11305" = NA,
  "GPL11306" = NA,   "GPL11309" = NA,   "GPL11316" = NA,   "GPL11319" = NA,   "GPL11320" = NA,   "GPL11329" = NA,   "GPL1133" = NA,   "GPL11338" = NA,   "GPL11350" = NA,   "GPL11357" = NA,   "GPL11383" = NA,   "GPL1140" = NA,
  "GPL11417" = NA,   "GPL1142" = NA,   "GPL11429" = NA,   "GPL11432" = NA,   "GPL11433" = NA,   "GPL11434" = NA,   "GPL11440" = NA,   "GPL11487" = NA,   "GPL1152" = NA,   "GPL11533" = NA,   "GPL11536" = NA,   "GPL11624" = NA,
  "GPL11653" = NA,   "GPL11662" = NA,   "GPL11670" = NA,   "GPL1172" = NA,   "GPL1194" = NA,   "GPL1197" = NA,   "GPL1198" = NA,   "GPL121" = NA,   "GPL1211" = NA,   "GPL1213" = NA,   "GPL1217" = NA,   "GPL1223" = NA,
  "GPL1225" = NA,   "GPL1227" = NA,   "GPL1228" = NA,   "GPL1241" = NA,   "GPL1260" = NA,   "GPL1261" = "mouse4302.db",   "GPL1266" = NA,   "GPL128" = NA,   "GPL1282" = NA,   "GPL1283" = NA,   "GPL129" = NA,   "GPL1290" = NA,
  "GPL1291" = NA,   "GPL1294" = NA,   "GPL130" = NA,   "GPL1301" = NA,   "GPL1309" = NA,   "GPL13112" = NA,   "GPL13118" = NA,   "GPL13128" = NA,   "GPL1313" = NA,   "GPL13132" = NA,   "GPL13135" = NA,   "GPL13136" = NA,
  "GPL13140" = NA,   "GPL13152" = NA,   "GPL13171" = NA,   "GPL13183" = NA,   "GPL13185" = NA,   "GPL1321" = NA,   "GPL13215" = NA,   "GPL13218" = NA,   "GPL13219" = NA,   "GPL1322" = "hugenefl.db",   "GPL13223" = NA,   "GPL13224" = NA,
  "GPL13248" = NA,   "GPL13250" = NA,   "GPL13251" = NA,   "GPL13252" = NA,   "GPL13264" = NA,   "GPL13270" = NA,   "GPL13273" = NA,   "GPL13290" = NA,   "GPL13301" = NA,   "GPL13303" = NA,   "GPL13354" = NA,   "GPL13359" = NA,
  "GPL13376" = NA,   "GPL13379" = NA,   "GPL13384" = NA,   "GPL13388" = NA,   "GPL13392" = NA,   "GPL13402" = NA,   "GPL13422" = NA,   "GPL13425" = NA,   "GPL13447" = NA,   "GPL13480" = NA,   "GPL13481" = NA,   "GPL13482" = NA,
  "GPL13490" = NA,   "GPL13493" = NA,   "GPL13507" = NA,   "GPL13514" = NA,   "GPL13519" = NA,   "GPL1355" = NA,   "GPL13611" = NA,   "GPL13635" = NA,   "GPL13648" = NA,   "GPL13658" = NA,   "GPL13668" = NA,   "GPL13679" = NA,
  "GPL13685" = NA,   "GPL13692" = NA,   "GPL13695" = NA,   "GPL13699" = NA,   "GPL13730" = NA,   "GPL13737" = NA,   "GPL13746" = NA,   "GPL13754" = NA,   "GPL13780" = NA,   "GPL13825" = NA,   "GPL13829" = NA,   "GPL1384" = NA,
  "GPL1387" = NA,   "GPL139" = NA,   "GPL1390" = "hgu95a.db",   "GPL13912" = NA,   "GPL1352" = "hgu133plus2.db",   "GPL3921" = "hgu133plus2.db",   "GPL887" = "hgu95b.db",   "GPL1312" = "mgu74av2.db",   "GPL1407" = NA,   "GPL13916" = NA,   "GPL13933" = NA,   "GPL13952" = NA,
  "GPL13976" = NA,   "GPL140" = NA,   "GPL14010" = NA,   "GPL1411" = NA,   "GPL14113" = NA,   "GPL1412" = NA,   "GPL14120" = NA,   "GPL1417" = NA,   "GPL14184" = NA,   "GPL14185" = NA,   "GPL14186" = NA,   "GPL14189" = NA,
  "GPL1420" = NA,   "GPL1424" = NA,   "GPL1426" = NA,   "GPL143" = NA,   "GPL14355" = NA,   "GPL14356" = NA,   "GPL14360" = NA,   "GPL14374" = NA,   "GPL14378" = NA,   "GPL1449" = NA,   "GPL1456" = NA,   "GPL14592" = NA,
  "GPL14593" = NA,   "GPL14602" = NA,   "GPL14604" = NA,   "GPL14613" = NA,   "GPL14617" = NA,   "GPL14622" = NA,   "GPL14663" = NA,   "GPL14668" = NA,   "GPL14670" = NA,   "GPL14674" = NA,   "GPL14713" = NA,   "GPL14723" = NA,
  "GPL14732" = NA,   "GPL14740" = NA,   "GPL14744" = NA,   "GPL14745" = NA,   "GPL14746" = NA,   "GPL14760" = NA,   "GPL14765" = NA,   "GPL14767" = NA,   "GPL14776" = NA,   "GPL1479" = NA,   "GPL14795" = NA,   "GPL14797" = NA,
  "GPL14820" = NA,   "GPL14835" = NA,   "GPL14837" = NA,   "GPL14838" = NA,   "GPL1485" = NA,   "GPL14867" = NA,   "GPL14871" = NA,   "GPL14873" = NA,   "GPL1488" = NA,   "GPL149" = NA,   "GPL14907" = NA,   "GPL14943" = NA,
  "GPL14954" = NA,   "GPL14957" = NA,   "GPL14961" = NA,   "GPL14963" = NA,   "GPL14987" = NA,   "GPL14996" = NA,   "GPL15017" = NA,   "GPL15018" = NA,   "GPL15034" = NA,   "GPL15069" = NA,   "GPL15090" = NA,   "GPL15097" = NA,
  "GPL15106" = NA,   "GPL15127" = NA,   "GPL15130" = NA,   "GPL15142" = NA,   "GPL15143" = NA,   "GPL15158" = NA,   "GPL15159" = NA,   "GPL15173" = NA,   "GPL15176" = NA,   "GPL1521" = NA,   "GPL15235" = NA,   "GPL15240" = NA,
  "GPL15241" = NA,   "GPL15242" = NA,   "GPL1526" = NA,   "GPL15271" = NA,   "GPL15279" = NA,   "GPL1528" = NA,   "GPL15285" = NA,   "GPL1530" = NA,   "GPL15308" = NA,   "GPL15315" = NA,   "GPL15331" = NA,   "GPL15338" = NA,
  "GPL15362" = NA,   "GPL15366" = NA,   "GPL15371" = NA,   "GPL15380" = NA,   "GPL15389" = NA,   "GPL15390" = NA,   "GPL15394" = NA,   "GPL15395" = NA,   "GPL15401" = NA,   "GPL15406" = NA,   "GPL15424" = NA,   "GPL15436" = NA,
  "GPL15440" = NA,   "GPL15445" = NA,   "GPL15446" = NA,   "GPL15454" = NA,   "GPL15468" = NA,   "GPL15496" = NA,   "GPL15497" = NA,   "GPL15516" = NA,   "GPL15517" = NA,   "GPL15524" = NA,   "GPL15542" = NA,   "GPL15544" = NA,
  "GPL15552" = NA,   "GPL15558" = NA,   "GPL15592" = NA,   "GPL15615" = NA,   "GPL15640" = NA,   "GPL15648" = NA,   "GPL15659" = NA,   "GPL15684" = NA,   "GPL15718" = NA,   "GPL15724" = NA,   "GPL15740" = NA,   "GPL15744" = NA,
  "GPL15748" = NA,   "GPL15762" = NA,   "GPL15770" = NA,   "GPL15774" = NA,   "GPL15784" = NA,   "GPL15789" = NA,   "GPL15793" = NA,   "GPL15797" = NA,   "GPL15802" = NA,   "GPL15803" = NA,   "GPL15826" = NA,   "GPL15829" = NA,
  "GPL15846" = NA,   "GPL15847" = NA,   "GPL15868" = NA,   "GPL15931" = NA,   "GPL15932" = NA,   "GPL15933" = NA,   "GPL15950" = NA,   "GPL15969" = NA,   "GPL15974" = NA,   "GPL15975" = NA,   "GPL15981" = NA,   "GPL15988" = NA,
  "GPL15997" = NA,   "GPL16006" = NA,   "GPL16016" = NA,   "GPL16021" = NA,   "GPL16022" = NA,   "GPL16025" = NA,   "GPL16027" = NA,   "GPL16062" = NA,   "GPL16066" = NA,   "GPL16068" = NA,   "GPL16070" = NA,   "GPL16086" = NA,
  "GPL16098" = NA,   "GPL161" = NA,   "GPL16100" = NA,   "GPL16142" = NA,   "GPL16163" = NA,   "GPL16166" = NA,   "GPL16212" = NA,   "GPL16221" = NA,   "GPL16230" = NA,   "GPL16231" = NA,   "GPL16233" = NA,   "GPL16236" = NA,
  "GPL16237" = NA,   "GPL16238" = NA,   "GPL16239" = NA,   "GPL16240" = NA,   "GPL16254" = NA,   "GPL16268" = NA,   "GPL16272" = NA,   "GPL16273" = NA,   "GPL16287" = NA,   "GPL16297" = NA,   "GPL16299" = NA,   "GPL16301" = NA,
  "GPL16302" = NA,   "GPL16304" = NA,   "GPL16329" = NA,   "GPL16332" = NA,   "GPL16333" = NA,   "GPL16340" = NA,   "GPL16341" = NA,   "GPL16353" = NA,   "GPL16356" = NA,   "GPL16359" = NA,   "GPL16368" = NA,   "GPL16372" = NA,
  "GPL16378" = NA,   "GPL16417" = NA,   "GPL16419" = NA,   "GPL16422" = NA,   "GPL16425" = NA,   "GPL16481" = NA,   "GPL16494" = NA,   "GPL16498" = NA,   "GPL16522" = NA,   "GPL16532" = NA,   "GPL16542" = NA,   "GPL16546" = NA,
  "GPL16570" = NA,   "GPL16585" = NA,   "GPL16591" = NA,   "GPL16592" = NA,   "GPL16601" = NA,   "GPL16604" = NA,   "GPL167" = NA,   "GPL16700" = NA,   "GPL16765" = NA,   "GPL16766" = NA,   "GPL16770" = NA,   "GPL16772" = NA,
  "GPL16786" = NA,   "GPL16790" = NA,   "GPL16807" = NA,   "GPL16869" = NA,   "GPL16876" = NA,   "GPL16895" = NA,   "GPL16898" = NA,   "GPL16899" = NA,   "GPL169" = NA,   "GPL16935" = NA,   "GPL16968" = NA,   "GPL16977" = NA,
  "GPL16980" = NA,   "GPL16981" = NA,   "GPL16987" = NA,   "GPL16998" = NA,   "GPL16999" = NA,   "GPL170" = NA,   "GPL17000" = NA,   "GPL17039" = NA,   "GPL1704" = NA,   "GPL17040" = NA,   "GPL17042" = NA,   "GPL17047" = NA,
  "GPL17061" = NA,   "GPL17062" = NA,   "GPL1707" = NA,   "GPL1709" = NA,   "GPL1711" = NA,   "GPL17117" = NA,   "GPL17120" = NA,   "GPL1714" = NA,   "GPL17157" = NA,   "GPL17180" = NA,   "GPL17223" = NA,   "GPL17230" = NA,
  "GPL17236" = NA,   "GPL1727" = NA,   "GPL17272" = NA,   "GPL17279" = NA,   "GPL17292" = NA,   "GPL17295" = NA,   "GPL1733" = NA,   "GPL1734" = NA,   "GPL1735" = NA,   "GPL1736" = NA,   "GPL17385" = NA,   "GPL17386" = NA,
  "GPL1739" = NA,   "GPL17397" = NA,   "GPL174" = NA,   "GPL1740" = NA,   "GPL17400" = NA,   "GPL1741" = NA,   "GPL17425" = NA,   "GPL17426" = NA,   "GPL17486" = NA,   "GPL17489" = NA,   "GPL175" = NA,   "GPL17513" = NA,
  "GPL17518" = NA,   "GPL1753" = NA,   "GPL17537" = NA,   "GPL17543" = NA,   "GPL17548" = NA,   "GPL17555" = NA,   "GPL17558" = NA,   "GPL17559" = NA,   "GPL17567" = NA,   "GPL17568" = NA,   "GPL17591" = NA,   "GPL176" = NA,
  "GPL17648" = NA,   "GPL17653" = NA,   "GPL17660" = NA,   "GPL17662" = NA,   "GPL17694" = NA,   "GPL17698" = NA,   "GPL177" = NA,   "GPL17737" = NA,   "GPL17760" = NA,   "GPL17784" = NA,   "GPL178" = NA,   "GPL17811" = NA,
  "GPL17835" = NA,   "GPL17837" = NA,   "GPL17841" = NA,   "GPL17843" = NA,   "GPL17845" = NA,   "GPL17848" = NA,   "GPL17867" = NA,   "GPL17882" = NA,   "GPL17889" = NA,   "GPL179" = NA,   "GPL1790" = NA,   "GPL17904" = NA,
  "GPL17909" = NA,   "GPL17915" = NA,   "GPL17930" = "hugene21sttranscriptcluster.db",   "GPL17938" = NA,   "GPL17943" = NA,   "GPL17954" = NA,   "GPL17976" = NA,   "GPL17989" = NA,   "GPL17996" = NA,   "GPL180" = NA,   "GPL18001" = NA,   "GPL18029" = NA,
  "GPL18034" = NA,   "GPL18044" = NA,   "GPL18053" = NA,   "GPL18056" = NA,   "GPL18080" = NA,   "GPL18096" = NA,   "GPL18109" = NA,   "GPL18122" = NA,   "GPL18130" = NA,   "GPL18134" = NA,   "GPL1814" = NA,   "GPL18155" = NA,
  "GPL18159" = NA,   "GPL1818" = NA,   "GPL18180" = NA,   "GPL18189" = NA,   "GPL18190" = NA,   "GPL1823" = NA,   "GPL18233" = NA,   "GPL1827" = NA,   "GPL18283" = NA,   "GPL1831" = NA,   "GPL18318" = NA,   "GPL18320" = NA,
  "GPL18348" = NA,   "GPL18387" = NA,   "GPL18389" = NA,   "GPL18390" = NA,   "GPL18401" = NA,   "GPL18410" = NA,   "GPL18411" = NA,   "GPL18412" = NA,   "GPL1843" = NA,   "GPL18437" = NA,   "GPL18461" = NA,   "GPL18476" = NA,
  "GPL18478" = NA,   "GPL18485" = NA,   "GPL18486" = NA,   "GPL18503" = NA,   "GPL18509" = NA,   "GPL1857" = NA,   "GPL18578" = NA,   "GPL18604" = NA,   "GPL18609" = NA,   "GPL18615" = NA,   "GPL18627" = NA,   "GPL1863" = NA,
  "GPL18631" = NA,   "GPL18641" = NA,   "GPL18642" = NA,   "GPL18649" = NA,   "GPL18671" = NA,   "GPL18695" = NA,   "GPL18712" = NA,   "GPL1872" = NA,   "GPL18721" = NA,   "GPL1873" = NA,   "GPL18734" = NA,   "GPL1874" = NA,
  "GPL1875" = NA,   "GPL18752" = NA,   "GPL18761" = NA,   "GPL1883" = NA,   "GPL1885" = NA,   "GPL18850" = NA,   "GPL18873" = NA,   "GPL18880" = NA,   "GPL18887" = NA,   "GPL18894" = NA,   "GPL18900" = NA,   "GPL18938" = NA,
  "GPL18941" = NA,   "GPL18942" = NA,   "GPL18943" = NA,   "GPL18947" = NA,   "GPL18959" = NA,   "GPL18964" = NA,   "GPL1897" = NA,   "GPL18976" = NA,   "GPL19034" = NA,   "GPL19061" = NA,   "GPL19066" = NA,   "GPL19099" = NA,
  "GPL19100" = NA,   "GPL19109" = NA,   "GPL19125" = NA,   "GPL19128" = NA,   "GPL19133" = NA,   "GPL19136" = NA,   "GPL19145" = NA,   "GPL19162" = NA,   "GPL19169" = NA,   "GPL19180" = NA,   "GPL19184" = NA,   "GPL1919" = NA,
  "GPL19198" = NA,   "GPL1923" = NA,   "GPL19232" = NA,   "GPL1926" = NA,   "GPL19264" = NA,   "GPL1927" = NA,   "GPL1928" = NA,   "GPL19298" = NA,   "GPL1930" = NA,   "GPL19302" = NA,   "GPL19306" = NA,   "GPL19309" = NA,
  "GPL19316" = NA,   "GPL19322" = NA,   "GPL19323" = NA,   "GPL1935" = NA,   "GPL1936" = NA,   "GPL19370" = "hugene21sttranscriptcluster.db",   "GPL19372" = NA,   "GPL19383" = NA,   "GPL19389" = NA,   "GPL19394" = NA,   "GPL19395" = NA,   "GPL19398" = NA,
  "GPL19413" = NA,   "GPL19420" = NA,   "GPL19433" = NA,   "GPL1947" = NA,   "GPL19471" = NA,   "GPL1953" = NA,   "GPL19541" = NA,   "GPL19546" = NA,   "GPL19550" = NA,   "GPL19563" = NA,   "GPL19589" = NA,   "GPL1962" = NA,
  "GPL19627" = NA,   "GPL19631" = NA,   "GPL19633" = NA,   "GPL19740" = NA,   "GPL1977" = NA,   "GPL19773" = NA,   "GPL19775" = NA,   "GPL19803" = NA,   "GPL19807" = NA,   "GPL19832" = NA,   "GPL19833" = NA,   "GPL19850" = NA,
  "GPL19864" = NA,   "GPL19874" = NA,   "GPL19882" = NA,   "GPL19883" = NA,   "GPL19886" = NA,   "GPL19902" = NA,   "GPL19915" = NA,   "GPL19917" = NA,   "GPL19918" = NA,   "GPL19941" = NA,   "GPL19963" = NA,   "GPL1998" = NA,
  "GPL19981" = NA,   "GPL1999" = NA,   "GPL19995" = NA,   "GPL2002" = NA,   "GPL20026" = NA,   "GPL2004" = NA,   "GPL2006" = NA,   "GPL20078" = NA,   "GPL2009" = NA,   "GPL20095" = NA,   "GPL20103" = NA,   "GPL2011" = NA,
  "GPL20111" = NA,   "GPL20113" = NA,   "GPL20149" = NA,   "GPL20151" = NA,   "GPL20157" = NA,   "GPL20163" = NA,   "GPL20164" = NA,   "GPL20182" = NA,   "GPL20187" = NA,   "GPL20188" = NA,   "GPL20194" = NA,   "GPL20207" = NA,
  "GPL20234" = NA,   "GPL20258" = NA,   "GPL20266" = NA,   "GPL20275" = NA,   "GPL20277" = NA,   "GPL20278" = NA,   "GPL2029" = NA,   "GPL20311" = NA,   "GPL20321" = NA,   "GPL2040" = NA,   "GPL2042" = NA,   "GPL2048" = NA,
  "GPL20562" = NA,   "GPL20573" = NA,   "GPL20574" = NA,   "GPL206" = NA,   "GPL20604" = NA,   "GPL20606" = NA,   "GPL20609" = NA,   "GPL20614" = NA,   "GPL20644" = NA,   "GPL20710" = NA,   "GPL20769" = NA,   "GPL20858" = NA,
  "GPL20880" = NA,   "GPL2094" = NA,   "GPL20945" = NA,   "GPL20952" = NA,   "GPL20967" = NA,   "GPL2097" = NA,   "GPL20982" = NA,   "GPL2099" = NA,   "GPL20995" = NA,   "GPL2100" = NA,   "GPL21032" = NA,   "GPL21047" = NA,
  "GPL21096" = NA,   "GPL21103" = NA,   "GPL21113" = NA,   "GPL2112" = NA,   "GPL21163" = NA,   "GPL21168" = NA,   "GPL21170" = NA,   "GPL21199" = NA,   "GPL21204" = NA,   "GPL21205" = NA,   "GPL21234" = NA,   "GPL21241" = NA,
  "GPL21242" = NA,   "GPL21250" = NA,   "GPL21251" = NA,   "GPL21266" = NA,   "GPL21272" = NA,   "GPL21288" = NA,   "GPL21289" = NA,   "GPL2129" = NA,   "GPL21292" = NA,   "GPL21293" = NA,   "GPL21339" = NA,   "GPL2136" = NA,
  "GPL21369" = NA,   "GPL2138" = NA,   "GPL21390" = NA,   "GPL21436" = NA,   "GPL21437" = NA,   "GPL21464" = NA,   "GPL21472" = NA,   "GPL21493" = NA,   "GPL21503" = NA,   "GPL21509" = "hta20transcriptcluster.db",   "GPL21589" = NA,   "GPL21609" = NA,
  "GPL21614" = NA,   "GPL21627" = NA,   "GPL21790" = NA,   "GPL21791" = NA,   "GPL21799" = NA,   "GPL21810" = NA,   "GPL21828" = NA,   "GPL21847" = NA,   "GPL21857" = NA,   "GPL21870" = NA,   "GPL21943" = NA,   "GPL21947" = NA,
  "GPL21950" = NA,   "GPL21962" = NA,   "GPL21975" = NA,   "GPL21980" = NA,   "GPL22020" = NA,   "GPL22085" = NA,   "GPL22109" = NA,   "GPL22111" = NA,   "GPL22121" = NA,   "GPL22166" = NA,   "GPL22167" = NA,   "GPL22194" = NA,
  "GPL22198" = NA,   "GPL222" = NA,   "GPL22286" = "hugene11sttranscriptcluster.db",   "GPL22299" = NA,   "GPL22303" = NA,   "GPL22321" = NA,   "GPL22330" = NA,   "GPL22358" = NA,   "GPL22361" = NA,   "GPL22366" = NA,   "GPL22377" = NA,   "GPL22382" = NA,
  "GPL22448" = NA,   "GPL22449" = NA,   "GPL22462" = NA,   "GPL22571" = NA,   "GPL22573" = NA,   "GPL22598" = NA,   "GPL22628" = NA,   "GPL22654" = NA,   "GPL22678" = NA,   "GPL22694" = NA,   "GPL22725" = NA,   "GPL22740" = NA,
  "GPL22757" = NA,   "GPL22763" = NA,   "GPL22841" = NA,   "GPL22891" = NA,   "GPL22932" = NA,   "GPL22936" = NA,   "GPL22937" = NA,   "GPL22945" = NA,   "GPL22949" = NA,   "GPL22975" = NA,   "GPL22995" = "hugene10sttranscriptcluster.db",   "GPL23026" = NA,
  "GPL23032" = NA,   "GPL23062" = NA,   "GPL23080" = NA,   "GPL23083" = NA,   "GPL23086" = NA,   "GPL23098" = NA,   "GPL23146" = NA,   "GPL23166" = NA,   "GPL23244" = NA,   "GPL23249" = NA,   "GPL23306" = NA,   "GPL23322" = NA,
  "GPL23342" = NA,   "GPL23345" = NA,   "GPL23434" = NA,   "GPL23464" = NA,   "GPL23507" = NA,   "GPL23526" = NA,   "GPL23541" = NA,   "GPL23556" = NA,   "GPL23587" = NA,   "GPL23613" = NA,   "GPL23642" = NA,   "GPL23643" = NA,
  "GPL23644" = NA,   "GPL23646" = NA,   "GPL23782" = NA,   "GPL23816" = NA,   "GPL23845" = NA,   "GPL23871" = NA,   "GPL23985" = NA,   "GPL23986" = NA,   "GPL24000" = NA,   "GPL24009" = NA,   "GPL24030" = NA,   "GPL24047" = NA,
  "GPL24052" = NA,   "GPL24082" = NA,   "GPL24120" = NA,   "GPL24170" = NA,   "GPL24252" = NA,   "GPL24267" = NA,   "GPL24280" = NA,   "GPL24403" = NA,   "GPL24404" = NA,   "GPL24410" = NA,   "GPL24440" = NA,   "GPL24444" = NA,
  "GPL24460" = NA,   "GPL246" = NA,   "GPL24688" = NA,   "GPL24704" = NA,   "GPL24789" = NA,   "GPL24796" = NA,   "GPL24914" = NA,   "GPL24915" = NA,   "GPL24989" = NA,   "GPL24993" = NA,   "GPL25047" = NA,   "GPL2507" = NA,
  "GPL25197" = NA,   "GPL25266" = NA,   "GPL2529" = NA,   "GPL2531" = NA,   "GPL25318" = NA,   "GPL25371" = NA,   "GPL25373" = NA,   "GPL25471" = NA,   "GPL25483" = NA,   "GPL25488" = NA,   "GPL25507" = NA,   "GPL25533" = NA,
  "GPL2555" = NA,   "GPL25577" = NA,   "GPL25636" = NA,   "GPL2565" = NA,   "GPL25652" = NA,   "GPL2567" = NA,   "GPL25683" = NA,   "GPL25684" = NA,   "GPL257" = NA,   "GPL2570" = NA,   "GPL25714" = NA,   "GPL25734" = NA,
  "GPL25740" = NA,   "GPL25741" = NA,   "GPL2575" = NA,   "GPL2586" = NA,   "GPL25864" = NA,   "GPL259" = NA,   "GPL2595" = NA,   "GPL2596" = NA,   "GPL25961" = NA,   "GPL25978" = NA,   "GPL25996" = NA,   "GPL25998" = NA,
  "GPL261" = NA,   "GPL2614" = NA,   "GPL26157" = NA,   "GPL26190" = NA,   "GPL26192" = NA,   "GPL26199" = NA,   "GPL262" = NA,   "GPL26223" = NA,   "GPL2624" = NA,   "GPL26299" = NA,   "GPL263" = NA,   "GPL2631" = NA,
  "GPL26355" = NA,   "GPL26356" = NA,   "GPL26371" = NA,   "GPL2641" = NA,   "GPL2648" = NA,   "GPL26591" = NA,   "GPL26593" = NA,   "GPL26599" = NA,   "GPL26612" = NA,   "GPL26654" = NA,   "GPL26689" = NA,   "GPL267" = NA,
  "GPL2670" = NA,   "GPL2671" = NA,   "GPL26710" = NA,   "GPL26734" = NA,   "GPL26748" = NA,   "GPL268" = NA,   "GPL26804" = NA,   "GPL2695" = NA,   "GPL2696" = NA,   "GPL26966" = NA,   "GPL26999" = NA,   "GPL2705" = NA,
  "GPL27054" = NA,   "GPL2706" = NA,   "GPL271" = NA,   "GPL2710" = NA,   "GPL27139" = NA,   "GPL2714" = NA,   "GPL27143" = NA,   "GPL2721" = NA,   "GPL2725" = NA,   "GPL273" = NA,   "GPL274" = NA,   "GPL27429" = NA,
  "GPL2746" = NA,   "GPL2747" = NA,   "GPL2758" = NA,   "GPL27624" = NA,   "GPL27630" = NA,   "GPL27634" = NA,   "GPL2767" = NA,   "GPL2768" = NA,   "GPL27692" = NA,   "GPL27713" = NA,   "GPL27719" = NA,   "GPL27734" = NA,
  "GPL2779" = NA,   "GPL27856" = NA,   "GPL27869" = NA,   "GPL27927" = NA,   "GPL2793" = NA,   "GPL27934" = NA,   "GPL27938" = NA,   "GPL27949" = NA,   "GPL27956" = NA,   "GPL27981" = NA,   "GPL2799" = NA,   "GPL28009" = NA,
  "GPL28019" = NA,   "GPL28102" = NA,   "GPL28226" = NA,   "GPL28228" = NA,   "GPL28236" = NA,   "GPL2824" = NA,   "GPL2827" = NA,   "GPL28271" = NA,   "GPL28275" = NA,   "GPL28285" = NA,   "GPL28292" = NA,   "GPL2830" = NA,
  "GPL2831" = NA,   "GPL2832" = NA,   "GPL28327" = NA,   "GPL2834" = NA,   "GPL28387" = NA,   "GPL28392" = NA,   "GPL284" = NA,   "GPL28426" = NA,   "GPL28436" = NA,   "GPL2845" = NA,   "GPL28460" = NA,   "GPL28470" = NA,
  "GPL28485" = NA,   "GPL28538" = NA,   "GPL28542" = NA,   "GPL28589" = NA,   "GPL2866" = NA,   "GPL2869" = NA,   "GPL28710" = NA,   "GPL2872" = NA,   "GPL28725" = NA,   "GPL2873" = NA,   "GPL28782" = NA,   "GPL28784" = NA,
  "GPL2891" = NA,   "GPL2893" = NA,   "GPL28943" = NA,   "GPL2895" = NA,   "GPL28965" = NA,   "GPL28991" = NA,   "GPL28993" = NA,   "GPL2902" = NA,   "GPL2905" = NA,   "GPL29069" = NA,   "GPL29077" = NA,   "GPL2911" = NA,
  "GPL2912" = NA,   "GPL2913" = NA,   "GPL2914" = NA,   "GPL292" = NA,   "GPL2932" = NA,   "GPL2934" = NA,   "GPL2936" = NA,   "GPL29366" = NA,   "GPL2937" = NA,   "GPL29376" = NA,   "GPL29377" = NA,   "GPL2938" = NA,
  "GPL29400" = NA,   "GPL29417" = NA,   "GPL2947" = NA,   "GPL29503" = NA,   "GPL29532" = NA,   "GPL29668" = NA,   "GPL29692" = NA,   "GPL29738" = NA,   "GPL29761" = NA,   "GPL29838" = NA,   "GPL2986" = NA,   "GPL2990" = NA,
  "GPL29913" = NA,   "GPL2992" = NA,   "GPL2995" = NA,   "GPL29983" = NA,   "GPL3003" = NA,   "GPL30106" = NA,   "GPL3014" = NA,   "GPL3016" = NA,   "GPL30188" = NA,   "GPL30198" = NA,   "GPL30206" = NA,   "GPL30216" = NA,
  "GPL30227" = NA,   "GPL30235" = NA,   "GPL30267" = NA,   "GPL3036" = NA,   "GPL3037" = NA,   "GPL3038" = NA,   "GPL3039" = NA,   "GPL3044" = NA,   "GPL3045" = NA,   "GPL3046" = NA,   "GPL3047" = NA,   "GPL30493" = NA,
  "GPL3050" = NA,   "GPL30506" = NA,   "GPL30516" = NA,   "GPL30522" = NA,   "GPL30538" = NA,   "GPL30558" = NA,   "GPL30564" = NA,   "GPL30569" = NA,   "GPL30570" = NA,   "GPL30571" = NA,   "GPL30575" = NA,   "GPL30587" = NA,
  "GPL3064" = NA,   "GPL30655" = NA,   "GPL30657" = NA,   "GPL3073" = NA,   "GPL30818" = NA,   "GPL30828" = NA,   "GPL30829" = NA,   "GPL3084" = NA,   "GPL30873" = NA,   "GPL30877" = NA,   "GPL3090" = NA,   "GPL3093" = NA,
  "GPL30967" = NA,   "GPL30971" = NA,   "GPL30981" = NA,   "GPL30991" = NA,   "GPL310" = NA,   "GPL31007" = NA,   "GPL31028" = NA,   "GPL31059" = NA,   "GPL3110" = NA,   "GPL3113" = NA,   "GPL3114" = NA,   "GPL31146" = NA,
  "GPL31171" = NA,   "GPL31197" = NA,   "GPL31198" = NA,   "GPL312" = NA,   "GPL3122" = NA,   "GPL31236" = NA,   "GPL31250" = NA,   "GPL31258" = NA,   "GPL31274" = NA,   "GPL31275" = NA,   "GPL31281" = NA,   "GPL3155" = NA,
  "GPL317" = NA,   "GPL318" = NA,   "GPL319" = NA,   "GPL31904" = NA,   "GPL31920" = NA,   "GPL31954" = NA,   "GPL31975" = NA,   "GPL32025" = NA,   "GPL32026" = NA,   "GPL32037" = NA,   "GPL32050" = NA,   "GPL32051" = NA,
  "GPL32054" = NA,   "GPL32055" = NA,   "GPL32064" = NA,   "GPL32069" = NA,   "GPL32087" = NA,   "GPL32130" = NA,   "GPL32141" = NA,   "GPL32170" = NA,   "GPL3218" = NA,   "GPL32193" = NA,   "GPL32205" = NA,   "GPL32284" = NA,
  "GPL32301" = NA,   "GPL32346" = NA,   "GPL32366" = NA,   "GPL32382" = NA,   "GPL32383" = NA,   "GPL32385" = NA,   "GPL32482" = NA,   "GPL32486" = NA,   "GPL32510" = NA,   "GPL3254" = NA,   "GPL32540" = NA,   "GPL3255" = NA,
  "GPL32550" = NA,   "GPL32571" = NA,   "GPL32574" = NA,   "GPL32578" = NA,   "GPL32584" = NA,   "GPL32602" = NA,   "GPL32605" = NA,   "GPL32619" = NA,   "GPL3262" = NA,   "GPL32643" = NA,   "GPL3268" = NA,   "GPL32704" = NA,
  "GPL3271" = NA,   "GPL32716" = NA,   "GPL3279" = NA,   "GPL3282" = NA,   "GPL32835" = NA,   "GPL32845" = NA,   "GPL32862" = NA,   "GPL32883" = NA,   "GPL3289" = NA,   "GPL3290" = NA,   "GPL32913" = NA,   "GPL3294" = NA,
  "GPL3295" = NA,   "GPL3296" = NA,   "GPL32993" = NA,   "GPL32994" = NA,   "GPL32998" = NA,   "GPL33019" = NA,   "GPL33038" = NA,   "GPL3305" = NA,   "GPL3306" = NA,   "GPL3307" = NA,   "GPL33081" = NA,   "GPL33087" = NA,
  "GPL33108" = NA,   "GPL33162" = NA,   "GPL33178" = NA,   "GPL3323" = NA,   "GPL33240" = NA,   "GPL33245" = NA,   "GPL33274" = NA,   "GPL33289" = NA,   "GPL33307" = NA,   "GPL33312" = NA,   "GPL3332" = NA,   "GPL3334" = NA,
  "GPL33340" = NA,   "GPL3335" = NA,   "GPL3341" = NA,   "GPL33415" = NA,   "GPL33436" = NA,   "GPL3344" = NA,   "GPL33442" = NA,   "GPL33458" = NA,   "GPL3347" = NA,   "GPL3348" = NA,   "GPL33484" = NA,   "GPL33497" = NA,
  "GPL335" = NA,   "GPL33505" = NA,   "GPL33512" = NA,   "GPL33519" = NA,   "GPL3355" = NA,   "GPL33557" = NA,   "GPL33581" = NA,   "GPL33582" = NA,   "GPL3367" = NA,   "GPL33679" = NA,   "GPL33696" = NA,   "GPL33697" = NA,
  "GPL3370" = NA,   "GPL33751" = NA,   "GPL33768" = NA,   "GPL3377" = NA,   "GPL33774" = NA,   "GPL33788" = NA,   "GPL3382" = NA,   "GPL3383" = NA,   "GPL3385" = NA,   "GPL3386" = NA,   "GPL339" = NA,   "GPL33903" = NA,
  "GPL3392" = NA,   "GPL33927" = NA,   "GPL33963" = NA,   "GPL33991" = NA,   "GPL3400" = NA,   "GPL3405" = NA,   "GPL3408" = NA,   "GPL341" = NA,   "GPL34104" = NA,   "GPL34112" = NA,   "GPL34133" = NA,   "GPL34160" = NA,
  "GPL3417" = NA,   "GPL3418" = NA,   "GPL34183" = NA,   "GPL3419" = NA,   "GPL342" = NA,   "GPL3423" = NA,   "GPL34246" = NA,   "GPL3427" = NA,   "GPL34276" = NA,   "GPL3428" = NA,   "GPL34289" = NA,   "GPL34293" = NA,
  "GPL34400" = NA,   "GPL34424" = NA,   "GPL34482" = NA,   "GPL34535" = NA,   "GPL34536" = NA,   "GPL34537" = NA,   "GPL3457" = NA,   "GPL34587" = NA,   "GPL34626" = NA,   "GPL3463" = NA,   "GPL34649" = NA,   "GPL3465" = NA,
  "GPL34684" = NA,   "GPL34719" = NA,   "GPL34724" = NA,   "GPL34748" = NA,   "GPL34763" = NA,   "GPL34764" = NA,   "GPL3480" = NA,   "GPL34817" = NA,   "GPL34905" = NA,   "GPL3494" = NA,   "GPL350" = NA,   "GPL3501" = NA,
  "GPL3515" = NA,   "GPL3519" = NA,   "GPL3523" = NA,   "GPL35247" = NA,   "GPL3528" = NA,   "GPL353" = NA,   "GPL3533" = NA,   "GPL35386" = NA,   "GPL3544" = NA,   "GPL355" = NA,   "GPL3566" = NA,   "GPL35738" = NA,
  "GPL35739" = NA,   "GPL35757" = NA,   "GPL35924" = NA,   "GPL36027" = NA,   "GPL3606" = NA,   "GPL36080" = NA,   "GPL36192" = NA,   "GPL36360" = NA,   "GPL3637" = NA,   "GPL3639" = NA,   "GPL367" = NA,   "GPL3676" = NA,
  "GPL3694" = NA,   "GPL3696" = NA,   "GPL371" = NA,   "GPL3718" = NA,   "GPL3720" = NA,   "GPL373" = NA,   "GPL3730" = NA,   "GPL3732" = NA,   "GPL3733" = NA,   "GPL3736" = NA,   "GPL3762" = NA,   "GPL3779" = NA,
  "GPL3784" = NA,   "GPL3787" = NA,   "GPL3796" = NA,   "GPL3834" = NA,   "GPL3836" = NA,   "GPL3837" = NA,   "GPL3865" = NA,   "GPL3871" = NA,   "GPL3872" = NA,   "GPL3877" = NA,   "GPL3883" = NA,   "GPL3895" = NA,
  "GPL3898" = NA,   "GPL3899" = NA,   "GPL3904" = NA,   "GPL3906" = NA,   "GPL3916" = NA,   "GPL3928" = NA,   "GPL3941" = NA,   "GPL3942" = NA,   "GPL3963" = NA,   "GPL3964" = NA,   "GPL3966" = NA,   "GPL3979" = NA,
  "GPL3985" = NA,   "GPL3991" = NA,   "GPL3992" = NA,   "GPL4" = NA,   "GPL4035" = NA,   "GPL4044" = NA,   "GPL4048" = NA,   "GPL4051" = NA,   "GPL4052" = NA,   "GPL4060" = NA,   "GPL4083" = NA,   "GPL4084" = NA,
  "GPL4104" = NA,   "GPL4124" = NA,   "GPL4125" = NA,   "GPL4134" = NA,   "GPL4190" = NA,   "GPL4191" = NA,   "GPL4203" = NA,   "GPL4204" = NA,   "GPL4213" = NA,   "GPL4214" = NA,   "GPL4227" = NA,   "GPL4234" = NA,
  "GPL4255" = NA,   "GPL4270" = NA,   "GPL4271" = NA,   "GPL4274" = NA,   "GPL4302" = NA,   "GPL4335" = NA,   "GPL4338" = NA,   "GPL4348" = NA,   "GPL4370" = NA,   "GPL4372" = NA,   "GPL4384" = NA,   "GPL4387" = NA,
  "GPL4390" = NA,   "GPL4391" = NA,   "GPL4397" = NA,   "GPL4419" = NA,   "GPL4452" = NA,   "GPL4457" = NA,   "GPL4463" = NA,   "GPL4475" = NA,   "GPL4512" = NA,   "GPL4525" = NA,   "GPL4540" = NA,   "GPL4541" = NA,
  "GPL4562" = NA,   "GPL4564" = NA,   "GPL4582" = NA,   "GPL4590" = NA,   "GPL4608" = NA,   "GPL4611" = NA,   "GPL4615" = NA,   "GPL4636" = NA,   "GPL4637" = NA,   "GPL4666" = NA,   "GPL4671" = NA,   "GPL4672" = NA,
  "GPL4685" = NA,   "GPL4692" = NA,   "GPL4693" = NA,   "GPL4695" = NA,   "GPL4700" = NA,   "GPL4717" = NA,   "GPL4718" = NA,   "GPL4719" = NA,   "GPL4723" = NA,   "GPL4725" = NA,   "GPL4742" = NA,   "GPL4747" = NA,
  "GPL4750" = NA,   "GPL4757" = NA,   "GPL4763" = NA,   "GPL4764" = NA,   "GPL4766" = NA,   "GPL4767" = NA,   "GPL4768" = NA,   "GPL4779" = NA,   "GPL4781" = NA,   "GPL4782" = NA,   "GPL4788" = NA,   "GPL4789" = NA,
  "GPL4803" = NA,   "GPL4811" = NA,   "GPL4814" = NA,   "GPL4820" = NA,   "GPL4840" = NA,   "GPL4857" = NA,   "GPL4858" = NA,   "GPL4861" = NA,   "GPL4862" = NA,   "GPL4866" = NA,   "GPL4868" = NA,   "GPL487" = NA,
  "GPL4880" = NA,   "GPL4882" = NA,   "GPL4883" = NA,   "GPL490" = NA,   "GPL4910" = NA,   "GPL4912" = NA,   "GPL4925" = NA,   "GPL4926" = NA,   "GPL4989" = NA,   "GPL4990" = NA,   "GPL4997" = NA,   "GPL503" = NA,
  "GPL5045" = NA,   "GPL5058" = NA,   "GPL506" = NA,   "GPL5060" = NA,   "GPL5067" = NA,   "GPL5071" = NA,   "GPL5080" = NA,   "GPL5082" = NA,   "GPL5089" = NA,   "GPL511" = NA,   "GPL5114" = NA,   "GPL5118" = NA,
  "GPL5121" = NA,   "GPL5134" = NA,   "GPL5139" = NA,   "GPL5150" = NA,   "GPL5152" = NA,   "GPL5162" = NA,   "GPL5163" = NA,   "GPL5166" = NA,   "GPL5186" = NA,   "GPL520" = NA,   "GPL5215" = NA,   "GPL5216" = NA,
  "GPL5325" = NA,   "GPL5326" = NA,   "GPL5346" = NA,   "GPL5352" = NA,   "GPL5354" = NA,   "GPL5355" = NA,   "GPL5356" = NA,   "GPL5366" = NA,   "GPL5376" = NA,   "GPL5377" = NA,   "GPL538" = NA,   "GPL5380" = NA,
  "GPL5388" = NA,   "GPL5391" = NA,   "GPL543" = NA,   "GPL544" = NA,   "GPL5444" = NA,   "GPL5447" = NA,   "GPL546" = NA,   "GPL5460" = NA,   "GPL547" = NA,   "GPL5474" = NA,   "GPL5477" = NA,   "GPL5481" = NA,
  "GPL549" = NA,   "GPL550" = NA,   "GPL5572" = NA,   "GPL561" = NA,   "GPL562" = NA,   "GPL5620" = NA,   "GPL5625" = NA,   "GPL5632" = NA,   "GPL564" = NA,   "GPL5642" = NA,   "GPL5645" = NA,   "GPL567" = NA,
  "GPL5676" = NA,   "GPL5732" = NA,   "GPL5742" = NA,   "GPL5752" = NA,   "GPL5760" = NA,   "GPL5764" = NA,   "GPL5770" = NA,   "GPL5799" = NA,   "GPL5802" = NA,   "GPL5804" = NA,   "GPL5807" = NA,   "GPL5811" = NA,
  "GPL5820" = NA,   "GPL5826" = NA,   "GPL5833" = NA,   "GPL5835" = NA,   "GPL5872" = NA,   "GPL5886" = NA,   "GPL5909" = NA,   "GPL5917" = NA,   "GPL5918" = NA,   "GPL5920" = NA,   "GPL5936" = NA,   "GPL5953" = NA,
  "GPL5959" = NA,   "GPL5968" = NA,   "GPL5981" = NA,   "GPL6011" = NA,   "GPL6014" = NA,   "GPL6017" = NA,   "GPL6042" = NA,   "GPL6071" = NA,   "GPL6087" = NA,   "GPL6096" = NA,   "GPL6097" = NA,   "GPL6098" = NA,
  "GPL6101" = NA,   "GPL6103" = NA,   "GPL6106" = NA,   "GPL6118" = NA,   "GPL6119" = NA,   "GPL6120" = NA,   "GPL6152" = NA,   "GPL6171" = NA,   "GPL6193" = NA,   "GPL6229" = NA,   "GPL6247" = NA,   "GPL6248" = NA,
  "GPL6254" = NA,   "GPL6255" = NA,   "GPL6256" = NA,   "GPL6270" = NA,   "GPL6271" = NA,   "GPL6288" = NA,   "GPL6311" = NA,   "GPL6313" = NA,   "GPL6325" = NA,   "GPL6333" = NA,   "GPL6353" = NA,   "GPL6370" = NA,
  "GPL6398" = NA,   "GPL6400" = NA,   "GPL6425" = NA,   "GPL6426" = NA,   "GPL6441" = NA,   "GPL6442" = NA,   "GPL6458" = NA,   "GPL6465" = NA,   "GPL6481" = NA,   "GPL6484" = NA,   "GPL6486" = NA,   "GPL6487" = NA,
  "GPL6511" = NA,   "GPL6522" = NA,   "GPL6534" = NA,   "GPL6557" = NA,   "GPL6565" = NA,   "GPL6569" = NA,   "GPL6602" = NA,   "GPL6603" = NA,   "GPL6604" = NA,   "GPL6647" = NA,   "GPL6648" = NA,   "GPL6650" = NA,
  "GPL6665" = NA,   "GPL6671" = NA,   "GPL6674" = NA,   "GPL6696" = NA,   "GPL6698" = NA,   "GPL6699" = NA,   "GPL6704" = NA,   "GPL6729" = NA,   "GPL6732" = NA,   "GPL6735" = NA,   "GPL6736" = NA,   "GPL6739" = NA,
  "GPL6762" = NA,   "GPL6780" = NA,   "GPL6782" = NA,   "GPL6787" = NA,   "GPL6788" = NA,   "GPL6789" = NA,   "GPL6790" = NA,   "GPL6791" = NA,   "GPL6793" = NA,   "GPL6794" = NA,   "GPL6802" = NA,   "GPL6803" = NA,
  "GPL6804" = NA,   "GPL6806" = NA,   "GPL6824" = NA,   "GPL6844" = NA,   "GPL6854" = NA,   "GPL6865" = NA,   "GPL6879" = NA,   "GPL6884" = NA,   "GPL6887" = NA,   "GPL6893" = NA,   "GPL6918" = NA,   "GPL6931" = NA,
  "GPL6955" = NA,   "GPL6964" = NA,   "GPL6977" = NA,   "GPL6978" = NA,   "GPL6982" = NA,   "GPL6984" = NA,   "GPL6985" = NA,   "GPL6986" = NA,   "GPL6991" = NA,   "GPL6999" = NA,   "GPL7" = NA,   "GPL7020" = NA,
  "GPL7025" = NA,   "GPL7026" = NA,   "GPL7032" = NA,   "GPL7034" = NA,   "GPL7038" = NA,   "GPL7046" = NA,   "GPL7047" = NA,   "GPL7052" = NA,   "GPL7088" = NA,   "GPL7089" = NA,   "GPL7091" = NA,   "GPL7141" = NA,
  "GPL7142" = NA,   "GPL7144" = NA,   "GPL7149" = NA,   "GPL7152" = NA,   "GPL7153" = NA,   "GPL7172" = NA,   "GPL7178" = NA,   "GPL7179" = NA,   "GPL7180" = NA,   "GPL7191" = NA,   "GPL7192" = NA,   "GPL7198" = NA,
  "GPL7202" = NA,   "GPL7203" = NA,   "GPL7210" = NA,   "GPL7217" = NA,   "GPL7220" = NA,   "GPL7247" = NA,   "GPL7260" = NA,   "GPL7262" = NA,   "GPL7264" = NA,   "GPL7280" = NA,   "GPL7304" = NA,   "GPL7306" = NA,
  "GPL7328" = NA,   "GPL7336" = NA,   "GPL7337" = NA,   "GPL7339" = NA,   "GPL7341" = NA,   "GPL7350" = NA,   "GPL737" = NA,   "GPL7384" = NA,   "GPL7392" = NA,   "GPL7393" = NA,   "GPL7397" = NA,   "GPL74" = NA,
  "GPL7409" = NA,   "GPL7425" = NA,   "GPL7430" = NA,   "GPL7473" = NA,   "GPL7474" = NA,   "GPL7486" = NA,   "GPL7490" = NA,   "GPL7491" = NA,   "GPL7492" = NA,   "GPL7502" = NA,   "GPL7504" = NA,   "GPL7540" = NA,
  "GPL7546" = NA,   "GPL7557" = NA,   "GPL7567" = NA,   "GPL7583" = NA,   "GPL7641" = NA,   "GPL7674" = NA,   "GPL7686" = NA,   "GPL7687" = NA,   "GPL7688" = NA,   "GPL7694" = NA,   "GPL7695" = NA,   "GPL7722" = NA,
  "GPL7723" = NA,   "GPL7724" = NA,   "GPL7731" = NA,   "GPL7732" = NA,   "GPL7753" = NA,   "GPL7759" = NA,   "GPL7763" = NA,   "GPL7768" = NA,   "GPL7775" = NA,   "GPL7785" = NA,   "GPL7791" = NA,   "GPL781" = NA,
  "GPL783" = NA,   "GPL7869" = NA,   "GPL787" = NA,   "GPL7870" = NA,   "GPL80" = NA,   "GPL8014" = NA,   "GPL8018" = NA,   "GPL8019" = NA,   "GPL8027" = NA,   "GPL8032" = NA,   "GPL8040" = NA,   "GPL8041" = NA,
  "GPL8042" = NA,   "GPL8060" = NA,   "GPL8070" = NA,   "GPL8078" = NA,   "GPL8079" = NA,   "GPL8091" = NA,   "GPL81" = NA,   "GPL8104" = NA,   "GPL8131" = NA,   "GPL8136" = NA,   "GPL8150" = NA,   "GPL8155" = NA,
  "GPL8158" = NA,   "GPL8159" = NA,   "GPL8170" = NA,   "GPL8171" = NA,   "GPL8173" = NA,   "GPL8177" = NA,   "GPL8178" = NA,   "GPL8186" = NA,   "GPL8187" = NA,   "GPL8191" = NA,   "GPL8217" = NA,   "GPL8220" = NA,
  "GPL8234" = NA,   "GPL8238" = NA,   "GPL8253" = NA,   "GPL8264" = NA,   "GPL8265" = NA,   "GPL8275" = NA,   "GPL8281" = NA,   "GPL8289" = NA,   "GPL8300" = "hgu95av2.db",   "GPL8307" = NA,   "GPL8312" = NA,   "GPL8315" = NA,
  "GPL8321" = NA,   "GPL8328" = NA,   "GPL8332" = NA,   "GPL8335" = NA,   "GPL8355" = NA,   "GPL8376" = NA,   "GPL8380" = NA,   "GPL8414" = NA,   "GPL8429" = NA,   "GPL8432" = "illuminaHumanv2.db",   "GPL8436" = NA,   "GPL8439" = NA,
  "GPL8444" = NA,   "GPL8445" = NA,   "GPL8450" = NA,   "GPL8469" = NA,   "GPL8476" = NA,   "GPL8482" = NA,   "GPL85" = NA,   "GPL8521" = NA,   "GPL8542" = NA,   "GPL8547" = NA,   "GPL8557" = NA,   "GPL8580" = NA,
  "GPL8581" = NA,   "GPL8583" = NA,   "GPL8601" = NA,   "GPL8607" = NA,   "GPL8617" = NA,   "GPL8637" = NA,   "GPL864" = NA,   "GPL8640" = NA,   "GPL8687" = NA,   "GPL8695" = NA,   "GPL8712" = NA,   "GPL8713" = NA,
  "GPL8715" = NA,   "GPL8733" = NA,   "GPL8736" = NA,   "GPL8737" = NA,   "GPL875" = NA,   "GPL8755" = NA,   "GPL8757" = NA,   "GPL876" = NA,   "GPL8774" = NA,   "GPL8786" = NA,   "GPL8791" = NA,   "GPL8792" = NA,
  "GPL8793" = NA,   "GPL8798" = NA,   "GPL8841" = NA,   "GPL8842" = NA,   "GPL8843" = NA,   "GPL885" = NA,   "GPL8870" = NA,   "GPL8882" = NA,   "GPL8888" = NA,   "GPL8894" = NA,   "GPL8899" = NA,   "GPL891" = NA,
  "GPL8910" = NA,   "GPL8926" = NA,   "GPL8930" = NA,   "GPL8933" = NA,   "GPL8936" = NA,   "GPL8941" = NA,   "GPL8950" = NA,   "GPL8957" = NA,   "GPL8971" = NA,   "GPL8979" = NA,   "GPL8986" = NA,   "GPL8987" = NA,
  "GPL9" = NA,   "GPL9006" = NA,   "GPL9021" = NA,   "GPL9035" = NA,   "GPL9053" = NA,   "GPL9075" = NA,   "GPL9077" = NA,   "GPL9099" = NA,   "GPL9101" = NA,   "GPL9102" = NA,   "GPL9116" = NA,   "GPL9137" = NA,
  "GPL9143" = NA,   "GPL9144" = NA,   "GPL9160" = NA,   "GPL9164" = NA,   "GPL9183" = NA,   "GPL9185" = NA,   "GPL9187" = NA,   "GPL9189" = NA,   "GPL9193" = NA,   "GPL92" = NA,   "GPL9202" = NA,   "GPL9243" = NA,
  "GPL9244" = NA,   "GPL9250" = NA,   "GPL9257" = NA,   "GPL9258" = NA,   "GPL9275" = NA,   "GPL93" = NA,   "GPL9301" = NA,   "GPL9318" = NA,   "GPL9324" = NA,   "GPL9335" = NA,   "GPL9356" = NA,   "GPL9365" = NA,
  "GPL9392" = NA,   "GPL94" = NA,   "GPL9419" = NA,   "GPL9423" = NA,   "GPL9435" = NA,   "GPL9448" = NA,   "GPL9451" = NA,   "GPL9452" = NA,   "GPL9454" = NA,   "GPL9472" = NA,   "GPL9486" = NA,   "GPL9494" = NA,
  "GPL9495" = NA,   "GPL9496" = NA,   "GPL9497" = NA,   "GPL95" = NA,   "GPL9530" = NA,   "GPL9541" = NA,   "GPL9550" = NA,   "GPL9551" = NA,   "GPL9557" = NA,   "GPL961" = NA,   "GPL962" = NA,   "GPL9677" = NA,
  "GPL9686" = NA,   "GPL9692" = NA,   "GPL970" = NA,   "GPL9700" = NA,   "GPL9711" = NA,   "GPL9716" = NA,   "GPL9717" = NA,   "GPL9718" = NA,   "GPL9733" = NA,   "GPL9735" = NA,   "GPL9741" = NA,   "GPL9742" = NA,
  "GPL976" = NA,   "GPL9767" = NA,   "GPL9770" = NA,   "GPL9787" = NA,   "GPL9793" = NA,   "GPL98" = NA,   "GPL980" = NA,   "GPL9801" = NA,   "GPL981" = NA,   "GPL9811" = NA,   "GPL9827" = NA,   "GPL9828" = NA,
  "GPL9851" = NA,   "GPL9869" = NA,   "GPL9870" = NA,   "GPL9872" = NA,   "GPL9886" = NA,   "GPL99" = NA,   "GPL9955" = NA,   "GPL9957" = NA,   "GPL9959" = NA,   "GPL996" = NA,   "GPL9960" = NA,   "GPL9962" = NA,
  "GPL9965" = NA,   "GPL9999" = NA,   "GPL5639" = NA,   "GPL6102" = NA,   "GPL8269" = NA,   "GPL9197" = NA,   "GPL10150" = NA,   "GPL15085" = NA,   "GPL17346" = NA,   "GPL17382" = NA,   "GPL17529" = NA,   "GPL18614" = NA,
  "GPL18888" = NA,   "GPL18999" = NA,   "GPL19069" = NA,   "GPL19079" = NA,   "GPL19221" = NA,   "GPL19314" = NA,   "GPL19327" = NA,   "GPL18116" = NA,   "GPL18951" = NA,   "GPL13886" = NA,   "GPL18869" = NA,   "GPL18868" = NA,
  "GPL18870" = NA,   "GPL18316" = NA,   "GPL9128" = NA,   "GPL18720" = NA,   "GPL16794" = NA,   "GPL17391" = NA,   "GPL4091" = NA,   "GPL15019" = NA,   "GPL18117" = NA,   "GPL18584" = NA,   "GPL13328" = NA,   "GPL13329" = NA,
  "GPL13938" = NA,   "GPL16597" = NA,   "GPL18226" = NA,   "GPL18386" = NA,   "GPL18346" = NA,   "GPL16851" = NA,   "GPL2616" = NA,   "GPL18121" = NA,   "GPL17727" = NA,   "GPL17176" = NA,   "GPL18290" = NA,   "GPL14851" = NA,
  "GPL1162" = NA,   "GPL15676" = NA,   "GPL18035" = NA,   "GPL17020" = NA,   "GPL16566" = NA,   "GPL3535" = NA,   "GPL15644" = NA,   "GPL14923" = NA,   "GPL11264" = NA,   "GPL11263" = NA,   "GPL17423" = NA,   "GPL17888" = NA,
  "GPL17854" = NA,   "GPL17842" = NA,   "GPL16483" = NA,   "GPL16452" = NA,   "GPL17341" = NA,   "GPL16209" = NA,   "GPL8855" = NA,   "GPL8169" = NA,   "GPL15838" = NA,   "GPL10561" = NA,   "GPL17740" = NA,   "GPL16850" = NA,
  "GPL16861" = NA,   "GPL16986" = NA,   "GPL16688" = NA,   "GPL8179" = NA,   "GPL15396" = NA,   "GPL15834" = NA,   "GPL16342" = NA,   "GPL16352" = NA,   "GPL8887" = NA,   "GPL15016" = NA,   "GPL4420" = NA,   "GPL2700" = NA,
  "GPL6848" = NA,   "GPL15096" = NA,   "GPL15852" = NA,   "GPL15919" = NA,   "GPL15720" = NA,   "GPL10658" = NA,   "GPL14895" = NA,   "GPL10122" = NA,   "GPL14722" = NA,   "GPL14786" = NA,   "GPL11239" = NA,   "GPL16036" = NA,
  "GPL9460" = NA,   "GPL13703" = NA,   "GPL14149" = NA,   "GPL13428" = NA,   "GPL14689" = NA,   "GPL15257" = NA,   "GPL14822" = NA,   "GPL15100" = NA,   "GPL9946" = NA,   "GPL9443" = NA,   "GPL15062" = NA,   "GPL13949" = NA,
  "GPL13889" = NA,   "GPL13454" = NA,   "GPL14874" = NA,   "GPL14552" = NA,   "GPL14858" = NA,   "GPL14836" = NA,   "GPL14829" = NA,   "GPL91" = NA,   "GPL14577" = NA,   "GPL5858" = NA,   "GPL10554" = NA,   "GPL9188" = NA,
  "GPL9517" = NA,   "GPL13755" = NA,   "GPL13732" = NA,   "GPL11095" = NA,   "GPL3980" = NA,   "GPL4599" = NA,   "GPL13347" = NA,   "GPL13505" = NA,   "GPL13506" = NA,   "GPL10406" = NA,   "GPL6350" = NA,   "GPL10063" = NA,
  "GPL2005" = NA,   "GPL5106" = NA,   "GPL11342" = NA,   "GPL9283" = NA,   "GPL10371" = NA,   "GPL7436" = NA,   "GPL9190" = NA,   "GPL10087" = NA,   "GPL6454" = NA,   "GPL10415" = NA,   "GPL10681" = NA,   "GPL5345" = NA,
  "GPL8949" = NA,   "GPL10487" = NA,   "GPL9666" = NA,   "GPL7408" = NA,   "GPL7363" = NA,   "GPL1180" = NA,   "GPL6127" = NA,   "GPL9987" = NA,   "GPL10254" = NA,   "GPL6326" = NA,   "GPL6773" = NA,   "GPL9966" = NA,
  "GPL8923" = NA,   "GPL9674" = NA,   "GPL8745" = NA,   "GPL9227" = NA,   "GPL8876" = NA,   "GPL7371" = NA,   "GPL7388" = NA,   "GPL8594" = NA,   "GPL8128" = NA,   "GPL6608" = NA,   "GPL6609" = NA,   "GPL6488" = NA,
  "GPL6547" = NA,   "GPL6866" = NA,   "GPL6745" = NA,   "GPL6354" = NA,   "GPL5860" = NA,   "GPL6183" = NA,   "GPL5962" = NA,   "GPL5679" = NA,   "GPL4454" = NA,   "GPL4242" = NA,   "GPL3999" = NA,   "GPL5445" = NA,
  "GPL4867" = NA,   "GPL3892" = NA,   "GPL4400" = NA,   "GPL4081" = NA,   "GPL4665" = NA,   "GPL3349" = NA,   "GPL4349" = NA,   "GPL990" = NA,   "GPL7009" = NA,   "GPL351" = NA,   "GPL1708" = NA,   "GPL784" = NA,
  "GPL10" = NA,   "GPL28650" = NA,   "GPL23694" = NA,   "GPL20571" = NA,   "GPL19534" = NA,   "GPL19535" = NA,   "GPL15135" = NA,   "GPL11384" = NA,   "GPL7080" = NA,   "GPL1115" = NA
)

# Annotation package -> human-readable platform type (for logs/UI; no more bare "NA")
annot_pkg_to_type <- c(
  "hgu133a.db" = "Affymetrix HG-U133A",
  "hgu133b.db" = "Affymetrix HG-U133B",
  "hgu133plus2.db" = "Affymetrix HG-U133 Plus 2.0",
  "hgu133a2.db" = "Affymetrix HG-U133A 2.0",
  "hugene10sttranscriptcluster.db" = "Affymetrix HuGene 1.0 ST",
  "hugene11sttranscriptcluster.db" = "Affymetrix HuGene 1.1 ST",
  "hugene20sttranscriptcluster.db" = "Affymetrix HuGene 2.0 ST",
  "hugene21sttranscriptcluster.db" = "Affymetrix HuGene 2.1 ST",
  "hta20transcriptcluster.db" = "Affymetrix HTA 2.0",
  "huex10sttranscriptcluster.db" = "Affymetrix HuEx 1.0 ST",
  "primeview.db" = "Affymetrix PrimeView",
  "clariomdhuman.db" = "Affymetrix Clariom D Human",
  "clarionshuman.db" = "Affymetrix Clariom S Human",
  "illuminahumanv2.db" = "Illumina HumanHT-12 v2",
  "illuminahumanht12v3.db" = "Illumina HumanHT-12 v3",
  "illuminahumanht12v4.db" = "Illumina HumanHT-12 v4",
  "illuminaMousev2.db" = "Illumina Mouse v2",
  "mouse4302.db" = "Affymetrix Mouse 430 2.0",
  "hgu95a.db" = "Affymetrix HG-U95A",
  "hgu95b.db" = "Affymetrix HG-U95B",
  "mgu74av2.db" = "Affymetrix MG-U74A v2",
  "hugenefl.db" = "Affymetrix HuGeneFL"
)
# Bulk RNA-seq and Microarray GPL -> full platform name (replaces "Other microarray (biomaRt)" / short type in UI lists)
platform_id_display_name_override <- c(
  # Microarray (Affymetrix, Illumina, Agilent)
  "GPL96" = "Affymetrix Human Genome U133 Array",
  "GPL97" = "Affymetrix Human Genome U133 Plus 2.0 Array (B-set)",
  "GPL570" = "[HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL571" = "Affymetrix Human Genome U133A 2.0 Array",
  "GPL887" = "Affymetrix Human Genome U95B",
  "GPL1053" = "Affymetrix Human Genome U133 Plus 2.0 Array (B-set)",
  "GPL1073" = "Affymetrix Human Genome U133A Array",
  "GPL1261" = "Affymetrix Mouse Genome 430 2.0 Array",
  "GPL1312" = "Affymetrix Mouse Genome U74A Version 2 Array",
  "GPL1322" = "Affymetrix Human Full Length HuGeneFL Array",
  "GPL1352" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL1390" = "Affymetrix Human Genome U95A Array",
  "GPL3921" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL5175" = "Affymetrix Human Exon 1.0 ST Array",
  "GPL6104" = "Illumina HumanWG-6 v2.0 Expression BeadChip",
  "GPL6244" = "Affymetrix Human Gene 1.0 ST Array",
  "GPL6885" = "Illumina MouseRef-8 v2.0 Expression BeadChip",
  "GPL10558" = "Illumina HumanHT-12 V4.0 expression beadchip",
  "GPL10739" = "Affymetrix Human Genome U133 Plus 2.0 Array (Custom)",
  "GPL17586" = "Affymetrix Human Transcriptome Array 2.0",
  "GPL17930" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL19370" = "Affymetrix Human Gene 2.1 ST Array (v1)",
  "GPL21509" = "Affymetrix Human Transcriptome Array 2.0",
  "GPL23126" = "Affymetrix Clariom D Human Array",
  "GPL23270" = "Affymetrix Clariom S Human Array",
  "GPL1843" = "Agilent-014850 Whole Human Genome (Custom/GEO Table)",
  "GPL17303" = "Agilent-028004 SurePrint G3 Human GE (Custom/Agilent)",
  "GPL13607" = "Agilent-028004 SurePrint G3 Human GE 8x60K Microarray (Feature Number version)",
  # Microarray (all remaining .db platforms — original full names)
  "GPL201" = "Affymetrix Human Genome U133 Array",
  "GPL5188" = "Affymetrix Human Exon 1.0 ST Array",
  "GPL8300" = "Affymetrix Human Genome U95A Version 2 Array",
  "GPL8432" = "Illumina HumanHT-12 V2.0 Expression BeadChip",
  "GPL10904" = "Illumina HumanHT-12 V4.0 Expression BeadChip",
  "GPL11532" = "Affymetrix Human Gene 1.1 ST Array",
  "GPL13158" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL13915" = "Affymetrix Human Gene 1.0 ST Array",
  "GPL14951" = "Affymetrix Human Gene 1.0 ST Array",
  "GPL15088" = "Affymetrix Human Gene 2.0 ST Array",
  "GPL15207" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL15314" = "Affymetrix Human Gene 1.1 ST Array",
  "GPL16043" = "Affymetrix PrimeView Array",
  "GPL16311" = "Affymetrix Human Gene 2.0 ST Array",
  "GPL16686" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL17556" = "Affymetrix Human Gene 1.1 ST Array",
  "GPL17585" = "Affymetrix Human Transcriptome Array 2.0",
  "GPL17692" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL18990" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL18991" = "Affymetrix Human Genome U133A 2.0 Array",
  "GPL19251" = "Affymetrix Human Transcriptome Array 2.0",
  "GPL19859" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL19983" = "Affymetrix Human Genome U133A 2.0 Array",
  "GPL20265" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL21061" = "Affymetrix Human Gene 2.0 ST Array",
  "GPL21559" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL21970" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL22286" = "Affymetrix Human Gene 1.1 ST Array",
  "GPL22995" = "Affymetrix Human Gene 1.0 ST Array",
  "GPL23159" = "Affymetrix Clariom D Human Array",
  "GPL23432" = "Affymetrix Human Genome U133 Plus 2.0 Array",
  "GPL23961" = "Affymetrix Clariom S Human Array",
  "GPL24299" = "Affymetrix Clariom S Human Array",
  "GPL24507" = "Affymetrix Clariom S Human Array",
  "GPL24532" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL24676" = "Illumina HumanHT-12 V3.0 Expression BeadChip",
  "GPL25249" = "Affymetrix Clariom S Human Array",
  "GPL25336" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL25381" = "Affymetrix Clariom S Human Array",
  "GPL26944" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL28718" = "Affymetrix Human Gene 1.0 ST Array",
  "GPL29829" = "Affymetrix Human Genome U133 Array",
  "GPL30572" = "Affymetrix Human Gene 2.1 ST Array",
  "GPL6947" = "Illumina HumanHT-12 V3.0 Expression BeadChip",
  # Additional microarray / non-RNA-seq platforms (user-provided official names)
  "GPL13667" = "[HG-U219] Affymetrix Human Genome U219 Array",
  "GPL14550" = "Agilent-028004 SurePrint G3 Human GE 8x60K Microarray (Probe Name version)",
  "GPL8227" = "Agilent-019118 Human miRNA Microarray 2.0 G4470B (miRNA ID version)",
  "GPL8363" = "[HG-U219] Affymetrix Human Genome U219 Array (Probe Set ID version)",
  "GPL8490" = "Illumina HumanMethylation27 BeadChip",
  "GPL9081" = "Agilent-016436 Human miRNA Microarray G4470A (miRNA ID version)",
  "GPL13534" = "Illumina HumanMethylation450 BeadChip",
  "GPL17077" = "Agilent-039494 SurePrint G3 Human GE v2 8x60K Microarray (Probe Name version)",
  "GPL20115" = "Agilent-067406 Human CBC lncRNA + mRNA microarray V4.0",
  "GPL21145" = "Infinium MethylationEPIC",
  "GPL21185" = "Agilent-072363 SurePrint G3 Human GE v3 8x60K Microarray (Probe Name version)",
  "GPL25758" = "Agilent-085775 Arraystar Human circRNA Epitranscriptomic microarray",
  "GPL20204" = "SABiosciences/Qiagen Human Cellular Senescence RT\u00b2 Profiler PCR Array",
  "GPL6359" = "[HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array (with custom CDF)",
  "GPL16020" = "[HuGene-2_1-st] Affymetrix Human Gene 2.1 ST Array [transcript (gene) version]",
  "GPL16956" = "Agilent-026652 Whole Human Genome Microarray 4x44K v2 (Feature Number version)",
  "GPL6883" = "Illumina HumanRef-8 v3.0 expression beadchip",
  "GPL4133" = "Agilent-014850 Whole Human Genome Microarray 4x44K G4112F (Feature Number version)",
  "GPL18451" = "NimbleGen Homo sapiens HG18 expression array [100718_HG18_opt_expr_HX12]",
  "GPL24072" = "Affymetrix HG-U133 Plus 2.0 (custom BrainArray v21)",
  "GPL21439" = "Affymetrix HG-U133 Plus 2.0 (custom BrainArray v20)",
  "GPL22003" = "Affymetrix HG-U133 Plus 2.0 (custom BrainArray v18)",
  "GPL24875" = "Agilent-028004 SurePrint G3 Human GE 8x60K",
  "GPL13987" = "Agilent-028004 SurePrint G3 Human GE 8x60K (Probe Name)",
  "GPL19387" = "Agilent-033501 SurePrint G3 Human CGH 2x400K",
  "GPL23178" = "Affymetrix Human Custom lncRNA Array [OElncRNAs520855F]",
  "GPL16847" = "Agilent-039494 SurePrint G3 Human GE v2 8x60K",
  "GPL17629" = "[HuEx-1_0-st-v2] Affymetrix Human Exon 1.0 ST Array",
  "GPL17810" = "Agilent-028004 SurePrint G3 Human GE 8x60K (Probe Name)",
  "GPL23649" = "Agilent-028004 SurePrint G3 Human GE 8x60K (Probe Name)",
  "GPL6480" = "Agilent-014850 Whole Human Genome Microarray 4x44K",
  "GPL22516" = "Agilent-072363 SurePrint G3 Human GE v3 8x60K",
  "GPL22517" = "Agilent-072363 SurePrint G3 Human GE v3 8x60K",
  "GPL17107" = "Exiqon miRCURY LNA microRNA Array (hsa, mmu, rno)",
  "GPL16384" = "[miRNA-3] Affymetrix Multispecies miRNA-3 Array",
  "GPL20791" = "Agilent-062918 Arraystar Human LncRNA microarray V3",
  "GPL19965" = "nCounter PanCancer Immune Profiling (Nanostring)",
  # Bulk RNA-seq
  "GPL10999" = "Illumina Genome Analyzer II (Homo sapiens)",
  "GPL11118" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL11154" = "Illumina HiSeq 2000 (Mus musculus / Homo sapiens)",
  "GPL15433" = "Illumina HiSeq 1000 (Homo sapiens)",
  "GPL15440" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL15456" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL15520" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL15524" = "Illumina HiSeq 2500 (Mus musculus)",
  "GPL16027" = "Illumina HiSeq 2000 (Mus musculus)",
  "GPL16287" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL16512" = "Illumina HiSeq 2000 (Multi-species)",
  "GPL16790" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL16791" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL18402" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL18448" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL18460" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL18461" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL18573" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL18578" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL18643" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL26525" = "Illumina HiSeq 3000 (Homo sapiens)",
  "GPL19072" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL18723" = "Illumina HiSeq 2500 (Mus musculus)",
  "GPL19065" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL19826" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL19828" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL23465" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL23578" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL24904" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL24879" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL25024" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL25166" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL25635" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL25672" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL25484" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL24605" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL24606" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL24607" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL26026" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL26747" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL26705" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL26683" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL27487" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL27765" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL29173" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL19124" = "PacBio RS (Homo sapiens)",
  "GPL19227" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL19969" = "Illumina MiSeq (Multi-species)",
  "GPL20050" = "Illumina HiSeq 2500 (Dual RNA-Seq)",
  "GPL20051" = "Illumina HiSeq 2000 (Homo sapiens)",
  "GPL20148" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL20149" = "Illumina NextSeq 500 (Homo sapiens)",
  "GPL20276" = "Illumina NextSeq 500 (Mixed Species)",
  "GPL20301" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL20311" = "Illumina HiSeq 2500 (Homo sapiens)",
  "GPL21219" = "Illumina HiSeq 3000 (Mus musculus)",
  "GPL21272" = "Illumina HiSeq 4000 (Homo sapiens)",
  "GPL21290" = "Illumina HiSeq 3000 (Homo sapiens)",
  "GPL24432" = "Illumina NovaSeq 6000 (Homo sapiens)",
  "GPL24632" = "Illumina HiSeq 3000 (Homo sapiens)",
  "GPL33023" = "Illumina NovaSeq 6000 (Homo sapiens)",
  "GPL33758" = "DNBSEQ-G400 (Homo sapiens)",
  "GPL33762" = "DNBSEQ-G400 (Homo sapiens)",
  "GPL20828" = "Illumina NextSeq 500 (Danio rerio)"
)
# Every GPL gets a display type: from display-name override, else annot package, else "Other microarray (biomaRt)"
platform_id_to_type <- setNames(
  vapply(names(platform_to_annot), function(gpl) {
    if (gpl %in% names(platform_id_display_name_override))
      return(platform_id_display_name_override[[gpl]])
    pkg <- platform_to_annot[[gpl]]
    if (is.na(pkg) || !nzchar(pkg)) return("Other microarray (biomaRt)")
    type <- if (pkg %in% names(annot_pkg_to_type)) annot_pkg_to_type[[pkg]] else NULL
    if (is.null(type) || is.na(type)) return("Microarray (annot)")
    type
  }, character(1), USE.NAMES = TRUE),
  names(platform_to_annot)
)

# Two main categories: Microarray | RNA-seq. Subtypes within each for ordered listing.
# GPLs used as sequencing platforms in GEO (bulk RNA-seq); add/remove as needed. Others = Microarray.
platform_ids_rnaseq <- c(
  "GPL10999", "GPL11118", "GPL11154", "GPL15433", "GPL15440", "GPL15456", "GPL15520", "GPL15524",
  "GPL16027", "GPL16287", "GPL16512", "GPL16790", "GPL16791", "GPL18402", "GPL18448", "GPL18460",
  "GPL18461", "GPL18573", "GPL18578", "GPL18643", "GPL18723", "GPL19065", "GPL19124", "GPL19227",
  "GPL19969", "GPL20050", "GPL20051", "GPL20148", "GPL20149", "GPL20276", "GPL20301", "GPL20311",
  "GPL20795", "GPL20822", "GPL20828", "GPL21219", "GPL21272", "GPL21290", "GPL24432", "GPL24632",
  "GPL26921", "GPL26922", "GPL27991", "GPL28867", "GPL33023", "GPL33758", "GPL33762", "GPL26525"
)
# Map display type -> subtype (for grouping). Order of subtype names defines sequence.
type_to_subtype_microarray <- c(
  "Affymetrix HG-U133A" = "Affymetrix HG-U133",
  "Affymetrix HG-U133B" = "Affymetrix HG-U133",
  "Affymetrix HG-U133 Plus 2.0" = "Affymetrix HG-U133",
  "Affymetrix HG-U133A 2.0" = "Affymetrix HG-U133",
  "Affymetrix HuGene 1.0 ST" = "Affymetrix HuGene",
  "Affymetrix HuGene 1.1 ST" = "Affymetrix HuGene",
  "Affymetrix HuGene 2.0 ST" = "Affymetrix HuGene",
  "Affymetrix HuGene 2.1 ST" = "Affymetrix HuGene",
  "Affymetrix HTA 2.0" = "Affymetrix HTA",
  "Affymetrix HuEx 1.0 ST" = "Affymetrix HuEx",
  "Affymetrix PrimeView" = "Affymetrix Other",
  "Affymetrix Clariom D Human" = "Affymetrix Other",
  "Affymetrix Clariom S Human" = "Affymetrix Other",
  "Affymetrix Mouse 430 2.0" = "Affymetrix Other",
  "Affymetrix HG-U95A" = "Affymetrix Other",
  "Affymetrix HG-U95B" = "Affymetrix Other",
  "Affymetrix MG-U74A v2" = "Affymetrix Other",
  "Affymetrix HuGeneFL" = "Affymetrix Other",
  "Illumina HumanHT-12 v2" = "Illumina",
  "Illumina HumanHT-12 v3" = "Illumina",
  "Illumina HumanHT-12 v4" = "Illumina",
  "Illumina Mouse v2" = "Illumina",
  "Microarray (annot)" = "Other microarray",
  "Other microarray (biomaRt)" = "Other microarray",
  # Full platform names (from platform_id_display_name_override)
  "Affymetrix Human Genome U133 Array" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U133 Plus 2.0 Array (B-set)" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U133 Plus 2.0 Array" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U133 Plus 2.0 Array (Custom)" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U133A Array" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U133A 2.0 Array" = "Affymetrix HG-U133",
  "Affymetrix Human Genome U95A Array" = "Affymetrix Other",
  "Affymetrix Human Genome U95B" = "Affymetrix Other",
  "Affymetrix Mouse Genome 430 2.0 Array" = "Affymetrix Other",
  "Affymetrix Mouse Genome U74A Version 2 Array" = "Affymetrix Other",
  "Affymetrix Human Full Length HuGeneFL Array" = "Affymetrix Other",
  "Affymetrix Human Exon 1.0 ST Array" = "Affymetrix HuEx",
  "Affymetrix Human Gene 1.0 ST Array" = "Affymetrix HuGene",
  "Affymetrix Human Gene 2.1 ST Array" = "Affymetrix HuGene",
  "Affymetrix Human Gene 2.1 ST Array (v1)" = "Affymetrix HuGene",
  "Affymetrix Human Transcriptome Array 2.0" = "Affymetrix HTA",
  "Affymetrix Clariom D Human Array" = "Affymetrix Other",
  "Affymetrix Clariom S Human Array" = "Affymetrix Other",
  "Illumina HumanWG-6 v2.0 Expression BeadChip" = "Illumina",
  "Illumina HumanHT-12 V4.0 Expression BeadChip" = "Illumina",
  "Illumina MouseRef-8 v2.0 Expression BeadChip" = "Illumina",
  "Illumina HumanHT-12 V2.0 Expression BeadChip" = "Illumina",
  "Illumina HumanHT-12 V3.0 Expression BeadChip" = "Illumina",
  "Affymetrix Human Gene 1.1 ST Array" = "Affymetrix HuGene",
  "Affymetrix Human Gene 2.0 ST Array" = "Affymetrix HuGene",
  "Affymetrix PrimeView Array" = "Affymetrix Other",
  "Affymetrix Human Genome U95A Version 2 Array" = "Affymetrix Other",
  "Agilent-014850 Whole Human Genome (Custom/GEO Table)" = "Other microarray",
  "Agilent-028004 SurePrint G3 Human GE (Custom/Agilent)" = "Other microarray",
  "Agilent-028004 SurePrint G3 Human GE 8x60K Microarray (Feature Number version)" = "Other microarray",
  "[HG-U219] Affymetrix Human Genome U219 Array" = "Affymetrix Other",
  "[HG-U219] Affymetrix Human Genome U219 Array (Probe Set ID version)" = "Affymetrix Other",
  "Agilent-028004 SurePrint G3 Human GE 8x60K Microarray (Probe Name version)" = "Other microarray",
  "Agilent-019118 Human miRNA Microarray 2.0 G4470B (miRNA ID version)" = "Other microarray",
  "Agilent-016436 Human miRNA Microarray G4470A (miRNA ID version)" = "Other microarray",
  "Agilent-072363 SurePrint G3 Human GE v3 8x60K Microarray (Probe Name version)" = "Other microarray",
  "Agilent-067406 Human CBC lncRNA + mRNA microarray V4.0" = "Other microarray",
  "Illumina HumanMethylation27 BeadChip" = "Other microarray",
  "Illumina HumanMethylation450 BeadChip" = "Other microarray",
  "Illumina HumanHT-12 V4.0 expression beadchip" = "Illumina",
  "Agilent-039494 SurePrint G3 Human GE v2 8x60K Microarray (Probe Name version)" = "Other microarray",
  "Agilent-085775 Arraystar Human circRNA Epitranscriptomic microarray" = "Other microarray",
  "Infinium MethylationEPIC" = "Other microarray",
  "[HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array" = "Affymetrix HG-U133",
  "[HG-U133_Plus_2] Affymetrix Human Genome U133 Plus 2.0 Array (with custom CDF)" = "Affymetrix HG-U133",
  "[HuGene-2_1-st] Affymetrix Human Gene 2.1 ST Array [transcript (gene) version]" = "Affymetrix HuGene",
  "Agilent-026652 Whole Human Genome Microarray 4x44K v2 (Feature Number version)" = "Other microarray",
  "Illumina HumanRef-8 v3.0 expression beadchip" = "Illumina",
  "Agilent-014850 Whole Human Genome Microarray 4x44K G4112F (Feature Number version)" = "Other microarray",
  "NimbleGen Homo sapiens HG18 expression array [100718_HG18_opt_expr_HX12]" = "Other microarray",
  "SABiosciences/Qiagen Human Cellular Senescence RT\u00b2 Profiler PCR Array" = "Other microarray"
)
microarray_subtype_order <- c(
  "Affymetrix HG-U133", "Affymetrix HuGene", "Affymetrix HTA", "Affymetrix HuEx",
  "Affymetrix Other", "Illumina", "Other microarray"
)
rnaseq_subtype_order <- c("Illumina HiSeq/NovaSeq", "Other RNA-seq")
other_subtype_order <- c("Other technologies")

# Explicit technology groups: Microarray, RNA-seq (bulk), and Other (miRNA, methylation, circRNA, etc.)
platform_ids_other <- c("GPL8227", "GPL9081", "GPL13534", "GPL25758", "GPL8490", "GPL21145", "GPL20204")

# GPL -> category: "Microarray", "RNA-seq", or "Other"
platform_id_category <- setNames(
  ifelse(
    names(platform_to_annot) %in% platform_ids_rnaseq, "RNA-seq",
    ifelse(names(platform_to_annot) %in% platform_ids_other, "Other", "Microarray")
  ),
  names(platform_to_annot)
)

# GPL -> subtype (within category)
platform_id_subtype <- setNames(
  vapply(names(platform_to_annot), function(gpl) {
    type <- platform_id_to_type[[gpl]]
    cat <- platform_id_category[[gpl]]
    if (cat == "RNA-seq") {
      if (grepl("Illumina|DNBSEQ", type, ignore.case = TRUE)) return("Illumina HiSeq/NovaSeq")
      return("Other RNA-seq")
    }
    if (cat == "Other") {
      return("Other technologies")
    }
    sub <- if (type %in% names(type_to_subtype_microarray)) type_to_subtype_microarray[[type]] else NULL
    if (is.null(sub) || is.na(sub)) return("Other microarray")
    sub
  }, character(1), USE.NAMES = TRUE),
  names(platform_to_annot)
)

# Nested list: category -> subtype -> vector of GPL ids (for display/export in sequence)
platform_ids_by_category_subtype <- local({
  out <- list(Microarray = list(), RNA_seq = list(), Other = list())
  for (sub in microarray_subtype_order) {
    out$Microarray[[sub]] <- names(platform_to_annot)[platform_id_category == "Microarray" & platform_id_subtype == sub]
  }
  for (sub in rnaseq_subtype_order) {
    out$RNA_seq[[sub]] <- names(platform_to_annot)[platform_id_category == "RNA-seq" & platform_id_subtype == sub]
  }
  for (sub in other_subtype_order) {
    out$Other[[sub]] <- names(platform_to_annot)[platform_id_category == "Other" & platform_id_subtype == sub]
  }
  out
})

# Ordered vector of all platform IDs: Microarray (by subtype order) then RNA-seq, then Other
platform_id_sequence <- c(
  unlist(platform_ids_by_category_subtype$Microarray, use.names = FALSE),
  unlist(platform_ids_by_category_subtype$RNA_seq, use.names = FALSE),
  unlist(platform_ids_by_category_subtype$Other, use.names = FALSE)
)
# Reorder platform_to_annot so names follow category/subtype sequence (existing GPLs not in sequence stay at end)
platform_to_annot <- c(
  platform_to_annot[intersect(platform_id_sequence, names(platform_to_annot))],
  platform_to_annot[setdiff(names(platform_to_annot), platform_id_sequence)]
)

# Detect gene ID format for logging and UI (returns human-readable label)
# Handles: Gene symbols (HGNC), Affymetrix HuGene (_st), HG-U133 (_at), Ensembl, Entrez, Unknown
detect_gene_id_format <- function(ids) {
  ids <- as.character(ids[!is.na(ids) & nzchar(trimws(ids))])
  if (length(ids) == 0) return("Unknown")
  sample_ids <- head(ids, min(300, length(ids)))
  # Affymetrix HuGene transcript cluster: 2824546_st, 2824549_st, etc.
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5)
    return("Affymetrix HuGene probe (_st)")
  # Affymetrix HG-U133: 200000_s_at, 200001_at, etc.
  if (mean(grepl("_at$|_x_at$", sample_ids), na.rm = TRUE) > 0.5)
    return("Affymetrix HG-U133 probe (_at)")
  # Ensembl: ENSG00000000003
  if (mean(grepl("^ENSG", sample_ids), na.rm = TRUE) > 0.5)
    return("Ensembl ID")
  # Entrez (numeric only)
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7)
    return("Entrez ID")
  # Gene symbols: OR4F4, PCMTD2, BRCA1, etc. (letters first, mixed)
  if (mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.6 &&
      mean(grepl("^ENSG|_at$|_st$|^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3)
    return("Gene symbol (HGNC)")
  return("Unknown / mixed")
}

# HuGene/HTA .db packages for PROBEID -> SYMBOL (Affymetrix transcript cluster IDs, e.g. 2824546_st)
# HTA 2.0 (GPL17586) uses _st format; hta20transcriptcluster.db maps to gene symbols
HUGENE_DB_PACKAGES <- c(
  "hugene10sttranscriptcluster.db", "hugene11sttranscriptcluster.db",
  "hugene20sttranscriptcluster.db", "hugene21sttranscriptcluster.db",
  "hta20transcriptcluster.db"  # HTA 2.0 (GSE123342 uses GPL17586)
)

# Map Affymetrix HuGene _st probe IDs to gene symbols using Bioconductor .db (PROBEID -> SYMBOL).
# Use when probe IDs look like 2824546_st; no network required. Returns named vector or NULL.
# GPL annotation tables often use IDs without _st; DB PROBEID may use either format - try both.
probe_ids_to_symbol_hugene_db <- function(probe_ids, gpl_id = NULL) {
  probe_ids <- as.character(probe_ids)
  pkgs_to_try <- character(0)
  if (!is.null(gpl_id) && nzchar(gpl_id) && (gpl_id %in% names(platform_to_annot))) {
    pkg <- platform_to_annot[[gpl_id]]
    if (!is.na(pkg) && nzchar(pkg) && (pkg %in% HUGENE_DB_PACKAGES))
      pkgs_to_try <- pkg
  }
  if (length(pkgs_to_try) == 0)
    pkgs_to_try <- HUGENE_DB_PACKAGES
  for (pkg in pkgs_to_try) {
    if (!requireNamespace(pkg, quietly = TRUE)) next
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    env <- get(pkg)
    # Bioconductor .db packages use numeric PROBEID (e.g. 2824546), not 2824546_st - try stripped FIRST
    if (any(grepl("_st$", probe_ids))) {
      stripped <- sub("_st$", "", probe_ids)
      out2 <- tryCatch({
        mapIds(env, keys = stripped, column = "SYMBOL", keytype = "PROBEID", multiVals = "first")
      }, error = function(e) NULL)
      if (!is.null(out2)) {
        out2 <- as.character(out2)
        if (sum(!is.na(out2)) > length(probe_ids) * 0.1) {
          names(out2) <- probe_ids
          return(out2)
        }
      }
    }
    # Fallback: try original IDs (some platforms may use _st in DB)
    out <- tryCatch({
      mapIds(env, keys = probe_ids, column = "SYMBOL", keytype = "PROBEID", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(out)) {
      out <- as.character(out)
      if (sum(!is.na(out)) > length(probe_ids) * 0.1)
        return(out)
    }
  }
  NULL
}

# Map probe IDs to gene symbols using GEO platform (GPL) annotation table.
# Fetches GPL via GEOquery, extracts probe ID and gene symbol columns, merges.
# Works for any GPL with Gene Symbol (or similar) column in the annotation file.
probe_ids_to_symbol_gpl <- function(probe_ids, gpl_id) {
  if (is.null(gpl_id) || !nzchar(gpl_id)) return(NULL)
  probe_ids <- as.character(probe_ids)
  gpl <- tryCatch({
    suppressMessages(GEOquery::getGEO(gpl_id, destdir = getwd()))
  }, error = function(e) NULL)
  if (is.null(gpl)) return(NULL)
  tab <- tryCatch({
    if (inherits(gpl, "GPL")) GEOquery::Table(gpl) else NULL
  }, error = function(e) NULL)
  if (is.null(tab) || nrow(tab) == 0) return(NULL)
  probe_col <- NULL
  for (cand in c("ID", "PROBE_ID", "Probe", "probe_id", "SPOT_ID")) {
    if (cand %in% colnames(tab)) { probe_col <- cand; break }
  }
  if (is.null(probe_col)) return(NULL)
  symbol_col <- NULL
  for (cand in c("Gene Symbol", "GENE_SYMBOL", "Gene.symbol", "Symbol", "gene_symbol", "SYMBOL", "GeneSymbol", "Gene_Symbol")) {
    if (cand %in% colnames(tab)) { symbol_col <- cand; break }
  }
  if (is.null(symbol_col)) return(NULL)
  tab[[probe_col]] <- as.character(tab[[probe_col]])
  tab[[symbol_col]] <- as.character(tab[[symbol_col]])
  tab <- tab[!is.na(tab[[symbol_col]]) & nzchar(trimws(tab[[symbol_col]])), , drop = FALSE]
  if (nrow(tab) == 0) return(NULL)
  gpl_probes <- tab[[probe_col]]
  idx <- match(probe_ids, gpl_probes)
  # HuGene arrays: GPL table may use "2824546" while data has "2824546_st" - try stripping _st
  if (mean(is.na(idx)) > 0.5 && any(grepl("_st$", probe_ids))) {
    stripped <- sub("_st$", "", probe_ids)
    idx2 <- match(stripped, gpl_probes)
    if (sum(!is.na(idx2)) > sum(!is.na(idx))) idx <- idx2
  }
  out <- tab[[symbol_col]][idx]
  names(out) <- probe_ids
  if (sum(!is.na(out)) > length(probe_ids) * 0.1) return(out)
  NULL
}

# GPL -> biomaRt probe-ID attribute (for mapping when .db package missing or ALIAS fails)
# Ensembl filter/attribute names for Affy and similar arrays (human)
GPL_to_biomart_probe_attr <- c(
  "GPL96" = "affy_hg_u133a", "GPL97" = "affy_hg_u133b", "GPL570" = "affy_hg_u133_plus_2",
  "GPL571" = "affy_hg_u133a2", "GPL201" = "affy_hg_u133a", "GPL15207" = "affy_hg_u133_plus_2",
  "GPL17586" = "affy_hta_2_0",  # HTA 2.0, not HG-U133
  "GPL13158" = "affy_hg_u133_plus_2", "GPL21970" = "affy_hg_u133_plus_2",
  "GPL23432" = "affy_hg_u133_plus_2", "GPL1352" = "affy_hg_u133_plus_2", "GPL3921" = "affy_hg_u133_plus_2",
  "GPL19859" = "affy_hg_u133_plus_2", "GPL10739" = "affy_hg_u133_plus_2",
  "GPL6244" = "affy_hugene_1_0_st_v1", "GPL16686" = "affy_hugene_2_0_st_v1", "GPL11532" = "affy_hugene_1_1_st_v1",
  "GPL15088" = "affy_hugene_2_0_st_v1", "GPL16311" = "affy_hugene_2_0_st_v1", "GPL21061" = "affy_hugene_2_0_st_v1",
  "GPL10558" = "illumina_humanht_12_v4", "GPL6947" = "illumina_humanht_12_v3", "GPL6104" = "illumina_humanht_12_v3"
)

# Map probe IDs to gene symbols via biomaRt (fallback when .db/ALIAS fail). Returns named vector of symbols (or original ids where unmapped).
# Batches large ID lists so server limits are not hit (e.g. HuGene 70k probes).
probe_ids_to_symbol_biomart <- function(probe_ids, gpl_id = NULL) {
  if (!requireNamespace("biomaRt", quietly = TRUE)) return(NULL)
  probe_ids <- as.character(probe_ids)
  attr_name <- if (!is.null(gpl_id) && nzchar(gpl_id)) GPL_to_biomart_probe_attr[[gpl_id]] else NULL
  if (is.null(attr_name) || is.na(attr_name)) {
    sample_ids <- head(probe_ids[!is.na(probe_ids) & nzchar(probe_ids)], 200)
    if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
      attrs_to_try <- c("affy_hta_2_0", "affy_hugene_1_0_st_v1", "affy_hugene_2_0_st_v1", "affy_hg_u133_plus_2", "affy_hg_u133a", "affy_hg_u133b")
    } else {
      attrs_to_try <- c("affy_hg_u133_plus_2", "affy_hg_u133a", "affy_hg_u133b", "affy_hugene_1_0_st_v1", "affy_hugene_2_0_st_v1")
    }
  } else {
    attrs_to_try <- attr_name
  }
  mart <- tryCatch({
    biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl")
  }, error = function(e) NULL)
  if (is.null(mart)) return(NULL)
  unique_ids <- unique(probe_ids[!is.na(probe_ids) & nzchar(probe_ids)])
  # biomaRt may expect IDs without _st; try stripped if we have _st and initial match is low
  ids_to_use <- list(unique_ids)
  if (any(grepl("_st$", unique_ids))) {
    stripped <- unique(sub("_st$", "", unique_ids))
    ids_to_use <- c(ids_to_use, list(stripped))
  }
  batch_size <- 5000L
  for (probe_attr in attrs_to_try) {
    out <- rep(NA_character_, length(probe_ids))
    names(out) <- probe_ids
    for (id_set in ids_to_use) {
      all_res <- NULL
      tryCatch({
      for (start in seq(1L, length(id_set), by = batch_size)) {
        chunk <- id_set[start:min(start + batch_size - 1L, length(id_set))]
        res <- biomaRt::getBM(
          attributes = c(probe_attr, "hgnc_symbol"),
          filters = probe_attr,
          values = chunk,
          mart = mart
        )
        if (is.null(res) || nrow(res) == 0) next
        colnames(res) <- c("probe", "symbol")
        res <- res[res$symbol != "" & !is.na(res$symbol), , drop = FALSE]
        if (nrow(res) > 0) all_res <- rbind(all_res, res)
      }
      if (is.null(all_res) || nrow(all_res) == 0) next
      # Map back to original probe_ids (may have _st; all_res$probe may be with or without _st)
      lookup <- all_res
      idx <- match(probe_ids, lookup$probe)
      if (mean(is.na(idx)) > 0.5 && any(grepl("_st$", probe_ids))) {
        stripped <- sub("_st$", "", probe_ids)
        idx <- match(stripped, lookup$probe)
      }
      out <- lookup$symbol[idx]
      names(out) <- probe_ids
      if (sum(!is.na(out)) > length(probe_ids) * 0.1) return(out)
      }, error = function(e) NULL)
    }
  }
  NULL
}

# Check GSE platform: when a GSE is added, get its platform(s); if any match platform_to_annot, return that platform so analysis can run.
get_platform_for_gse <- function(gse_id) {
  gse_id <- trimws(toupper(gse_id))
  if (!grepl("^GSE[0-9]+", gse_id)) {
    warning("Invalid GSE id: ", gse_id)
    return(NULL)
  }
  gse <- tryCatch(
    GEOquery::getGEO(GEO = gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = getwd()),
    error = function(e) { message("getGEO failed: ", e$message); return(NULL) }
  )
  if (is.null(gse)) return(NULL)
  if (inherits(gse, "list")) {
    platforms <- vapply(gse, function(x) Biobase::annotation(x), character(1))
  } else {
    platforms <- Biobase::annotation(gse)
  }
  known <- names(platform_to_annot)
  for (pl in platforms) {
    if (pl %in% known) return(pl)
  }
  message("No platform in platform_to_annot for GSE: ", paste(platforms, collapse = ", "))
  return(NULL)
}

# Run annotation and download for a GSE: when platform matches platform_to_annot, run analysis (annotation + optional save).
run_gse_annotation_and_download <- function(gse_id, dest_dir = getwd(), save_annotated = TRUE) {
  gse_id <- trimws(toupper(gse_id))
  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)
  gse <- tryCatch(
    GEOquery::getGEO(GEO = gse_id, GSEMatrix = TRUE, getGPL = TRUE, destdir = dest_dir),
    error = function(e) { message("getGEO failed: ", e$message); return(invisible(NULL)) }
  )
  if (is.null(gse)) return(invisible(NULL))
  if (inherits(gse, "list")) {
    platforms <- vapply(gse, function(x) Biobase::annotation(x), character(1))
    known <- names(platform_to_annot)
    idx <- which(platforms %in% known)[1]
    if (is.na(idx)) {
      message("No platform in platform_to_annot; using first: ", platforms[1])
      idx <- 1
    } else {
      message("GSE has multiple platforms; using first matching: ", platforms[idx])
    }
    micro_eset <- gse[[idx]]
  } else {
    micro_eset <- gse
  }
  platform_id <- Biobase::annotation(micro_eset)
  micro_expr <- Biobase::exprs(micro_eset)
  fdata <- Biobase::fData(micro_eset)
  gene_symbols <- map_microarray_ids(micro_expr, fdata, micro_eset, gse_id = gse_id)
  rownames(micro_expr) <- gene_symbols
  keep <- !is.na(rownames(micro_expr)) & rownames(micro_expr) != ""
  micro_expr <- micro_expr[keep, , drop = FALSE]
  if (any(duplicated(rownames(micro_expr)))) {
    micro_expr <- aggregate(micro_expr, by = list(rownames(micro_expr)), FUN = median, na.rm = TRUE)
    rownames(micro_expr) <- micro_expr[, 1]
    micro_expr <- as.matrix(micro_expr[, -1, drop = FALSE])
  }
  if (save_annotated) {
    out_file <- file.path(dest_dir, paste0(gse_id, "_annotated_", platform_id, ".rds"))
    saveRDS(list(expr = micro_expr, platform = platform_id, gse_id = gse_id), out_file)
    message("Saved annotated matrix: ", out_file)
  }
  invisible(list(expr = micro_expr, platform = platform_id, gse_id = gse_id, eset = micro_eset))
}

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

# Map microarray probe IDs to gene symbols (matches multi-platform pipeline script)
map_microarray_ids <- function(micro_expr, fdata, micro_eset, gse_id = NULL) {
  probe_ids <- rownames(micro_expr)
  possible_symbol_cols <- c("Gene Symbol", "Gene.symbol", "GENE_SYMBOL", "Symbol",
                            "gene_symbol", "SYMBOL", "GeneSymbol", "gene.symbol",
                            "Gene_Symbol", "GENE_SYMBOL")
  gene_symbol_col <- NULL
  for (col in possible_symbol_cols) {
    if (col %in% colnames(fdata)) {
      gene_symbol_col <- col
      break
    }
  }
  if (!is.null(gene_symbol_col)) {
    gene_symbols <- as.character(fdata[[gene_symbol_col]])
    gene_symbols[gene_symbols == "" | is.na(gene_symbols)] <- NA
    return(gene_symbols)
  }
  platform_id <- Biobase::annotation(micro_eset)
  annot_pkg <- platform_to_annot[[platform_id]]
  if (!is.null(annot_pkg) && !is.na(annot_pkg) && requireNamespace(annot_pkg, quietly = TRUE)) {
    suppressPackageStartupMessages(library(annot_pkg, character.only = TRUE))
    gene_symbols <- tryCatch({
      mapIds(get(annot_pkg), keys = probe_ids, column = "SYMBOL",
             keytype = "PROBEID", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }
  sample_ids <- head(probe_ids[!is.na(probe_ids) & probe_ids != ""], min(100, length(probe_ids)))
  if (length(sample_ids) == 0) return(probe_ids)
  is_likely_symbol <- mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.7 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3
  if (is_likely_symbol && !any(grepl("^[0-9]{5,}", sample_ids))) {
    test_symbols <- head(sample_ids, 10)
    verified <- tryCatch({
      mapped <- mapIds(org.Hs.eg.db, keys = test_symbols, column = "SYMBOL",
                      keytype = "SYMBOL", multiVals = "first")
      sum(!is.na(mapped)) / length(test_symbols) > 0.5
    }, error = function(e) FALSE)
    if (verified) return(probe_ids)
  }
  if (any(grepl("^ENSG", sample_ids))) {
    clean_keys <- gsub("\\..*", "", probe_ids)
    gene_symbols <- tryCatch({
      mapIds(org.Hs.eg.db, keys = clean_keys, column = "SYMBOL",
             keytype = "ENSEMBL", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7) {
    gene_symbols <- tryCatch({
      mapIds(org.Hs.eg.db, keys = as.character(probe_ids), column = "SYMBOL",
             keytype = "ENTREZID", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }
  # Fallback: try ALIAS (many Affy probe IDs are in org.Hs.eg.db as alias)
  gene_symbols <- tryCatch({
    mapIds(org.Hs.eg.db, keys = as.character(probe_ids), column = "SYMBOL",
           keytype = "ALIAS", multiVals = "first")
  }, error = function(e) NULL)
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
  }
  # Fallback: HuGene .db (PROBEID -> SYMBOL) for _st probe IDs
  if (mean(grepl("^[0-9]+_st$", sample_ids), na.rm = TRUE) > 0.5) {
    gene_symbols <- probe_ids_to_symbol_hugene_db(probe_ids, platform_id)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1)
      return(as.character(gene_symbols))
  }
  # Fallback: GEO platform (GPL) annotation table - probe ID -> Gene Symbol from NCBI
  gene_symbols <- probe_ids_to_symbol_gpl(probe_ids, platform_id)
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
  }
  gene_symbols <- probe_ids_to_symbol_biomart(probe_ids, platform_id)
  if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(probe_ids) * 0.1) {
    return(as.character(gene_symbols))
  }
  return(probe_ids)
}

# Entrez ID -> gene symbol via biomaRt (fallback when org.Hs.eg.db fails or is offline)
# Returns character vector same length as ids; NA where unmapped. Batches to avoid server limits.
entrez_to_symbol_biomart <- function(ids) {
  if (!requireNamespace("biomaRt", quietly = TRUE)) return(NULL)
  if (is.null(ids) || length(ids) == 0) return(NULL)
  ids <- as.character(ids)
  ids_clean <- gsub("\\.0+$", "", ids)  # strip trailing .0 from numeric-as-character
  keep <- !is.na(ids_clean) & nzchar(trimws(ids_clean)) & grepl("^[0-9]+$", ids_clean)
  if (sum(keep) == 0) return(NULL)
  unique_entrez <- unique(ids_clean[keep])
  mart <- tryCatch(biomaRt::useMart("ensembl", dataset = "hsapiens_gene_ensembl"), error = function(e) NULL)
  if (is.null(mart)) return(NULL)
  out <- rep(NA_character_, length(ids))
  batch_size <- 5000L
  for (start in seq(1L, length(unique_entrez), by = batch_size)) {
    chunk <- unique_entrez[start:min(start + batch_size - 1L, length(unique_entrez))]
    res <- tryCatch({
      biomaRt::getBM(attributes = c("entrezgene_id", "hgnc_symbol"),
                     filters = "entrezgene_id", values = as.integer(chunk), mart = mart)
    }, error = function(e) NULL)
    if (is.null(res) || nrow(res) == 0) next
    res <- res[!is.na(res$hgnc_symbol) & nzchar(trimws(res$hgnc_symbol)), , drop = FALSE]
    if (nrow(res) == 0) next
    res$entrezgene_id <- as.character(res$entrezgene_id)
    idx <- match(ids_clean, res$entrezgene_id)
    mapped <- res$hgnc_symbol[idx]
    out[!is.na(mapped)] <- as.character(mapped[!is.na(mapped)])
  }
  if (sum(!is.na(out)) > length(ids) * 0.1) out else NULL
}

# Convert any gene ID (probe, Entrez, Ensembl, or symbol) to gene symbol for overlap
# Used so all datasets end up with symbols and common-gene intersection is meaningful
# gpl_id: optional platform ID (e.g. GPL6244) so biomaRt uses the right probe attribute for _st/_at IDs
any_id_to_symbol <- function(ids, gpl_id = NULL) {
  if (is.null(ids) || length(ids) == 0) return(ids)
  ids <- as.character(ids)
  ids[is.na(ids) | trimws(ids) == ""] <- NA
  sample_ids <- head(ids[!is.na(ids)], min(500, length(ids)))
  if (length(sample_ids) == 0) return(ids)
  # Already look like symbols (mostly letters, not mostly numeric, no ENSG, no _st/_at)
  is_likely_symbol <- mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.6 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.4 &&
    mean(grepl("^ENSG", sample_ids), na.rm = TRUE) < 0.5 &&
    mean(grepl("_at$|_st$|_x_at$", sample_ids), na.rm = TRUE) < 0.2
  if (is_likely_symbol) return(ids)
  out <- rep(NA_character_, length(ids))
  # Try ENSEMBL
  if (any(grepl("^ENSG", sample_ids))) {
    clean <- gsub("\\..*", "", ids)
    sym <- tryCatch(suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db, keys = clean, column = "SYMBOL", keytype = "ENSEMBL", multiVals = "first")), error = function(e) NULL)
    if (!is.null(sym) && length(sym) == length(ids) && sum(!is.na(sym)) > length(ids) * 0.1) return(as.character(sym))
  }
  # Try ENTREZID (numeric): coerce to character and strip trailing .0 so keys match DB
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.3) {
    keys_entrez <- gsub("\\.0+$", "", ids)
    sym <- tryCatch(suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db, keys = keys_entrez, column = "SYMBOL", keytype = "ENTREZID", multiVals = "first")), error = function(e) NULL)
    if (!is.null(sym) && sum(!is.na(sym)) > length(ids) * 0.1) { out <- as.character(sym); return(out) }
    bm_sym <- entrez_to_symbol_biomart(ids)
    if (!is.null(bm_sym) && sum(!is.na(bm_sym)) > length(ids) * 0.1) return(bm_sym)
  }
  # Try ALIAS (probe IDs etc.)
  sym <- tryCatch(suppressMessages(AnnotationDbi::mapIds(org.Hs.eg.db, keys = ids, column = "SYMBOL", keytype = "ALIAS", multiVals = "first")), error = function(e) NULL)
  if (!is.null(sym) && sum(!is.na(sym)) > length(ids) * 0.1) return(as.character(sym))
  # Affymetrix probe IDs (e.g. 2824546_st): try HuGene .db, GEO GPL table, then biomaRt
  if (mean(grepl("_at$|_st$|_x_at$", sample_ids), na.rm = TRUE) > 0.2) {
    db_sym <- probe_ids_to_symbol_hugene_db(ids, gpl_id)
    if (!is.null(db_sym) && sum(!is.na(db_sym)) > length(ids) * 0.1) return(as.character(db_sym))
    gpl_sym <- probe_ids_to_symbol_gpl(ids, gpl_id)
    if (!is.null(gpl_sym) && sum(!is.na(gpl_sym)) > length(ids) * 0.1) return(as.character(gpl_sym))
    bm <- probe_ids_to_symbol_biomart(ids, gpl_id)
    if (!is.null(bm) && sum(!is.na(bm)) > length(ids) * 0.1) return(as.character(bm))
  }
  return(ids)
}

# Convert RNA-seq IDs to symbols (matches multi-platform pipeline script)
convert_rnaseq_ids <- function(gene_ids, gse_id = NULL) {
  sample_ids <- head(gene_ids[!is.na(gene_ids) & gene_ids != ""], min(100, length(gene_ids)))
  if (length(sample_ids) == 0) return(gene_ids)
  is_likely_symbol <- mean(grepl("^[A-Za-z]", sample_ids), na.rm = TRUE) > 0.7 &&
    mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) < 0.3
  if (is_likely_symbol && !any(grepl("^[0-9]{5,}", sample_ids))) {
    test_symbols <- head(sample_ids, 10)
    verified <- tryCatch({
      mapped <- AnnotationDbi::mapIds(org.Hs.eg.db, keys = test_symbols, column = "SYMBOL",
                                      keytype = "SYMBOL", multiVals = "first")
      sum(!is.na(mapped)) / length(test_symbols) > 0.5
    }, error = function(e) FALSE)
    if (verified) return(gene_ids)
  }
  if (any(grepl("^ENSG", sample_ids))) {
    clean_keys <- gsub("\\..*", "", gene_ids)
    gene_symbols <- tryCatch({
      AnnotationDbi::mapIds(org.Hs.eg.db, keys = clean_keys, column = "SYMBOL",
                             keytype = "ENSEMBL", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(gene_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
  }
  if (mean(grepl("^[0-9]+$", sample_ids), na.rm = TRUE) > 0.7) {
    keys_entrez <- gsub("\\.0+$", "", as.character(gene_ids))
    gene_symbols <- tryCatch({
      AnnotationDbi::mapIds(org.Hs.eg.db, keys = keys_entrez, column = "SYMBOL",
                            keytype = "ENTREZID", multiVals = "first")
    }, error = function(e) NULL)
    if (!is.null(gene_symbols) && sum(!is.na(gene_symbols)) > length(gene_ids) * 0.1) {
      return(as.character(gene_symbols))
    }
    bm_sym <- entrez_to_symbol_biomart(gene_ids)
    if (!is.null(bm_sym) && sum(!is.na(bm_sym)) > length(gene_ids) * 0.1) {
      return(bm_sym)
    }
  }
  return(gene_ids)
}

# Download NCBI-generated raw count matrices (matches script: multiple genome versions)
download_ncbi_raw_counts <- function(gse_id, dest_dir) {
  genome_versions <- c(
    "GRCh38.p13_NCBI", "GRCh38.p14_NCBI", "GRCh38.p12_NCBI",
    "GRCh38.p11_NCBI", "GRCh38.p10_NCBI", "GRCh37.p13_NCBI",
    "GRCh38_NCBI", "GRCh37_NCBI"
  )
  for (genome in genome_versions) {
    filename <- paste0(gse_id, "_raw_counts_", genome, ".tsv.gz")
    dest_file <- file.path(dest_dir, filename)
    if (file.exists(dest_file) && file.info(dest_file)$size > 1000) {
      return(dest_file)
    }
    url <- paste0("https://www.ncbi.nlm.nih.gov/geo/download/?type=rnaseq_counts&acc=",
                  gse_id, "&format=file&file=", filename)
    result <- tryCatch({
      download.file(url, dest_file, mode = "wb", quiet = TRUE)
      if (file.exists(dest_file) && file.info(dest_file)$size > 1000) return(dest_file)
      if (file.exists(dest_file)) file.remove(dest_file)
      NULL
    }, error = function(e) NULL, warning = function(w) NULL)
    if (!is.null(result)) return(result)
  }
  return(NULL)
}

# Read count matrix (handles .gz; matches script)
# Suppress "Discarded single-line footer" from fread (GEO/NCBI files sometimes have a trailing summary line)
read_count_matrix <- function(file_path) {
  read_with_suppress <- function(path) {
    suppressWarnings(data.table::fread(path, data.table = FALSE))
  }
  if (grepl("\\.gz$", file_path, ignore.case = TRUE)) {
    df <- tryCatch(
      read_with_suppress(file_path),
      error = function(e) {
        if (!requireNamespace("R.utils", quietly = TRUE)) {
          stop("Cannot read .gz file: R.utils is required. Install with: install.packages(\"R.utils\").")
        }
        R.utils::gunzip(file_path, remove = FALSE, overwrite = TRUE)
        tmp <- gsub("\\.gz$", "", file_path, ignore.case = TRUE)
        out <- suppressWarnings(data.table::fread(tmp, data.table = FALSE))
        if (file.exists(tmp)) file.remove(tmp)
        out
      }
    )
  } else {
    df <- read_with_suppress(file_path)
  }
  return(df)
}

# Classify groups
classify_groups <- function(group_info, normal_keywords, disease_keywords) {
  group_clean <- tolower(trimws(as.character(group_info)))
  normal_clean <- tolower(trimws(normal_keywords))
  disease_clean <- tolower(trimws(disease_keywords))
  
  final_groups <- rep(NA_character_, length(group_clean))
  final_groups[group_clean %in% disease_clean] <- "Disease"
  final_groups[group_clean %in% normal_clean & is.na(final_groups)] <- "Normal"
  
  keep_samples <- !is.na(final_groups)
  return(list(groups = final_groups, keep = keep_samples))
}

# Normalize microarray (enhanced version with tracking)
# method: "quantile" (default, for Series Matrix) or "quantile" only; RMA is via normalize_microarray_rma()
normalize_microarray <- function(expr_matrix, dataset_name = NULL, method = "quantile") {
  # Track initial state
  initial_genes <- nrow(expr_matrix)

  expr_matrix <- expr_matrix[rowSums(is.na(expr_matrix)) < ncol(expr_matrix), ]
  expr_matrix <- expr_matrix[, colSums(is.na(expr_matrix)) < nrow(expr_matrix)]

  genes_after_na_removal <- nrow(expr_matrix)

  max_val <- max(expr_matrix, na.rm = TRUE)
  log2_applied <- FALSE
  if (max_val > 50) {
    min_val <- min(expr_matrix, na.rm = TRUE)
    if (min_val < 0) expr_matrix <- expr_matrix - min_val + 1
    expr_matrix <- log2(expr_matrix + 1)
    log2_applied <- TRUE
  }

  expr_norm <- limma::normalizeBetweenArrays(expr_matrix, method = "quantile")

  # Store normalization info as attribute
  attr(expr_norm, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_na_removal = genes_after_na_removal,
    final_genes = nrow(expr_norm),
    log2_applied = log2_applied,
    method = method
  )

  return(expr_norm)
}

# RMA normalization from CEL files (probe-level). Returns matrix with probe rownames, or NULL on failure.
# Platforms using oligo (Gene/Exon/HTA): HuGene, HTA. Others use affy (3' IVT).
GPL_USE_OLIGO <- c("GPL6244", "GPL16686", "GPL11532", "GPL17585", "GPL17586", "GPL15088",
                   "GPL16311", "GPL21061", "GPL13915", "GPL19251", "GPL26944", "GPL21559",
                   "GPL24532", "GPL25336", "GPL18990", "GPL17692", "GPL20265", "GPL30572",
                   "GPL14951", "GPL28718", "GPL15314", "GPL17556")
normalize_microarray_rma <- function(cel_paths, platform_id, dataset_name = NULL) {
  if (length(cel_paths) == 0) return(NULL)
  cel_paths <- cel_paths[file.exists(cel_paths)]
  if (length(cel_paths) == 0) return(NULL)
  use_oligo <- platform_id %in% GPL_USE_OLIGO
  if (use_oligo && requireNamespace("oligo", quietly = TRUE)) {
    tryCatch({
      suppressPackageStartupMessages(library(oligo, quietly = TRUE))
      affyBatch <- oligo::read.celfiles(cel_paths)
      expr_rma <- oligo::rma(affyBatch)
      mat <- Biobase::exprs(expr_rma)
      attr(mat, "normalization_info") <- list(
        initial_genes = nrow(mat), final_genes = nrow(mat),
        method = "RMA (oligo)", log2_applied = TRUE
      )
      return(mat)
    }, error = function(e) NULL)
  }
  if (requireNamespace("affy", quietly = TRUE)) {
    tryCatch({
      suppressPackageStartupMessages(library(affy, quietly = TRUE))
      affyBatch <- affy::ReadAffy(filenames = cel_paths)
      expr_rma <- affy::rma(affyBatch)
      mat <- Biobase::exprs(expr_rma)
      attr(mat, "normalization_info") <- list(
        initial_genes = nrow(mat), final_genes = nrow(mat),
        method = "RMA (affy)", log2_applied = TRUE
      )
      return(mat)
    }, error = function(e) NULL)
  }
  NULL
}

# Normalize RNA-seq (enhanced version with tracking)
# method: "TMM" (default, recommended) or "log2cpm_only" (log2(CPM+1) only, no TMM; quick exploration)
normalize_rnaseq <- function(count_matrix, dataset_name = NULL, method = "TMM") {
  # Track initial state
  initial_genes <- nrow(count_matrix)

  count_matrix <- as.matrix(count_matrix)
  mode(count_matrix) <- "numeric"
  count_matrix <- round(count_matrix)
  count_matrix[count_matrix < 0] <- 0

  # Filter: keep genes with counts >= 10 in at least 3 samples
  keep <- rowSums(count_matrix >= 10) >= 3
  count_matrix <- count_matrix[keep, ]

  genes_after_filtering <- nrow(count_matrix)
  genes_removed <- initial_genes - genes_after_filtering

  dge <- edgeR::DGEList(counts = count_matrix)
  if (method == "TMM") {
    dge <- edgeR::calcNormFactors(dge, method = "TMM")
  }
  # log2(CPM+1): same call; when norm.factors are 1 (no TMM), this is simple log2(CPM+1)
  logcpm_matrix <- edgeR::cpm(dge, log = TRUE, prior.count = 1)

  # Store normalization info as attribute
  attr(logcpm_matrix, "normalization_info") <- list(
    initial_genes = initial_genes,
    genes_after_filtering = genes_after_filtering,
    genes_removed = genes_removed,
    final_genes = nrow(logcpm_matrix),
    method = method
  )

  return(logcpm_matrix)
}

cat("✓ Helper functions loaded\n")

