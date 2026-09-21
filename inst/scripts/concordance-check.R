outdir <- "validation_manual"
methods <- c("deseq2", "edger", "limma_voom", "limma")
read_sig <- function(gse, m) {
  f <- file.path(outdir, paste0(gse, "_", m, "_DE_sig.csv"))
  unique(read.csv(f, stringsAsFactors = FALSE)$Gene)
}
for (gse in c("GSE50760", "GSE104836")) {
  cat("\n", gse, " method concordance (vs deseq2):\n", sep = "")
  ref <- read_sig(gse, "deseq2")
  for (m in methods) {
    g <- read_sig(gse, m)
    both <- length(intersect(ref, g))
    uni <- length(union(ref, g))
    cat(sprintf("  %-12s DEGs=%5d overlap=%5d Jaccard=%.3f\n", m, length(g), both, both / uni))
  }
}
de <- read.csv(file.path(outdir, "GSE104836_deseq2_DE_all.csv"), stringsAsFactors = FALSE)
sig_pub <- de[!is.na(de$P.Value) & de$P.Value < 0.05 & abs(de$logFC) >= 1, ]
cat("\nGSE104836 DESeq2 |log2FC|>=1 & P<0.05:", nrow(sig_pub), "(GEO paper ~3221 mRNAs)\n")
