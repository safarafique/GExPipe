#!/usr/bin/env Rscript
## Generate horizontal workflow figure for Figure 1A / Supplementary Figure S1
## Usage: Rscript inst/scripts/make-workflow-figure.R --out inst/manuscript/Figure_1A_workflow.png

args <- commandArgs(trailingOnly = TRUE)
get_arg <- function(flag, default) {
  i <- match(flag, args)
  if (!is.na(i) && i < length(args)) args[i + 1L] else default
}
out <- get_arg("--out", file.path("inst", "manuscript", "Figure_1A_workflow.png"))
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Install ggplot2: install.packages('ggplot2')")
}

steps <- data.frame(
  step = 1:15,
  x = c(1:5, 6:8 + 0.5, 9:12 + 1, 13:15 + 1.5),
  label = c(
    "1\nDownload", "2\nQC", "3\nNorm", "4\nGroups", "5\nBatch",
    "6\nDE", "7\nWGCNA", "8\nCommon",
    "9\nPPI", "10\nML", "11\nVal", "12\nROC",
    "13\nNom", "14\nGSEA", "15\nReport"
  ),
  phase = rep(
    c("Data preparation", "Gene discovery", "Candidate refinement", "Clinical translation"),
    c(5, 3, 4, 3)
  ),
  stringsAsFactors = FALSE
)

phase_cols <- c(
  "Data preparation" = "#4E79A7",
  "Gene discovery" = "#59A14F",
  "Candidate refinement" = "#F28E2B",
  "Clinical translation" = "#E15759"
)

p <- ggplot2::ggplot(steps, ggplot2::aes(x = x, y = 1, fill = phase)) +
  ggplot2::geom_tile(
    ggplot2::aes(width = 0.85, height = 0.55),
    colour = "white", linewidth = 0.5
  ) +
  ggplot2::geom_text(
    ggplot2::aes(label = label),
    colour = "white", size = 2.8, fontface = "bold", lineheight = 0.85
  ) +
  ggplot2::geom_segment(
    data = steps[-nrow(steps), ],
    ggplot2::aes(x = x + 0.42, xend = x + 0.58, y = 1, yend = 1),
    inherit.aes = FALSE,
    arrow = grid::arrow(length = grid::unit(0.12, "cm"), type = "closed"),
    linewidth = 0.35, colour = "#4D4D4D"
  ) +
  ggplot2::annotate(
    "text", x = 0.3, y = 1.45, label = "GEO\ninput", size = 3, fontface = "italic"
  ) +
  ggplot2::annotate(
    "text", x = max(steps$x) + 0.6, y = 1.45, label = "PDF\nexport", size = 3, fontface = "italic"
  ) +
  ggplot2::scale_fill_manual(values = phase_cols, name = "Phase") +
  ggplot2::scale_x_continuous(limits = c(0, max(steps$x) + 1), expand = c(0, 0)) +
  ggplot2::coord_fixed(ratio = 0.35) +
  ggplot2::labs(
    title = "GExPipe data-flow pipeline (15 steps)",
    x = NULL, y = NULL
  ) +
  ggplot2::theme_void() +
  ggplot2::theme(
    legend.position = "bottom",
    plot.title = ggplot2::element_text(hjust = 0.5, face = "bold", size = 11),
    legend.text = ggplot2::element_text(size = 8)
  )

ggplot2::ggsave(out, p, width = 12, height = 3.5, dpi = 300, bg = "white")
cat("Wrote:", out, "\n")
