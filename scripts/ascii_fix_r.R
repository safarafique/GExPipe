# Replace non-ASCII punctuation in R sources (R CMD check portable packages).
files <- list.files("R", pattern = "\\.R$", full.names = TRUE)
replacements <- c(
  "\u2500" = "-",
  "\u2014" = "-",
  "\u2013" = "-",
  "\u2192" = "->",
  "\u2265" = ">=",
  "\u00b2" = "^2",
  "\u2026" = "...",
  "\u03b2" = "beta",
  "\u2229" = "intersect",
  "\u2713" = "OK",
  "\u2717" = "X",
  "\u00d4\u00c7\u00f6" = "-",
  "\u00d4\u00e5\u00c6" = "->",
  "\u00d4\u00f6\u00c7" = "-",
  "\u00d4\u00c7\u00aa" = "..."
)
for (f in files) {
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  orig <- paste(x, collapse = "\n")
  for (i in seq_along(replacements)) {
    x <- gsub(names(replacements)[i], replacements[i], x, fixed = TRUE)
  }
  if (!identical(orig, paste(x, collapse = "\n"))) {
    writeLines(x, f, useBytes = FALSE)
    message("fixed: ", f)
  }
}
