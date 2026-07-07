# One-off: replace non-ASCII in R/server_*.R and R/ui_*.R
files <- list.files("R", pattern = "^(server_|ui_).*\\.R$", full.names = TRUE)
repl <- c(
  "\u2192" = "->",
  "\u2014" = "-",
  "\u2013" = "-",
  "\u00d7" = "x",
  "\u2713" = "OK",
  "\u2717" = "X",
  "\u00b0" = " deg",
  "\u00b7" = "-"
)
for (f in files) {
  txt <- readLines(f, warn = FALSE, encoding = "UTF-8")
  for (i in seq_along(repl)) {
    txt <- gsub(names(repl)[i], repl[i], txt, fixed = TRUE)
  }
  writeLines(txt, f, useBytes = FALSE)
}
message("ASCII cleanup: ", length(files), " files")
