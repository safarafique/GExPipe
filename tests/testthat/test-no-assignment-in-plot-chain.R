test_that("no assignment sits inside a ggplot `+` chain", {
  # `p <- ggplot(...) + theme() +\n x <- 1` parses as `(ggplot(...) + x) <- 1`,
  # which fails at run time with: could not find function "+<-".
  # This broke the volcano PNG/JPG/PDF downloads (browser saved an .htm file).
  ns <- asNamespace("GExPipe")
  bad_lhs <- function(e) {
    if (!is.call(e)) return(FALSE)
    if (as.character(e[[1]])[1] %in% c("<-", "=") && length(e) == 3L &&
        is.call(e[[2]]) && identical(e[[2]][[1]], as.name("+"))) {
      return(TRUE)
    }
    any(vapply(as.list(e)[-1], function(a) !missing(a) && bad_lhs(a), logical(1)))
  }
  offenders <- Filter(function(nm) {
    f <- get(nm, envir = ns)
    is.function(f) && !is.primitive(f) && bad_lhs(body(f))
  }, ls(ns, all.names = TRUE))
  expect_identical(offenders, character(0))
})
