non_white_share <- function(f) {
  img <- png::readPNG(f)
  mean(img[, , 1:3] < 0.95)
}

test_that("plot exports draw ggplot results (were blank: QC boxplot, DCA)", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("png")
  draw <- getFromNamespace("gexp_draw_plot_result", "GExPipe")
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 400, height = 300)
  ok <- draw(function() ggplot2::ggplot(data.frame(x = 1:10, y = 1:10), ggplot2::aes(x, y)) + ggplot2::geom_point(size = 4))
  grDevices::dev.off()
  expect_true(ok)
  expect_gt(non_white_share(f), 0.01)
})

test_that("base-graphics plots still export", {
  skip_if_not_installed("png")
  draw <- getFromNamespace("gexp_draw_plot_result", "GExPipe")
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 400, height = 300)
  draw(function() graphics::plot(1:10, pch = 19, cex = 3))
  grDevices::dev.off()
  expect_gt(non_white_share(f), 0.01)
})

test_that("a failing plot is drawn as a visible message, not a blank file", {
  skip_if_not_installed("png")
  draw <- getFromNamespace("gexp_draw_plot_result", "GExPipe")
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 400, height = 300)
  ok <- draw(function() stop("no DCA data"), "Nomogram_DCA_Training")
  grDevices::dev.off()
  expect_false(ok)
  expect_gt(non_white_share(f), 0.001)
})
