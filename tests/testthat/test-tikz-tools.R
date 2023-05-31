test_that("has tikz image", {
  article_dir <- system.file("examples/article", package = "texor")
  dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
  x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
  your_article_path <- paste(your_article_folder,"article",sep="/")
  expect_equal(article_has_tikz(your_article_path), FALSE)
  unlink(your_article_folder,recursive = TRUE)
})
