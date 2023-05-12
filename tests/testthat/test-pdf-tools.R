test_that("multiplication works", {
  article_dir <- system.file("examples/pdf_conversion",
                             package = "texor")
  file_path <- paste0(article_dir,"/normal.pdf")
  texor::convert_to_png(file_path)
  expect_equal(file.exists(paste0(article_dir,"/normal.png")), TRUE)
})
