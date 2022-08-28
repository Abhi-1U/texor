
test_that("Include style file", {
  wd <- system.file("examples/article", package = "texor")
  test_wd <- paste0(wd,"/test1")
  dir.create(test_wd,showWarnings = FALSE)
  file.copy(list.files(wd, full.names = TRUE),to = test_wd)
  file_data <- readLines(paste0(test_wd,"/RJwrapper.tex"))
  texor::include_style_file(test_wd)
  expect_equal(file.exists(paste0(test_wd,"/Metafix.sty")), TRUE)
  mod_file_data <- readLines(paste0(test_wd,"/RJwrapper.tex"))
  expect_equal(which(grepl("\\usepackage\\{Metafix\\}",mod_file_data)),10)
  unlink(test_wd)
})
