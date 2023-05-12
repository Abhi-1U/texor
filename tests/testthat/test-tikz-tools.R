test_that("has tikz image", {
  wd <- system.file("examples/article", package = "texor")
  expect_equal(article_has_tikz(wd), FALSE)
})
