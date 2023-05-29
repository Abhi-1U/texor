test_that("figure reader", {
  wd <- system.file("examples/article", package = "texor")
  data <- texor:::figure_reader(wd,texor::get_texfile_name(wd))
  gen_yaml <- readLines(paste0(wd,"/texor-figure-meta.yaml"))
  exp_yaml <- readLines(paste0(wd,"/expected-texor-figure-meta.yaml"))
  # yaml test
  expect_equal(gen_yaml, exp_yaml)
  # extension test
  expect_equal(data[[1]]$extension, "png")
  # relative path test
  expect_equal(data[[1]]$path, "Rlogo-5.png")
  # label test
  expect_equal(data[[1]]$label, "figure:rlogo")
  # caption test
  expect_equal(data[[1]]$caption, "{The logo of R.}")
  # image count test
  expect_equal(data[[1]]$image_count, 1)
  # istikz test
  expect_equal(data[[1]]$istikz, FALSE)
  # relative fig text includegraphics position test
  expect_equal(data[[1]]$image_pos, 3)
})
