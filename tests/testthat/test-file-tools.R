test_that("get wrapper name", {
  wd <- system.file("examples/article", package = "texor")
  wrapper_file <- get_wrapper_type(wd)
  expect_equal(wrapper_file, "RJwrapper.tex")
  file.rename(paste0(wd,"/",wrapper_file),to = paste0(wd,"/RJwrap.tex"))
  wrapper_file <- get_wrapper_type(wd)
  expect_equal(wrapper_file, "RJwrap.tex")
  file.rename(paste0(wd,"/",wrapper_file),to = paste0(wd,"/wrapper.tex"))
  wrapper_file <- get_wrapper_type(wd)
  expect_equal(wrapper_file, "wrapper.tex")
  file.rename(paste0(wd,"/",wrapper_file),to = paste0(wd,"/RJwrapper.tex"))
})

test_that("get texfile name", {
  wd <- system.file("examples/article", package = "texor")
  file_name <- get_texfile_name(wd)
  expect_equal(file_name, "example.tex")
})

test_that("write_external_file", {
  wd <- system.file("examples/article", package = "texor")
  file_path <- paste0(wd, "/texor.txt")
  write_external_file(file_path = file_path, mode = "w", raw_text = "hello texor!")
  expect_equal(readLines(file_path), "hello texor!")
})

test_that("comment filter", {
    raw_text <- c("This is supposed to be a latex file",
                  "% oops this is a latex comment",
                  "%% this is also a comment",
                  "This is not a comment")
    expected_text <- c("This is supposed to be a latex file",
                                   "This is not a comment")

    expect_equal(comment_filter(raw_text), expected_text)
})
