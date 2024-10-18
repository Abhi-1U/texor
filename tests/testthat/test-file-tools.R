test_that("get wrapper name", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    wrapper_file <- get_wrapper_type(your_article_path)
    expect_equal(wrapper_file, "RJwrapper.tex")
    file.rename(paste0(your_article_path,"/",wrapper_file),to = paste0(your_article_path,"/RJwrap.tex"))
    wrapper_file <- get_wrapper_type(your_article_path)
    expect_equal(wrapper_file, "RJwrap.tex")
    file.rename(paste0(your_article_path,"/",wrapper_file),to = paste0(your_article_path,"/wrapper.tex"))
    wrapper_file <- get_wrapper_type(your_article_path)
    expect_equal(wrapper_file, "wrapper.tex")
    file.rename(paste0(your_article_path,"/",wrapper_file),to = paste0(your_article_path,"/RJwrapper.tex"))
    unlink(your_article_folder,recursive = TRUE)
})

test_that("get texfile name", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    file_name <- get_texfile_name(your_article_path)
    expect_equal(file_name, "example.tex")
    unlink(your_article_folder,recursive = TRUE)
})

test_that("write_external_file", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    file_path <- paste0(your_article_path, "/texor.txt")
    write_external_file(file_path = file_path, mode = "w", raw_text = "hello texor!")
    expect_equal(readLines(file_path), "hello texor!")
    unlink(your_article_folder,recursive = TRUE)
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

test_that("find_wrapper",{
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    expect_equal(texor::find_wrapper(your_article_path),"RJwrapper.tex")
    unlink(your_article_folder,recursive = TRUE)
})
