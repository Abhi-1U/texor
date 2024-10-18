test_that("Convert Rnw to Rmd", {
    article_dir <- system.file("examples/sweave_article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE)
    your_article_path <- paste(your_article_folder, "sweave_article", "example.Rnw",sep="/")
    your_article_path <- xfun::normalize_path(your_article_path)
    expect_equal(suppressMessages(texor::rnw_to_rmd(your_article_path, output_format = "bookdown", clean_up = TRUE, autonumber_sec = FALSE)), texor::pandoc_version_check())
    # expect_equal(file.exists(
    #                 xfun::normalize_path(
    #                     paste(your_article_folder, "sweave_article", "example.Rmd", sep="/"))),
    #              TRUE)
    unlink(your_article_folder, recursive = TRUE)
})
