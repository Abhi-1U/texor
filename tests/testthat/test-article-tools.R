
test_that("Include style file", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    file_data <- readLines(paste0(your_article_path,"/RJwrapper.tex"))
    texor::include_style_file(your_article_path)
    expect_equal(file.exists(paste0(your_article_path,"/Metafix.sty")), TRUE)
    mod_file_data <- readLines(paste0(your_article_path,"/RJwrapper.tex"))
    expect_equal(which(grepl("\\usepackage\\{Metafix\\}",mod_file_data)),10)
    unlink(your_article_folder,recursive = TRUE)
})
