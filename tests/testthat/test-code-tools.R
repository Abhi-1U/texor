test_that("filter_code_env", {
    target = "example"
    replacement = "verbatim"
    raw_lines = "\\begin{example}\n print('helloworld')\n \\end{example}"
    expected_raw_lines = "\\begin{verbatim}\n print('helloworld')\n \\end{verbatim}"
    fun_out =  suppressMessages(texor:::filter_code_env(raw_lines,target,replacement))
    expect_equal(fun_out,expected_raw_lines)
})

test_that("patch_code_env", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    file_data <- readLines(paste0(your_article_path,"/example.tex"))
    suppressMessages(texor:::patch_code_env(your_article_path))
    after_patch_data <- readLines(paste0(your_article_path,"/example.tex"))
    expect_equal(after_patch_data,suppressMessages(texor:::filter_code_env(file_data,"example","verbatim")))
    unlink(your_article_folder,recursive = TRUE)
})
