test_that("figure reader", {
    article_dir <- system.file("examples/article", package = "texor")
    dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
    x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
    your_article_path <- paste(your_article_folder,"article",sep="/")
    data <- texor:::figure_reader(your_article_path,texor::get_texfile_name(your_article_path))
    if (file.exists(paste0(your_article_path,"/texor-figure-meta.yaml"))){
        gen_yaml <- readLines(paste0(your_article_path,"/texor-figure-meta.yaml"))
        if (file.exists(paste0(your_article_path,"/expected-texor-figure-meta.yaml"))){
            exp_yaml <- readLines(paste0(your_article_path,"/expected-texor-figure-meta.yaml"))
            # yaml test
            expect_equal(gen_yaml, exp_yaml)
        }
        else {
            #pass
        }
    }
    else{
        #pass
    }

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
    unlink(your_article_folder,recursive = TRUE)
})
