# texor [![texor package hex sticker with icons showing transistion from PDF documents to web pages.](https://abhi-1u.github.io/template_samples/img/texor-hex-sticker.png)](https://abhi-1u.github.io/texor/)

The R package texor will ease your LaTeX R Journal / Sweave article
migration to web format by providing tools and utilities.

## Installation

Install pandoc v3.1 or greater, ideally latest version. If you are using
Rstudio, it should be pre-installed.

``` r

# To check the version of pandoc
rmarkdown::pandoc_version()
# or to simply check if you are good to go in terms of pandoc
texor::pandoc_version_check()
# TRUE if pandoc is good to go, else FALSE
```

For included PDF conversions also install poppler-utils (used with
pdftools package) Ideally install the latest version of poppler-utils.

Install the mainline version from CRAN with:

``` r

install.packages('texor')
```

Install the development version from GitHub with:

``` r

# install.packages("remotes")
remotes::install_github("Abhi-1U/texor")
# install.packages("pak")
pak::pak("Abhi-1U/texor")
```

## General Usage

#### R Journal articles

here is a quick example to use texor package with a sample RJournal
article (included with the package
[inst/article](https://github.com/Abhi-1U/texor/tree/master/inst/examples/article))

``` r

# for a single LaTeX article 
texor::latex_to_web(article_dir)
```

``` r

# A running example
article_dir <- system.file("examples/article", package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
# view your original article at
your_article_path
texor::latex_to_web(your_article_path,..)
# view  your converted and original article at
paste0(your_article_path,"/web")
```

#### Sweave articles

Similar to the conversion of R journal articles, pass the file
path(instead of the folder path) to the function.

``` r

texor::rnw_to_rmd(file_path,..) 
# Additionally you can set the options to modify the end result as per your needs.
```

## Using texor over multiple files

``` R
# for multiple articles in RJ folder structure
#base dir 
article_dir <- "C:/Users/abhis/path/to/base"
# list of journal number directories
journal_dirs <- list.dirs(article_dir,recursive = FALSE)
# list of individual slug directories
slug_dirs <- lapply(journal_dirs,function(journal_dir) {
    list.dirs(journal_dir,recursive = FALSE)
})
# creating a single list of all slug directories
slug_dirs <- unlist(slug_dirs)
# Calling 
for (dir in slug_dirs) {
    #print(dir)
    getwd()
    # below function will create a log file of success/error in current
    # working directory while running the texor::latex_to_web() function
    texor:::convert_to_html(dir)
}
```
