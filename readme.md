
<!-- README.md is generated from README.Rmd. Please edit that file -->

# texor : Tools for converting LaTeX source files into RJ-web-articles

<!-- badges: start -->

[![CRAN/METACRAN](https://img.shields.io/cran/v/texor?color=blue&style=for-the-badge)](https://cran.r-project.org/package=texor)
[![GitHub Workflow Status
(branch)](https://img.shields.io/github/actions/workflow/status/Abhi-1U/texor/pkg_down.yaml?branch=master&label=pkgdown&style=for-the-badge)](https://github.com/Abhi-1U/texor/actions/workflows/pkg_down.yaml)
[![GitHub Workflow
Status](https://img.shields.io/github/actions/workflow/status/Abhi-1U/texor/cmdcheck.yaml?branch=master&label=R-CMD-CHECK&style=for-the-badge)](https://github.com/Abhi-1U/texor/actions/workflows/cmdcheck.yaml)
[![GitHub R package version (subdirectory of
monorepo)](https://img.shields.io/github/r-package/v/Abhi-1U/texor?filename=DESCRIPTION&label=texor&logo=github&color=navy&style=for-the-badge)](https://github.com/Abhi-1U/texor/blob/master/DESCRIPTION)
![Github
Issues](https://img.shields.io/github/issues/Abhi-1U/texor?color=orange&logo=github&logoColor=&style=for-the-badge)
<!-- badges: end -->

texor is a package that deals with multiple challenges that occour in
conversion of LaTeX source files (which typically generate a PDF) to a
web friendly RJ-web-article format.

### Currently texor can handle R-Journal structured LaTeX files with support for:

1.  RJournal based LaTeX files with macros such as
    `\pkg{}`,`\CRANpkg{}`,`\BIOpkg{}`, `\code{}`, `\acronym{}`
2.  Reading Metadata from LaTeX file as well as DESCRIPTION.
3.  Reading bibliography from bibtex files and ignoring
    `\thebibliography`
4.  Supports code environments like `Sinput`,`Soutput`, `example`
    ,`example*`,`verbatim`, `smallverbatim` with code highlight.
5.  Supports graphics included as PDF, PNG, JPG, tikz, algorithm2e
    graphics supported (some figures might not work).
6.  Functions to convert PDF graphics to PNG.
7.  Almost all tables are supported with a few exceptions.
8.  Other things like citations,links,footnotes,math,CTV,package
    references are also supported.
9.  Bibliography management and aggregation.(moved to
    [rebib](https://github.com/Abhi-1U/rebib))
10. Figure/Table/Equation numbering
11. Stream Editor to rename alien commands/environments to accepted
    defaults.
12. Pre and Post conversion statistics of environments for verification.
13. Support for included files (1 degree related to main article only).
14. Logging of events.

## Installation

Install Pandoc v3.1 or greater, ideally latest version. If you are using
Rstudio, it should be pre-installed.

``` r
# To check the version of pandoc
rmarkdown::pandoc_version()
# or to simply check if you are good to go in terms of pandoc
texor::pandoc_version_check()
# TRUE if pandoc is good to go
# FALSE if pandoc is too old for texor
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
texor::latex_to_web(your_article_path,log_steps = FALSE, example = TRUE)
# view  your converted and original article at
paste0(your_article_path,"/web")
```

## Using texor over multiple files

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
