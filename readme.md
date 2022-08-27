
<!-- README.md is generated from README.Rmd. Please edit that file -->

# texor : Tools for converting LaTeX source files into RJ-web-articles

<!-- badges: start -->

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
5.  Supports graphics included as PDF,PNG,JPG.
6.  Functions to convert PDF graphics to PNG.
7.  Almost all tables are supported with a few exceptions.
8.  Other things like citations,links,footnotes,math,ctv,package
    references are also supported.
9.  A parser to read and minimally convert inbuilt `\thebibliography` to
    bibtex (moved to [rebib](https://github.com/Abhi-1U/rebib))
10. Figure/Table/Equation numbering
11. Stream Editor to rename alien commands/environments to accepted
    defaults.
12. Pre and Post conversion statistics of environments for verification.
13. Bibliography aggregation.(moved to
    [rebib](https://github.com/Abhi-1U/rebib))

### Work in Progress Features

1.  Tikz graphics compilation support functions
2.  Better Documentation as vignettes, man pages, articles, pkgdown
3.  Logging Events in Log Files
4.  Passing R-CMD-check ![texor features](man/figures/texor.svg)

## Installation

install these dependency packages (github development versions)

``` r
# install.packages("remotes")
remotes::install_github("rstudio/distill")
remotes::install_github("Abhi-1U/rjtools")
remotes::install_github("Abhi-1U/rebib")
```

install the development version from GitHub with:

``` r
# install.packages("remotes")
remotes::install_github("Abhi-1U/texor")
# install.packages("pak")
pak::pak("Abhi-1U/texor")
# also install rebib for bibliography
remotes::install_github("Abhi-1U/rebib")
```

## General Usage

here is a quick example to use texor package with a sample RJournal
article (included with the package
[inst/article](https://github.com/Abhi-1U/texor/tree/master/inst/examples/article))

``` r
# similarly any file path
wd <-  system.file("examples/article", package = "texor")
# first include the style file
texor::include_style_file(wd)
# handle bibliography
rebib::aggregate_bibliography(wd)
# texor::handle_bibliography(wd)
# will also work
# patch table env using stream editor
texor::patch_table_env(wd)
# patch code env using stream editor
texor::patch_code_env(wd)
# check for tikz if TRUE call tikz related functions(still under work)
texor::article_has_tikz(wd)
# convert pdf to png
texor::pdf_to_png(wd)
# copy files to a /web folder
texor::copy_other_files(wd)
# generate environment stats for tex file
texor::pre_conversion_statistics(wd)
# convert to markdown
texor::convert_to_markdown(wd)
# the below function wont work on any article as it needs a folder structure
# similar to RJournal style /YYYY-ZZ/YYYY-MMM where YYYY is the year, 
# ZZ is the Journal issue number and MMM is the DOI referral(unique article number)
texor::generate_rmd(wd)
texor::produce_html(wd)
```
