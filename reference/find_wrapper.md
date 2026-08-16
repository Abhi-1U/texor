# find wrapper file

Finds a different named wrapper file for RJournal article

## Usage

``` r
find_wrapper(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

wrapper file name or empty string if none

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::find_wrapper(your_article_path)
#> [1] "RJwrapper.tex"
unlink(your_article_folder,recursive = TRUE)
```
