# Include Style file

Includes the Metafix.sty style file

## Usage

``` r
include_style_file(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

adds Metafix.sty file in the article_dir also includes it in RJwrapper
file.

## Details

This style file helps texor and pandoc to retain metadata and ease the
conversion process.

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::include_style_file(your_article_path)
#> [1] TRUE
unlink(your_article_folder,recursive = TRUE)
```
