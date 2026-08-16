# Check if article has tikz images or not

This simple utiliy function will check for tikzpicture environment

## Usage

``` r
article_has_tikz(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

TRUE if tikz image is present else FALSE

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::article_has_tikz(your_article_path)
#> [1] FALSE
unlink(your_article_folder,recursive = TRUE)
```
