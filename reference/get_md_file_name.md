# get markdown file name

get markdown file name

## Usage

``` r
get_md_file_name(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

markdown file name

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::get_md_file_name(your_article_path)
#> [1] "RJwrapper.md"
unlink(your_article_folder,recursive = TRUE)
```
