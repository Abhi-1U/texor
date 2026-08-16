# Get the name of the tex file included within wrapper file

The wrapper file refers to an external tex file which contains the
actual document content.

## Usage

``` r
get_texfile_name(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

String name of the tex-file name

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::get_texfile_name(your_article_path)
#> [1] "example.tex"
unlink(your_article_folder,recursive = TRUE)
```
