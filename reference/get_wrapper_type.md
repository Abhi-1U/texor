# Get the name of the wrapper file in the article dir

This function gets the wrapper file name from the commonly named
R-Journal wrapper files.

## Usage

``` r
get_wrapper_type(article_dir, auto_wrapper = FALSE, interactive_mode = FALSE)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- auto_wrapper:

  automatically creates a wrapper if TRUE, else asks user. default value
  FALSE

- interactive_mode:

  interactive mode for converting articles with options.

## Value

String with name of wrapper file or empty

## Details

Usually the R journal wrapper files are named either 1. RJwrapper.tex 2.
RJwrap.tex 3. wrapper.tex

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::get_wrapper_type(your_article_path)
#> [1] "RJwrapper.tex"
unlink(your_article_folder,recursive = TRUE)
```
