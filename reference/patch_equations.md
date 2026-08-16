# patch equations

this function patches equations (particularly eqnarray)

## Usage

``` r
patch_equations(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

patches equations environments in LaTeX file and also backs up the old
file before modification

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::patch_equations(your_article_path)
unlink(your_article_folder,recursive = TRUE)
```
