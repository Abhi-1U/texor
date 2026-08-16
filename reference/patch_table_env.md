# patch table environment

function to modify env and commands in TeX using GNU sed

These are due to the pandoc's limitations and ease in conversion.

## Usage

``` r
patch_table_env(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

patches table environments in LaTeX file and also backs up the old file
before modification

## Details

changes are made to : 1. table\* environment to table environment 2.
\multicolumn to \multicolumnx \multicolumnx is redefined in Metafix.sty
as \renewcommand{\multicolumnx}\[3\]{\multicolumn{#1}{c}{#3}}

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::patch_table_env(your_article_path)
#> Found  1 Matches for target : table\*
#> Changed \begin{table\*} to \begin{table}
#> Found  1 Matches for target : table\*
#> Changed \end{table*} to \end{table}
#> Found  2 Matches for target : multicolumn
#> changed \multicolumn to \multicolumnx
#> Found 0 Matches for target :  widetable
#> Changed \begin{widetable} to \begin{table}
#> Found 0 Matches for target :  widetable
#> Changed \end{widetable} to \end{table}
#> Auto add label for table env
unlink(your_article_folder,recursive = TRUE)
```
