# pre conversion statistics

count common environments,inlines for debugging purposes

## Usage

``` r
pre_conversion_statistics(article_dir, write_yaml = TRUE)
```

## Arguments

- article_dir:

  path to the directory which contains RJ article

- write_yaml:

  write to a yaml file (default = TRUE)

## Value

conversion stat block with details also a yaml file if param enabled.

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::patch_code_env(your_article_path)
#> Found  1 Matches for target : example
#> Found  1 Matches for target : example
#> Found 0 Matches for target :  example\*
#> Found 0 Matches for target :  example\*
#> Found 0 Matches for target :  Sin
#> Found 0 Matches for target :  Sin
#> Found 0 Matches for target :  Sout
#> Found 0 Matches for target :  Sout
#> Found 0 Matches for target :  Scode
#> Found 0 Matches for target :  Scode
#> Found 0 Matches for target :  Sinput
#> Found 0 Matches for target :  Sinput
#> Found 0 Matches for target :  Soutput
#> Found 0 Matches for target :  Soutput
#> Found 0 Matches for target :  smallverbatim
#> Found 0 Matches for target :  smallverbatim
#> Found 0 Matches for target :  boxedverbatim
#> Found 0 Matches for target :  boxedverbatim
#> Found 0 Matches for target :  smallexample
#> Found 0 Matches for target :  smallexample
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
texor::patch_equations(your_article_path)
texor::patch_figure_env(your_article_path)
#> Found 0 Matches for target :  figure\*
#> Changed \begin{figure\*} to \begin{figure}
#> Found 0 Matches for target :  figure\*
#> Changed \end{figure\*} to \end{figure}
#> Found 0 Matches for target :  widefigure
#> Changed \begin{widefigure} to \begin{figure}
#> Found 0 Matches for target :  widefigure
#> Changed \end{widefigure} to \end{figure}
#> Found 0 Matches for target :  widefigure\*
#> Changed \begin{widefigure\*} to \begin{figure}
#> Found 0 Matches for target :  widefigure\*
#> Changed \end{widefigure\*} to \end{figure}
#> Found 0 Matches for target :  algorithm
#> Changed \begin{algorithm} to \begin{figure}
#> Found 0 Matches for target :  algorithm
#> Changed \end{algorithm} to \end{figure}
texor::pre_conversion_statistics(your_article_path,write_yaml = FALSE)
#> $table
#> [1] 1
#> 
#> $figure
#> [1] 1
#> 
#> $math
#> [1] 2
#> 
#> $citations
#> [1] 2
#> 
#> $code
#> $code$block
#> [1] 1
#> 
#> $code$inline
#> [1] 0
#> 
#> 
unlink(your_article_folder,recursive = TRUE)
```
