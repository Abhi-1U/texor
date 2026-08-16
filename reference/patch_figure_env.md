# patch figure environments

This function calls the stream editor to change figure\* to figure 1.
figure\*

## Usage

``` r
patch_figure_env(article_dir, with_alg = TRUE)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- with_alg:

  to include algorihtm environment or not

## Value

patches figure environments in LaTeX file and also backs up the old file
before modification

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
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
unlink(your_article_folder,recursive = TRUE)
```
