# convert LaTeX wrapper to markdown

Uses pandoc along with several lua filters found at inst/extdata/filters
in texor package

## Usage

``` r
convert_to_markdown(
  article_dir,
  kable_tab = TRUE,
  autonumber_eq = FALSE,
  fig_in_r = TRUE
)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- kable_tab:

  converts to kable table instead of markdown tables

- autonumber_eq:

  whether to autonumber the equations, default is FALSE

- fig_in_r:

  whether to include figures in R code chunks, default is TRUE

## Value

creates a converted markdown file, as well as a pkg_meta.yaml file

## Details

convert latex(wrapper) file to markdown

## Note

pandoc (along with lua interpreter) is already installed with R-studio,
hence if not using R-studio you will need to install pandoc.
https://pandoc.org/installing.html

Use pandoc version greater than or equal to 3.1

Kable tables will work for simple static data, any math / code / image
within any table will send the package into fallback mode (normal
markdown tables) for the rest of tables in the article.

## Examples

``` r
# Note This is a minimal example to execute this function
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
rmarkdown::pandoc_version()
#> [1] ‘3.8.3’
texor::include_style_file(your_article_path)
#> [1] TRUE
rebib::aggregate_bibliography(your_article_path)
#> No Bib files found !
#> Cannot aggregate bibliography as there is no BibTeX bibliography
#> No Bib files found !
#> BibTeX file does not exist
#> will parse for bibliography
#> bibtex file created
texor::convert_to_markdown(your_article_path)
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.md -s --resource-path /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/fig_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/conversion_compat_check.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
unlink(your_article_folder,recursive = TRUE)
```
