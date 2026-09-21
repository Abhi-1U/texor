# Sweave to RMarkdown

automated function for converting a single Sweave file to R Markdown
file

## Usage

``` r
rnw_to_rmd(
  input_file,
  output_format,
  clean_up = TRUE,
  autonumber_eq = FALSE,
  autonumber_sec = TRUE,
  suppress_package_startup_message = FALSE,
  kable_tab = TRUE,
  fig_in_r = TRUE,
  algorithm_render = FALSE
)
```

## Arguments

- input_file:

  input Sweave file path

- output_format:

  knit output type for the RMarkdown file options for "bookdown",
  "biocstyle", "litedown"

- clean_up:

  whether to clean up the intermediate files, default is TRUE

- autonumber_eq:

  whether to autonumber the equations, default is FALSE

- autonumber_sec:

  whether to autonumber the sections, default is TRUE

- suppress_package_startup_message:

  whether to suppress the package startup message, default is FALSE

- kable_tab:

  converts to kable table instead of markdown tables

- fig_in_r:

  whether to include figures in R code chunks, default is TRUE

- algorithm_render:

  Enable to include algorithms with pseudocode.js, default is FALSE
  optional is TRUE

## Value

True if R Markdown file successfully generated in the same folder

## Note

Use pandoc version greater than or equal to 3.1

## Examples

``` r
# move example Sweave article and associated files to a temporary directory
example_dir <- system.file("examples", "sweave_article", package = "texor")
file.copy(from = example_dir, to = tempdir(), recursive = TRUE)
#> [1] TRUE
article_dir <- file.path(tempdir(), "sweave_article")

# convert example Sweave article to Rmd
rnw_to_rmd(file.path(article_dir, "example.Rnw"),
           output_format = "bookdown",
           clean_up = TRUE,
           autonumber_eq = TRUE,
           autonumber_sec = FALSE)
#> removing the unnecessary option fig=TRUE:
#>     * image,fig=TRUE, fig.cap="matrix package's image"
#> removing the unnecessary option fig=TRUE:
#>     * keep.source=TRUE, fig=TRUE
#> changing keep.source=TRUE to tidy=FALSE:
#>     * keep.source=TRUE,
#> changing \SweaveOpts{} to opts_chunk$set():
#>     * \SweaveOpts{keep.source=TRUE, fig=TRUE}
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-knitr.Rnw --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-knitr-part1.md --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article -f /Users/runner/work/_temp/Library/texor/sweave_code_reader.lua --lua-filter /Users/runner/work/_temp/Library/texor/r_code_chunk_patcher.lua 
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-knitr.Rnw --to latex --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-generated.tex --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article -f /Users/runner/work/_temp/Library/texor/sweave_code_remove.lua 
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-generated.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-knitr-part2.md --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article -f /Users/runner/work/_temp/Library/texor/algorithm_reader.lua 
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-generated.tex --to latex --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/example-generated.tex --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article -f /Users/runner/work/_temp/Library/texor/algorithm_remove.lua 
#> Found Bib file  example.bib
#> Cannot aggregate bibliography as there is no embedded bibliography
#> Found 0 Matches for target :  example
#> Found 0 Matches for target :  example
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
#> Found 0 Matches for target :  table\*
#> Changed \begin{table\*} to \begin{table}
#> Found 0 Matches for target :  table\*
#> Changed \end{table*} to \end{table}
#> Found 0 Matches for target :  multicolumn
#> changed \multicolumn to \multicolumnx
#> Found 0 Matches for target :  widetable
#> Changed \begin{widetable} to \begin{table}
#> Found 0 Matches for target :  widetable
#> Changed \end{widetable} to \end{table}
#> Auto add label for table env
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/RJwrapper.tex --to native --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/temp-native.txt -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/RJwrapper.tex --lua-filter /Users/runner/work/_temp/Library/texor/find_pdf_files.lua 
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article/RJwrapper.md -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/fig_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/auto_number_equations.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/conversion_compat_check.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
#> ! `package.dir` (/private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/sweave_article) does not contain a DESCRIPTION, hence not updated
#> [1] TRUE

# convert Rmd to HTML (comment this step to avoid failure on R CMD Check)
# rmarkdown::render(file.path(article_dir, "example.Rmd"))
# browseURL(file.path(article_dir, "example.html"))

# remove temporary files
unlink(article_dir, recursive = TRUE)
```
