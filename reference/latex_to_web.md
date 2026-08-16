# latex to web

automated function for converting a single RJarticle to web

## Usage

``` r
latex_to_web(
  dir,
  log_steps = TRUE,
  example = FALSE,
  auto_wrapper = TRUE,
  temp_mode = TRUE,
  web_dir = FALSE,
  interactive_mode = FALSE,
  autonumber_eq = FALSE,
  compile_rmd_in_temp = !temp_mode,
  kable_tab = FALSE,
  fig_in_r = FALSE
)
```

## Arguments

- dir:

  directory path

- log_steps:

  Enable/Disable Logging of conversion steps

- example:

  for examples only by default keep it FALSE.

- auto_wrapper:

  automatically creates a wrapper if TRUE, else asks user. default value
  TRUE

- temp_mode:

  temp mode will convert the document in a temporary folder and keep the
  original article untouched. default value = TRUE

- web_dir:

  option to create a new web directory, default FALSE

- interactive_mode:

  interactive mode for converting articles with options. default FALSE

- autonumber_eq:

  whether to autonumber the equations, default is FALSE

- compile_rmd_in_temp:

  This works only with a forked version of rjtools.

- kable_tab:

  converts to kable table instead of markdown tables, default is FALSE

- fig_in_r:

  whether to include figures in R code chunks, default is FALSE Not
  recommended to use with CRAN or github version of the rjtools package.
  (default FALSE)

## Value

RJweb article document in /web folder

## Note

Use pandoc version greater than or equal to 3.1

Do not set example = TRUE param when working with conversions.

example param is set TRUE in example, to conform with CRAN check
restrictions.

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::latex_to_web(your_article_path,log_steps = FALSE, example = TRUE, temp_mode =FALSE)
#> /var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T//Rtmp9bTkhd/tempdir/article
#> No Bib files found !
#> Cannot aggregate bibliography as there is no BibTeX bibliography
#> No Bib files found !
#> BibTeX file does not exist
#> will parse for bibliography
#> bibtex file created
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.tex --to native --from latex --output /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/temp-native.txt -s --resource-path /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.tex --lua-filter /Users/runner/work/_temp/Library/texor/find_pdf_files.lua 
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article/RJwrapper.md -s --resource-path /private/var/folders/df/djsxfhc17x95674wsm_g8s980000gn/T/Rtmp9bTkhd/tempdir/article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
#> [1] TRUE
unlink(your_article_folder, recursive = TRUE)
```
