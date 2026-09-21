# Modify Markdown to R-markdown

generate rmarkdown file in output folder

## Usage

``` r
generate_rmd(article_dir, web_dir = TRUE, interactive_mode = FALSE)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- web_dir:

  option to create a new web directory, default TRUE

- interactive_mode:

  interactive mode for converting articles with options. default FALSE

## Value

R-markdown file in the web folder

## Note

Use pandoc version greater than or equal to 3.1

## Examples

``` r
# Note This is a minimal example to execute this function
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir2"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <-  xfun::normalize_path(paste(your_article_folder,"article",sep="/"))
texor::include_style_file(your_article_path)
#> [1] TRUE
rebib::aggregate_bibliography(your_article_path)
#> No Bib files found !
#> Cannot aggregate bibliography as there is no BibTeX bibliography
#> No Bib files found !
#> BibTeX file does not exist
#> will parse for bibliography
#> bibtex file created
data <- texor::handle_figures(your_article_path,
                    texor::get_texfile_name(your_article_path))
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article/RJwrapper.tex --to native --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article/temp-native.txt -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article/RJwrapper.tex --lua-filter /Users/runner/work/_temp/Library/texor/find_pdf_files.lua 
texor::patch_code_env(your_article_path) # Step 4
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
texor::patch_table_env(your_article_path) # Step 5
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
texor::patch_equations(your_article_path) # Step 5.5
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
rmarkdown::pandoc_version()
#> [1] ‘3.8.3’
texor::convert_to_markdown(your_article_path)
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article/RJwrapper.md -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir2/article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/fig_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/conversion_compat_check.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
texor::generate_rmd(your_article_path)
unlink(your_article_folder,recursive = TRUE)
```
