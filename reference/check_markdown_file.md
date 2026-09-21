# check markdown file

Checks if the markdown file generated is empty or not due to some pandoc
related error during conversion to markdown.

## Usage

``` r
check_markdown_file(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

FALSE if markdown file is corrupted/empty else TRUE

## Examples

``` r
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
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.md -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/fig_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/conversion_compat_check.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
texor::check_markdown_file(your_article_path)
#> [1] TRUE
unlink(your_article_folder, recursive = TRUE)
```
