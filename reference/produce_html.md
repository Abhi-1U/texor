# call rmarkdown::render to generate html file

call rmarkdown::render to generate html file

## Usage

``` r
produce_html(
  article_dir,
  example = FALSE,
  web_dir = TRUE,
  interactive_mode = FALSE
)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- example:

  only enabled for running examples for documentation and to enable
  export of this function.

- web_dir:

  option to create a new web directory, default TRUE

- interactive_mode:

  interactive mode for converting articles with options. default FALSE

## Value

Renders a RJwrapper.html file in the /web folder, in example it will
return TRUE

## Note

Use pandoc version greater than or equal to 3.1

Do not set example = TRUE param when working with conversions.

example param is set TRUE in example, to conform with CRAN check
restrictions.

## Examples

``` r
# Note This is a minimal example to execute this function
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::include_style_file(your_article_path)
#> [1] TRUE
rmarkdown::pandoc_version()
#> [1] ‘3.8.3’
texor::convert_to_markdown(your_article_path)
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.tex --to markdown-simple_tables-pipe_tables-fenced_code_attributes --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.md -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article --lua-filter /Users/runner/work/_temp/Library/texor/issue_checker.lua --lua-filter /Users/runner/work/_temp/Library/texor/abs_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/bib_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/equation_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_filter.lua --lua-filter /Users/runner/work/_temp/Library/texor/sec_depth.lua --lua-filter /Users/runner/work/_temp/Library/texor/image_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/fig_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/widetable_patcher.lua --lua-filter /Users/runner/work/_temp/Library/texor/R_code.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_caption.lua --lua-filter /Users/runner/work/_temp/Library/texor/table_code_chunk.lua --lua-filter /Users/runner/work/_temp/Library/texor/conversion_compat_check.lua --lua-filter /Users/runner/work/_temp/Library/texor/bookdown_ref.lua --citeproc 
#> Detected the following packages from article:
#>   CRAN: 
#>   Bioconductor: 
texor::generate_rmd(your_article_path)
texor::copy_other_files(your_article_path)
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.tex --to native --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/temp-native.txt -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpBY0FOp/tempdir/article/RJwrapper.tex --lua-filter /Users/runner/work/_temp/Library/texor/image_list_filter.lua 
#> logical(0)
texor::produce_html(your_article_path,example = TRUE)
#> [1] TRUE
unlink(your_article_folder,recursive = TRUE)
```
