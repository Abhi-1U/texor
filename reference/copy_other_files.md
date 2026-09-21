# Copy Supporting Documents like images,bib file,etc.

Copies supporting documents like images,pdf,bib files into the output
folder for building the HTML version of the R-Markdown file.

## Usage

``` r
copy_other_files(from_path)
```

## Arguments

- from_path:

  String indicating base path for the working directory

## Value

copies dependency files into the output folder.

## Examples

``` r
article_dir <- system.file("examples/article", package = "texor")
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
texor::copy_other_files(your_article_path)
#> /usr/local/bin/pandoc +RTS -K512m -RTS /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpJEW4U9/tempdir/article/RJwrapper.tex --to native --from latex --output /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpJEW4U9/tempdir/article/temp-native.txt -s --resource-path /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpJEW4U9/tempdir/article/RJwrapper.tex --lua-filter /Users/runner/work/_temp/Library/texor/image_list_filter.lua 
#> [1] TRUE
list.files(paste0(your_article_path,"/web/"))
#> [1] "Rlogo-5.png" "example.bib"
unlink(your_article_folder,recursive = TRUE)
```
