# convert one single pdf file to png

function to invoke \`pdftools:pdf_convert()\`

This function is designed to be used internally and is called by
\`texor::pdf_to_png(file_dir)\` function for converting individual of
pdf image.

Note : The extensions in LaTeX source code will automatically be changed
during pandoc conversion by a lua filter (refer :
inst/extdata/image_filter.lua)

## Usage

``` r
convert_to_png(file_path, dpi = 180)
```

## Arguments

- file_path:

  path to the pdf file

- dpi:

  Set DPI for converting PDF files. default: 180

## Value

png file of the same

## Note

If you find inconsistencies in the raster image generated from PDF using
this function. Please update poppler utils to newer versions (possibly
latest one).

## Examples

``` r
article_dir <- system.file("examples/pdf_conversion",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"pdf_conversion",sep="/")
rmarkdown::pandoc_version()
#> [1] ‘3.8.3’
texor::convert_to_png(paste0(your_article_path,"/normal.pdf"))
#> Converting page 1 to /private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpJEW4U9/tempdir/pdf_conversion/normal.png... done!
#> [1] "/private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpJEW4U9/tempdir/pdf_conversion/normal.png"
unlink(your_article_folder,recursive = TRUE)
```
