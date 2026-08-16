# handle figures

handle figures

## Usage

``` r
handle_figures(article_dir, file_name)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- file_name:

  name of the LaTeX file

## Value

A block of figure data for better conversion.

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::handle_figures(your_article_path,texor::get_texfile_name(your_article_path))
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
#> [[1]]
#> [[1]]$image_number
#> [1] 1
#> 
#> [[1]]$doc_start_line
#> [1] 12
#> 
#> [[1]]$doc_end_line
#> [1] 17
#> 
#> [[1]]$data
#> [1] "\\begin{figure}[htbp]"        "  \\centering"               
#> [3] "  \\includegraphics{Rlogo-5}" "  \\caption{The logo of R.}" 
#> [5] "  \\label{figure:rlogo}"      "\\end{figure}"               
#> 
#> [[1]]$istikz
#> [1] FALSE
#> 
#> [[1]]$isalgorithm
#> [1] FALSE
#> 
#> [[1]]$image_count
#> [1] 1
#> 
#> [[1]]$image_pos
#> [1] 3
#> 
#> [[1]]$caption
#> [1] "{The logo of R.}"
#> 
#> [[1]]$label
#> [1] "figure:rlogo"
#> 
#> [[1]]$path
#> [1] "Rlogo-5.png"
#> 
#> [[1]]$extension
#> [1] "png"
#> 
#> 
unlink(your_article_folder,recursive = TRUE)
```
