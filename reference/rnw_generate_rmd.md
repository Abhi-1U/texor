# Modify Markdown from Sweave to R-markdown

generate rmarkdown file in output folder

## Usage

``` r
rnw_generate_rmd(
  article_dir,
  web_dir = TRUE,
  interactive_mode = FALSE,
  output_format,
  autonumber_eq = FALSE,
  autonumber_sec = TRUE,
  algorithm_render = FALSE
)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- web_dir:

  option to create a new web directory, default TRUE

- interactive_mode:

  interactive mode for converting articles with options. default FALSE

- output_format:

  knit output type for the RMarkdown file, options for "bookdown",
  "biocstyle", "litedown"

- autonumber_eq:

  whether to autonumber the equations, default is FALSE

- autonumber_sec:

  whether to autonumber the sections, default is TRUE

- algorithm_render:

  Enable to include algorithms with pseudocode.js, default is FALSE
  optional is TRUE

## Value

R-markdown file in the web folder

## Note

Use pandoc version greater than or equal to 3.1

## Examples

``` r
# Note This is a minimal example to execute this function
# Please refer to texor::rnw_to_rmd for a detailed example
```
