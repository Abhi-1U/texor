# count inline elements

counts inline elements embedded within the latex file currently
supported inlines : math (based on \$\$), code (based on \code) and
Citations (based on \cite,\citealp, \citep, \citet)

## Usage

``` r
count_inline(article_dir, inline)
```

## Arguments

- article_dir:

  path to the directory which contains RJ article

- inline:

  name of the inline element

## Value

count of the inline element, FALSE otherwise

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
math <- texor::count_inline(article_dir, "math")
code <- texor::count_inline(article_dir, "inlinecode")
cite <- texor::count_inline(article_dir, "cite")
print(paste("math inlines : ", math, "\n",
            "code inlines : ", code, "\n",
            "citations    : ", cite))
#> [1] "math inlines :  2 \n code inlines :  0 \n citations    :  2"
```
