# check texor pandoc compatibility

texor package requires minimum pandoc version above or equal to 3.1,
hence this utility will check for the installation and version status.

## Usage

``` r
pandoc_version_check()
```

## Value

TRUE if v \>= 3.1, else FALSE

## Examples

``` r
rmarkdown::pandoc_version()
#> [1] ‘3.8.3’

texor::pandoc_version_check()
#> [1] TRUE
```
