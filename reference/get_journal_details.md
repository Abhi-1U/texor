# get Journal details

get Journal details

## Usage

``` r
get_journal_details(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

journal details in an object

## Examples

``` r
article_dir <- "/home/user/documents/2022-1/2020-36/"
texor::get_journal_details(article_dir)
#> $volume
#> [1] 12
#> 
#> $issue
#> [1] 36
#> 
#> $slug
#> [1] "RJ-"
#> 
#> $sample
#> [1] FALSE
#> 
```
