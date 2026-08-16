# count latex environments

count common environments like table,figure,verbatim etc..

## Usage

``` r
count_env(article_dir, env_name)
```

## Arguments

- article_dir:

  path to the directory which contains RJ article

- env_name:

  name of the environment

## Value

count of the environment, FALSE otherwise

## Examples

``` r
article_dir <- system.file("examples/article",
                 package = "texor")
figures <- texor::count_env(article_dir, "figure")
print(paste("figure count : ", figures))
#> [1] "figure count :  1"
```
