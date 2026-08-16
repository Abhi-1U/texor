# patch all code environments

This function calls the \`filter_code_env()\` function over the
following code environments described in Rjournal.sty

1\. example 2. example\* 3. Sin 4. Sout 5. Sinput 6. Soutput 7.
smallverbatim

## Usage

``` r
patch_code_env(article_dir)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

## Value

patches code environments in LaTeX file and also backs up the old file
before modification , FALSE otherwise

## Examples

``` r
# Note This is a minimal example to execute this function
article_dir <- system.file("examples/article",
                 package = "texor")
dir.create(your_article_folder <- file.path(tempdir(), "tempdir2"))
x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
your_article_path <-  xfun::normalize_path(paste(your_article_folder,"article",sep="/"))
texor::patch_code_env(your_article_path)
#> Found  1 Matches for target : example
#> Found  1 Matches for target : example
#> Found 0 Matches for target :  example\*
#> Found 0 Matches for target :  example\*
#> Found 0 Matches for target :  Sin
#> Found 0 Matches for target :  Sin
#> Found 0 Matches for target :  Sout
#> Found 0 Matches for target :  Sout
#> Found 0 Matches for target :  Scode
#> Found 0 Matches for target :  Scode
#> Found 0 Matches for target :  Sinput
#> Found 0 Matches for target :  Sinput
#> Found 0 Matches for target :  Soutput
#> Found 0 Matches for target :  Soutput
#> Found 0 Matches for target :  smallverbatim
#> Found 0 Matches for target :  smallverbatim
#> Found 0 Matches for target :  boxedverbatim
#> Found 0 Matches for target :  boxedverbatim
#> Found 0 Matches for target :  smallexample
#> Found 0 Matches for target :  smallexample
unlink(your_article_folder,recursive = TRUE)
```
