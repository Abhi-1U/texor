# log messages for various categories

a wrapper function for logging different types of log entries

## Usage

``` r
texor_log(message, category, idx)
```

## Arguments

- message:

  message to be sent

- category:

  category of the log message

- idx:

  index of log level

## Value

NUll, but also appends message to the log file in article_dir

## Examples

``` r
dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
example_files <- system.file("examples/article", package = "texor")
x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
your_article_path <- paste(your_article_folder,"article",sep="/")
texor::log_setup(your_article_path, "log-file.log", "texor" , 2)
texor::texor_log("Hello", "INFO", 2)
#> INFO [2026-09-21 14:23:06] Hello
#> NULL
cat(readLines(paste(your_article_path,"/log-file.log",sep="")),sep="\n")
#> INFO [2026-09-21 14:23:06] Hello
unlink(your_article_folder,recursive = TRUE)
```
