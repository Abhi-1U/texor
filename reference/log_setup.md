# texor log setup

a wrapper function for logger package to set up log file for logging

## Usage

``` r
log_setup(article_dir, file_name, namespace, idx)
```

## Arguments

- article_dir:

  path to the directory which contains tex article

- file_name:

  name of the log file

- namespace:

  namespace of log file

- idx:

  index of log level

## Value

NULL but also creates a log file in the article_dir

## Examples

``` r
dir.create(your_article_folder <- file.path(tempdir(), "exampledir"))
example_files <-  system.file("examples/article", package = "texor")
x <- file.copy(from = example_files,to=your_article_folder,recursive = TRUE)
your_article_path <- paste(your_article_folder,"article",sep="/")
 texor::log_setup(your_article_path, "log-file.log", "texor", 2)
unlink(your_article_folder,recursive = TRUE)
```
