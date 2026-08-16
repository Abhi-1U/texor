# stream editor

R equivalent of GNU-sed

## Usage

``` r
stream_editor(raw_lines, pattern, target, replacement)
```

## Arguments

- raw_lines:

  a vector of readLines from the file

- pattern:

  a regex pattern to match

- target:

  target string to be replaced

- replacement:

  replacement string to be substituted

## Value

raw_lines : modified vector of lines

## Examples

``` r
example_string <- "\\target{} \\not_a_target{}"
texor::stream_editor(example_string,"\\s*\\\\target\\{\\}", "\\\\target", "\\\\hit")
#> Found  1 Matches for target : \\target
#> [1] "\\hit{} \\not_a_target{}"
```
