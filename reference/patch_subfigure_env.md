# Patch subfigure environments in a LaTeX file

Converts \`subfigure\` environments contained in LaTeX figures into
\`tabular\` layouts using the Lua converter shipped with texor.

## Usage

``` r
patch_subfigure_env(
  file,
  output_file = file,
  ref_commands = "ref",
  backup = TRUE
)
```

## Arguments

- file:

  Path to the LaTeX file to modify.

- output_file:

  Path to the output LaTeX file. Defaults to \`file\`, so the input file
  is modified in place.

- ref_commands:

  Character vector of LaTeX reference commands to rewrite. Defaults to
  \`"ref"\`.

- backup:

  Logical; if \`TRUE\` and \`output_file\` is the same as \`file\`,
  create \`file.bk\` before modifying the file.

## Value

Invisibly returns the path to the converted LaTeX file.

## Examples

``` r
if (FALSE) { # \dontrun{
tex_file <- "article.tex"
patch_subfigure_env(tex_file)

# Or write to a separate file
patch_subfigure_env(
  tex_file,
  output_file = "article-converted.tex"
)

# Rewrite additional cross-reference commands
patch_subfigure_env(
  tex_file,
  ref_commands = c("ref", "autoref", "cref", "Cref")
)
} # }
```
