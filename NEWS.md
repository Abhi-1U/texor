# Version 1.0.2

## Bug Fixes

1. Added a pre-check before reading a file in `texor::check_markdown_file()`. 
2. Fixed a bug where empty lines could not be filtered out.
3. Usage of `file.exists()` before actual `readLines()`.

# Version 1.0.1

## Feature Updates

1. Algorithm figures are now numbered.
2. Added `boxedverbatim` support for code environments.
3. Included `web_only` parameter for embedding PDF or using Rmarkdown to produce HTML.
4. Updated to MathJax Version3 and added `-tex_math_single_backslash` extension to pandoc for better math handling.
5. Support for sample articles without the directory structure as required for RJ articles.
6. Updated Instructions and examples (hence closing GitHub Issue #23).
7. Updated contributor information.
8. More examples and test cases.
9. Pandoc Version checker for better compatibility and experience.
10. Markdown checker for better compatibility and experience.
11. CRAN release.

## Bug Fixes

1. Using `xfun::normalize_path()` instead of `normalizePath()` for relative/absolute path handling 
2. Stream editor also works on algorithm environments.
3. Fixed image sizing paramaters.
4. New algorithm to find, convert and copy images appropriately.
5. Fixed a bug where, if the `metadata$address` is missing then `metadata$author` will be used as author name.
6. Removes `RJournal.sty` file as it conflicts with inner workings of pandoc.
7. Fixed file extension editing using `xfun::with_ext()` instead of `gsub()`.
8. More precise `title` and `description`.
9. Added rebib as a dependency given it is now available on cran(hence closing #38)
10. Fixed bugs related to copying files and images (Github Issue #36 and #34)



# Version 0.6.9

## Feature Updates

1. Added algorithm2e graphics support (beta) (GitHub Issue #32)
2. Added tikz graphics support (GitHub Issue #7)
3. Added automation workflow with a separate logger for logging success and failure of conversion.

## Bug Fixes

1. Fixing equation labeling issue with ` `, `.`, `_` in equations and also added
`eq:` before equation starts if not present.
2. Added support for relative paths
3. Fixed bugs in figure parser with new regex.
4. Fixed tikz lib extraction method

## Deprecated Features

1. Removing old bibliographic parser/converter from texor in favour of new 
implementation in [rebib](https://abhi-1u.github.io/rebib/) which supports 
many more features.

# Version 0.6.7

## Feature Updates

1. Equation labeling and numbering
2. Code/table/equation/figure pre-processors
3. fixed many meta data related issues
4. We now use Mathjax V3
5. New testthat test cases
6. pkgdown website
7. `texor::latex_to_web()` for a single article
8. new stat tools for some metrics and conversion coverage check
9. environment/word count for pandoc convert

## Bug Fixes

1. Support for absolute paths
2. Fixed a lot of logic errors
3. Fixed incompatibilities
4. removed unnecessary code and features
5. R-CMD-check passing
6. Github workflows should work now
7. Improved stat filter which exports pre and post conversion stats yaml file
8. namespaces in logger
9. URL fix for non http:// web Links
10. Figure max width : 100% extra option added to figures.
11. fixed pre processing of tikz
12. fixed problems in journal volume
13. comment filter mechanism for latex
14. improved lua filter for numbering and centering figures
15. fixed slug issue

# Version 0.5.9

## Feature Updates

1. Automated orchestration of conversion
2. New stream editor in R 
3. Using stream editor instead of lua filter for code
4. Using stream editor instead of GNU sed for table
5. changes in lua filter for R code
6. Using stream editor for figure environment patching
7. Automatic fetching of volume and issue from folder

## Bug Fixes

1. Support for absolute paths fixed(in almost all functions)
2. Fixed some logic errors
3. Fixes in metafix.sty

# Version 0.5.5 

## Update Highlights :

1. Placeholder abstract text for articles missing abstract
2. `\CRANpkg()`,`\BIOpkg()` and `CRAN_TASK_VIEWS` are supported
3. Tables have numbering
4. A new lua filter for numbering tables
5. Patched Metafix.sty
6. More vignettes

# Version 0.5.2 

This package matures at this point where almost all features are working, and soon will be ready for CRAN deployment.
The following are the features working:

1. LaTeX macros such as `\pkg`,`\CRANpkg`,`\code` are supported.
2. Reading Metadata from LaTeX file as well as DESCRIPTION.
3. Reading bibliography from bibtex files and ignoring `\thebibliography`
4. A parser to read and convert inbuilt `\thebibliography` to bibtex
5. Supports code environments like `Sinput`,`Soutput`, `example` ,`example*`,`verbatim` with code highlight.
6. Supports graphics included as PDF,PNG,JPG.
7. Functions to convert PDF graphics to PNG.
8. Almost all tables are converted with a few exceptions.
9. Other things like citations,links,footnotes,math(limited) are also supported.
