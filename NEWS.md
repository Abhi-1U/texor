# Version 1.5.6

## Bug Fixes

1. Table/Figure/Algorithm Numbering Issues which were broken by updates in pandoc and texor are fixed.
2. Fixed table references being shown as `??` instead of number or reference name.
3. Added soft fuse for the `fig_code_chunk` to skip converting further images, on encountering a non standard Image.
4. Updated workflows and badges.

# Version 1.5.3

## Bug Fixes

1. Fixed the path to search for DESCRIPTION file in a package directory.
2. Moved clean up function to `on.exit()`, instead of sequential execution.
3. Fixed issue #99 where a Lua filter would not work with newer pandoc version.

# Version 1.5.0

## Feature Updates

1. Included CodeCov code coverage workflow closing issue #70.
2. Included custom JavaScript for auto-numbering equations independent of `number_sections:` option, fixing issue #83 and issue #86.
3. Optional automatic equation numbering with a custom pandoc Lua filter, fixing issue #68. 
4. New option to convert figures into R markdown code chunks instead of regular markdown syntax, somewhat fixing issue #80.
5. New option to convert most table structures into a csv data file, which is included into the article using `kable()` in a code block.
6. New option to suppress package start up messages in the generated document, fixing issue #81.
7. New options for vignette output styles including `bookdown`,`litedown` and `biocstyle`, closing issue #76.
8. Use of `pseudocodejs` for rendering Algorithm environment in the R markdown files, closing issue #69.
9. `texor::rnw_to_rmd()` now also modifies the DESCRIPTION file (if available) with Dependency `Suggests` and `VignetteBuilder` if missing (#94)

## Bug Fixes

1. Fixed Issue #78 where references are not hyperlinked.
2. Fixed Issue #67 where unlabeled tables were not numbered.
3. Updated example Sweave file.
4. Added support for some jss style LaTeX macros under issue #77.
5. Partially fixed Issue #84 renaming bibliography file more appropriately.
6. Fixed Issue #79 where `\vspace*{}`,`\hspace*{}` are ignored.
7. Removed `web_only` parameter from yaml front matter, adapting to changes in `rjtools` package.
8. Using updated [`table_code_chunk`](https://github.com/Abhi-1U/table-code-chunk) pandoc Lua filter, which preserves the table data more effectively, see [supported types](https://github.com/Abhi-1U/table-code-chunk/issues/1) for more information. 
9. `texor::rnw_to_rmd()`: `front_matter_type` parameter is now called `output_format` with no defaults (#91).
10. To support offline math in bookdown format, `math_method` option is now set to `katex` (better for accessibility) by default, in case of equation numbering it is set to `mathjax` (#92,#90).
11. Fixed Relative Paths Issue in `texor::rnw_to_rmd()` (#93).
12. VignetteEngine in bookdown format is now `knitr::rmarkdown` (#94).
13. Removed excess `print()` statements from Lua Filters.
14. If the figure/table referenced to a sweave code chunk is labelled with the prefix (`fig:`,`tab:`,`table:`) then the references will be transformed to bookdown style label.

# Version 1.4.0

## Feature Updates

1. Initial Support for Sweave (.Rnw) article conversion.
2. A new Hex-sticker for the package.
3. Simplification of Readme file.

## Bug Fixes

1. Fixed Rd files with LaTeX like syntax to conform with recent R changes.
2. Removed redundant comments regarding pandoc version checks from examples.
3. Updates warning messages and notes in Rd section.


# Version 1.3.0

## Feature Updates

1. Equations are better handled and equation references are edited to match bookdown format.
2. Knitting of Rmarkdown is now done outside the `temp` folder to avoid PDF compilation errors.
3. Added workaround macros for `\nameref{}` and `\autoref{}`.
4. Added workaround macros for `\scalebox{}`, `\fbox{}`, `\adjustbox{}`, `\framebox{}`.
5. Option to set DPI when invoking `convert_to_png` function.

## Bug Fixes

1. `CodeBlocks` and `WideTables` are now back to the original form.
2. `Algorithm` get separate numbering in references.
3. Removed unnecessary warnings.
4. Better mechanism to remove leftover text after processing tikz.
5. Package names using `\pkg{}` are now bold.
6. Reduced DPI of raster images to reduce file sizes.
7. `Algorithmic` not treated as figures.
8. Fixes in lua filters for equation which now adds basic support for tabular and removes nested inline math.
9. Fixing logic in table reference numbering.

## Accessibility Feature Updates

1. All images processed are set with a default alt-text : 'graphic without alt text' 

# Version 1.2.0

## Feature Updates

1. `CodeBlock` and `WideTable` get their own numbering (but they share references with figures), Enhancement #51.
2. added support for `\command{..}` macro.
3. Included dummy `slug` and `citation_url`, Issue #50.
4. Included a constructor for sample article, Enhancement #52.
5. Added support for `\file{}`,`\var{}`,`\env{}`,`\option{}`,`\dfn{}`, `\cpkg{}` and `\samp{}`  macro.
6. Asking user for Inputs in `interactive_mode`, Enhancement #53.

## Bug Fixes

1. Modified the workflow to avoid missing Images.
2. Modified Lua filters to suppress numbering widetables in captions.
3. Fixed Issue #49 where some text of algorithms were left behind.
4. Updated pdf conversion functions with poppler version check and note in man pages.
5. Fixed copying markdown files back in temp_mode.
6. Set `unlink()` as `on.exit()` function in temp_mode
7. Removed table Numbering for images gridded in a table environment within a Figure environment.

# Version 1.1.0

## Feature Updates

1. New mechanism to convert articles without wrapper files.
2. Support for non-standard named RJwrapper files.
3. Support for files with inclusive articles, however only articles included in the main article are included.
4. Support for `smallexample` code environment
5. Enhancement #47 : temporary mode for conversions to keep the original LaTeX article untouched.
6. Bumped up pandoc version requirement from 2.17 to 3.1 (minimum), latest (recommended)

## Bug Fixes

1. Figure Environment wont be numbered if there is no Image in it.
2. Now tikz images can be hot reloaded, no need to remove `\includegraphics{}` line manually.
3. Fixed #46 where tikz style blocks would persist.
4. Fixed #45 with Enhancement #47.
5. Closed #41 momentarily.
6. Default issue and year is now based on current date.
7. Fixed #48 - in Lua filter added check for `DisplayMath` for equation numbering.
8. Closed #40  as most of the issues have been fixed.

# Version 1.0.5

## Feature Updates

1. A new Lua Filter to check for possible issues in LaTeX file during conversion
2. New function to check multiple labels used in an environment

## Bug Fixes

1. Updated Image caption Lua filter which was broken by pandoc v3 (Figure update)
2. Updated pkgdown website theme
3. Fixed a Lua Filter to add extensions to some LaTeX Figure elements

# Version 1.0.3

## Bug Fixes

1. Fixed a test case where it was attempting to write in user-library. 

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
