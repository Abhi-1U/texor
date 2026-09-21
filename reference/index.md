# Package index

## Pre-Processing Articles

Functions using Regular Expressions and Stream Editor to pre-process
LaTeX files

- [`stream_editor()`](stream_editor.md) : stream editor
- [`patch_code_env()`](patch_code_env.md) : patch all code environments
- [`patch_equations()`](patch_equations.md) : patch equations
- [`patch_figure_env()`](patch_figure_env.md) : patch figure
  environments
- [`patch_table_env()`](patch_table_env.md) : patch table environment
- [`patch_subfigure_env()`](patch_subfigure_env.md) : Patch subfigure
  environments in a LaTeX file

## Conversion

Functions to convert articles from one form or the other

- [`convert_to_markdown()`](convert_to_markdown.md) : convert LaTeX
  wrapper to markdown
- [`convert_to_native()`](convert_to_native.md) : convert LaTeX wrapper
  to native pandoc AST
- [`generate_rmd()`](generate_rmd.md) : Modify Markdown to R-markdown
- [`produce_html()`](produce_html.md) : call rmarkdown::render to
  generate html file
- [`latex_to_web()`](latex_to_web.md) : latex to web
- [`rnw_generate_rmd()`](rnw_generate_rmd.md) : Modify Markdown from
  Sweave to R-markdown
- [`rnw_to_rmd()`](rnw_to_rmd.md) : Sweave to RMarkdown

## Graphics

Methods to work with Figures

- [`convert_to_png()`](convert_to_png.md) : convert one single pdf file
  to png
- [`handle_figures()`](handle_figures.md) : handle figures
- [`article_has_tikz()`](article_has_tikz.md) : Check if article has
  tikz images or not

## Logger

Methods to work with Logging using Logger package

- [`log_setup()`](log_setup.md) : texor log setup
- [`texor_log()`](texor_log.md) : log messages for various categories

## Utility

Utility and Stat tools for various tasks

- [`create_article()`](create_article.md) : Create an R Journal article
  with a modified template for texor.
- [`check_markdown_file()`](check_markdown_file.md) : check markdown
  file
- [`copy_other_files()`](copy_other_files.md) : Copy Supporting
  Documents like images,bib file,etc.
- [`count_env()`](count_env.md) : count latex environments
- [`count_inline()`](count_inline.md) : count inline elements
- [`get_journal_details()`](get_journal_details.md) : get Journal
  details
- [`get_md_file_name()`](get_md_file_name.md) : get markdown file name
- [`get_texfile_name()`](get_texfile_name.md) : Get the name of the tex
  file included within wrapper file
- [`get_wrapper_type()`](get_wrapper_type.md) : Get the name of the
  wrapper file in the article dir
- [`find_wrapper()`](find_wrapper.md) : find wrapper file
- [`include_style_file()`](include_style_file.md) : Include Style file
- [`pandoc_version_check()`](pandoc_version_check.md) : check texor
  pandoc compatibility
- [`pre_conversion_statistics()`](pre_conversion_statistics.md) : pre
  conversion statistics
