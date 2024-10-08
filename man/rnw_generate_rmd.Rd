% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/article-tools.R
\name{rnw_generate_rmd}
\alias{rnw_generate_rmd}
\title{Modify Markdown from Sweave to R-markdown}
\usage{
rnw_generate_rmd(
  article_dir,
  web_dir = TRUE,
  interactive_mode = FALSE,
  output_format,
  autonumber_eq = FALSE,
  autonumber_sec = TRUE,
  algorithm_render = FALSE
)
}
\arguments{
\item{article_dir}{path to the directory which contains tex article}

\item{web_dir}{option to create a new web directory, default TRUE}

\item{interactive_mode}{interactive mode for converting articles with options. default FALSE}

\item{output_format}{knit output type for the RMarkdown file, options for "bookdown", "biocstyle", "litedown"}

\item{autonumber_eq}{whether to autonumber the equations, default is FALSE}

\item{autonumber_sec}{whether to autonumber the sections, default is TRUE}

\item{algorithm_render}{Enable to include algorithms with pseudocode.js, default is FALSE optional is TRUE}
}
\value{
R-markdown file in the web folder
}
\description{
generate rmarkdown file in output folder
}
\note{
Use pandoc version greater than or equal to 3.1
}
\examples{
# Note This is a minimal example to execute this function
# Please refer to texor::rnw_to_rmd for a detailed example
}
