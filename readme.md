# texor : tools for converting LaTeX source files into RJ-web-articles
![](https://img.shields.io/badge/R-%3E=3.6.3-blue?style=for-the-badge&logo=R&logoColor=lightblue)
![](https://img.shields.io/badge/Developemnt-active-green?style=for-the-badge&logo=)
![](https://img.shields.io/badge/R_CMD_check-failing-red?style=for-the-badge)
![](https://img.shields.io/github/issues/Abhi-1U/texor?color=orange&logo=github&logoColor=&style=for-the-badge)

texor is a package that deals with multiple challenges that occour in conversion of LaTeX source files (which typically generate a PDF) to a web friendly RJ-web-article format. 

### Currently texor can handle R-Journal structured LaTeX files with support for:

1. RJournal based LaTeX files with macros such as `\pkg{}`,`\CRANpkg{}`,`\BIOpkg{}`, `\code{}`, `\acronym{}`
2. Reading Metadata from LaTeX file as well as DESCRIPTION.
3. Reading bibliography from bibtex files and ignoring `\thebibliography`
4. Supports code environments like `Sinput`,`Soutput`, `example` ,`example*`,`verbatim` with code highlight.
5. Supports graphics included as PDF,PNG,JPG.
6. Functions to convert PDF graphics to PNG.
7. Almost all tables are supported with a few exceptions.
8. Other things like citations,links,footnotes,math are also supported.
9. A parser to read and minimally convert inbuilt `\thebibliography` to bibtex
10. Figure/Table numbering

### Work in Progress Features

1. Tikz graphics compilation support functions
2. Better Documentation as vignettes, man pages, articles, web-resource
3. Bibliography aggregation.
4. New GNU-sed like find and replace function in R to manage alien commands/environments.
5. Automated orchestration function to make the process one seamless process.
6. Logging activities and generating analytics.
![texor features](man/figures/texor.svg)
