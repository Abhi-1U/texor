# texor : tools for converting LaTeX source files into RJ-web-articles
![](https://img.shields.io/badge/R-%3E=3.6.3-blue?style=for-the-badge&logo=R&logoColor=lightblue)
![](https://img.shields.io/badge/Developemnt-active-green?style=for-the-badge&logo=)
![](https://img.shields.io/badge/R_CMD_check-failing-red?style=for-the-badge)
![](https://img.shields.io/github/issues/Abhi-1U/texor?color=orange&logo=github&logoColor=&style=for-the-badge)

texor is a package that deals with multiple challenges that occour in conversion of LaTeX source files which typically generate a PDF, to a web friendly RJ-web-article format. 

### Currently texor can handle simple tex files with support for:

1. RJournal based LaTeX files with macros such as `\pkg`,`\CRANpkg`,`\code`
2. Reading Metadata from LaTeX file as well as DESCRIPTION
3. Reading Bibliography from Bibtex files and ignoring `\thebibliography`
4. Supports Code Environments like `Sinput`,`Soutput`,	example	,`example*`,`verbatim`.
5. Supports graphics included as PDF,PNG,JPG.
6. Functions to Convert PDF graphics to PNG.
7. Simple Tables are also supported
8. Other things like citations,links,footnotes,math are also supported

### Work in Progress Features

1. Tikz graphics compilation support functions
2. Better Documentation as vignettes,man pages
3. Comprehensive table support for various types
4. Figure/table/equation Numbering
5. A parser to read inbuilt `\thebibliography`
