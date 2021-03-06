# Version 0.5.5 
Update Highlights :

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
