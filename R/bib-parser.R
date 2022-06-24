# Abhi-1U 
# Latex bibliography to bibtex



bib_item<-function(...,quiet=FALSE){
    tryCatch(make_bib_item(...),
        error=function(e){
        bib_item<-unparsed(...)
            if(!quiet){
                message("Failed to parse :")
                print(bib_item)
                message(e,"\n")
            }
            bib_item
        }
    )
}
sample<- list("\\bibitem[Venkatraman and Begg(1996)]{venkatraman1996distribution}",
                "E.~Venkatraman and C.~B. Begg.",
                "\\newblock A distribution-free procedure for comparing receiver operating",
                "characteristic curves from a paired experiment.",
                "\\newblock \\emph{Biometrika}, 83\\penalty0 (4):\\penalty0 835--848, 1996.",
                "\\newblock \\doi{10.1093/biomet/83.4.835}.")

make_bib <- function(unique_shorthand, author = "",title = "",journal = "",
                         volume = "", number = "", pages = "",year = "",doi = "" ) {
    structure(list(
        type="book",# It will try to recognize the type of bib_entry
        shorthand=unique_shorthand,# shorthand or cite reference text
        author = author,# author name
        journal = journal,# publisher details 
        title = title,# title of the article/book/manual
        year = parse_year(year),# publishing year
        volume = volume, # volume detail
        number = number, # volume number detail
        pages = pages,# pages referred
        doi = doi# doi from \doi
    ), class = "bib_item")
}

unparsed <- function(...) {
    structure(list(...), class = c("unparsed", "bib_item"))
}
parse_year<-function(year){
    if(is.year(year)){

    }
}
is.year <- function(x) {
  parsed <- strptime(x, "%Y")
  !is.na(parsed) && format(parsed) == x
}