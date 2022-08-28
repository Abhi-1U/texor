#' @title convert one single pdf file to png
#' @description
#' function to invoke `pdftools:pdf_convert()`
#'
#' This function is designed to be used internally and is called
#' by `texor::pdf_to_png(file_dir)` function for converting individual of pdf
#' image.
#' @param file_path path to the pdf file
#'
#' @return png file of the same
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/pdf_conversion",
#'                  package = "texor")
#' texor::convert_to_png(paste0(article_dir,"/normal.pdf"))
convert_to_png <- function(file_path){
    if (! grepl(".pdf$",file_path)) {
        file_path <- paste0(file_path,".pdf")
    }
    png_file <- paste(toString(
        tools::file_path_sans_ext(file_path)), ".png",
        sep = "")
    pdftools::pdf_convert(file_path,
                          dpi = 600,
                          pages = 1,
                          filenames = png_file)
}
