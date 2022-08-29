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

#' @title convert all pdf images to png
#' @description reads figure object to deduce conversion and convert the PDF images
#' into PNG
#' @param article_dir path to the article working directory
#' @param fig_block block of image data
#'
#' @return modified fig_block
#' @export
convert_all_pdf_png <- function(article_dir, fig_block) {
    for (iterator in seq_along(fig_block)) {
        if (fig_block[[iterator]]$image_count == 1){
            if (fig_block[[iterator]]$extension == "pdf") {
                image_path <- paste0(article_dir,"/",fig_block[[iterator]]$path)
                convert_to_png(image_path)
                fig_block[[iterator]]$pdf_to_png <- TRUE
            } else {
                # -- pass
            }
        } else {
            for (iter_2 in seq_along(fig_block[[iterator]]$image_count)) {
                if (fig_block[[iterator]]$extension[iter_2] == "pdf") {
                    image_path <- paste0(article_dir,"/",fig_block[[iterator]]$path[iter_2])
                    convert_to_png(image_path)
                    fig_block[[iterator]]$pdf_to_png <- TRUE
                } else {
                    # -- pass
                }
            }
        }
    }
    return(fig_block)
}
