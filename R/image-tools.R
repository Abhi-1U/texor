#' convert pdf images in the directory to png format
#' using pdftools and poppler utils.
#'
#'
#' Note : The extensions in LaTeX source code will
#' automatically be changed during pandoc conversion
#' by a lua filter (refer : inst/extdata/image_filter.lua)
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#'
#' @export All PDF files in the directory except RJwrapper.pdf
#'  will be converted to PNG, so avoid keeping any other pdf
#' files in the directory.
#'
#' @examples
#' file_dir <- system.file("examples/pdf_conversion/",
#'              package = "texor")
#' texor::pdf_to_png(file_dir)
pdf_to_png <- function(article_dir) {
    path = dirname(article_dir)
    old_working_directory = getwd()
    if (old_working_directory != path) {
        print("Working directory path is same")
    } else {
        setwd(path)
    }
    input_files = find_pdf_files(".")
    texor::make_png_files(input_files)
    setwd(old_working_directory)
}

#' find pdf files in a given directory
#'
#' This function will find all pdf images and also filter out pdf article
#'
#' @param article_dir path to the directory which contains pdf images
#'
#' @return list of filtered pdf files or "NA" string which signals no files.
#' @export
#'
#' @examples
#'file_dir <- system.file("examples/pdf_conversion/",
#'              package = "texor")
#' print(find_pdf_files(file_dir))
find_pdf_files <- function(article_dir) {
    print("Finding inclusive PDF files")
    file_list <- list.files(article_dir, recursive = FALSE)
    extensions <- c("*.pdf")
    pdf_files <- unique(
        grep(paste(extensions, collapse = "|"),
        file_list, value = TRUE))
    pdf_files_native <- c("RJwrapper.pdf",
                          "RJwrap.pdf",
                          "wrapper.pdf")
    filtered_pdf_files <- setdiff(pdf_files, pdf_files_native)
    if (identical(filtered_pdf_files, character(0))) {
        print("Image : No PDF graphic files found !")
        return("NA")
    } else {
        print(paste("Image : Found",
            length(filtered_pdf_files), "PDF graphic files"))
        return(filtered_pdf_files)
    }
}

#' Internal function to invoke `pdftools:pdf_convert()`
#'
#' This function is designed to be used internally and is called
#' by `texor::pdf_to_png(file_dir)` function for converting a
#' filtered list of pdf images.
#'
#' @param input_files list of file names to be converted to png
#'
#' @return
#' @export Converts listed pdf files to png format
#'
#' @examples
#'pdf_file <- system.file("examples/pdf_conversion/normal.pdf",
#'                  package = "texor")
#'texor::make_png_files(c(pdf_file))
#'

make_png_files <- function(input_files) {
    if (input_files == "NA") {
        print("No files to convert")
        return("")
    }
    for (file in input_files) {
        png_file <- paste(toString(
            tools::file_path_sans_ext(file)), ".png", sep = "")
        pdftools::pdf_convert(file,
                              dpi = 600,
                              pages = 1,
                              filenames = png_file)
    }
    print("made PNG graphics @ 600 dpi density")
}
