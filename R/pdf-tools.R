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
#' @return All PDF files in the directory except RJwrapper.pdf
#'  will be converted to PNG, so avoid keeping any other pdf
#' files in the directory.
#'
#' @export
#'
#' @examples
#' file_dir <- system.file("examples/pdf_conversion/",
#'              package = "texor")
#' texor::pdf_to_png(file_dir)
pdf_to_png <- function(article_dir) {
    input_files <- find_pdf_files(article_dir)
    input_file_paths <- lapply(input_files, function(file) {
        paste(article_dir, file, sep = "/")
    })
    make_png_files(input_file_paths)
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
#' @param input_file_paths list of file paths to be converted to png
#'
#' @return Converts listed pdf files to png format
#' @export
#'
#' @examples
#'pdf_file <- system.file("examples/pdf_conversion/normal.pdf",
#'                  package = "texor")
#'texor::make_png_files(c(pdf_file))
#'

make_png_files <- function(input_file_paths) {
    if (length(input_file_paths) == 1) {
        if (basename(input_file_paths[[1]]) == "NA") {
            print("No files to convert")
            return("")
        }
    }
    for (file in seq_along(input_file_paths)) {
        png_file <- paste(toString(
            tools::file_path_sans_ext(input_file_paths[[file]][1])), ".png",
            sep = "")
        pdftools::pdf_convert(input_file_paths[[file]][1],
                              dpi = 600,
                              pages = 1,
                              filenames = png_file)
    }
    print("made PNG graphics @ 600 dpi density")
}

#' @title patch figure environments
#' @description This function calls the stream editor to change
#' figure* to figure
#' 1. figure*
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return writes modified file and also backs up the old file before modification
#' @export
patch_figure_env <- function(article_dir) {
    # find tex file
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # read Lines
    raw_lines <- readLines(file_path)
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\begin\\{figure\\*\\}", "figure\\*", "figure")
    print("Changed \\begin{figure\\*} to \\begin{figure}")
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\end\\{figure\\*\\}", "figure\\*", "figure")
    print("Changed \\end{figure\\*} to \\end{figure}")
    # testing functionality
    #return(raw_lines)
    # backup old file
    src_file_data <- readLines(file_path)
    backup_file <- paste(file_path, ".bk", sep = "")
    write_external_file(backup_file, "w", src_file_data)
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
