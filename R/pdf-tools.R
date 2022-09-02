#' @title convert one single pdf file to png
#' @description
#' function to invoke `pdftools:pdf_convert()`
#'
#' This function is designed to be used internally and is called
#' by `texor::pdf_to_png(file_dir)` function for converting individual of pdf
#' image.
#'
#' Note : The extensions in LaTeX source code will
#' automatically be changed during pandoc conversion
#' by a lua filter (refer : inst/extdata/image_filter.lua)
#'
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
    file_path <- normalizePath(file_path)
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
convert_all_pdf <- function(article_dir, fig_block) {
    article_dir <- normalizePath(article_dir)
    for (iterator in seq_along(fig_block)) {
        if (fig_block[[iterator]]$image_count == 1){
            if (fig_block[[iterator]]$extension == "pdf") {
                image_path <- paste0(article_dir,"/",fig_block[[iterator]]$path)
                convert_to_png(image_path)
                pdf_rel_path <- fig_block[[iterator]]$path
                if (! grepl(".pdf$",pdf_rel_path)) {
                    fig_block[[iterator]]$path <- paste0(pdf_rel_path,".png")
                } else {
                    fig_block[[iterator]]$path <- gsub(".pdf",".png",pdf_rel_path)
                }
                fig_block[[iterator]]$converted <- TRUE
                fig_block[[iterator]]$copied <- TRUE
                web_image_path <- paste0(article_dir,"/web/",fig_block[[iterator]]$path)
                tryCatch(file.copy(image_path, web_image_path),
                         error = function(c) {
                             c$message <- paste0(c$message, " (in ", article_dir , ")")
                             warning(c$message)
                             fig_block[[iterator]]$copied <- FALSE
                         }
                )

            } else {
                # -- pass
            }
        } else {
            for (iter_2 in 1:(fig_block[[iterator]]$image_count)) {
                if (fig_block[[iterator]]$extension[iter_2] == "pdf") {
                    image_path <- paste0(article_dir,"/",fig_block[[iterator]]$path[iter_2])
                    convert_to_png(image_path)
                    pdf_rel_path <- fig_block[[iterator]]$path[iter_2]
                    if (! grepl(".pdf$",pdf_rel_path)) {
                        fig_block[[iterator]]$path[iter_2] <- paste0(pdf_rel_path,".png")
                    } else {
                        fig_block[[iterator]]$path[iter_2] <- gsub(".pdf",".png",pdf_rel_path)
                    }
                    fig_block[[iterator]]$converted[iter_2] <- TRUE
                    fig_block[[iterator]]$copied[iter_2] <- TRUE
                    web_image_path <- paste0(article_dir,"/web/",fig_block[[iterator]]$path[iter_2])
                    tryCatch(file.copy(image_path, web_image_path),
                             error = function(c) {
                                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                                 warning(c$message)
                                 fig_block[[iterator]]$copied[iter_2] <- FALSE
                             }
                    )
                } else {
                    # -- pass
                }
            }
        }
    }
    return(fig_block)
}

pdf_to_png <- function(article_dir) {
    article_dir <- normalizePath(article_dir)
    input_files <- find_pdf_files(article_dir)
    input_file_paths <- lapply(input_files, function(file) {
        paste(article_dir, file, sep = "/")
    })
    make_png_files(input_file_paths)
}

find_pdf_files <- function(article_dir) {
    article_dir <- normalizePath(article_dir)
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

make_png_files <- function(input_file_paths) {
    input_file_paths <- normalizePath(input_file_paths)
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
