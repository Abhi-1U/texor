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
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"pdf_conversion",sep="/")
#' rmarkdown::pandoc_version()
#' texor::convert_to_png(paste0(your_article_path,"/normal.pdf"))
#' unlink(your_article_folder,recursive = TRUE)
convert_to_png <- function(file_path){
    file_path <- xfun::normalize_path(file_path)
    if (! grepl(".pdf$",file_path)) {
        file_path <- paste0(file_path,".pdf")
    }
    png_file <- paste(toString(
        tools::file_path_sans_ext(file_path)), ".png",
        sep = "")
    pdftools::pdf_convert(file_path,
                          format = "png",
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
#' @noRd
convert_all_pdf <- function(article_dir, fig_block) {
    article_dir <- xfun::normalize_path(article_dir)
    for (iterator in seq_along(fig_block)) {
        if (fig_block[[iterator]]$image_count == 1){
            if (fig_block[[iterator]]$extension == "pdf") {
                image_path <- paste0(article_dir,"/",fig_block[[iterator]]$path)
                convert_to_png(image_path)
                pdf_rel_path <- fig_block[[iterator]]$path
                if (! grepl(".pdf$",pdf_rel_path)) {
                    fig_block[[iterator]]$path <- paste0(pdf_rel_path,".png")
                } else {
                    fig_block[[iterator]]$path <- xfun::with_ext(pdf_rel_path,"png")
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
                        fig_block[[iterator]]$path[iter_2] <- xfun::with_ext(pdf_rel_path,"png")
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

#' convert pdf files to png
#'
#' @param article_dir path to the article working directory
#'
#' @return makes png files in the directory
#' @noRd
pdf_to_png <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    input_files <- find_pdf_files(article_dir)
    input_file_paths <- lapply(input_files, function(file) {
        paste(article_dir, file, sep = "/")
    })
    make_png_files(input_file_paths)
}

#' find pdf files in article_dir
#'
#' @param article_dir path to the article working directory
#'
#' @return image paths of pdf files
#' @noRd
find_pdf_files <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # wrapper file name
    input_file <- get_wrapper_type(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- tools::file_path_as_absolute(input_file_path)
    # markdown equivalent filename
    temp_file <- "temp-native.txt"
    temp_file_path <- paste(article_dir, temp_file, sep = "/")

    pdf_files_list_filter <- system.file(
        "find_pdf_files.lua", package = "texor")
    pandoc_opt <- c("-s",
                    "--resource-path", abs_file_path,
                    "--lua-filter", pdf_files_list_filter)
    if (! pandoc_version_check()){
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17\n","Please Install a newer version of pandoc to run texor"))
        pdf_image_paths <- NULL
        return(pdf_image_paths)
    }
    else {
        #pass
    }
    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = "native",
                              options = pandoc_opt,
                              output = temp_file_path,
                              verbose = TRUE)
    if (file.exists(paste0(article_dir,"/pdf_image_source.txt"))){
        pdf_image_paths <- readLines(paste0(article_dir,"/pdf_image_source.txt"))
    } else {
        pdf_image_paths <- NULL
    }

    return(pdf_image_paths)
}

#' Make png file out of pdf file
#'
#' @param input_file_paths a list of pdf file paths to be converted
#'
#' @return converts pdf to png
#' @noRd
make_png_files <- function(input_file_paths) {
        if (length(input_file_paths)==0) {
        return()
    }
    input_file_paths[[1]] <- xfun::normalize_path(input_file_paths[[1]])
    if (length(input_file_paths) == 1) {
        if (basename(input_file_paths[[1]]) == "NA") {
            warning("No files to convert")
            return("")
        }
    }
    for (file_iter in seq_along(input_file_paths)) {
        png_file <- paste(toString(
            tools::file_path_sans_ext(input_file_paths[[file_iter]][1])), ".png",
            sep = "")
        pdftools::pdf_convert(input_file_paths[[file_iter]][1],
                              format = "png",
                              dpi = 600,
                              pages = 1,
                              filenames = png_file)
    }
    message("made PNG graphics @ 600 dpi density")
}
