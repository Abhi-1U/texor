#' @title extract caption
#'
#' @description this function will extract \\caption{} from LaTeX figure
#' environment.
#' @param figure_lines the block of raw figure data from LaTeX document
#'
#' @return caption element
#' @noRd
extract_caption <- function(figure_lines) {
    unified_line <- ""
    for (line in figure_lines) {
        unified_line <- paste(unified_line, line)
    }
    caption_start_patt <- "\\s*\\\\caption\\{\\s*(.*?)\\s*\\}"
    caption_line <- stringr::str_extract(unified_line,caption_start_patt)
    if (is.na(caption_line)) {
        return(" ")
    }
    caption_line <- trimws(caption_line, which = "both")
    caption_with_brackets <- stringr::str_extract(caption_line,"\\{\\w+(.+?\\})")
    return(caption_with_brackets)
}

#' @title extract label
#'
#' @description this function will extract \\label{} from LaTeX figure
#' environment.
#' @param figure_lines the block of raw figure data from LaTeX document
#'
#' @return label element
#' @noRd
extract_label <- function(figure_lines) {
    unified_line <- ""
    for (line in figure_lines) {
        unified_line <- paste(unified_line, line)
    }
    label_start_patt <- "\\s*\\\\label\\{\\s*(.*?)\\s*\\}"
    label_line <- stringr::str_extract(unified_line,label_start_patt)
    if (is.na(label_line)) {
        return(" ")
    }
    label_line <- trimws(label_line, which = "both")
    label_with_brackets <- stringr::str_extract(label_line,"\\{\\w+(.+?\\})")
    label <- gsub("^\\{","", label_with_brackets)
    label <- gsub("\\}$","", label)
    return(label)
}

#' @title extract path
#'
#' @description this function will extract  from LaTeX figure
#' environment.
#' @param figure_line the block of raw figure data from LaTeX document
#'
#' @return path element
#' @noRd
extract_path <- function(figure_line) {
    src_start_patt <- "\\s*\\\\includegraphics"
    src_line <- figure_line
    src_line <- trimws(src_line, which = "both")
    src_with_brackets <- stringr::str_extract(src_line, "\\{\\w+(.+?\\})")
    if (identical(src_with_brackets,character(0))) {
        return(" ")
    }
    if (is.na(src_with_brackets)) {
        return(" ")
    }
    src <- gsub("^\\{","", src_with_brackets)
    src <- gsub("\\}$","", src)
    return(src)
}
#' @title find image extension
#'
#' @description this function will extract extension from LaTeX figure
#' files
#' @param article_dir the root directory where the article is located
#' @param image_path the value from extract_path()
#' @param is_tikz tikz image status, default is FALSE
#' @return extension element
#' @noRd
find_image_extension <- function(article_dir, image_path, is_tikz = FALSE) {
    # tikz/algorithm images are isolated
    # then compiled into a standalone PDF,
    # and converted back to PNG for inclusion in web articles
    article_dir <- xfun::normalize_path(article_dir)
    if (is_tikz) {
        return("png")
    } else {
        # png
        if (grepl("\\.png",image_path)) {
            abs_img_path <- paste(article_dir, image_path, sep = "/")
            if (file.exists(abs_img_path)) {
                return("png")
            }
        }
        # jpg
        if (grepl("\\.jpg",image_path)) {
            abs_img_path <- paste(article_dir, image_path, sep = "/")
            if (file.exists(abs_img_path)) {
                return("jpg")
            }
        }
        # jpeg
        if (grepl("\\.jpeg", image_path)) {
            abs_img_path <- paste(article_dir, image_path, sep = "/")
            if (file.exists(abs_img_path)) {
                return("jpeg")
            }
        }
        # pdf
        if (grepl("\\.pdf", image_path)) {
            abs_img_path <- paste(article_dir, image_path, sep = "/")
            if (file.exists(abs_img_path)) {
                return("pdf")
            }
        } else {
            ext <- find_file(article_dir, image_path)
            if (ext != "") {
                return(ext)
            } else {
                warning("File does not exist in the directory or wrong directory path")
            }
        }
    }
}

#' find a file in the directory
#'
#' @param article_dir path to the directory which contains tex article
#' @param src_path path of the file relative to article_dir
#'
#' @return image extension
#' @noRd
find_file <- function(article_dir, src_path) {
    article_dir <- xfun::normalize_path(article_dir)
    abs_img_path <- paste(article_dir,src_path,sep="/")
    main_dir <- dirname(abs_img_path)
    file_name <- basename(abs_img_path)
    all_files <- list.files(main_dir)
    target_extension <- ""
    for (file in all_files) {
        if (tools::file_path_sans_ext(file) == file_name) {
            ext <- tools::file_ext(file)
            if(ext == "pdf" | ext == "png" | ext == "jpg" | ext == "jpeg" ) {
                target_extension <- tools::file_ext(file)
            } else {
                #skip
            }
        }
    }
    return(target_extension)
}

#' Figure count
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return count of images
#' @noRd
figure_count <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    tex_file <- get_texfile_name(article_dir)
    figure <- 0
    fig_image_count <- list()
    fig_image_count$figure <- image_count(article_dir,tex_file)
    if (article_has_tikz(article_dir)) {
        check_tikz <- TRUE
        fig_image_count$tikz <- tikz_count(article_dir, tex_file)
    } else {
        check_tikz <- FALSE
    }
    fig_image_count$env <- count_env(article_dir, "figure")
    return(fig_image_count)
}

#' image count in an environment
#'
#' @param fig_lines the enclosed figure lines from LaTeX file
#'
#' @return Number of images in an environment block
#' @noRd
env_image_count <- function(fig_lines) {
    graphics_patt <- "\\s*\\\\includegraphics"
    begin_break_points <- which(grepl(graphics_patt, fig_lines))
    return(length(begin_break_points))
}
#' Relative image position in environment
#'
#' @param fig_lines the enclosed figure lines from LaTeX file
#'
#' @return the relative position(line no) of target image
#' @noRd
env_image_position <- function(fig_lines) {
    graphics_patt <- "\\s*\\\\includegraphics"
    begin_break_points <- which(grepl(graphics_patt, fig_lines))
    return(begin_break_points)
}

#' count for all the images in a LaTeX file
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the LaTeX file
#'
#' @return number of images in the LaTeX file, FALSE otherwise
#' @noRd
image_count <- function(article_dir, file_name) {
    article_dir <- xfun::normalize_path(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    if (file.exists(file_path)){
        raw_lines <- readLines(file_path)
    }
    else {
        warning("LaTeX file not found !")
        return(FALSE)
    }
    graphics_patt <- "\\s*\\\\includegraphics"
    begin_break_points <- which(grepl(graphics_patt, raw_lines))
    return(length(begin_break_points))
}
