#' @title Find Algorithm images
#'
#' @param fig_lines lines of code enclosing algorithm figure
#' @keywords internal
#' @return TRUE/FALSE
#' @noRd
find_algorithm <- function(fig_lines) {
    alg_image_start <- which(grepl("^\\s*\\\\begin\\{algorithm",
                                    fig_lines))
    alg_image_end <-  which(grepl("^\\s*\\\\end\\{algorithm",
                                   fig_lines))
    if(length(alg_image_end) == length(alg_image_start) &
       (!identical(alg_image_start,integer(0)))){
        return(TRUE)
    } else {
        #skip
    }
    return(FALSE)
}

#' @title convert algorithm2e diagrams to pdf,png
#' @description this function will create algorithm2e images to be included
#' in the RJ-web-article
#' @param alg_object the figure_block of algorithm
#' @param article_dir path to the directory which contains RJ article
#' @keywords internal
#' @return algorithm image
#' @noRd
convert_algorithm <- function(alg_object, article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    if (alg_object$algdata[1] != "\\begin{algorithm}[H]"){
        alg_object$algdata[1] <- "\\begin{algorithm}[H]"
    }
    M <- which(grepl("\\\\usepackage\\{Metafix\\}", alg_object$alglib))
    if (! identical(M,integer(0))) {
        alg_object$alglib[M] <- ""
    }
    algorithm_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage{hyperref}",
        "\\newcommand{\\code}[1]{\\texttt{#1}}",
        "\\newcommand{\\R}{R}",
        "\\newcommand{\\pkg}[1]{#1}",
        "\\newcommand{\\CRANpkg}[1]{\\pkg{#1}}%",
        "\\newcommand{\\BIOpkg}[1]{\\pkg{#1}}",
        "\\usepackage{algorithm2e}",
        alg_object$alglib,
        "\\begin{document}",
        "\\nopagecolor",
        alg_object$algdata,
        "\\end{document}"
    )
    alg_file_name <- paste0(gsub(":","",alg_object$label),".tex")
    # convert the tex file into pdf
    alg_dir <- paste(article_dir,"alg",sep="/")
    alg_path <- paste(alg_dir,alg_file_name,sep="/")
    dir.create(alg_dir, showWarnings = FALSE)
    fileconn <- file(alg_path)
    writeLines(algorithm_template, fileconn)
    close(fileconn)
    alg_object$compiled <- TRUE
    tryCatch(tinytex::latexmk(alg_path, engine = "pdflatex"),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 alg_object$compiled <- FALSE
             }
    )
    # run pdf to png
    alg_png_file <- xfun::with_ext(alg_file_name,"png")
    alg_object$path <- paste0("alg/",alg_png_file)
    alg_object$converted <- TRUE
    tryCatch(texor::convert_to_png(xfun::with_ext(alg_path,"pdf")),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 alg_object$converted <- FALSE
             }
    )

    alg_png_path <- xfun::with_ext(alg_path,"png")
    web_alg_folder <- paste(article_dir,"web/alg",sep="/")
    web_alg_png_path <- paste0(web_alg_folder,"/",xfun::with_ext(alg_file_name,"png"))
    dir.create(web_alg_folder)
    alg_object$copied <- TRUE
    tryCatch(file.copy(alg_png_path, web_alg_png_path),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 alg_object$copied <- FALSE
             }
    )
    alg_object$included_as_png <- TRUE
    tryCatch(insert_algorithm_png(alg_object, article_dir),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 alg_object$included_as_png <- FALSE
             }
    )
    return(alg_object)
}

#' @title extract extra algorithm libraries
#'
#' @param article_dir article directory
#' @keywords internal
#' @return string of libraries
#' @noRd
extract_extra_lib <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    wrapper_file <- get_wrapper_type(article_dir)
    wrapper_path <- paste(article_dir,wrapper_file,sep = "/")
    wrapper_lines <- readLines(wrapper_path)
    rjournal_line <- which(grepl("\\usepackage\\{RJournal\\}",wrapper_lines))
    begin_doc_line <- which(grepl("\\s*\\\\begin\\{document\\}",wrapper_lines))
    alg_libs <- wrapper_lines[(rjournal_line+1):(begin_doc_line-1)]
    alg_libs <- comment_filter(alg_libs)
    return(alg_libs)
}

#' @title insert algorithm image lines
#'
#' @param fig_block fig block of data
#' @param article_dir article working directory
#' @keywords internal
#' @return TRUE/FALSE
#' @noRd
insert_algorithm_png <- function(fig_block,article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_texfile_name(article_dir)
    raw_lines <- readLines(file.path(article_dir, file_name))
    file_path <- paste0(article_dir,"/",file_name)
    alg_start_patt <- "\\s*\\\\begin\\{algorithm\\}"
    start_patt <- "\\s*\\\\begin\\{figure\\}"
    figure_starts <- which(grepl(start_patt, raw_lines))
    alg_figure_starts <- which(grepl(alg_start_patt, raw_lines))
    figure_starts <- c(figure_starts,alg_figure_starts)
    figure_starts <- sort(figure_starts)
    before_including_image <- raw_lines[1:figure_starts[fig_block$image_number]]
    remaining_line <- raw_lines[((figure_starts[fig_block$image_number])+1):length(raw_lines)]
    if (!identical(which(grepl("\\\\includegraphics\\{alg/",remaining_line)),integer(0))) {
        message("Image already included")
        return(TRUE)
    }
    include_png_line <- paste0("\\includegraphics{alg/",gsub(":","",fig_block$label),".png}")
    # Backup original wrapper file
    file.rename(file_path, paste(file_path, ".bk", sep = ""))
    # write to original wrapper file and save it as .new
    xfun::write_utf8(c(
        before_including_image,
        include_png_line,
        "",
        remaining_line),
        paste(file_path, ".new", sep = ""))
    # remove .new from extension
    file.rename(paste(file_path, ".new", sep = ""), file_path)
}

#' @title remove algorithm caption from LaTeX file
#'
#' @param fig_data figure data block
#' @keywords internal
#' @return raw_lines
#' @noRd
remove_alg_caption <- function(fig_data) {
    alg_start_regex <- which(grepl("^\\s*\\\\begin\\{algorithm",
                                   fig_data))
    caption_regex <- which(grepl("\\s*\\\\caption\\{",fig_data))
    alg_end_regex <- which(grepl("^\\s*\\\\end\\{algorithm",
                                 fig_data))
    raw_lines <- c(fig_data[alg_start_regex:(caption_regex - 1)],fig_data[alg_end_regex])
    return(raw_lines)
}
