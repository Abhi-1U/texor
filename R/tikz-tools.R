#' @title convert tikz diagrams to pdf,png
#' @description this function will create tikz images to be included
#' in the RJ-web-article
#' @param fig_block the figure_block of algorithm
#' @param article_dir path to the directory which contains RJ article
#'
#' @return tikz image
#' @noRd
convert_tikz <- function(fig_block, article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    caption_point <- which(grepl("\\\\captionsetup\\{",fig_block$data))
    if (!identical(caption_point, integer(0))) {
        fig_block$data[caption_point] <- ""
    }
    tikz_picture_end_point <- which(grepl("\\\\end\\{tikzpicture\\}",fig_block$data))
    tikz_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage{tikz}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage{graphics}",
        "\\usepackage{hyperref}",
        "\\newcommand{\\code}[1]{\\texttt{#1}}",
        "\\newcommand{\\R}{R}",
        "\\newcommand{\\pkg}[1]{#1}",
        "\\newcommand{\\CRANpkg}[1]{\\pkg{#1}}%",
        "\\newcommand{\\BIOpkg}[1]{\\pkg{#1}}",
        fig_block$tikzlib,
        "\\begin{document}",
        "\\nopagecolor",
        fig_block$tikzstyle,
        fig_block$data[2:tikz_picture_end_point],
        #fig_block$data,
        "\\end{document}"
    )
    tikz_file_name <- paste0(gsub(":","",fig_block$label),".tex")
    # convert the tex file into pdf
    tikz_dir <- paste(article_dir,"tikz",sep="/")
    tikz_path <- paste(tikz_dir,tikz_file_name,sep="/")
    if(! dir.exists(tikz_dir)) {
        dir.create(tikz_dir, showWarnings = FALSE)
    }
    # write the template
    fileconn <- file(tikz_path)
    writeLines(tikz_template, fileconn)
    close(fileconn)
    fig_block$compiled <- TRUE
    tryCatch(tinytex::latexmk(tikz_path, engine = "pdflatex"),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 fig_block$compiled <- FALSE
             }
    )

    # run pdf to png
    tikz_png_file <- xfun::with_ext(tikz_file_name,"png")
    fig_block$path <- paste0("tikz/",tikz_png_file)
    fig_block$converted <- TRUE
    tryCatch(texor::convert_to_png(xfun::with_ext(tikz_path,"pdf")),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 fig_block$converted <- FALSE
             }
    )
    tikz_png_path <- xfun::with_ext(tikz_path,"png")
    web_tikz_folder <- paste(article_dir,"web/tikz",sep="/")
    web_tikz_png_path <- paste0(web_tikz_folder,"/",tikz_png_file)
    if(! dir.exists(web_tikz_folder)) {
        dir.create(web_tikz_folder)
    }
    fig_block$copied <- TRUE
    tryCatch(file.copy(tikz_png_path,web_tikz_png_path),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 fig_block$copied <- FALSE
             }
    )
    fig_block$included_as_png <- TRUE
    tryCatch(insert_tikz_png(fig_block, article_dir),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
                 fig_block$included_as_png <- FALSE
             }
    )
    return(fig_block)
}

#' a sub-function to isolate a single tikz image
#'
#' @param fig_data figure tikz block
#' @param article_dir path to the article working directory
#' @param iterator tikz image number
#' @return tikz image data in a list of strings
#' @noRd
extract_tikz_style <- function(fig_data, article_dir, iterator) {
    article_dir <- xfun::normalize_path(article_dir)
    tikz_file_name <- "tikz_style_data.yaml"
    tikz_file_path <- paste(article_dir, tikz_file_name, sep = "/")
    if (! file.exists(tikz_file_path)) {
        pre_process_tikz(article_dir)
    }
    if(file.exists(tikz_file_path)){
        tikz_data <- readLines(tikz_file_path)
    } else {
        return("")
    }
    tikz_start_point <- which(grepl(paste0("image: ",iterator), tikz_data))
    tikz_end_point <- which(grepl(paste0("image-end: ",iterator), tikz_data))
    # delete tikz_data of first image from the temp file
    un_processed_tikz <- ""
    if(! identical(tikz_start_point,integer(0))) {
        un_processed_tikz <- tikz_data[(tikz_start_point+1):(tikz_end_point-1)]
    } else {
        un_processed_tikz <- ""
    }

    return(un_processed_tikz)
}


#' extracts tikz libraries from the wrapper file
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return lines of tikz library data
#' @noRd
extract_tikz_lib <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    wrapper_file <- get_wrapper_type(article_dir)
    wrapper_path <- paste(article_dir,wrapper_file,sep = "/")
    wrapper_lines <- readLines(wrapper_path)
    rjournal_line <- which(grepl("\\usepackage\\{RJournal\\}",wrapper_lines))
    begin_doc_line <- which(grepl("\\s*\\\\begin\\{document\\}",wrapper_lines))
    tikz_libs <- wrapper_lines[(rjournal_line+1):(begin_doc_line-1)]
    tikz_libs <- comment_filter(tikz_libs)
    tikz_libs_pos <- which(grepl("\\\\usetikzlibrary\\{", tikz_libs))
    tikz_libs[tikz_libs_pos]
    return(tikz_libs[tikz_libs_pos])
}

#' Search for Tikz image in a LaTeX file
#'
#' @param fig_lines block of figure lines from LaTeX file
#'
#' @return TRUE/FALSE
#' @noRd
find_tikz <- function(fig_lines) {
    tikz_image_start <- which(grepl("\\\\begin\\{tikzpicture",
                                    fig_lines))
    tikz_image_end <-  which(grepl("\\\\end\\{tikzpicture",
                                   fig_lines))
    if(length(tikz_image_end) == length(tikz_image_start) &
       (!identical(tikz_image_start,integer(0)))){
        return(TRUE)
    } else {
        #skip
    }
    return(FALSE)
}

#' Count tikz image instances
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the LaTeX file
#'
#' @return count of tikz images
#' @noRd
tikz_count <- function(article_dir, file_name) {
    article_dir <- xfun::normalize_path(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    tikz_start_patt <- "\\\\begin\\{tikzpicture\\}"
    tikz_end_patt <- "\\\\end\\{tikzpicture\\}"
    begin_break_points <- which(grepl(tikz_start_patt, raw_lines))
    end_break_points <- which(grepl(tikz_end_patt, raw_lines))
    # to do (ignore commented code)
    # if an environment opens and closes then the breakpoints would
    # be equal in length, otherwise it may indicate something wrong

    if (length(begin_break_points) == length(end_break_points)) {
        return(length(begin_break_points))
    }
    if (length(begin_break_points) < length(end_break_points)) {
        return(length(end_break_points))
    } else {
        return(length(begin_break_points))
    }
}

#' pre-process tex file for handling tikz images
#'
#' This function will remove tikz images from raw_latex and replace
#' it with placeholders, which will be filled by a lua filter later.
#' @param article_dir path to the directory which contains tex article
#'
#' @return outfile.tex a modified latex document
#' @noRd
pre_process_tikz <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # wrapper file name
    input_file <- get_texfile_name(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- dirname(input_file_path)
    latex_template <- system.file("latex.template", package = "texor")
    tikz_filter <- system.file("extract_tikz.lua", package = "texor")
    pandoc_opt <- c(
        "--resource-path", abs_file_path,
        "--lua-filter", tikz_filter,
        "--template", latex_template)
    input_format <- "latex+raw_tex"
    output_format <- "latex"
    out_file <- "outfile.tex"
    rmarkdown::pandoc_convert(input_file_path,
                              from = input_format,
                              to = output_format,
                              options = pandoc_opt,
                              output = out_file,
                              verbose = TRUE)
}

#' Check if article has tikz images or not
#'
#'This simple utiliy function will check for tikzpicture environment
#' @param article_dir path to the directory which contains tex article
#'
#' @return TRUE if tikz image is present else FALSE
#' @export
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::article_has_tikz(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
article_has_tikz <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # reads the complete file
    file_name <- get_texfile_name(article_dir)
    src_file_data <- readLines(file.path(article_dir, file_name))
    # all possibke values of tikz start points
    tikz_image_list <- which(grepl("^\\s*\\\\begin\\{tikzpicture",
                                   src_file_data))
    if (identical(tikz_image_list, integer(0))) {
        return(FALSE)
    } else {
        return(TRUE)
    }
}

#' include tikz image in the modified LaTeX file
#'
#' @param fig_block block of figure lines
#' @param article_dir path to the directory which contains tex article
#'
#' @return adds a line to the file
#' @noRd
insert_tikz_png <- function(fig_block,article_dir) {
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
    if (!identical(which(grepl("\\\\includegraphics\\{tikz/",remaining_line)),integer(0))) {
        warning("Image already included")
        return(TRUE)
    }
    include_png_line <- paste0("\\includegraphics{tikz/",gsub(":","",fig_block$label),".png}")
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
