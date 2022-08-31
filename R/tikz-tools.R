#' @title convert tikz diagrams to pdf,png
#' @description this function will create tikz images to be included
#' in the RJ-web-article
#' @param fig_block the figure_block of algorithm
#' @param article_dir path to the directory which contains RJ article
#'
#' @return tikz image
#' @export
convert_tikz <- function(fig_block, article_dir) {
    tikz_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage{tikz}",
        fig_block$tikzlib,
        "\\begin{document}",
        "\\nopagecolor",
        fig_block$tikz_lib,
        fig_block$data,
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

    tinytex::latexmk(tikz_path, engine = "pdflatex")
    # run pdf to png
    tikz_png_file <- gsub(".tex",".png",tikz_file_name)
    texor::convert_to_png(gsub(
        ".tex", ".pdf", tikz_png_path
    ))
    web_tikz_folder <- paste(article_dir,"web/tikz",sep="/")
    if(! dir.exists(web_tikz_folder)) {
        dir.create(web_tikz_folder)
    }
    file.copy(tikz_path, paste0(article_dir,"/web/tikz/",tikz_png_file))

}

#' a sub-function to isolate a single tikz image
#'
#' @param fig_data figure tikz block
#' @param article_dir path to the article working directory
#' @return tikz image data in a list of strings
extract_tikz_block <- function(fig_data, article_dir) {
    tikz_file_name <- "tikz_temp_data.txt"
    tikz_file_path <- paste(article_dir, tikz_file_name, sep = "/")
    if (! file.exists(tikz_file_path)) {
        pre_process_tikz(article_dir)
    }
    tikz_data <- readLines(tikz_file_path)
    tikz_end_points <- which(grepl("\\s*\\\\end\\{tikzpicture", tikz_data))
    # delete tikz_data of first image from the temp file
    un_processed_tikz <- tikz_data[(tikz_end_points[1]+1):length(tikz_data)]
    #
    file.rename(tikz_file_path, paste(tikz_file_path, ".bk", sep = ""))
    # write to original wrapper file and save it as .new
    write_external_file(tikz_file_path, "a", un_processed_tikz)
    return(tikz_data[1:tikz_end_points[1]])
}



extract_tikz_lib <- function(article_dir) {
    wrapper_file <- get_wrapper_type(article_dir)
    wrapper_path <- paste(article_dir,wrapper_file,sep = "/")
    wrapper_lines <- readLines(wrapper_path)
    rjournal_line <- which(grepl("\\usepackage\\{RJournal\\}",wrapper_lines))
    begin_doc_line <- which(grepl("\\s*\\\\begin\\{document\\}",wrapper_lines))
    tikz_libs <- wrapper_lines[(rjournal_line+1):(begin_doc_line-1)]
    tikz_libs <- comment_filter(tikz_libs)
    return(tikz_libs)
}

find_tikz <- function(fig_lines) {
    tikz_image_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture",
                                    fig_lines))
    tikz_image_end <-  which(grepl("^\\s*\\\\end\\{tikzpicture",
                                   fig_lines))
    if(length(tikz_image_end) == length(tikz_image_start) &
       (!identical(tikz_image_start,integer(0)))){
        return(TRUE)
    } else {
        #skip
    }
    return(FALSE)
}

tikz_count <- function(article_dir, file_name) {
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    tikz_start_patt <- "\\s*\\\\begin\\{tikzpicture\\}"
    tikz_end_patt <- "\\s*\\\\end\\{tikzpicture\\}"
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
pre_process_tikz <- function(article_dir) {
    # wrapper file name
    input_file <- get_texfile_name(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- dirname(input_file_path)
    latex_template <- system.file("latex.template", package = "texor")
    tikz_filter <- system.file("extract_tikz.lua", package = "texor")
    alg_filter <- system.file("extract_algo.lua", package = "texor")
    pandoc_opt <- c(
        "--resource-path", abs_file_path,
        "--lua-filter", tikz_filter,
        "--lua-filter", alg_filter,
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
#' @return TRUE/FALSE(boolean)
#' @export
article_has_tikz <- function(article_dir) {
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
