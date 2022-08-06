# manage_tikz_images<-function(article_dir){
#     if (getwd()!= article_dir) {
#         setwd(article_dir)
#     }
#     # checking for RJwrapper and fetching the file name for tex file
#     file_name <- get_texfile_name(article_dir)
#     # extract tikz blocks as objects
#     tikz_object <- extract_embeded_tikz_image(article_dir, file_name)
#     # isolate tikz into a template latex file
#     tikz_template <- c(
#         "\\documentclass{standalone}",
#         "\\usepackage{xcolor}",
#         "\\usepackage{verbatim}",
#         "\\usepackage{tikz}",
#         "\\newcommand{\\CRANpkg}[1]{\\href{https://CRAN.R-project.org/package=#1}{\\pkg{#1}}}%",
#         "\\newcommand{\\pkg}[1]{#1}",
#         "\\newcommand{\\code}[1]{#1 }",
#         "\\usetikzlibrary{fit}",
#         "\\begin{document}",
#         "\\nopagecolor",
#         tikz_object,
#         "\\end{document}"
#     )
#     fileconn <- file("tikz.tex")
#     writeLines(tikz_template, fileconn)
#     close(fileconn)
#     # convert the tex file into pdf
#     dir.create("tikz", showWarnings = FALSE)
#     tikz_dir <- paste(article_dir, "tikz", sep = "/")
#     file.copy("tikz.tex", tikz_dir, copy.mode = TRUE, recursive = FALSE)
#     tinytex::latexmk("tikz/tikz.tex", engine = "pdflatex")
#     # run pdf to png
#     texor::make_png_files("./tikz/tikz.pdf")
#     # copy over the file
#     texor::copy_other_files(".")
#     # include tikz as pdf in the tex document
#     image_path <- "tikz/tikz.png"
#     texor::inject_generated_image(article_dir, file_name, image_path)
# }

#' Check if article has tikz images or not
#'
#'This simple utiliy function will check for tikzpicture environment
#' @param article_dir path to the directory which contains tex article
#' @param file_name file_name of the tex file
#'
#' @return TRUE/FALSE(boolean)
#' @export
#'
#' @examples
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

#' a sub-function to isolate a single tikz image
#'
#' @param src_file_data path to the directory which contains tex article
#' @param fig_start start point of figure around tikz image (integer)
#' @param fig_end end point of figure around tikz image (integer)
#'
#' @return tikz image data in a list of strings
#' @export
#'
#' @examples
extract_single_tikz <- function(src_file_data, fig_start, fig_end) {
    print(fig_start)
    print(fig_end)
    fig_data <- src_file_data[fig_start[[1]]:fig_end[[1]]]
    tikz_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture", fig_data))
    tikz_end <- which(grepl("^\\s*\\\\end\\{tikzpicture", fig_data))
    tikz_data <- fig_data[tikz_start:tikz_end]
    return(tikz_data)
}
#' a sub-function to isolate multiple tikz images
#'
#' @param src_file_data path to the directory which contains tex article
#' @param fig_start_list start points of figures around a tikz image (list)
#' @param fig_end_list end points of figures around a tikz image (list)
#'
#' @return tikz image data in a list of list of strings
#' @export
#'
#' @examples
extract_multiple_tikz <- function(src_file_data,
                        fig_start_list, fig_end_list) {
    tikz_image_lines <- list()
    for (iterator in seq_along(fig_start_list)) {
        print(fig_start_list[iterator])
        tikz_image <- extract_single_tikz(src_file_data,
                      fig_start_list[iterator],
                      fig_end_list[iterator])
        tikz_image_lines[iterator] <- tikz_image
    }
    return(tikz_image_lines)
}

#' extracts embedded tikz image(s) from a tex file
#'
#' @description
#'This will isolate the tikz image into a new text file which will be
#'read by a lua filter
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the tex file
#'
#' @return
#' @export tikz_main_data.txt this file contains isolated tikz code
#'
#' @examples
extract_embeded_tikz_image <- function(article_dir, file_name) {
    print(paste("TKZ-S2 : extracting Tikz Code from ", file_name))
    if (! article_has_tikz(article_dir, file_name)) {
       print("No tikz Image found")
       return(FALSE)
    }
    # reads the complete file
    src_file_data <- readLines(file.path(article_dir, file_name))
    # all possible values of figure start points
    fig_start_list <- which(grepl("^\\s*\\\\begin\\{figure", src_file_data))
    # all possible values of figure end points
    fig_end_list <- which(grepl("^\\s*\\\\end\\{figure", src_file_data))
    # all possibke values of tikz start points
    tikz_image_list <- which(grepl("^\\s*\\\\begin\\{tikzpicture",
                            src_file_data))
    print(paste("Debug 1 : ", tikz_image_list))
    # fig containing tikz  start_pos
    fil_start <- filter_fig_start(fig_start_list,tikz_image_list)
    # fig containing tikz end_pos
    fil_end <- filter_fig_end(fig_end_list,tikz_image_list)

    if (length(fil_start) == 1) {
        tikz_image <- extract_single_tikz(src_file_data,
                                            fil_start,
                                            fil_end)
        write_external_file("tikz_main_data.txt", "a", tikz_image)
    # handle a single tikz image
    } else {

        for (iterator in seq_along(fil_start)) {

            lines <- extract_single_tikz(src_file_data,
                                              fil_start[iterator],
                                              fil_end[iterator])
            write_external_file("tikz_main_data.txt", "a", lines)
            write_external_file("tikz_main_data.txt", "a", "")

        }
    }
}

#' an internal method to isolate tikz styleset
#'
#' @param src_file_data source file contents
#' @param fig_start starting point of figure of interest
#' @param fig_end end point of figure of interest
#'
#' @return tikz_styleset lines as a list of strings
#' @export
#'
#' @examples
get_single_tikz_styleset <- function(src_file_data, fig_start, fig_end) {
    tikz_set_data <- ""
    set_end_patt <- "\\]$"
    style_end_patt <- "^\\}"
    tikz_set_pos <- which(grepl("^\\s*\\\\tikzset\\{", src_file_data))
    if (identical(tikz_set_pos, integer(0))) {
        tikz_set_pos <- which(grepl("^\\s*\\\\tikzstyle\\{", src_file_data))
    }
    if (identical(tikz_style_pos, integer(0))) {
        tikz_set_pos <- FALSE
        return(0)
    }
    end_patt <- style_end_patt
    if (! tikz_style_pos) {
        end_patt <- set_end_patt
    }
    for (iterator in seq_along(tikz_set_pos)) {
        # if styleset is in middle of fig_start and fig_end
        if (tikz_set_pos[iterator] > fig_start[iterator] && tikz_set_pos[iterator] < fig_end[iterator]) {
            style_ending <- which(grepl(end_patt, src_file_data[fig_start[iterator]:fig_end[iterator]]))[1]
            style_ending <- style_ending+fig_start[iterator]
        }
        # if styleset is above the fig_start
        if (tikz_set_pos[iterator] < fig_end[iterator && tikz_set_pos[iterator] < fig_start[iterator]]) {
            style_ending <- which(grepl(end_patt, rev(src_file_data[1:fig_start[iterator]])))[1]
            style_ending <- fig_start[iterator] - style_ending
        }
    }
    # ----- new above
    # ----- old below
    # if styleset is in middle of fig_start and fig_end
    if  (tikz_set_pos < fig_end && tikz_set_pos) {
        pre_tikz_set <- src_file_data[seq_len(tikz_style_pos)-1]
        post_tikz_set <- src_file_data[seq_len(tikz_style_pos)-1:fig_end]

                                       # single line style
                                       if (style_ending == 1) {
                                           tikz_set_data <- post_tikz_set[style_ending]
                                       } else {
                                           tikz_set_data <- post_tikz_set[seq_len(style_ending)]
                                       }

    if (! identical(class(tikz_set_pos), "logical") &&
        tikz_set_pos < fig_end) {
        pre_tikz_set <- src_file_data[seq_len(tikz_set_pos) - 1]
        post_tikz_set<- src_file_data[(tikz_set_pos - 1):length(src_file_data)]
        style_ending <- which(grepl("^\\}", post_tikz_set))[1]
        # single line style content
        if (style_ending == 1) {
            tikz_set_data <- post_tikz_set[style_ending]
            # multiple line style content
        } else {
            tikz_set_data <- post_tikz_set[seq_len(style_ending)]
        }
    }
    return(tikz_set_data)
    }
}



filter_fig_start <- function(fig_start_list,tikz_image_list){
    filtered_list <- list()
    for (fig in seq_along(fig_start_list)) {
        for (tikz in seq_along(tikz_image_list)) {
            if (tikz_image_list[tikz] > fig_start_list[fig]) {
                filtered_list[tikz] <- fig_start_list[fig]
            }
        }
    }
    return(filtered_list)
}

filter_fig_end <- function(fig_end_list,tikz_image_list){
    filtered_list <- list()
    for (fig in rev(seq_along(fig_end_list))) {
        for (tikz in rev(seq_along(tikz_image_list))) {
            if (fig_end_list[fig] > tikz_image_list[tikz] ) {
                filtered_list[tikz] <- fig_end_list[fig]
            }
        }
    }
    return(filtered_list)
}

#
# inject_generated_image <- function(article_dir, file_name, image_path){
#     tex_file <- readLines(file.path(article_dir, file_name))
#     tikz_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture", tex_file))
#     pre_tikz <- tex_file[seq_len(tikz_start - 1)]
#     post_tikz <- setdiff(tex_file, pre_tikz)
#     include_graphics <- paste("\\includegraphics[scale=1]{",
#                              image_path, "}" ,sep = "")
#     # write to original wrapper file
#     write_file <- file(file_name, "w")
#     writeLines(c(pre_tikz,include_graphics, "", post_tikz), write_file)
#     close(write_file)
# }


#' pre-process tex file for handling tikz images
#'
#' This function will remove tikz images from raw_latex and replace
#' it with placeholders, which will be filled by a lua filter later.
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export outfile.tex a modified latex document
#'
#' @examples
pre_process_tikz <- function(article_dir) {
    # wrapper file name
    input_file <- get_texfile_name(article_dir)
    print(input_file)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    print(input_file_path)
    abs_file_path <- dirname(input_file_path)
    latex_template <- system.file(
                        "extdata/template/latex.template",
                        package = "texor")
    tikz_filter <- system.file(
                        "extdata/filters/extract_tikz_filter.lua",
                        package = "texor")
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
