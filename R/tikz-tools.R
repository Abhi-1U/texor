manage_tikz_images<-function(article_dir){
    if (getwd()!= article_dir) {
        setwd(article_dir)
    }
    # checking for RJwrapper and fetching the file name for tex file
    file_name <- get_texfile_name(article_dir)
    # extract tikz blocks as objects
    tikz_object <- extract_embeded_tikz_image(article_dir, file_name)
    # isolate tikz into a template latex file
    tikz_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage{tikz}",
        "\\newcommand{\\CRANpkg}[1]{\\href{https://CRAN.R-project.org/package=#1}{\\pkg{#1}}}%",
        "\\newcommand{\\pkg}[1]{#1}",
        "\\newcommand{\\code}[1]{#1 }",
        "\\usetikzlibrary{fit}",
        "\\begin{document}",
        "\\nopagecolor",
        tikz_object,
        "\\end{document}"
    )
    fileconn <- file("tikz.tex")
    writeLines(tikz_template, fileconn)
    close(fileconn)
    # convert the tex file into pdf
    dir.create("tikz", showWarnings = FALSE)
    tikz_dir <- paste(article_dir, "tikz", sep = "/")
    file.copy("tikz.tex", tikz_dir, copy.mode = TRUE, recursive = FALSE)
    tinytex::latexmk("tikz/tikz.tex", engine = "pdflatex")
    # run pdf to png
    texor::make_png_files("./tikz/tikz.pdf")
    # copy over the file
    texor::copy_other_files(".")
    # include tikz as pdf in the tex document
    image_path <- "tikz/tikz.png"
    texor::inject_generated_image(article_dir, file_name, image_path)
}

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
article_has_tikz <- function(article_dir, file_name) {
    # reads the complete file
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

extract_single_tikz <- function(src_file_data, fig_start, fig_end) {
    pre_fig <- src_file_data[seq_len(fig_start) - 1]
    post_fig <- src_file_data[seq_len(fig_end)]
    fig_data <- setdiff(post_fig,pre_fig)
    tikz_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture", fig_data))
    tikz_end <- which(grepl("^\\s*\\\\end\\{tikzpicture", fig_data))
    pre_tikz <- fig_data[seq_len(tikz_start - 1)]
    post_tikz <- fig_data[seq_len(tikz_end)]
    tikz_data <- setdiff(post_tikz, pre_tikz)
    return(tikz_data)
}
extract_multiple_tikz <- function(src_file_data,
                        fig_start_list, fig_end_list) {
    tikz_image_lines <- list()
    for (iterator in seq_along(fig_start_list)) {
        tikz_image <- extract_single_tikz(src_file_data,
                      fig_start_list[iterator],
                      fig_end_list[iterator])
        tikz_image_lines[iterator] <- tikz_image
    }
    return(tikz_image_lines)
}

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
    # checks if there are multiple figures or not
    if (identical(class(fig_start_list), "integer")) {
        # if there is only one figure then it is possibly the one with tikz
        if (identical(class(tikz_image_list), "integer") &&
            (! identical(tikz_image_list, integer(0)))) {
            tikz_fig_start <- fig_start_list
            tikz_fig_end <- fig_end_list
        } else {
            print("No Tikz images found")
        }
    } else {
        # case of multiple figure but single tikz image
        if (identical(class(tikz_image_list), "integer") &&
            (! identical(tikz_image_list, integer(0)))) {
            for (iterator in seq_along(fig_start_list)) {
                if (tikz_image_list > fig_start_list[iterator] &&
                    tikz_image_list < fig_end_list[iterator]) {
                        tikz_fig_start <- fig_start_list[iterator]
                        tikz_fig_end <- fig_end_list[iterator]
                }
            }
        # case of multiple tikz images and multiple figures
        } else {
            tikz_fig_start <- list()
            tikz_fig_end <- list()
            for (Ti in seq_along(tikz_image_list)) {
                for (iterator in seq_along(fig_start_list)) {
                    if (tikz_image_list[Ti] > fig_start_list[iterator] &&
                        tikz_image_list[Ti] < fig_end_list[iterator]) {
                           tikz_fig_start[Ti] <- fig_start_list[iterator]
                           tikz_fig_end[Ti] <- fig_end_list[iterator]
                    }
                }
            }
        }
    }
    # handle multiple tikz images
    if (! identical(class(tikz_fig_start), "integer")) {
        tikz_image <- extract_multiple_tikz(src_file_data,
                                            tikz_fig_start,
                                            tikz_fig_end)
        for (image in tikz_image) {
            write_external_file("tikz_main_data.txt", "a", image)
        }
    # handle a single tikz image
    } else {
        tikz_image <- extract_single_tikz(src_file_data,
                                        tikz_fig_start,
                                        tikz_fig_end)
        write_external_file("tikz_main_data.txt", "a", tikz_image)
    }
}

check_tikz_set <- function(pre_fig, fig_data) {
    tikz_set_start <- NULL
    tikz_set_end <- NULL
    tikz_set_data <- ""
    tikz_style_in_fig <- which(grepl("^\\s*\\\\tikzstyle\\{", fig_data))
    tikz_style_pre_fig <- which(grepl("^\\s*\\\\tikzstyle\\{", pre_fig))
    tikz_set_in_fig <- which(grepl("^\\s*\\\\tikzset\\{", fig_data))
    tikz_set_pre_fig <- which(grepl("^\\s*\\\\tikzset\\{", pre_fig))
    if (identical(tikz_style_in_fig, integer(0))) {
        tikz_style_in_fig <- FALSE
    } else if (identical(tikz_style_pre_fig, integer(0))) {
        tikz_style_pre_fig <- FALSE
    } else if (identical(tikz_set_in_fig, integer(0))) {
        tikz_set_in_fig <- FALSE
    } else if (identical(tikz_set_pre_fig, integer(0))) {
        tikz_set_pre_fig <- FALSE
    }
    points <- c(
        tikz_style_in_fig,
        tikz_style_pre_fig,
        tikz_set_pre_fig,
        tikz_set_in_fig
    )
    check_points <- c(
        "tikz_style_in_fig",
        "tikz_style_pre_fig",
        "tikz_set_pre_fig",
        "tikz_set_in_fig"
    )
    iterator <- 1
    for (point in points) {
        if (point != FALSE) {
             tikz_set_start <- point
             break
        } else {
            iterator <- iterator + 1
        }
    }
    post_tikz_set_start <- points[iterator]
    return(tikz_set_data)
}








inject_generated_image <- function(article_dir, file_name, image_path){
    tex_file <- readLines(file.path(article_dir, file_name))
    tikz_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture", tex_file))
    pre_tikz <- tex_file[seq_len(tikz_start - 1)]
    post_tikz <- setdiff(tex_file, pre_tikz)
    include_graphics <- paste("\\includegraphics[scale=1]{",
                             image_path, "}" ,sep = "")
    # write to original wrapper file
    write_file <- file(file_name, "w")
    writeLines(c(pre_tikz,include_graphics, "", post_tikz), write_file)
    close(write_file)
}
pre_process_tikz <- function(article_dir){
    input_file <- get_texfile_name(article_dir)
    abs_file_path <- tools::file_path_as_absolute(article_dir)
    latex_template <- system.file(
                        "extdata/latex.template",
                        package = "texor")
    tikz_filter <- system.file(
                        "extdata/extract_tikz_filter.lua",
                        package = "texor")
    pandoc_opt <- c(
                  "--resource-path", abs_file_path,
                  "--lua-filter", tikz_filter,
                  "--template", latex_template)
    input_format <- "latex+raw_tex"
    output_format <- "latex"
    out_file <- "outfile.tex" # temporary solution will change it later
    rmarkdown::pandoc_convert(input_file,
                              from = input_format,
                              to = output_format,
                              options = pandoc_opt,
                              output = out_file,
                              verbose = TRUE)
}

read_tikz_metadata <- function(article_dir) {
    file_name <- get_texfile_name(article_dir)
    src_file_data <- readLines(file.path(article_dir, file_name))
    fig_start <- which(grepl(
            "^\\s*\\\\begin\\{figure", src_file_data))
    fig_end <- which(grepl(
            "^\\s*\\\\end\\{figure", src_file_data))
    pre_fig <- src_file_data[seq_len(fig_start) - 1]
    post_fig <- src_file_data[seq_len(fig_end)]
    fig_data <- setdiff(post_fig,pre_fig)
    caption_line <- which(grepl("^\\s*\\\\caption\\{", fig_data))
    caption_data <- fig_data[caption_line]
    caption_text <- gsub("\\\\caption\\{|", "", caption_data)
    caption_raw_text <- str_match(caption_text, "\\{\\s*(.*?)\\s*\\}")[, 2]
    label_line <- which(grepl("^\\s*\\\\label\\{", fig_data))
    label_data <- fig_data[label_line]
    label_raw_text <- gsub("fig:", "",
                      gsub("[[:space:]]", "",
                      gsub("\\\\label\\{|\\}", "", label_data)))
    # write caption and label into a temp_metadata file
    tikz_label_file <- file("tikz_label_meta.txt")
    writeLines(label_raw_text, tikz_label_file)
    close(tikz_label_file)
    tikz_caption_file <- file("tikz_caption_meta.txt")
    writeLines(caption_raw_text, tikz_caption_file)
    close(tikz_caption_file)
}
