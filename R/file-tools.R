#' Get the name of the wrapper file in the article dir
#'
#'This function gets the wrapper file name from the
#'commonly named R-Journal wrapper files.
#'@details
#'Usually the R journal wrapper files are named either
#'1. RJwrapper.tex
#'2. RJwrap.tex
#'3. wrapper.tex
#' @param article_dir path to the directory which contains tex article
#'
#' @return String with name of wrapper file or empty
#' @export
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' texor::get_wrapper_type(article_dir)
get_wrapper_type <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    wrapper_types <- c("wrapper.tex",
                       "RJwrap.tex",
                       "RJwrapper.tex")
    wrapper_file <- ""
    for (w_type in wrapper_types) {
        if (file.exists(file.path(article_dir, w_type))) {
            wrapper_file <- w_type
        }
    }
    if (wrapper_file == "") {
        print("Error : No Wrapper File Found in the article dir")
    }
    return(wrapper_file)
}

#' @title comment filter for latex lines data
#'
#' @description
#' removes commented latex lines to avoid wrong reference data
#' @param data block of data
#'
#' @return filtered data
#' @export
comment_filter <- function(data) {
    comment_break_points <- which(grepl("^%", data))
    for (pos in comment_break_points) {
        data[pos] <- ""
    }
    comment_break_points <- which(grepl("^%%", data))
    for (pos in comment_break_points) {
        data[pos] <- ""
    }
    return(data[nzchar(data)])
}

#' quick function to writelines to a file
#'
#' @param file_path path of text file to write contents to
#' @param mode mode of opening
#' @param raw_text the text/ list of lines to be written
#'
#' @return create/append/write a new file
#' @export
write_external_file <- function(file_path, mode, raw_text) {
    file_path <- xfun::normalize_path(file_path)
    write_file <- file(file_path, mode)
    writeLines(raw_text, write_file)
    close(write_file)
}

#' Get the name of the tex file included within wrapper file
#'
#'The wrapper file refers to an external tex file which contains
#'the actual document content.
#' @param article_dir path to the directory which contains tex article
#'
#' @return String name of the tex-file name
#' @export
get_texfile_name <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    lookup_file <- get_wrapper_type(article_dir)
    wrapper_file <- readLines(file.path(article_dir, lookup_file))
    article_start <- which(grepl(
        "^\\s*\\\\begin\\{article\\}",
        wrapper_file))
    pre_marker <- wrapper_file[seq_len(article_start)]
    post_marker <- wrapper_file[seq_len(article_start) + 1]
    source_line <- setdiff(post_marker, pre_marker)
    tex_file <- gsub("[[:space:]]", "",
                     gsub("\\\\input\\{|\\}", "", source_line))
    if (!grepl(".tex$", tex_file)) {
        tex_file <- paste0(tex_file, ".tex")
    }
    return(tex_file)
}

get_md_file_name <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    lookup_file <- get_wrapper_type(article_dir)
    markdown_file <- gsub(".tex", ".md", lookup_file)
}

get_journal_details <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    journal_details <- list()
    # windows
    if( grepl("\\\\",article_dir)){
        hierarchy <- str_split(article_dir, "\\\\")[[1]]
    }
    if( grepl("/",article_dir)){
        hierarchy <- str_split(article_dir, "/")[[1]]
    } else {
        #--pass
    }
    journal_folder <- hierarchy[length(hierarchy)-1]
    if (journal_folder == "") {
        journal_folder <- hierarchy[length(hierarchy)-2]
    }
    journal_info <- str_split(journal_folder, "-")[[1]]
    journal_details$volume <- strtoi(journal_info[1],10) - 2008
    journal_details$issue <- strtoi(journal_info[2],10)
    journal_details$slug <- hierarchy[length(hierarchy)]
    return(journal_details)
}

#' Copy Supporting Documents like images,bib file,etc.
#'
#' Copies supporting documents like images,pdf,bib files into the output
#' folder for building the HTML version of the R-Markdown file.
#'
#' @param from_path : String indicating base path for the working directory
#' @return copies dependency files into the output folder.
#' @export
copy_other_files <- function(from_path) {
    old_working_directory <- getwd()
    setwd(from_path)
    image_paths <- generate_image_paths(from_path)
    #dir_list <- list.dirs(recursive = FALSE)
    #possible_dirs <- c("*_files", "figures", "images", "tikz")
    #target_dir <- basename(dir_list[grep(
    #    paste(possible_dirs, collapse = "|"), dir_list)])
    #print(target_dir)
    if (! dir.exists("web/")) {
        dir.create("web/", showWarnings = FALSE)
    }
    #for (t_path in target_dir) {
    #    dir.create(paste("web/", t_path, sep = ""),
    #           showWarnings = FALSE)
    #}
    for (path in image_paths) {
        print(path)
        print(paste0("web/", path))
        if(!dir.exists(paste0("web/",dirname(path)))) {
                dir.create(paste0("web/",dirname(path)),showWarnings = TRUE)
        }
        file.copy(path, paste0("web/", path), overwrite = TRUE)
    }
    #file.copy(image_paths,
    #    paste("web/", target_dir, sep = ""), recursive = TRUE)
    file_list <- list.files(recursive = FALSE)
    extensions <- c("*.bib", "*.pdf", "*.R", "*.bbl")
    target_files <- unique(grep(paste(
        extensions, collapse = "|"), file_list, value = TRUE))
    print(target_files)
    file.copy(target_files,
              to = "web/",
              copy.mode = TRUE,
              recursive = FALSE, )
    setwd(old_working_directory)
}

copy_to_web <- function(rel_path, ext, article_dir){
    article_dir <- xfun::normalize_path(article_dir)
    if (! grepl(paste0(".",ext,"$"),rel_path)) {
        rel_path <- paste0(rel_path,".",ext)
    }
    web_dir <- paste0(article_dir,"/web")
    if (! dir.exists(web_dir)){
        dir.create(web_dir)
    }
    if (dirname(rel_path) == ".") {
        image_target_file_path <- (paste0(article_dir,"/web/",rel_path))
    } else {
        image_target_file_path <- (paste0(article_dir,"/web/",rel_path))
    }
    image_folder <- paste0(article_dir,"/web/",dirname(rel_path))
    if (! dir.exists(paste0(article_dir,"/web/",dirname(rel_path)))) {
        dir.create(paste0(article_dir,"/web/",dirname(rel_path)))
    }
    file_path <- paste0(article_dir,"/",rel_path)
    web_file_path <- paste0(article_dir,"/web/",rel_path)
    file.copy(file_path, web_file_path)
    png_file <- paste(toString(
        tools::file_path_sans_ext(file_path)), ".png",
        sep = "")

}

generate_image_paths <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # wrapper file name
    input_file <- get_wrapper_type(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- tools::file_path_as_absolute(input_file_path)
    # markdown equivalent filename
    temp_file <- "temp-native.txt"
    temp_file_path <- paste(article_dir, temp_file, sep = "/")

    image_list_filter <- system.file(
        "image_list_filter.lua", package = "texor")
    pandoc_opt <- c("-s",
                    "--resource-path", abs_file_path,
                    "--lua-filter", image_list_filter)
    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = "native",
                              options = pandoc_opt,
                              output = temp_file_path,
                              verbose = TRUE)
    if (file.exists(paste0(article_dir,"/image_source.txt"))) {
        image_paths <- readLines(paste0(article_dir,"/image_source.txt"))
    } else {
        image_paths <- NULL
    }

    return(image_paths)
}
