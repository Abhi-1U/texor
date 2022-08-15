
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
get_wrapper_type <- function(article_dir) {
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

#' Get the name of the tex file included within wrapper file
#'
#'The wrapper file refers to an external tex file which contains
#'the actual document content.
#' @param article_dir path to the directory which contains tex article
#'
#' @return String name of the tex-file name
#' @export
get_texfile_name <- function(article_dir) {
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

#' finds the bib file in directory which is referenced in the article
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the tex file
#'
#' @return name of bib file (character)
#' @export
get_bib_file <- function(article_dir, file_name) {
    file_list <- list.files(article_dir, recursive = FALSE)
    extensions <- c("*.bib")
    linked_bib <- toString(paste(tools::file_path_sans_ext(file_name),
                    ".bib", sep = ""))
    bib_file <- unique(grep(paste(extensions, collapse = "|"),
                            file_list, value = TRUE))
    if (identical(bib_file, character(0))) {
        print("No Bib files found !")
        return("")
    }
    if (identical(class(bib_file), "character") &&
        identical(linked_bib, bib_file)) {
        print(paste("Found Bib file ", bib_file))
        return(bib_file)
    } else {
        for (file in bib_file) {
            if (identical(file, linked_bib)) {
                print(paste("Found Bib file ", bib_file))
                return(file)
            }
        }
    }
}

#' quick function to writelines to a file
#'
#' @param file_name name of text file to write contents to
#' @param mode mode of opening
#' @param raw_text the text/ list of lines to be written
#'
#' @return create/append/write a new file
#' @export
write_external_file <- function(file_name, mode, raw_text) {
    write_file <- file(file_name, mode)
    writeLines(raw_text, write_file)
    close(write_file)
}

get_md_file_name <- function(article_dir) {
    lookup_file <- get_wrapper_type(article_dir)
    markdown_file <- gsub(".tex", ".md", lookup_file)
}

get_journal_details <- function(article_dir) {
    journal_details <- list()
    hierarchy <- texor::str_split(article_dir, "/")[[1]]
    journal_folder <- hierarchy[length(hierarchy)-1]
    if (journal_folder == "") {
        journal_folder <- hierarchy[length(hierarchy)-2]
    }
    journal_info <- texor::str_split(journal_folder, "-")[[1]]
    journal_details$volume <- strtoi(journal_info[1],10) - 2008
    journal_details$issue <- strtoi(journal_info[2],10)
    journal_details$slug <- hierarchy[length(hierarchy)]
    return(journal_details)
}

