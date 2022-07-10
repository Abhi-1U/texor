
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
#'
#' @examples
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
#'
#' @examples
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
    if (!grepl(".tex$",tex_file)) {
        tex_file <- paste0(tex_file, ".tex")
    }
    return(tex_file)
}
