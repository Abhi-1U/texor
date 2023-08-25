#' @title check texor pandoc compatibility
#' @description
#'  texor package requires minimum pandoc version above or equal to 3.1,
#'  hence this utility will check for the installation and version status.
#' @return TRUE if v >= 3.1, else FALSE
#' @export
#'
#' @examples
#' rmarkdown::pandoc_version()
#'
#' texor::pandoc_version_check()
pandoc_version_check <- function(){
    pandoc_installation_check <- rmarkdown::pandoc_available()
    if (!pandoc_installation_check) {
        message("Pandoc not installed !, please install pandoc >= v3.1 ")
        return(FALSE)
    }
    else {
        # pass
    }
    current_version <- rmarkdown::pandoc_version()
    if (toString(current_version) != "") {
        version_list <- unlist(strsplit(toString(current_version),split = "\\."))
    }
    ## Pandoc Not Installed
    else {
        message("Pandoc not installed !, please install pandoc >= v3.1 ")
        return(FALSE)
    }
    # Pandoc Installed with version 3.1 exactly or above
    if (as.integer(version_list[1]) == 3 && as.integer(version_list[2]) >= 1) {
        return(TRUE)
    }
    # Pandoc Installed above 3.1 (eg v4)
    if (as.integer(version_list[1]) > 3) {
        return(TRUE)
    }
    # Pandoc Installed but older than 3.1
    else {
        return(FALSE)
    }
}

#' check markdown file
#'
#' Checks if the markdown file generated is empty or not due to some pandoc
#' related error during conversion to markdown.
#' @param article_dir path to the directory which contains tex article
#'
#' @return FALSE if markdown file is corrupted/empty else TRUE
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rmarkdown::pandoc_version()
#' texor::include_style_file(your_article_path)
#' rebib::aggregate_bibliography(your_article_path)
#' texor::convert_to_markdown(your_article_path)
#' texor::check_markdown_file(your_article_path)
#' unlink(your_article_folder, recursive = TRUE)
check_markdown_file <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_md_file_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    if (file.exists(file_path)) {
        raw_lines <- readLines(file_path)
    }
    else {
        return(FALSE)
    }
    failed_conversion_doc_2 <- c("::: article",
                                 ":::")
    failed_conversion_doc_1 <- c("")
    if (identical(failed_conversion_doc_2,raw_lines)) {
        return(FALSE)
    }
    if (file.size(file_path) == 0L) {
        return(FALSE)
    }
    else {
        return(TRUE)
    }
}
