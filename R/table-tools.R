#' @title patch table environment
#' @description
#' function to modify env and commands in TeX using GNU sed
#'
#'These are due to the pandoc's limitations and ease in conversion.
#'
#' @details
#' changes are made to :
#' 1. table* environment to table environment
#' 2. \\multicolumn to \\multicolumnx
#' \\multicolumnx is redefined in Metafix.sty as
#' \\renewcommand{\\multicolumnx}[3]{\\multicolumn{#1}{c}{#3}}
#' @param article_dir path to the directory which contains tex article
#'
#' @return patches table environments in LaTeX file and also backs up the old file before modification
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::patch_table_env(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
patch_table_env <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    if (file.exists(file_path)){
        raw_lines <- readLines(file_path)
    }
    else {
        warning("LaTeX file not found !")
        return(FALSE)
    }
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\begin\\{table\\*\\}", "table\\*", "table")
    warning("Changed \\begin{table\\*} to \\begin{table}")
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\end\\{table\\*\\}", "table\\*", "table")
    warning("Changed \\end{table*} to \\end{table}")
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\multicolumn", "multicolumn", "multicolumnx")
    warning("changed \\multicolumn to \\multicolumnx")
    # testing functionality
    #return(raw_lines)
    # backup old file
    if (file.exists(file_path)){
        src_file_data <- readLines(file_path)
    }
    else {
        warning("LaTeX file not found !")
        return(FALSE)
    }
    backup_file <- paste(file_path, ".bk", sep = "")
    write_external_file(backup_file, "w", src_file_data)
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
