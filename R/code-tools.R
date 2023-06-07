
#' @title filter code environment
#'
#' @description use stream editor to find and replace
#' code environments
#' @param raw_lines a vector of lines from text file
#' @param target target environment name
#' @param replacement replacement environment name
#' @keywords internal
#' @return modified raw_lines
#' @noRd
filter_code_env <- function(raw_lines, target, replacement) {
    begin_patt <- paste("\\s*\\\\begin\\{", target, "\\}", sep = "")
    end_patt <- paste("\\s*\\\\end\\{", target, "\\}", sep = "")
    raw_lines <- stream_editor(raw_lines, begin_patt, target, replacement)
    raw_lines <- stream_editor(raw_lines, end_patt, target, replacement)
    return(raw_lines)
}

#' @title patch all code environments
#' @description This function calls the `filter_code_env()` function
#' over the following code environments described in Rjournal.sty
#'
#' 1. example
#' 2. example*
#' 3. Sin
#' 4. Sout
#' 5. Sinput
#' 6. Soutput
#' 7. smallverbatim
#'
#' @param article_dir path to the directory which contains tex article
#' @keywords internal
#' @return patches code environments in LaTeX file and also backs up the old file before modification
#' @export
#' @examples
#' # Note This is a minimal example to execute this function
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir2"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <-  xfun::normalize_path(paste(your_article_folder,"article",sep="/"))
#' texor::patch_code_env(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
patch_code_env <- function(article_dir) {
    # find tex file
    code_env <- c("example",
                  "example\\*",
                  "Sin",
                  "Sout",
                  "Scode",
                  "Sinput",
                  "Soutput",
                  "smallverbatim",
                  "boxedverbatim")
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    replacement <- "verbatim"
    # apply filter_code_env over all the env's
    for (env in code_env) {
        raw_lines <- filter_code_env(raw_lines, env, replacement)
    }
    # backup old file
    src_file_data <- readLines(file_path)
    backup_file <- paste(file_path, ".bk", sep = "")
    write_external_file(backup_file, "w", src_file_data)
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
