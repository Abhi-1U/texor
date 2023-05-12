
#' @title filter code environment
#'
#' @description use stream editor to find and replace
#' cide environments
#' @param raw_lines a vector of lines from text file
#' @param target target environment name
#' @param replacement replacement environment name
#'
#' @return modified raw_lines
#' @export
filter_code_env <- function(raw_lines, target, replacement) {
    begin_patt <- paste("\\s*\\\\begin\\{", target, "\\}", sep = "")
    end_patt <- paste("\\s*\\\\end\\{", target, "\\}", sep = "")
    raw_lines <- stream_editor(raw_lines, begin_patt, target, replacement)
    raw_lines <- stream_editor(raw_lines, end_patt, target, replacement)
    return(raw_lines)
}

code_env <- c("example",
              "example\\*",
              "Sin",
              "Sout",
              "Scode",
              "Sinput",
              "Soutput",
              "smallverbatim",
              "boxedverbatim")

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
#'
#' @return writes modified file and also backs up the old file before modification
#' @export
patch_code_env <- function(article_dir) {
    # find tex file
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
    # testing functionality
    #return(raw_lines)
    # backup old file
    src_file_data <- readLines(file_path)
    backup_file <- paste(file_path, ".bk", sep = "")
    write_external_file(backup_file, "w", src_file_data)
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
