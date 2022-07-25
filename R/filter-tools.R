
filter_code_env <- function(raw_lines, target, replacement) {
    begin_patt <- paste("^\\s*\\\\begin\\{", target, "\\}", sep = "")
    end_patt <- paste("^\\s*\\\\end\\{", target, "\\}", sep = "")
    raw_lines <- stream_editor(raw_lines, begin_patt, target, replacement)
    raw_lines <- stream_editor(raw_lines, end_patt, target, replacement)
    return(raw_lines)
}

code_env <- c("example",
              "example*",
              "Sin",
              "Sout",
              "Scode",
              "Sinput",
              "Soutput",
              "smallverbatim")

patch_code_env <- function(article_dir) {
    # find tex file
    file_name <- get_texfile_name(article_dir)
    # readLines
    raw_lines <- readLines(paste(article_dir, file_name, sep = "/"))
    replacement <- "verbatim"
    # apply filter_code_env over all the env's
    for (env in code_env) {
        raw_lines <- filter_code_env(raw_lines, env, replacement)
    }
    # testing functionality
    return(raw_lines)
    # remove old tex file
    # write same tex file with new data
}
