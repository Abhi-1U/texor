
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

code_env_patch <- function(article_dir) {
    # find tex file
    # readLines
    # apply filter_code_env over all the env's
    # remove old tex file
    # write same tex file with new data
}
