
filter_code_env <- function(raw_lines, target, replacement) {
    begin_patt <- paste("\\s*\\\\begin\\{", target, "\\}", sep = "")
    end_patt <- paste("\\s*\\\\end\\{", target, "\\}", sep = "")
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
    write_external_file(backup_file, "w")
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
