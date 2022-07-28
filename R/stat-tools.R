pre_conversion_statistics <- function(article_dir){
    con_stat <- list()
    con_stat$table <- count_env(article_dir, "table")
    con_stat$figure <- count_env(article_dir, "figure")
    con_stat$math <- count_inline(article_dir, "math")
    con_stat$codeblock <- count_env(article_dir, "verbatim")
    con_stat$inlinecode <- count_inline(article_dir, "inlinecode")
    return(con_stat)
}

conversion_coverage_checK <- function(article_dir) {

}

count_env <- function(article_dir, env_name) {
    # find tex file
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    begin_patt <- paste("\\s*\\\\begin\\{", env_name, "\\}", sep = "")
    end_patt <- paste("\\s*\\\\end\\{", env_name, "\\}", sep = "")
    begin_break_points <- which(grepl(begin_patt, raw_lines))
    end_break_points <-  which(grepl(end_patt, raw_lines))
    # to do (ignore commented code)
    # if an environment opens and closes then the breakpoints would
    # be equal in length, otherwise it may indicate something wrong

    if (length(begin_break_points) == length(end_break_points)) {
        return(length(begin_break_points))
    }
    if (length(begin_break_points) < length(end_break_points)) {
        return(length(end_break_points))
    } else {
        return(length(begin_break_points))
    }
}
count_inline <- function(article_dir, inline) {
    # find tex file
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    raw_words <- str_split(raw_lines," ")
    # filters comments in the given tex file
    comments <- which(grepl("\\%",raw_lines))
    for ( comment in comments) {
        raw_lines[comment] <- ""
    }
    if (tolower(inline) == "math"){
        begin_patt <- "\\$\\s*(.*?)\\s*\\$"
        begin_break_points <- which(grepl(begin_patt,raw_lines))
        count <- 0
        for (pos in begin_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt, word)) {
                    count = count + 1
                }
            }
        }
        return(count)
    }
    if (tolower(inline) == "inlinecode") {
        count <- 0
        begin_patt <- "\\\\code\\{"
        begin_break_points <- which(grepl(begin_patt,raw_lines))
        for (pos in begin_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt, word)) {
                    count = count + 1
                }
            }
        }
        return(count)
    } else {
        return(0)
    }

}
