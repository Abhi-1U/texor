
multi_label_check <- function(article_dir){
    figure_blocks <- figure_reader(article_dir, get_texfile_name(article_dir))
    image_count <- count_env(article_dir,"figure")
    label_start_patt <- "\\s*\\\\label\\{\\s*(.*?)\\s*\\}"
    raw_blocks <- list()
    for (image in 1:image_count) {
        unified_line <- ""
        for (line in figure_blocks[[image]]$data) {
            unified_line <- paste(unified_line, line)
        }
        matches <- stringr::str_match_all(unified_line,label_start_patt)[[1]]
        if (length(matches)>2) {
            warning("Multiple labels found in a figure environment!")
        }
    }

}
