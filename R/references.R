
multi_label_check <- function(article_dir){
    figure_blocks <- figure_reader(article_dir, get_texfile_name(article_dir))
    image_count <- count_env(article_dir,"figure")
    label_start_patt <- "\\s*\\\\label\\{\\s*(.*?)\\s*\\}"
    used_references <- ref_reader(paste0(article_dir,"/", get_texfile_name(article_dir)))
    unused_references <- list()
    for (image in 1:image_count) {
        unified_line <- ""
        for (line in figure_blocks[[image]]$data) {
            unified_line <- paste(unified_line, line)
        }
        matches <- stringr::str_match_all(unified_line,label_start_patt)[[1]]
        nrows <- length(matches)/2
        start_point <- nrows + 1
        end_point <- length(matches)
        matches_x <- matches[start_point:end_point]
        referenced_matches <- list()
        if (length(matches) > 2) {
            message("Multiple labels found in a figure environment !")
            for (match in matches_x) {
                for (ref in unique(used_references$references)) {
                    if (ref == paste0("{",match,"}")) {
                        referenced_matches <- append(referenced_matches, match)
                    }
                }
            }
            message("used matches :", paste0(referenced_matches,sep = ","))
            message("unused matches :", paste0(setdiff(matches_x,referenced_matches),sep = ","))
            unused_references = append(unused_references, setdiff(matches_x,referenced_matches))
        }
    }
    unused_references <- unlist(unused_references)
    return_list <- list()
    return_list$used_ref <- unique(used_references$references)
    return_list$unused_ref <- unique(unused_references)
    return(return_list)
}

ref_reader <- function(file_path) {
    # readLines
    file_path <- xfun::normalize_path(file_path)
    if (file.exists(file_path)) {
        raw_lines <- readLines(file_path)
    }
    else {
        message("File Does not exist !")
        return(FALSE)
    }
    raw_words <- str_split(raw_lines," ")
    # filters comments in the given tex file
    comments <- which(grepl("\\%",raw_lines))
    for ( comment in comments) {
        raw_lines[comment] <- ""
    }
    label_references <- list()
    count <- 0
    begin_patt <- "\\\\ref\\{"
    ref_break_points <- which(grepl(begin_patt,raw_lines))
    if (!identical(ref_break_points,integer(0))) {
        for (pos in ref_break_points) {
            raw_words <- str_split(raw_lines[pos]," ")
            for (word in raw_words[[1]]) {
                if (grepl(begin_patt, word)) {
                    count = count + 1
                    label_references <- append(label_references, word)
                }
            }
        }
    }
    label_references <- unlist(label_references)
    label_references <- filter_label_references(label_references)
    reference_data <- list()
    reference_data$count <- length(label_references)
    reference_data$references <- label_references
    return(reference_data)
}

filter_label_references <- function(label_references) {
    filtered_references <- list()
    for (iterator in seq_along(label_references)) {
        ref_line <- label_references[iterator]
        # filtering out things
        ref_line <- stringr::str_extract(ref_line, "(\\{.*?\\})")
        if (grepl(",", ref_line)) {
            cites <- stringr::str_split(ref_line,",")
            filtered_references <- append(filtered_references,paste0(cites[[1]][1],"}"))
            end <- length(cites[[1]]) - 1
            for (value in 2:end) {
                filtered_references <- append(filtered_references,paste0("{",cites[[1]][value],"}"))
            }
            filtered_references <- append(filtered_references,paste0("{",cites[[1]][end + 1]))
        } else {
            filtered_references <- append(filtered_references,ref_line)
        }
    }
    return(unlist(filtered_references))
}
