# global variable
tikz_count_var <- 0

#' @title figure reader
#'
#' @param article_dir the directory path where the file and its dependencies are
#' located
#' @param file_name name of the LaTeX file
#'
#' @return figure blocks
#' @noRd
figure_reader <- function(article_dir, file_name) {
    article_dir <- xfun::normalize_path(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    raw_lines <- readLines(file_path)
    alg_start_patt <- "\\s*\\\\begin\\{algorithm\\}"
    alg_end_patt <- "\\s*\\\\end\\{algorithm\\}"
    start_patt <- "\\s*\\\\begin\\{figure\\}"
    end_patt <- "\\s*\\\\end\\{figure\\}"
    raw_lines <- comment_filter(raw_lines)
    figure_starts <- which(grepl(start_patt, raw_lines))
    figure_ends <-  which(grepl(end_patt, raw_lines))
    alg_figure_starts <- which(grepl(alg_start_patt, raw_lines))
    alg_figure_ends <- which(grepl(alg_end_patt, raw_lines))
    figure_starts <- c(figure_starts,alg_figure_starts)
    figure_starts <- sort(figure_starts)
    figure_ends <- c(figure_ends,alg_figure_ends)
    figure_ends <- sort(figure_ends)
    figure_blocks <- list()
    # blocks of figure code
    if (length(figure_starts) == length(figure_ends)) {
        for (iterator in seq_along(figure_starts)) {
            block_start <- figure_starts[iterator]
            block_end <- figure_ends[iterator]

            fig_data <- raw_lines[block_start:block_end]
            figure_blocks <- append(figure_blocks,list(fig_block_reader(article_dir,
                                                                        fig_data,
                                                                        raw_lines,
                                                                        iterator,
                                                                        block_start,
                                                                        block_end
                                                                        )))
        }
    } else {
        #pass
    }

    image_yaml <- paste0(article_dir,"/texor-figure-meta.yaml")
    yaml::write_yaml(x = figure_blocks, file = image_yaml)
    return(figure_blocks)
}

#' Figure block Reader
#'
#' @param article_dir path to the directory which contains tex article
#' @param fig_data a block of figure data
#' @param raw_data raw text extracted from LaTeX document
#' @param iterator current iterator position
#' @param start_pos start position of image lines in LaTeX document
#' @param end_pos end position of image lines in LaTeX document
#' @param ac_start_pos Relative start position of image lines in LaTeX document
#' @param ac_end_pos Relative end position of image lines in LaTeX document
#'
#' @return a block of figure data
#' @noRd
fig_block_reader <- function(article_dir,fig_data, raw_data, iterator, start_pos, end_pos, ac_start_pos, ac_end_pos){
    # block of figure_data with extra meta data
    f_block <- list()
    f_block$image_number <- iterator
    f_block$doc_start_line <- start_pos
    f_block$doc_end_line <- end_pos

    # raw figure lines
    f_block$data <- fig_data
    # is the figure a tikz image
    f_block$istikz <- find_tikz(fig_data)
    # is an algorithm environment (treated like an image)
    f_block$isalgorithm <- find_algorithm(fig_data)
    if (find_algorithm(fig_data)) {
        alg_lib <- extract_extra_lib(article_dir)
        f_block$alglib <- alg_lib
        f_block$image_count <- 1
        # caption
        f_block$caption <- extract_caption(fig_data)
        # label
        f_block$label <- extract_label(fig_data)
        alg_data <- remove_alg_caption(fig_data)
        f_block$algdata <- alg_data
        f_block$extension <- find_image_extension(article_dir, "" , is_tikz = TRUE)
        return(f_block)
    }
    if (find_tikz(fig_data)) {
        tikz_count_var <- tikz_count_var + 1
        # extract tikz libraries from RJwrapper
        tikz_lib <- extract_tikz_lib(article_dir)
        f_block$tikzlib <- tikz_lib
        # caption
        f_block$image_count <- 1
        f_block$caption <- extract_caption(fig_data)
        # label
        f_block$label <- extract_label(fig_data)
        # extract tikz image data
        f_block$tikzstyle <- extract_tikz_style(fig_data, article_dir, tikz_count_var)
        f_block$extension <- find_image_extension(article_dir, "" , is_tikz = TRUE)
    } else {
        f_block$image_count <- env_image_count(fig_data)
        f_block$image_pos <- env_image_position(fig_data)
        if (f_block$image_count > 1) {
            paths <- list()
            for(iterator in 1:f_block$image_count) {
                paths[iterator] <- extract_path(fig_data[f_block$image_pos[iterator]])
            }
            f_block$path <- unlist(paths)
            f_block$caption <- extract_caption(fig_data)
            f_block$label <- extract_label(fig_data)
            extensions <- list()
            for (iterator in 1:f_block$image_count) {
                extensions[iterator] <- find_image_extension(article_dir, f_block$path[iterator])
                if(xfun::file_ext(f_block$path[iterator]) == ""){
                    f_block$path[iterator] <- xfun::with_ext(f_block$path[iterator],f_block$extension[iterator])
                }
            }
            f_block$extension <- unlist(extensions)
        } else {
            f_block$caption <- extract_caption(fig_data)
            f_block$label <- extract_label(fig_data)
            f_block$path <- extract_path(fig_data[f_block$image_pos])
            f_block$extension <- find_image_extension(article_dir,f_block$path)
            if(xfun::file_ext(f_block$path) == ""){
                f_block$path <- xfun::with_ext(f_block$path,f_block$extension)
            }
        }
    }
    return(f_block)
}

#' @title patch figure environments
#' @description This function calls the stream editor to change
#' figure* to figure
#' 1. figure*
#'
#' @param article_dir path to the directory which contains tex article
#' @param with_alg to include algorihtm environment or not
#'
#' @return patches figure environments in LaTeX file and also backs up the old file before modification
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::patch_figure_env(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
patch_figure_env <- function(article_dir, with_alg = TRUE) {
    article_dir <- xfun::normalize_path(article_dir)
    # find tex file
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # read Lines
    raw_lines <- readLines(file_path)
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\begin\\{figure\\*\\}", "figure\\*", "figure")
    warning("Changed \\begin{figure\\*} to \\begin{figure}")
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\end\\{figure\\*\\}", "figure\\*", "figure")
    warning("Changed \\end{figure\\*} to \\end{figure}")

    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\begin\\{algorithmic}", "algorithmic", "algorithm")
    warning("Changed \\begin{algorithmic} to \\begin{algorithm}")
    raw_lines <- stream_editor(raw_lines,
                               "\\s*\\\\end\\{algorithmic}", "algorithmic", "algorithm")
    warning("Changed \\end{algorithmic} to \\end{algorithm}")

    if (with_alg) {
        raw_lines <- stream_editor(raw_lines,
                                   "\\s*\\\\begin\\{algorithm}", "algorithm", "figure")
        warning("Changed \\begin{algorithm} to \\begin{figure}")
        raw_lines <- stream_editor(raw_lines,
                                   "\\s*\\\\end\\{algorithm}", "algorithm", "figure")
        warning("Changed \\end{algorithm} to \\end{figure}")
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

#' handle figures
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the LaTeX file
#'
#' @return A block of figure data for better conversion.
#'
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::handle_figures(your_article_path,texor::get_texfile_name(your_article_path))
#' unlink(your_article_folder,recursive = TRUE)
handle_figures <- function(article_dir, file_name){
    patch_figure_env(article_dir,with_alg = FALSE)
    fig_data <- figure_reader(article_dir, file_name)
    seperate_multiple_figures(article_dir)
    for (fig_iter in seq_along(fig_data)) {
        if(fig_data[[fig_iter]]$isalgorithm) {
            fig_data[[fig_iter]] <- convert_algorithm(fig_data[[fig_iter]], article_dir)
        }
        if(fig_data[[fig_iter]]$istikz) {
            fig_data[[fig_iter]] <- convert_tikz(fig_data[[fig_iter]], article_dir)
        } else {
            #--pass
        }
    }
    pdf_to_png(article_dir)
    return(fig_data)
}

#' seperate multiple figures
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return writes new data to the file
#' @noRd
seperate_multiple_figures <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    raw_lines <- readLines(file_path)
    inc_gr_patt <- "\\s*\\\\includegraphics"
    breakpoints <- which(grepl(inc_gr_patt,raw_lines))
    for (iter in seq_along(breakpoints)){
        if(iter == length(breakpoints)){
            break
        }
        if (breakpoints[iter] == (breakpoints[iter+1]-1)){
            raw_lines[breakpoints[iter]] <- paste0(raw_lines[breakpoints[iter]],"\n \n")
        }
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
