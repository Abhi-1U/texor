#' @title patch equations
#' @description this function patches equations (particularly eqnarray)
#' @param article_dir path to the directory which contains tex article
#' @keywords internal
#' @return patches equations environments in LaTeX file and also backs up the old file before modification
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::patch_equations(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
patch_equations <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # find tex file
    file_name <- get_texfile_name(article_dir)
    file_path <- paste(article_dir, file_name, sep = "/")
    # readLines
    delimiter <- "$$"
    raw_lines <- readLines(file_path)
    eqn_arr_begin_bps <- which(grepl("\\s*\\\\begin\\{eqnarray\\}",raw_lines))
    eqn_arr_end_bps <- which(grepl("\\s*\\\\end\\{eqnarray\\}",raw_lines))
    if (length(eqn_arr_begin_bps) == length(eqn_arr_end_bps)) {
        for (iterator in seq_along(eqn_arr_begin_bps)) {
            begin_pos <- eqn_arr_begin_bps[iterator]
            end_pos <- eqn_arr_end_bps[iterator]
            mod_begin_line <- gsub("\\\\begin\\{eqnarray(\\*?)\\}", "\\n\\$\\$\\\\begin\\{eqnarray\\1\\}",raw_lines[begin_pos])
            mod_end_line <- gsub("\\\\end\\{eqnarray(\\*?)\\}", "\\\\end\\{eqnarray\\1\\}\\$\\$", raw_lines[end_pos])
            raw_lines[begin_pos] <- mod_begin_line
            raw_lines[end_pos] <- mod_end_line
        }
    } else {
        warning("The equations do not align")
    }
    src_file_data <- readLines(file_path)
    backup_file <- paste(file_path, ".bk", sep = "")
    write_external_file(backup_file, "w", src_file_data)
    # remove old tex file
    file.remove(file_path)
    # write same tex file with new data
    write_external_file(file_path, "w", raw_lines)
}
