#' Copy Supporting Documents like images,bib file,etc.
#'
#' Copies supporting documents like images,pdf,bib files into the output
#' folder for building the HTML version of the R-Markdown file.
#'
#' @param from_path : String indicating base path for the working directory
#' @return none
#' @export
#'   copies dependency files into the output folder.
#'
copy_other_files <- function(from_path) {
    old_working_directory <- getwd()
    setwd(from_path)
    dir_list <- list.dirs(recursive = FALSE)
    possible_dirs <- c("*_files", "figures", "images", "tikz")
    target_dir <- basename(dir_list[grep(
        paste(possible_dirs, collapse = "|"), dir_list)])
    print(target_dir)
    dir.create("web/", showWarnings = FALSE)
    dir.create(paste("web/", target_dir, sep = ""),
                showWarnings = FALSE)
    file.copy(list.dirs(
            target_dir, full.names = TRUE),
            paste("web/", target_dir, sep = ""), recursive = TRUE)
    file_list <- list.files(recursive = FALSE)
    extensions <- c("*.png", "*.jpg", "*.bib", "*.pdf",
                    "*.tex", "*.R", "*.bbl")
    target_files <- unique(grep(paste(
            extensions, collapse = "|"), file_list, value = TRUE))
    print(target_files)
    file.copy(target_files,
              to = "web/",
              copy.mode = TRUE,
              recursive = FALSE, )
    setwd(old_working_directory)
}

#' call rmarkdown::render to generate html file
#'
#' @param input_file_path ; String path for the R-Markdown file
#'
#' @return none
#' @export HTML output
#'
#'
produce_html <- function(input_file_path) {
    rmarkdown::render(
        input = input_file_path)
}