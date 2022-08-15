#' Copy Supporting Documents like images,bib file,etc.
#'
#' Copies supporting documents like images,pdf,bib files into the output
#' folder for building the HTML version of the R-Markdown file.
#'
#' @param from_path : String indicating base path for the working directory
#' @return copies dependency files into the output folder.
#' @export
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
#' @param article_dir path to the directory which contains tex article
#'
#' @return HTML output
#' @export
produce_html <- function(article_dir) {
    input_file_path <- gsub(".tex", ".Rmd", paste(article_dir, "web",
                        texor::get_wrapper_type(article_dir), sep = "/"))
    rmarkdown::render(
        input = input_file_path)
}


find_file <- function(article_dir, src_path) {
    abs_img_path <- paste(article_dir,src_path,sep="/")
    main_dir <- dirname(abs_img_path)
    file_name <- basename(abs_img_path)
    all_files <- list.files(main_dir)
    target_extension <- ""
    for (file in all_files) {
        if (tools::file_path_sans_ext(file) == file_name) {
            ext <- tools::file_ext(file)
            if(ext == "pdf" | ext == "png" | ext == "jpg" | ext == "jpeg" ) {
                target_extension <- tools::file_ext(file)
            } else {
                #skip
            }
        }
    }
    return(target_extension)
}
