#' Get the name of the wrapper file in the article dir
#'
#'This function gets the wrapper file name from the
#'commonly named R-Journal wrapper files.
#'@details
#'Usually the R journal wrapper files are named either
#'1. RJwrapper.tex
#'2. RJwrap.tex
#'3. wrapper.tex
#' @param article_dir path to the directory which contains tex article
#' @param auto_wrapper automatically creates a wrapper if TRUE, else asks user. default value FALSE
#' @param interactive_mode interactive mode for converting articles with options.
#' @return String with name of wrapper file or empty
#' @export
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::get_wrapper_type(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
get_wrapper_type <- function(article_dir, auto_wrapper = FALSE, interactive_mode = FALSE) {
    article_dir <- xfun::normalize_path(article_dir)
    article_files <- list.files(article_dir, recursive = FALSE)
    ignore_files <- basename(article_files) %in% c("RJournal.sty", "DESCRIPTION", "RJwrapper.pdf", "supplementaries.zip")
    top_dir <- function(x) {
        is_top <- dirname(x) == "."
        if (all(is_top)) return(x)
        x[!is_top] <- top_dir(dirname(x[!is_top]))
        x
    }
    ignore_dirs <- top_dir(article_files) %in% c("correspondence", "history")
    article_files <- article_files[!(ignore_files | ignore_dirs)]

    wrapper_types <- c("wrapper.tex",
                       "RJwrap.tex",
                       "RJwrapper.tex")
    wrapper_file <- ""
    for (w_type in wrapper_types) {
        if (file.exists(file.path(article_dir, w_type))) {
            wrapper_file <- w_type
        }
    }
    if (wrapper_file == "") {
        wrapper_file <- find_wrapper(article_dir)
    }
    if (wrapper_file == "") {
        #warning(" No Wrapper File Found in the article dir")
        if (!auto_wrapper) {
            cli::cli_alert_warning("Could not find RJwrapper.tex for {basename(article_dir)}, would you like to create a default one?")
            if (utils::menu(c("Yes", "No")) != 1) {
                cli::cli_abort("RJwrapper.tex not found for a legacy article, so article could not be migrated.")
            }
        }
        wrapper_input <- xfun::sans_ext(article_files[xfun::file_ext(article_files) == "tex"])
        if (interactive_mode && (length(wrapper_input) > 1)) {
            wrapper_input <- utils::select.list(xfun::with_ext(wrapper_input,"tex"), preselect = NULL, multiple = FALSE,
                    title = "Which is the main article file ?", graphics = getOption("menu.graphics"))
        }
        if (wrapper_input == ""){
            cli::cli_abort("Could not automatically identify appropriate article tex file. Check that exactly 1 input tex file exists for the wrapper.")
        }
        rjwrapper <- whisker::whisker.render(
            xfun::read_utf8(system.file("RJwrapper_template.tex", package = "texor")),
            data = list(article_input = sprintf("\\input{%s}", wrapper_input))
        )
        xfun::write_utf8(rjwrapper, file.path(article_dir, "RJwrapper.tex"))
        article_files <- c(article_files, "RJwrapper.tex")
    }
    for (w_type in wrapper_types) {
        if (file.exists(file.path(article_dir, w_type))) {
            wrapper_file <- w_type
        }
    }
    return(wrapper_file)
}


#' find wrapper file
#'
#' Finds a different named wrapper file for RJournal article
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return wrapper file name or empty string if none
#' @export
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::find_wrapper(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
find_wrapper <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    article_files <- list.files(article_dir, recursive = FALSE)
    latex_files <- article_files[grep(pattern = "[.]tex$", article_files)]
    for (article in latex_files) {
        data <- readLines(paste0(article_dir,"/",article))
        if (grepl(pattern = "\\\\documentclass\\[a4paper\\]", data[1])) {
            return(article)
        }
    }
    return("")
}
