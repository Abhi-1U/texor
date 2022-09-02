texor_orchestrate <- function(article_dir) {
    old_wd <- getwd()
    setwd(article_dir)
    article_dirs <- list.dirs(recursive = FALSE)
    for (dir in article_dirs) {
        latex_to_web(dir)
    }
    print(article_dir)
    on.exit(setwd(old_wd), add = TRUE)
}

#' @title latex to web
#' @description automated function for converting a single RJarticle to web
#' @param dir directory path
#'
#' @return RJweb article document
#'
#' @export
latex_to_web <- function(dir) {
    print(dir)
    dir <- normalizePath(dir)
    date <- Sys.Date()
    file_name <- get_texfile_name(dir)
    log_file <- paste0("texor-log-",date,".log")
    log_setup(dir, log_file, "texor" ,2)
    texor_log(paste0("working directory : ", dir), "info", 2)
    file_name <- get_texfile_name(dir)
    texor_log(paste0("file name : ", file_name), "info", 2)
    # Step - 0 : Set working directory
    #            pdf directory
    # Step - 1 : Include Meta-fix style file
    wrapper <- get_wrapper_type(dir)
    texor_log(paste0("Stage-01 | ", "including Style File to : ", wrapper), "info", 2)
    texor_log(paste0("Stage-01 | ", "Style File :  Metafix.sty"), "info", 2)
    include_style_file(dir)
    texor_log(paste0("Stage-01 | ", "Included Style File :  Metafix.sty"), "info", 2)
    # Step - 2 : Manage Bibliography(ies)
    texor_log(paste0("Stage-02 | ", "aggregating Bibliography using :  rebib"), "info", 2)
    texor_log(paste0("Stage-02 | ", "Check rebib logs for more info"), "info", 2)

    rebib::aggregate_bibliography(dir)
    log_setup(dir, log_file, "texor", 2)
    # Step - 3 : Check for PDF and then convert
    #            PDF to PNG based on condition
    texor_log(paste0("Stage-03 | ","converting Images to png"), "info", 2)
    data <- handle_figures(dir, file_name)
    texor_log(paste0("Stage-03 | ","converted pdf files to png"), "info", 2)
    # Step - 4 : patch code environments to verbatim
    texor_log(paste0("Stage-04 | ","Patching Code Env"), "info", 2)
    patch_code_env(dir)
    texor_log(paste0("Stage-04 | ","Patched Code Env"), "info", 2)
    # Step - 5 : patch custom table environments to table
    texor_log(paste0("Stage-05 | ","Patching Table Env"), "info", 2)
    patch_table_env(dir)
    texor_log(paste0("Stage-05 | ","Patched Table Env"), "info", 2)
    # Step - 5.5 : patch math or  latex commands
    patch_equations(dir)
    # Step - 6 : patch figure environments to figure
    texor_log(paste0("Stage-06 | ","Patching Figure Env"), "info", 2)
    patch_figure_env(dir)
    texor_log(paste0("Stage-06 | ","Patched Figure Env"), "info", 2)
    # Step - 7 : Check for Tikz images and pre-process
    #            it based on condition.
    # Step - 8 : Convert to markdown + find package
    #            references
    texor_log(paste0("Stage-07 | ","Converting LaTeX to Markdown"), "info", 2)
    meta <- pre_conversion_statistics(dir)
    convert_to_markdown(dir)
    texor_log(paste0("Stage-07 | ","Converted LaTeX to Markdown"), "info", 2)
    # Step - 9 : Create a new directory and copy
    #            dependent files/folders
    texor_log(paste0("Stage-08 | ","Copying Dependencies to /web"), "info", 2)
    copy_other_files(dir)
    texor_log(paste0("Stage-08 | ","Copied Dependencies to /web"), "info", 2)
    # Step - 10 : generate R markdown file with
    #             metadata from DESCRIPTION, tex file
    #             and file path
    # Note : the below function wont work on any article as it needs a
    #folder structure similar to RJournal style /YYYY-ZZ/YYYY-MMM where
    #YYYY is the year, ZZ is the Journal issue number and MMM is the DOI
    # referral(unique article number)
    texor_log(paste0("Stage-09 | ","Creating R-markdown File to /web"), "info", 2)
    texor::generate_rmd(dir)
    texor_log(paste0("Stage-09 | ","Created R-markdown File to /web"), "info", 2)
    # Step - 11 : produce html (using rj_web_article) format
    texor_log(paste0("Stage-10 | ","Knitting Rmd to html"), "info", 2)
    texor::produce_html(dir)
    texor_log(paste0("Stage-10 | ","Knitted Rmd to html"), "info", 2)
    post_data <- yaml::read_yaml(paste0(dir,"/post-conversion-meta.yaml"))
    if (post_data$text$words == 0) {
        texor_log(paste0("Pandoc produced an empty file"), "error", 2)
    } else {
        texor_log(paste0("Conversion Summary | ", "Pre-Conversion  ", "Post-Conversion"), "debug", 2)
        texor_log(paste0("Tables | ", post_data$table, "  ", meta$table), "debug", 2)
        texor_log(paste0("Figures | ", post_data$figure, "  ", meta$figure), "debug", 2)
        texor_log(paste0("Math | ", post_data$math, "  ", meta$math), "debug", 2)
        texor_log(paste0("Citations | ", post_data$citations, "  ", meta$citations), "debug", 2)
        texor_log(paste0("Code Inline | ", post_data$code$inline, "  ", meta$code$inline), "debug", 2)
        texor_log(paste0("Code Block | ", post_data$code$block, "  ", meta$code$block), "debug", 2)
    }
    return(TRUE)
}

#' @title conversion function with logging success and failure
#' @description When applied to a list of article_dirs it will also log
#' successful and failed slugs.
#' @param dir absolute path to the article_dir
#'
#' @return Log file
#' @export
convert_to_html <- function(dir) {
    wd <- getwd()
    dir <- normalizePath(dir)
    texor::log_setup(wd,"texor-conversions.log","texor-stats",2)
    x<- FALSE
    x<- tryCatch(texor::latex_to_web(dir),
                 error = function(c) {
                     c$message <- paste0(c$message, " (in ", basename(dir), ")")
                     logger::log_error(paste("Conversion Failed",basename(dir), sep=" "),namespace = "texor-stats")
                     logger::log_error(paste(c$message, sep=" "),namespace = "texor-stats")
                     warning(c$message)
                 }
    )
    if (identical(x,TRUE)) {
        logger::log_success(paste("Conversion Success", basename(dir), sep=" "),namespace = "texor-stats")
    }
}
