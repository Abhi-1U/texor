#' @title texor log setup
#' @description a wrapper function for logger package to set up log file for
#' logging
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the log file
#' @param namespace namespace of log file
#' @param idx index of log level
#' @return null
#' @export
#'
#' @examples
#'  wd <-  system.file("examples/article", package = "texor")
#'  texor::log_setup(wd, "log-file.log", "texor", 2)
log_setup <- function(article_dir, file_name, namespace, idx) {
    article_dir <- xfun::normalize_path(article_dir)
    log_file_path <- paste(article_dir, file_name, sep = "/")
    if(! file.exists(log_file_path)) {
        file.create(log_file_path,showWarnings = T)
    } else {
        #pass
    }
    logger::log_threshold(namespace = namespace, index = idx)
    logger::log_appender(logger::appender_file(log_file_path),
                         namespace = namespace,
                         index=idx)
}



#' @title log messages for various categories
#' @description a wrapper function for logging different types of log entries
#' @param message message to be sent
#' @param category category of the log message
#' @param idx index of log level
#' @return null
#' @export
#'
#' @examples
#'  wd <-  system.file("examples/article", package = "texor")
#' texor::log_setup(wd, "log-file.log", "texor" , 2)
#' texor::texor_log("Hello", "INFO", 2)
#' cat(readLines(paste(wd,"/log-file.log",sep="")),sep="\n")
texor_log <- function(message, category, idx) {
    if (identical(tolower(category), "info")) {
        logger::log_info(message, namespace = "texor")
        logger::log_appender(namespace = "texor",index = idx)
    }
    if (identical(tolower(category), "success")) {
        logger::log_success(message, namespace = "texor")
        logger::log_appender(namespace = "texor",index = idx)
    }
    if (identical(tolower(category), "warning")) {
        logger::log_warn(message, namespace = "texor")
        logger::log_appender(namespace = "texor",index = idx)
    }
    if (identical(tolower(category), "debug")) {
        logger::log_debug(message,namespace = "texor",index =2)
        logger::log_appender(namespace = "texor",index = idx)
    }
    if (identical(tolower(category), "error")) {
        logger::log_error(message, namespace = "texor")
        logger::log_appender(namespace = "texor",index = idx)
    }
    if (identical(tolower(category), "failure")) {
        logger::log_failure(message, namespace = "texor")
        logger::log_appender(namespace = "texor",index = idx)
    } else {
        #pass
        #logger::log_info(message)
    }
}

