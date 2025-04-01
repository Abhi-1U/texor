#' @title latex to web
#' @description automated function for converting a single RJarticle to web
#' @param dir directory path
#' @param log_steps Enable/Disable Logging of conversion steps
#' @param example for examples only by default keep it FALSE.
#' @param auto_wrapper automatically creates a wrapper if TRUE, else asks user. default value TRUE
#' @param temp_mode temp mode will convert the document in a temporary folder and keep the original
#' article untouched. default value = TRUE
#' @param web_dir option to create a new web directory, default FALSE
#' @param interactive_mode interactive mode for converting articles with options. default FALSE
#' @param autonumber_eq whether to autonumber the equations, default is FALSE
#' @param compile_rmd_in_temp This works only with a forked version of rjtools.
#' @param kable_tab converts to kable table instead of markdown tables, default is FALSE
#' @param fig_in_r whether to include figures in R code chunks, default is FALSE
#' Not recommended to use with CRAN or github version of the rjtools package. (default FALSE)
#' @note Use pandoc version greater than or equal to 3.1
#' @note Do not set example = TRUE param when working with conversions.
#' @note example param is set TRUE in example, to conform with CRAN check restrictions.
#' @return RJweb article document in /web folder
#'
#' @export
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::latex_to_web(your_article_path,log_steps = FALSE, example = TRUE, temp_mode =FALSE)
#' unlink(your_article_folder, recursive = TRUE)
latex_to_web <- function(dir,log_steps = TRUE, example = FALSE, auto_wrapper = TRUE,
                         temp_mode = TRUE, web_dir = FALSE, interactive_mode = FALSE,
                         autonumber_eq = FALSE, compile_rmd_in_temp = !temp_mode,
                         kable_tab = FALSE, fig_in_r = FALSE) {
    message(dir)
    if (!pandoc_version_check()) {
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=3.1"))
        return(FALSE)
    }
    else {
        #pass
    }
    dir <- xfun::normalize_path(dir)
    date <- Sys.Date()
    wrapper <- get_wrapper_type(dir,
                                auto_wrapper = auto_wrapper,
                                interactive_mode = interactive_mode) #wrapper file name
    file_name <- get_texfile_name(dir)
    # PHINNEY: patch abstract here
    patch_rnw_abstract(paste0(dir, "/", file_name))
    # temp mode
    if (temp_mode) {
        dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
        dir.create(your_article_folder_2 <- paste(your_article_folder, basename(dirname(dir)),sep = '/'))
        x <- file.copy(from = dir, to = your_article_folder_2,recursive = TRUE,)
        your_article_path <- paste(your_article_folder_2, basename(dir),"",sep = "/")
        on.exit(unlink(your_article_folder, recursive = TRUE))
        # run latex to web recursively on a temp folder
        x <- tryCatch(texor::latex_to_web(your_article_path,
                                          auto_wrapper = auto_wrapper,
                                          temp_mode = FALSE,
                                          web_dir = web_dir,
                                          interactive_mode = interactive_mode,
                                          compile_rmd_in_temp = compile_rmd_in_temp,
                                          kable_tab = kable_tab,
                                          fig_in_r = fig_in_r),
                      error = function(c) {
                          warning(c)
                      })
        all_files <- list.files(your_article_path)
        exculde_files <- c("*[.]bk$","*[.]tex$","*[.]yaml$","*[.]sty$","*[.]log$","*[.]txt$")
        for (exp in exculde_files) {
            all_files <- all_files[!grepl(exp,all_files)]
        }
        y <- file.copy(from = paste(your_article_path,all_files,sep = "/"),
                       to = xfun::normalize_path(dir),
                       recursive = TRUE,
                       )
        if (!compile_rmd_in_temp) {
            message(paste0("Knitting Rmd to html"))
            texor::produce_html(dir, web_dir = web_dir, interactive_mode = interactive_mode)
        }
        return(TRUE)
    }
    if (log_steps) {
        log_file <- paste0("texor-log-",date,".log")
        log_setup(dir, log_file, "texor" ,2)
        texor_log(paste0("working directory : ", dir), "info", 2)
        texor_log(paste0("file name : ", file_name), "info", 2)
        # Step - 0 : Set working directory
        #            pdf directory
        # Step - 1 : Include Meta-fix style file
        wrapper <- get_wrapper_type(dir, auto_wrapper = auto_wrapper)
        texor_log(paste0("Stage-01 | ", "including Style File to : ", wrapper), "info", 2)
        texor_log(paste0("Stage-01 | ", "Style File :  Metafix.sty"), "info", 2)
        include_style_file(dir)
        texor_log(paste0("Stage-01 | ", "Included Style File :  Metafix.sty"), "info", 2)
        # Step - 2 : Manage Bibliography(ies)
        texor_log(paste0("Stage-02 | ", "aggregating Bibliography using :  rebib"), "info", 2)
        texor_log(paste0("Stage-02 | ", "Check rebib logs for more info"), "info", 2)
        rebib::aggregate_bibliography(dir)
        log_setup(dir, log_file, "texor", 2)
        # Step - 3 : patch code environments to verbatim
        texor_log(paste0("Stage-03| ","Patching Code Env"), "info", 2)
        patch_code_env(dir)
        texor_log(paste0("Stage-03 | ","Patched Code Env"), "info", 2)
        # Step - 4 : patch custom table environments to table
        texor_log(paste0("Stage-04 | ","Patching Table Env"), "info", 2)
        patch_table_env(dir)
        texor_log(paste0("Stage-04 | ","Patched Table Env"), "info", 2)
        # Step - 5 : Check for PDF and then convert
        #            PDF to PNG based on condition
        texor_log(paste0("Stage-05 | ","converting Images to png"), "info", 2)
        data <- handle_figures(dir, file_name)
        texor_log(paste0("Stage-05 | ","converted pdf files to png"), "info", 2)
        # Step - 5.5 : patch math or  latex commands
        patch_equations(dir)
        # Step - 6 : patch figure environments to figure
        texor_log(paste0("Stage-06 | ","Patching Figure Env"), "info", 2)
        patch_figure_env(dir)
        texor_log(paste0("Stage-06 | ","Patched Figure Env"), "info", 2)
        # Step - 7 : Convert to markdown + find package
        #            references
        texor_log(paste0("Stage-07 | ","Converting LaTeX to Markdown"), "info", 2)
        meta <- pre_conversion_statistics(dir)
        convert_to_markdown(dir, autonumber_eq = autonumber_eq, kable_tab = kable_tab, fig_in_r = fig_in_r)
        texor_log(paste0("Stage-07 | ","Converted LaTeX to Markdown"), "info", 2)
        # Step - 8 : Create a new directory and copy
        #            dependent files/folders
        texor_log(paste0("Stage-08 | ","Copying Dependencies to /web"), "info", 2)
        if (web_dir) {
            copy_other_files(dir)
        }
        texor_log(paste0("Stage-08 | ","Copied Dependencies to /web"), "info", 2)
        # Step - 9 : generate R markdown file with
        #             metadata from DESCRIPTION, tex file
        #             and file path
        # Note : the below function will work on any article, However ideally it needs a
        # folder structure similar to RJournal style /YYYY-ZZ/YYYY-MMM where
        # YYYY is the year, ZZ is the Journal issue number and MMM is the DOI
        # referral(unique article number).
        texor_log(paste0("Stage-09 | ","Creating R-markdown File to /web"), "info", 2)
        texor::generate_rmd(dir,web_dir = web_dir, interactive_mode = interactive_mode)
        texor_log(paste0("Stage-09 | ","Created R-markdown File to /web"), "info", 2)
        # Step - 10 : produce html (using rj_web_article) format
        if (compile_rmd_in_temp) {
            texor_log(paste0("Stage-10 | ","Knitting Rmd to html"), "info", 2)
            texor::produce_html(dir, web_dir = web_dir, interactive_mode = interactive_mode)
            texor_log(paste0("Stage-10 | ","Knitted Rmd to html"), "info", 2)
        }
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
    else{
        wrapper <- get_wrapper_type(dir, auto_wrapper = auto_wrapper) #wrapper file name
        include_style_file(dir) # Step 1
        rebib::aggregate_bibliography(dir) # Step 2
        patch_code_env(dir) # Step 3
        patch_table_env(dir) # Step 4
        data <- handle_figures(dir, file_name) # Step 5
        patch_equations(dir) # Step 5.5
        patch_figure_env(dir) # Step 6
        meta <- pre_conversion_statistics(dir) # Step 6.5
        if (example) {
            if (web_dir) {
                copy_other_files(dir) # Step 8
            }
            convert_to_markdown(dir, autonumber_eq = autonumber_eq, kable_tab = kable_tab, fig_in_r = fig_in_r) # Step 7
            texor::generate_rmd(dir,web_dir = web_dir) # Step 9
            if (compile_rmd_in_temp) {
            texor::produce_html(dir,example = TRUE, web_dir = web_dir,
                                interactive_mode = interactive_mode) # Step 10
            }
        }
        else {
            if (web_dir) {
                copy_other_files(dir) # Step 8
            }
            convert_to_markdown(dir, autonumber_eq = autonumber_eq, kable_tab = kable_tab, fig_in_r = fig_in_r) # Step 7
            texor::generate_rmd(dir,web_dir = web_dir,
                                interactive_mode = interactive_mode) # Step 9
            if (compile_rmd_in_temp) {
                texor::produce_html(dir,web_dir = web_dir,
                                interactive_mode = interactive_mode) # Step 10
            }
        }

        return(TRUE)
    }
}
#' @title conversion function with logging success and failure
#' @description When applied to a list of article_dirs it will also log
#' successful and failed slugs.
#' @param dir absolute path to the article_dir
#' @param log_steps Enable/Disable Logging of conversion steps
#' @return Log file
#' @noRd
convert_to_html <- function(dir,log_steps = TRUE) {
    wd <- getwd()
    dir <- xfun::normalize_path(dir)
    if(log_steps){
        texor::log_setup(wd,"texor-conversions.log","texor-stats",2)
    }
    else{
        message("Logging Disabled")
    }
    x<- FALSE
    x<- tryCatch(texor::latex_to_web(dir),
                 error = function(c) {
                     c$message <- paste0(c$message, " (in ", basename(dir), ")")
                     if(log_steps){
                         logger::log_error(paste("Conversion Failed",basename(dir), sep=" "),namespace = "texor-stats")
                         logger::log_error(paste(c$message, sep=" "),namespace = "texor-stats")
                     }
                     else{
                         warning(c$message)
                     }
                 }
    )
    if (identical(x,TRUE)) {
        if(log_steps){
            logger::log_success(paste("Conversion Success", basename(dir), sep=" "),namespace = "texor-stats")
        }
        else{
            message(paste("Conversion Success", basename(dir), sep=" "))
        }
    }
}
