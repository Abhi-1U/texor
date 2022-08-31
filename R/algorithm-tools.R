
find_algorithm <- function(fig_lines) {
    alg_image_start <- which(grepl("^\\s*\\\\begin\\{algorithm",
                                    fig_lines))
    alg_image_end <-  which(grepl("^\\s*\\\\end\\{algorithm",
                                   fig_lines))
    if(length(alg_image_end) == length(alg_image_start) &
       (!identical(alg_image_start,integer(0)))){
        return(TRUE)
    } else {
        #skip
    }
    return(FALSE)
}

#' @title convert algorithm2e diagrams to pdf,png
#' @description this function will create algorithm2e images to be included
#' in the RJ-web-article
#' @param alg_object the figure_block of algorithm
#' @param article_dir path to the directory which contains RJ article
#'
#' @return algorithm image
#' @export
convert_algorithm <- function(alg_object, article_dir) {
    if (alg_object$data[1] != "\\begin{algorithm}[H]"){
        alg_object$data[1] <- "\\begin{algorithm}[H]"
    }

    algorithm_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage[T1]{fontenc}",
        "\\usepackage{hyperref}",
        "\\newcommand{\\code}[1]{\\texttt{#1}}",
        "\\newcommand{\\R}{R}",
        "\\newcommand{\\pkg}[1]{#1}",
        "\\newcommand{\\CRANpkg}[1]{\\pkg{#1}}%",
        "\\newcommand{\\BIOpkg}[1]{\\pkg{#1}}",
        "\\usepackage{algorithm2e}",
        alg_object$alglib,
        "\\begin{document}",
        "\\nopagecolor",
        alg_object$data,
        "\\end{document}"
    )
    alg_file_name <- paste0(gsub(":","",alg_object$label),".tex")
    # convert the tex file into pdf
    alg_dir <- paste(article_dir,"alg",sep="/")
    alg_path <- paste(alg_dir,alg_file_name,sep="/")
    dir.create(alg_dir, showWarnings = FALSE)
    fileconn <- file(alg_path)
    writeLines(algorithm_template, fileconn)
    close(fileconn)
    tryCatch(tinytex::latexmk(alg_path, engine = "pdflatex"),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
             }
    )
    tinytex::latexmk(alg_path, engine = "pdflatex")
    # run pdf to png
    alg_png_file <- gsub(".pdf",".png",alg_file_name)
    tryCatch(texor::convert_to_png(gsub(".tex", ".pdf", alg_path)),
             error = function(c) {
                 c$message <- paste0(c$message, " (in ", article_dir , ")")
                 warning(c$message)
             }
    )

    alg_png_path <- gsub(
        ".tex", ".png", alg_path
    )
    web_alg_folder <- paste(article_dir,"web/alg",sep="/")
    web_alg_png_path <- paste0(web_alg_folder,"/",gsub(
        ".tex", ".png", alg_file_name
    ))
    if(! dir.exists(web_alg_folder)) {
        dir.create(web_alg_folder)
    }
    file.copy(alg_png_path, web_alg_png_path)
}
