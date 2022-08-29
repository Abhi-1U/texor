
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

convert_algorithm <- function(alg_object, article_dir) {
    algorithm_template <- c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        fig_block$alglib,
        "\\begin{document}",
        "\\nopagecolor",
        fig_block$data,
        "\\end{document}"
    )
    alg_file_name <- paste0(gsub(":","",x$label),".tex")
    # convert the tex file into pdf
    alg_dir <- paste(article_dir,"alg",sep="/")
    alg_path <- paste(alg_dir,alg_file_name,sep="/")
    dir.create(alg_dir, showWarnings = FALSE)
    fileconn <- file(alg_path)
    writeLines(algorithm_template, fileconn)
    close(fileconn)

    tinytex::latexmk(alg_path, engine = "pdflatex")
    # run pdf to png
    alg_png_file <- gsub(".pdf",".png",alg_file_name)
    texor::convert_to_png(gsub(
        ".tex", ".pdf", alg_path
    ))
    web_alg_folder <- paste(article_dir,"web/alg",sep="/")
    if(! dir.exists(web_alg_folder)) {
        dir.create(web_alg_folder)
    }
    file.copy(alg_path, paste0(article_dir,"/web/alg/",alg_png_file))
}
