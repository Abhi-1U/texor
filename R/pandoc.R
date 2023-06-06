#' check texor pandoc compatibility
#' minimum pandoc version required is 2.17
#' @return TRUE if v >= 2.17, else FALSE
#' @export
#'
#' @examples
#' rmarkdown::pandoc_version()
#'
#' texor::pandoc_version_check()
pandoc_version_check <- function(){
    current_version <- rmarkdown::pandoc_version()
    if (toString(current_version) != ""){
        version_list <- unlist(strsplit(toString(current_version),split = "\\."))
    }
    else {
        warning("Pandoc not installed !, please install pandoc >= v2.17 ")
        return(FALSE)
    }

    if (as.integer(version_list[1]) == 2 && as.integer(version_list[2]) >= 17) {
        return(TRUE)
    }
    if (as.integer(version_list[1]) > 2){
        return(TRUE)
    }
    else {
        return(FALSE)
    }
}
