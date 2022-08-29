
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
