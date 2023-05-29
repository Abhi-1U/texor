#' scans LaTeX document for any package references
#'
#' Usually this was handled by rjtools package, since the generated Rmd lacks
#' these commands (they are transformed to links during pandoc conversion), we
#' need to add one more step to pre-process.
#' @param input_file name of the input file
#'
#' @return pkg_meta.yaml file containing meta data
#' @noRd
find_pkg_references <- function(input_file){
    input_file <- xfun::normalize_path(input_file)
    input <- readLines(input_file)
    pkgs <- gregexpr("\\\\(CRAN|BIO)pkg\\{.+?\\}", input)
    pkgs <- mapply(
        function(pos, line) {
            if (pos[1] == -1) return(NULL)
            substr(rep_len(line, length(pos)),
                   pos, pos + pos%@%"match.length" - 1)
        },
        pkgs, input,
        SIMPLIFY = FALSE
    )
    pkgs <- unique(do.call(c, pkgs))
    pkg_is_cran <- grepl("^\\\\CRAN", pkgs)
    pkgs <- sub("\\\\(CRAN|BIO)pkg\\{(.+?)\\}$", "\\2", pkgs)
    message(paste0(
        "Detected the following packages from article:\n  ",
        "CRAN: ", paste0(pkgs[pkg_is_cran], collapse = ", "), "\n  ",
        "Bioconductor: ", paste0(pkgs[!pkg_is_cran], collapse = ", ")
    ))
    packages <- list(
        cran = pkgs[pkg_is_cran],
        bioc = pkgs[!pkg_is_cran]
    )
    pkg_yaml_path <- paste(dirname(input_file), "pkg_meta.yaml", sep = "/" )
    write_external_file(pkg_yaml_path, "w", yaml::as.yaml(packages))
}


