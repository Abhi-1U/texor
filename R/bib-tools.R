#' @title function to solve bibliography problems(legacy)
#' @description
#' if bibliography exists in bibtex format then (filename.bib) bibtex file will
#' be preferred.
#' else this function will generate a minimal bibliography
#' @note Use rebib package to use the convertor, aggregator. This is a
#' legacy parser with limited support and features
#' @param article_dir path to the directory which contains tex article
#'
#' @return bibliography links the bibtex file with latex source code or
#' generates a minimal bibtex file from embedded bibliography and links that
#' file to the latex file
#' @export
handle_bibliography <- function(article_dir) {
    # checking for RJwrapper and fetching the file name for tex file
    old_wd <- getwd()
    setwd(article_dir)
    file_name <- get_texfile_name(article_dir)
    bib_file <- get_bib_file(article_dir, file_name)
    if (! identical(bib_file, "")) {
            link_bibliography_line(article_dir, file_name)
    } else {
        print("will need to convert bbl to .bib")
        bib_items <- extract_embeded_bib_items(article_dir, file_name)
        bibtex_data <- bib_handler(bib_items)
        make_bibtex_file(bibtex_data, file_name)
        link_bibliography_line(article_dir, file_name)
    }
    on.exit(setwd(old_wd), add = TRUE)
}

#' writes bibtex data in a structured format to the .bib file
#'
#' @param bibtex_data a list of minimal bibtex data
#' @param file_name name of the tex file
#'
#' @return bibtex_file a bibtex file is generated
#' @export
make_bibtex_file <-function(bibtex_data,file_name) {
    bib_file_name <- gsub(".tex", ".bib", file_name)
    for (iterator in seq_along(bibtex_data[["book"]])){
        unique_id <- bibtex_data[["book"]][[iterator]]$unique_id
        #print(unique_id)
        author <- bibtex_data[["book"]][[iterator]]$author
        title <- bibtex_data[["book"]][[iterator]]$title
        #print(author)
        #print(title)
        line1 <- sprintf("@book{ %s,", unique_id)
        line2 <- sprintf("author = %s,", author)
        line3 <- sprintf("title = %s", title)
        line4 <- sprintf("}")
        write_external_file(bib_file_name,"a",toString(line1))
        write_external_file(bib_file_name,"a",toString(line2))
        write_external_file(bib_file_name,"a",toString(line3))
        write_external_file(bib_file_name,"a",toString(line4))
    }
}



#' applies minimal bibliography over bib entries generated
#'
#' which are extracted by the extraction function.
#' @param bib_items bib entries extracted from extraction function
#'
#' @return bbl_record nested list
#' @export
bib_handler <- function(bib_items) {
    bbl_record <- list()
    # applies minimal bibliography to all the items
    bbl_record$book <- lapply(bib_items, function(entry) {
                                    bib_content <- minimal_bibliography(entry)
                                    book <- list(
                                        unique_id = bib_content$unique_id,
                                        author = bib_content$author,
                                        title = bib_content$title)
                                    book
                                    }
                              )
    return(bbl_record)
}

#' Main parser logic for converting inbuilt latex bibliography
#'
#' A function meant to be used internally or in a sequence as described
#' in the documentation
#' @param single_bib_data a single block of bibliographic entries
#'
#' @return bib_record with unique_id, author and
#'         title(also containing other info)
#' @export
minimal_bibliography <- function(single_bib_data) {
    bib_record <- list()
    # minimal bibliography parser / generator
    if (which(grepl("\\}$", single_bib_data)) ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))) {
        start_idx <- which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))
        # start_idx =1
        bib_record$unique_id <- str_split(str_split(gsub("\\\\bibitem\\[|\\]",
         "", single_bib_data[start_idx]), "\\{")[[1]][2], "\\}")[[1]][1]
        break_points <- which(grepl("\\\\newblock", single_bib_data))
        # author_names
        # difference between start of identifier and authors = 2
        if ((break_points[1] - start_idx) == 2) {
            bib_record$author <- paste("{{", gsub("\\.$", "",
                                        single_bib_data[start_idx + 1]), "}}")
        }
        # difference between start of identifier and authors = 3
        if ((break_points[1] - start_idx) == 3) {
            bib_record$author <- paste("{{", gsub("\\.$", "",
                            single_bib_data[start_idx + 1]),
                        gsub("\\.$", "", single_bib_data[start_idx + 2]), "}}")
        }
    }
    if ((which(grepl("\\}$", single_bib_data)) - 1) ==
        which(grepl("^\\s*\\\\bibitem\\[", single_bib_data))) {
        start_idx <- which(grepl("\\}$", single_bib_data))
        bib_record$unique_id <- gsub("\\}$", "",
                    str_split(single_bib_data[start_idx], "\\{")[[1]][2])
        break_points <- which(grepl("\\\\newblock", single_bib_data))
        # difference between start of identifier and authors = 2
        if ((break_points[1] - start_idx) == 2) {
            bib_record$author <- paste("{{",
                        gsub("\\.$", "", single_bib_data[start_idx + 1]), "}}")
        }
        # difference between start of identifier and authors = 3
        if ((break_points[1] - start_idx) == 3) {
            bib_record$author <- paste("{{",
                            gsub("\\.$", "", single_bib_data[start_idx + 1]),
                            gsub("\\.$", "", single_bib_data[start_idx + 2]),
                            "}}")
        }
    }
    remaining_data <- single_bib_data[break_points[1]:length(single_bib_data)]
    latex_macros <- list(
                        "^\\bibitem",
                        "newblock",
                        "emph",
                        "\\penalty0",
                        "\\url",
                        "\\{",
                        "\\}",
                        "\\\\"
                        )
    filtered_data <- remaining_data
    for (line in seq_along(remaining_data)) {
        filtered_data[line] <- remaining_data[line]
        for (patt in latex_macros){
            if (patt == "newblock"){
                filtered_data[line] <- gsub(patt, "", filtered_data[line])
            } else {
                filtered_data[line] <- gsub(patt, "", filtered_data[line])
            }

        }
    }
    title_line <- "{{"
    for (line in filtered_data) {
        title_line <- paste(title_line, line)
    }
    title_line <- paste(title_line, "}}")
    #print(filtered_data)
    bib_record$title <- title_line
    return(bib_record)
}

#' export embedded bibliography to a bbl file
#' @description
#' This function will extract the embedded bibliography and store it in .bbl
#' file
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the tex file
#'
#' @return bbl_file
#' @export
export_embeded_bibliography <- function(article_dir, file_name) {
    src_file_data <- readLines(file.path(article_dir, file_name))
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                    src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}",
                    src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    bbl_file_name <- gsub(".tex", ".bbl", file_name)
    write_external_file(bbl_file_name, "w", bbl_data)
}

#' extract the bibliography in chunks seperated at bibitem
#'
#' @description intended to be an internal function which is used with other
#'  functions in flow.
#' @param article_dir path to the directory which contains tex article
#' @param file_name name of the file
#'
#' @return a list of bib entries separated at bibitem
#' @export
extract_embeded_bib_items <- function(article_dir, file_name){
    src_file_data <- readLines(file.path(article_dir, file_name))
    bbl_start <- which(grepl("^\\s*\\\\begin\\{thebibliography\\}",
                        src_file_data))
    bbl_end <- which(grepl("^\\s*\\\\end\\{thebibliography\\}", src_file_data))
    bbl_data <- src_file_data[bbl_start:bbl_end]
    ##
    bib_breakpoints <- which(grepl("^\\s*\\\\bibitem\\[", bbl_data))
    bib_items <- list()
    # creating chunks of bibliography entries
    for (i in 1:(length(bib_breakpoints) - 1)) {
        bib_items[length(bib_items)+1] <- list(bbl_data[(bib_breakpoints[i]):(bib_breakpoints[(i+1)]-1)])
        if (i == (length(bib_breakpoints) - 1)) {
            bib_items[length(bib_items)+1] <- list(bbl_data[(bib_breakpoints[i+1]+1):length(bbl_data)-1])
        }
    }
    return(bib_items)
}

#' append the tex file with a line to link bibliography
#'
#' @param article_dir path to the directory which contains tex article
#' @param file_name file name of the tex document
#'
#' @return appends the tex file with a line to link bibliography
#' @export
link_bibliography_line <- function(article_dir, file_name) {
    src_file_data <- readLines(file.path(article_dir, file_name))
    bib_exist <- FALSE
    for (line in src_file_data) {
        if (grepl("^\\\\bibliography", line)) {
            bib_exist <- TRUE
            break
        }
    }
    if (bib_exist) {
        print("\\bibliography{bib_file} exists!")
        return("")
    } else {
        bib_line <- paste("\\bibliography{",
                toString(tools::file_path_sans_ext(file_name)), "}", sep = "")
    }
    # Backup original wrapper file
    backup_file <- paste(file_name,".bk",sep="")
    write_external_file(backup_file, "w", src_file_data)
    # write to original wrapper file
    write_external_file(file_name, "a", bib_line)
}
