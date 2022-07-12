#' Include a Metafix.sty style file
#'
#' This style file helps texor and pandoc to retain metadata and ease
#' the conversion process.
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export Metafix.sty file in the article_dir
#'
#' @examples
#' texor::include_style_file(".")
include_style_file <- function(article_dir) {
    # Copy over the Metafix.sty file to the article directory
    file.copy(
            system.file("extdata/style/Metafix.sty", package = "texor"),
            file.path(article_dir),
        )
    # Modify LaTeX source to include the \include(Metafix) line
    file_name <- get_wrapper_type(article_dir)
    wrapper_file <- readLines(file.path(article_dir, file_name))
    doc_start <- which(grepl("^\\s*\\\\begin\\{document\\}", wrapper_file))
    before_doc_start <- setdiff(wrapper_file[seq_len(doc_start) - 1], "\n")
    after_doc_start <- setdiff(wrapper_file, before_doc_start)
    usepackage_metafix <- "\\usepackage{Metafix}"
    # Backup original wrapper file
    file.rename(file_name, paste(file_name, ".bk", sep = ""))
        # write to original wrapper file
        xfun::write_utf8(c(
            before_doc_start,
            usepackage_metafix,
            "",
            after_doc_start),
            paste(file_name, ".new", sep = ""))
        #
        file.rename(paste(file_name, ".new", sep = ""), file_name)
}

#' convert latex(wrapper) file to markdown
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export creates a converted markdown file
#'
#' @examples
#' texor::convert_to_markdown(".")
convert_to_markdown <- function(article_dir) {
    input_file <- get_wrapper_type(article_dir)
    print(input_file)
    abs_file_path <- tools::file_path_as_absolute(article_dir)
    md_file <- paste(toString(tools::file_path_sans_ext(input_file)),
                             ".md", sep = "")
    print(md_file)
    bib_filter <- system.file(
                "extdata/filters/bib_filter.lua", package = "texor")
    code_block_filter <- system.file(
                "extdata/filters/code_block_filter.lua", package = "texor")
    image_filter <- system.file(
                "extdata/filters/image_filter.lua", package = "texor")
    post_tikz_filter <- system.file(
                "extdata/filters/reinstate_tikz_filter.lua", package = "texor")
    knitr_filter <- system.file(
                "extdata/filters/knitr_filter.lua",package = "texor")
    pandoc_opt <- c("-s",
                  "--resource-path", abs_file_path,
                  "--lua-filter", bib_filter,
                  "--lua-filter", image_filter,
                  "--lua-filter", code_block_filter,
                  "--lua-filter", knitr_filter,
                  "--lua-filter", post_tikz_filter)
    #output_format<-"markdown-simple_tables-multiline_tables-pipe_tables"
    output_format <- "markdown-simple_tables-pipe_tables"
    # This will generate a markdown file with YAML headers.
    rmarkdown::pandoc_convert(input_file,
                              from = "latex",
                              to = output_format,
                              options = pandoc_opt,
                              output = md_file,
                              citeproc = TRUE,
                              verbose = TRUE)
}

#' generate rmarkdown file in output folder
#'
#' @param markdown_file relative path along with name of the markdown file
#' @param volume volume number (int)
#' @param issue issue number (int)
#'
#' @return
#' @export .Rmd file in the /output folder
#'
#' @examples
#' texor::generate_rmd("/2009-05/RJwrapper.md", 1, 1)
generate_rmd <- function(markdown_file, volume, issue) {
    metadata <- rmarkdown::yaml_front_matter(markdown_file)
    metadata$abstract <- metadata$author[2]
    #metadata$emails<-metadata$author[3:length(metadata$author)]
    metadata$author <- lapply(
            strsplit(metadata$address, "\\\n", fixed = TRUE),
            function(person) {
                author <- list(
                    name = person[1],
                    affiliation = person[2]
                )
                if (any(orcid <- grepl("^ORCiD:", person))) {
                    author$orcid <- sub("^ORCiD: ", "", person[orcid])
                }
                if (any(email <- grepl("^email:", person))) {
                    author$email <- sub("^email:", "", person[email])
                }
                #if(!is.na(metadata$emails)){
                #    author$email <- metadata$emails[2+person]
                #}
                fields <- logical(length(person))
                fields[1:2] <- TRUE
                if (any(address <- !(fields | orcid | email))) {
                    author$address <- person[address]
                }
                author
            }
        )
    metadata$address <- NULL

    # article metadata from DESCRIPTION
    article_metadata <- if (file.exists(file.path(
            dirname(markdown_file), "DESCRIPTION"))) {
        art <- load_article(file.path(dirname(markdown_file), "DESCRIPTION"))
        online_date <- Filter(function(x) x$status == "online", art$status)
        online_date <- if (length(online_date) == 0) {
            Sys.Date()
        } else {
            online_date[[1]]$date
        }

        list(
            slug = dirname(markdown_file),
            acknowledged = Filter(function(x) x$status == "acknowledged", art$status)[[1]]$date,
            online = online_date
        )

    } else {
        # Produce minimal article metadata for news
        issue_year <- volume + 2008
        issue_month <- if (issue_year < 2022) issue * 6 else issue * 3
        list(
            slug = basename(dirname(markdown_file)),
            online = as.Date(paste(volume + 2008,
            issue_month, "01"), format = "%Y %m %d")
        )
    }
    front_matter <- list(
        title = metadata$title,
        abstract = metadata$abstract, #%||% paste0('The "', metadata$title, '" article from the ', issue_year, '-', issue, ' issue.'),
        author = metadata$author,
        date = format_non_null(article_metadata$online),
        date_received = format_non_null(article_metadata$acknowledged),
        journal = list(
            firstpage = article_metadata$pages[1],
            lastpage = article_metadata$pages[2]
        ),
        volume = as.integer(volume),
        issue = as.integer(issue),
        slug = article_metadata$slug,
        packages = list(
            cran = article_metadata$CRANpkgs,
            bioc = article_metadata$BIOpkgs
        ),
        preview = "preview.png",
        bibliography = metadata$bibliography,
        CTV = article_metadata$CTV_rev,
        output = list(
            `rjtools::rjournal_web_article` = list(
                self_contained = FALSE,
                toc = FALSE,
                legacy_pdf = TRUE
            )
            # `rjtools::rjournal_pdf_article` = pdf_args
        )
    )
    pandoc_md_contents <- readLines(markdown_file)
    delimiters <- grep("^(---|\\.\\.\\.)\\s*$", pandoc_md_contents)
    article_body <- c()
    if (delimiters[1] > 1)
        article_body <- c(article_body, pandoc_md_contents[1:delimiters[1] - 1])
    if (delimiters[2] < length(pandoc_md_contents))
        article_body <- c(article_body, pandoc_md_contents[-(1:delimiters[2])])

    input_file <- basename(markdown_file)
    template_path <- paste(find.package("texor"),
                "extdata/template/rmd-style-markdown.template", sep = "/")
    output_file_name <- paste(dirname(markdown_file),
                                    "/output/",
                            toString(tools::file_path_sans_ext(input_file)),
                                    ".Rmd", sep = "")
    dir.create(dirname(output_file_name), showWarnings = FALSE)
    xfun::write_utf8(
        c("---", yaml::as.yaml(front_matter), "---", article_body),
        output_file_name)
}
