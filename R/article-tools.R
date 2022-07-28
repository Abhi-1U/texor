#' @title Include Style file
#'
#' @description
#' Includes the Metafix.sty style file
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @details
#' This style file helps texor and pandoc to retain metadata and ease
#' the conversion process.
#' @usage
#' This function is used to include a custom style file (containing
#' useful macros for conversion) in the wrapper file
#'
#' @export Metafix.sty file in the article_dir
#'
#' @example
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' texor::include_style_file(article_dir)
include_style_file <- function(article_dir) {
    # Copy over the Metafix.sty file to the article directory
    file.copy(
            system.file("extdata/style/Metafix.sty", package = "texor"),
            file.path(article_dir),
        )
    # Modify LaTeX source to include the \include(Metafix) line
    file_name <- get_wrapper_type(article_dir)
    wrapper_file_content <- readLines(file.path(article_dir, file_name))
    wrapper_path <- paste(article_dir, file_name, sep = "/")
    doc_start <- which(grepl("^\\s*\\\\begin\\{document\\}",
                        wrapper_file_content))
    before_doc_start <- wrapper_file_content[1:(doc_start - 1)]
    after_doc_start <- wrapper_file_content[doc_start:
                            length(wrapper_file_content)]
    usepackage_metafix <- "\\usepackage{Metafix}"
    # Backup original wrapper file
    file.rename(wrapper_path, paste(wrapper_path, ".bk", sep = ""))
    # write to original wrapper file and save it as .new
    xfun::write_utf8(c(
            before_doc_start,
            usepackage_metafix,
            "",
            after_doc_start),
            paste(wrapper_path, ".new", sep = ""))
    # remove .new from extension
    file.rename(paste(wrapper_path, ".new", sep = ""), wrapper_path)
}

#' @title convert LaTeX wrapper to markdown
#' @details
#' convert latex(wrapper) file to markdown
#'
#' @param article_dir path to the directory which contains tex article
#' @description
#' Uses pandoc along with several lua filters
#' found at inst/extdata/filters in texor package
#' @note  pandoc (along with lua interpreter) is already installed with
#'  R-studio, hence if not using R-studio you will need to install pandoc.
#'  https://pandoc.org/installing.html
#' @export creates a converted markdown file, as well as a pkg_meta.yaml file
#'
#' @example
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' texor::convert_to_markdown(article_dir)
convert_to_markdown <- function(article_dir) {
    # wrapper file name
    input_file <- get_wrapper_type(article_dir)
    print(input_file)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    print(input_file_path)
    abs_file_path <- dirname(input_file_path)
    print(abs_file_path)
    # markdown equivalent filename
    md_file <- paste(toString(tools::file_path_sans_ext(input_file)),
                             ".md", sep = "")
    print(md_file)
    md_file_path <- paste(article_dir, md_file, sep = "/")
    print(md_file_path)
    # a filter to remove embedded bibliography (if any)
    bib_filter <- system.file(
                "extdata/filters/bib_filter.lua", package = "texor")
    # a filter to handle Sinput/Soutput/Scode/example/example*
    code_block_filter <- system.file(
                "extdata/filters/R_code.lua", package = "texor")
    # renames pdf images to png images (to be used with pdf_to_png())
    image_filter <- system.file(
                "extdata/filters/image_filter.lua", package = "texor")
    # replaces tikz placeholder with tikz image data/metadata
    post_tikz_filter <- system.file(
                "extdata/filters/reinstate_tikz_filter.lua", package = "texor")
    # converts markdown images to knitr image blocks for R-markdown
    #knitr_filter <- system.file(
    #            "extdata/filters/knitr_filter.lua", package = "texor")
    # enables table numbering in captions
    table_filter <- system.file(
        "extdata/filters/table_caption.lua", package = "texor")
    # enables figure numbering in captions
    figure_filter <- system.file(
        "extdata/filters/image_caption.lua", package = "texor")
    stat_filter <- system.file(
        "extdata/filters/conversion_compat_check.lua", package = "texor")
    #math_filter <- system.file(
    #    "extdata/filters/math2svg.lua", package = "texor")
    pandoc_opt <- c("-s",
                  "--resource-path", abs_file_path,
                  "--lua-filter", bib_filter,
                  "--lua-filter", image_filter,
                  "--lua-filter", code_block_filter,
                  "--lua-filter", figure_filter,
                  "--lua-filter", table_filter,
                  "--lua-filter", stat_filter,
                  #"--lua-filter", math_filter,
                  "--lua-filter", post_tikz_filter)
    output_format <- "markdown-simple_tables-pipe_tables-fenced_code_attributes"
    # This will generate a markdown file with YAML headers.
    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = output_format,
                              options = pandoc_opt,
                              output = md_file_path,
                              citeproc = TRUE,
                              verbose = TRUE)
    # post conversion process
    tex_file_path <- paste(article_dir,
                            get_texfile_name(article_dir),
                            sep = "/")
    find_pkg_references(tex_file_path)
}

#' @title Modify Markdown to R-markdown
#'
#' @description
#' generate rmarkdown file in output folder
#'
#' @param article_dir path to the directory which contains tex article
#' @param volume volume number (int)
#' @param issue issue number (int)
#'
#'
#' @export R-markdown file in the web folder
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' texor::generate_rmd(article_dir)
generate_rmd <- function(article_dir) {
    volume <- 1 # placeholder value
    issue <- 1 # placeholder value
    journal_details <- get_journal_details(article_dir)
    volume <- journal_details$volume
    issue <- journal_details$issue
    markdown_file <- gsub(".tex", ".md", paste(article_dir,
                    texor::get_wrapper_type(article_dir), sep = "/"))
    metadata <- rmarkdown::yaml_front_matter(markdown_file)
    # reads the abstract from the second author field
    # reason : abstract is patched as author in metafix.sty
    metadata$abstract <- metadata$author[2]
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
                #if (!is.na(metadata$emails)) {
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
            slug = journal_details$slug,
            acknowledged = Filter(function(x) x$status == "acknowledged", art$status)[[1]]$date,
            online = online_date
        )

    } else {
        # Produce minimal article metadata for news
        issue_year <- volume + 2008
        issue_month <- if (issue_year < 2022) issue * 6 else issue * 3
        list(
            slug = journal_details$slug,
            online = as.Date(paste(volume + 2008,
            issue_month, "01"), format = "%Y %m %d")
        )
    }
    # if article has no abstract
    if (toString(metadata$abstract) =="NA") {
        issue_year <- volume + 2008
        issue_month <- if (issue_year < 2022) issue * 6 else issue * 3
        metadata$abstract <- paste0("The '", metadata$title,
                                    "' article from the'", issue_year,
                                    "'-'", issue, "' issue.")
    }
    pkg_yaml_path <- paste(dirname(markdown_file), "pkg_meta.yaml", sep = "/" )
    front_matter <- list(
        title = metadata$title,
        abstract = metadata$abstract, #%||% ,
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
        packages = yaml::read_yaml(pkg_yaml_path),
        preview = "preview.png",
        bibliography = metadata$bibliography,
        CTV = article_metadata$CTV_rev,
        output = list(
            `rjtools::rjournal_web_article` = list(
                self_contained = FALSE,
                toc = FALSE,
                web_export = TRUE
                #legacy_pdf = FALSE
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
    output_file_name <- paste(dirname(markdown_file),
                                    "/web/",
                            toString(tools::file_path_sans_ext(input_file)),
                                    ".Rmd", sep = "")
    dir.create(dirname(output_file_name), showWarnings = FALSE)
    xfun::write_utf8(
        c("---", yaml::as.yaml(front_matter), "---", article_body),
        output_file_name)
}

#' @title convert LaTeX wrapper to native pandoc AST
#' @details
#' convert latex(wrapper) file to pandoc AST
#'
#' @param article_dir path to the directory which contains tex article
#' @description
#' Uses pandoc along with several lua filters
#' found at inst/extdata/filters in texor package
#' @note  pandoc (along with lua interpreter) is already installed with
#'  R-studio, hence if not using R-studio you will need to install pandoc.
#'  https://pandoc.org/installing.html
#' @export creates a converted markdown file, as well as a pkg_meta.yaml file
#'
#' @example
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' texor::convert_to_native(article_dir)
convert_to_native <- function(article_dir) {
    # wrapper file name
    input_file <- get_wrapper_type(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- tools::file_path_as_absolute(input_file_path)
    # markdown equivalent filename
    md_file <- paste(toString(tools::file_path_sans_ext(input_file)),
                     ".txt", sep = "")
    md_file_path <- paste(article_dir, md_file, sep = "/")
    # a filter to remove embedded bibliography (if any)
    bib_filter <- system.file(
        "extdata/filters/bib_filter.lua", package = "texor")
    # a filter to handle Sinput/Soutput/Scode/example/example*
    code_block_filter <- system.file(
        "extdata/filters/R_code.lua", package = "texor")
    # renames pdf images to png images (to be used with pdf_to_png())
    image_filter <- system.file(
        "extdata/filters/image_filter.lua", package = "texor")
    # replaces tikz placeholder with tikz image data/metadata
    post_tikz_filter <- system.file(
        "extdata/filters/reinstate_tikz_filter.lua", package = "texor")
    # converts markdown images to knitr image blocks for R-markdown
    knitr_filter <- system.file(
        "extdata/filters/knitr_filter.lua", package = "texor")
    # enables table numbering in captions
    table_filter <- system.file(
        "extdata/filters/table_caption.lua", package = "texor")
    #math_filter <- system.file(
    #    "extdata/filters/math2svg.lua", package = "texor")
    pandoc_opt <- c("-s",
                    "--resource-path", dirname(abs_file_path),
                    "--lua-filter", bib_filter,
                    "--lua-filter", image_filter,
                    "--lua-filter", code_block_filter,
                    "--lua-filter", knitr_filter,
                    "--lua-filter", table_filter,
                    #"--lua-filter", math_filter,
                    "--lua-filter", post_tikz_filter)
    output_format <- "native"
    # This will generate a markdown file with YAML headers.
    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = output_format,
                              options = pandoc_opt,
                              output = md_file_path,
                              citeproc = TRUE,
                              verbose = TRUE)
    # post conversion process
    tex_file_path <- paste(article_dir,
                           get_texfile_name(article_dir),
                           sep = "/")
    find_pkg_references(tex_file_path)
}
