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
#' @return adds Metafix.sty file in the article_dir also includes it in RJwrapper file.
#' @export
#'
#' @examples
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::include_style_file(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
include_style_file <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # Remove the RJournal.sty file to the article directory
    rj_style_path <- paste0(article_dir,"/RJournal.sty")
    if (file.exists(rj_style_path)){
        file.remove(rj_style_path)
    }
    # Copy over the Metafix.sty file to the article directory
    file.copy(
            system.file("extdata/Metafix.sty", package = "texor"),
            file.path(article_dir),
        )
    # Modify LaTeX source to include the \include(Metafix) line
    file_name <- get_wrapper_type(article_dir)
    wrapper_file_content <- readLines(file.path(article_dir, file_name))
    wrapper_path <- paste(article_dir, file_name, sep = "/")
    if (!identical(which(grepl("\\usepackage\\{Metafix\\}",wrapper_file_content)),integer(0))) {
        return(NULL)
    }
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
#'  @note Use pandoc version greater than or equal to 2.17
#'
#' @return creates a converted markdown file, as well as a pkg_meta.yaml file
#' @export
#' @examples
#' # Note This is a minimal example to execute this function
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rmarkdown::pandoc_version()
#' texor::include_style_file(your_article_path)
#' rebib::aggregate_bibliography(your_article_path)
#' texor::convert_to_markdown(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
convert_to_markdown <- function(article_dir) {
    # wrapper file name
    article_dir <- xfun::normalize_path(article_dir)
    input_file <- get_wrapper_type(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- dirname(input_file_path)
    # markdown equivalent filename
    md_file <- paste(toString(tools::file_path_sans_ext(input_file)),
                     ".md", sep = "")
    md_file_path <- paste(article_dir, md_file, sep = "/")
    # a filter to remove new para character from article abstract
    abs_filter <- system.file(
        "abs_filter.lua", package = "texor")
    # a filter to remove embedded bibliography (if any)
    bib_filter <- system.file(
        "bib_filter.lua", package = "texor")
    # a filter to add r language param
    code_block_filter <- system.file(
        "R_code.lua", package = "texor")
    # renames pdf images to png images (to be used with pdf_to_png())
    image_filter <- system.file(
        "image_filter.lua", package = "texor")
    # enables table numbering in captions
    table_filter <- system.file(
        "table_caption.lua", package = "texor")
    # enables figure numbering in captions
    figure_filter <- system.file(
        "image_caption.lua", package = "texor")
    stat_filter <- system.file(
        "conversion_compat_check.lua", package = "texor")
    equation_filter <- system.file(
        "equation_filter.lua", package = "texor")
    bookdown_ref_filter <- system.file(
        "bookdown_ref.lua", package = "texor")
    pandoc_opt <- c("-s",
                    "--resource-path", abs_file_path,
                    "--lua-filter", abs_filter,
                    "--lua-filter", bib_filter,
                    "--lua-filter", image_filter,
                    "--lua-filter", code_block_filter,
                    "--lua-filter", figure_filter,
                    "--lua-filter", table_filter,
                    "--lua-filter", stat_filter,
                    "--lua-filter", equation_filter,
                    "--lua-filter", bookdown_ref_filter)
    output_format <- "markdown-simple_tables-pipe_tables-fenced_code_attributes"
    # This will generate a markdown file with YAML headers.
    if (! pandoc_version_check()){
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17\n","Please Install a newer version of pandoc to run texor"))
        return(FALSE)
    }
    else {
        #pass
    }
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
#' @note Use pandoc version greater than or equal to 2.17
#' @return R-markdown file in the web folder
#' @export
#' @examples
#' # Note This is a minimal example to execute this function
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir2"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <-  xfun::normalize_path(paste(your_article_folder,"article",sep="/"))
#' texor::include_style_file(your_article_path)
#' rebib::aggregate_bibliography(your_article_path)
#' data <- texor::handle_figures(your_article_path,
#'                     texor::get_texfile_name(your_article_path))
#' texor::patch_code_env(your_article_path) # Step 4
#' texor::patch_table_env(your_article_path) # Step 5
#' texor::patch_equations(your_article_path) # Step 5.5
#' texor::patch_figure_env(your_article_path)
#' rmarkdown::pandoc_version()
#' texor::convert_to_markdown(your_article_path)
#' texor::generate_rmd(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
generate_rmd <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    if (! pandoc_version_check()){
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17\n","Please Install a newer version of pandoc to run texor"))
        return(FALSE)
    }
    if (! check_markdown_file(article_dir)){
        warning(paste0("There seems to be a problem with converted markdown file, Please check again !"))
        return(FALSE)
    }
    else {
        #pass
    }
    volume <- 1 # placeholder value
    issue <- 1 # placeholder value
    journal_details <- get_journal_details(article_dir)
    if (! journal_details$sample) {
        volume <- journal_details$volume
        issue <- journal_details$issue
    }

    markdown_file <- paste(article_dir,xfun::with_ext(get_wrapper_type(article_dir),"md"),sep="/")
    metadata <- rmarkdown::yaml_front_matter(markdown_file)
    # reads the abstract from the second author field
    # reason : abstract is patched as author in metafix.sty
    abstract <- metadata$author[2]
    metadata$abstract <- abstract
    # if metadata$address is NULL
    if (is.null(metadata$address)) {
        metadata$author <- metadata$author[1]
    } else {
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
            fields <- logical(length(person))
            fields[1:2] <- TRUE
            if (any(address <- !(fields | orcid | email))) {
                author$address <- person[address]
            }
            author
        }
    )
    }
    metadata$address <- NULL
    # article metadata from DESCRIPTION
    article_metadata <- if (file.exists(file.path(
        dirname(markdown_file), "DESCRIPTION"))) {
        if (! journal_details$sample) {
            art <- load_article(file.path(dirname(markdown_file), "DESCRIPTION"))
            online_date <- Filter(function(x) x$status == "online", art$status)
            acknowledged_date <- Filter(function(x) x$status == "acknowledged", art$status)[[1]]$date
        } else {
            online_date <- list()
            acknowledged_date <- Sys.Date()
        }
        online_date <- if (length(online_date) == 0) {
            Sys.Date()
        } else {
            online_date[[1]]$date
        }
        list(
            slug = if (journal_details$sample){'~'} else {journal_details$slug},
            acknowledged = acknowledged_date,
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
                                    "' article from the ", issue_year,
                                    "-", issue, " issue.")
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
                self_contained = TRUE,
                toc = FALSE,
                legacy_pdf = TRUE,
                web_only = TRUE,
                mathjax = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml-full.js",
                md_extension = "-tex_math_single_backslash"
            )
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
#' @note Use pandoc version greater than or equal to 2.17
#' @return creates a converted native AST file, as well as a pkg_meta.yaml file
#' @export
#' @examples
#' # Note This is a minimal example to execute this function
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' rmarkdown::pandoc_version()
#' texor::include_style_file(your_article_path)
#' rebib::aggregate_bibliography(your_article_path)
#' texor::convert_to_native(your_article_path)
#' unlink(your_article_folder,recursive = TRUE)
convert_to_native <- function(article_dir) {
    article_dir <- xfun::normalize_path(article_dir)
    # wrapper file name
    input_file <- get_wrapper_type(article_dir)
    # resource path for pandoc
    input_file_path <- paste(article_dir, input_file, sep = "/")
    abs_file_path <- tools::file_path_as_absolute(input_file_path)
    # markdown equivalent filename
    md_file <- paste(toString(tools::file_path_sans_ext(input_file)),
                     ".txt", sep = "")
    md_file_path <- paste(article_dir, md_file, sep = "/")
    # a filter to remove new para character from article abstract
    abs_filter <- system.file(
        "abs_filter.lua", package = "texor")
    # a filter to remove embedded bibliography (if any)
    bib_filter <- system.file(
        "bib_filter.lua", package = "texor")
    # a filter to add r language param
    code_block_filter <- system.file(
        "R_code.lua", package = "texor")
    # renames pdf images to png images (to be used with pdf_to_png())
    image_filter <- system.file(
        "image_filter.lua", package = "texor")
    # enables table numbering in captions
    table_filter <- system.file(
        "table_caption.lua", package = "texor")
    # enables figure numbering in captions
    figure_filter <- system.file(
        "image_caption.lua", package = "texor")
    stat_filter <- system.file(
        "conversion_compat_check.lua", package = "texor")
    equation_filter <- system.file(
        "equation_filter.lua", package = "texor")
    bookdown_ref_filter <- system.file(
        "bookdown_ref.lua", package = "texor")
    pandoc_opt <- c("-s",
                    "--resource-path", abs_file_path,
                    "--lua-filter", abs_filter,
                    "--lua-filter", bib_filter,
                    "--lua-filter", image_filter,
                    "--lua-filter", code_block_filter,
                    "--lua-filter", figure_filter,
                    "--lua-filter", table_filter,
                    "--lua-filter", stat_filter,
                    "--lua-filter", equation_filter,
                    "--lua-filter", bookdown_ref_filter)
    output_format <- "native"
    # This will generate a markdown file with YAML headers.
    if (! pandoc_version_check()){
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17\n","Please Install a newer version of pandoc to run texor"))
        return(FALSE)
    }
    else {
        #pass
    }
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

#' call rmarkdown::render to generate html file
#'
#' @param article_dir path to the directory which contains tex article
#' @param example only enabled for running examples for documentation and
#'  to enable export of this function.
#' @note Use pandoc version greater than or equal to 2.17
#' @note Do not use example = TRUE param when working with conversions.
#' @return Renders a RJwrapper.html file in the /web folder, in example it will
#' return TRUE
#' @export
#' @examples
#' # Note This is a minimal example to execute this function
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
#' dir.create(your_article_folder <- file.path(tempdir(), "tempdir"))
#' x <- file.copy(from = article_dir, to = your_article_folder,recursive = TRUE,)
#' your_article_path <- paste(your_article_folder,"article",sep="/")
#' texor::include_style_file(your_article_path)
#' rmarkdown::pandoc_version()
#' texor::convert_to_markdown(your_article_path)
#' texor::generate_rmd(your_article_path)
#' texor::copy_other_files(your_article_path)
#' texor::produce_html(your_article_path,example = TRUE)
#' unlink(your_article_folder,recursive = TRUE)
produce_html <- function(article_dir,example = FALSE) {
    if (example){
        return(TRUE)
    }
    if (! pandoc_version_check()){
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17\n","Please Install a newer version of pandoc to run texor"))
        return(FALSE)
    }
    else {
        #pass
    }
    article_dir <- xfun::normalize_path(article_dir)
    article_type <- rjtools::rjournal_web_article
    input_file_path <- paste(article_dir, "web",
                    xfun::with_ext(get_wrapper_type(article_dir),"Rmd"),sep="/")
    rmarkdown::render(
        input = input_file_path,
        output_format = "rjtools::rjournal_web_article")
}
