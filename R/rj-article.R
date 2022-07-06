#' Include Style File
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export
#'
#' @examples
include_style_file<-function(article_dir){
    # Copy over the Metafix.sty file to the article directory
    file.copy(
            system.file("extdata/Metafix.sty", package = "texor"),
            file.path(article_dir),
        )
    # Modify LaTeX source to include the \include(Metafix) line
    # If the wrapper file name is RJwrapper.tex
    if(file.exists(file.path(article_dir, "RJwrapper.tex"))) {
        wrapper_file <- readLines(file.path(article_dir, "RJwrapper.tex"))
        doc_start <- which(grepl("^\\s*\\\\begin\\{document\\}", wrapper_file))
        before_doc_start<- setdiff(wrapper_file[seq_len(doc_start)-1], '\n')
        after_doc_start<-setdiff(wrapper_file,before_doc_start)
        usepackage_metafix<-"\\usepackage{Metafix}"
        # Backup original wrapper file
        file.rename('RJwrapper.tex','RJwrapper.tex.bk')
        # write to original wrapper file
        xfun::write_utf8(
            c(before_doc_start,usepackage_metafix,"",after_doc_start),
            "RJwrapper.tex.new")
        #
        file.rename('RJwrapper.tex.new','RJwrapper.tex')
    }
    else{
    # If the wrapper file name is RJwrap.tex
        if(file.exists(file.path(article_dir, "RJwrap.tex"))) {
            wrapper_file <- readLines(file.path(article_dir, "RJwrap.tex"))
            doc_start <- which(grepl("^\\s*\\\\begin\\{document\\}", wrapper_file))
            before_doc_start<- setdiff(wrapper_file[seq_len(doc_start)-1], '\n')
            after_doc_start<-setdiff(wrapper_file,before_doc_start)
            usepackage_metafix<-"\\usepackage{Metafix}"
            # Backup original wrapper file
            write_file<-file("RJwrap.tex.bk")
            writeLines(wrapper_file, write_file)
            close(write_file)
            # write to original wrapper file
            write_file<-file("RJwrap.tex",'w')
            writeLines(c(before_doc_start,usepackage_metafix,"",after_doc_start), write_file)
            close(write_file)
        }
    # No wrapper file found !
    else {
            print('RJwrapper.tex or RJwrap.tex does not exist !')
        }
    }
}
#' convert to Rmarkdown
#'
#' @param article_dir
#'
#' @return
#' @export
#'
#' @examples
convert_to_markdown<-function(article_dir){
    # a precautionary measure to ensure that working directory is managed
    if(file.exists(file.path(article_dir, "RJwrapper.tex"))) {
        input_file='RJwrapper.tex'
    }
    else{
        if(file.exists(file.path(article_dir, "RJwrap.tex"))) {
            input_file='RJwrap.tex'
        }
        else{
            print('RJwrapper.tex or RJwrap.tex does not exist !')
        }
    }
    print(input_file)
    #library(tools)
    abs_file_path<-tools::file_path_as_absolute(article_dir)
    md_file=paste(toString(tools::file_path_sans_ext(input_file)),".md",sep="")
    print(md_file)
    bib_filter<-system.file("extdata/bib_filter.lua", package = "texor")
    code_block_filter<-system.file("extdata/code_block_filter.lua", package = "texor")
    image_filter<-system.file("extdata/image_filter.lua", package = "texor")
    tikz_filter<-system.file("extdata/tikz_image_filter.lua", package = "texor")
    knitr_filter<-system.file("extdata/knitr_filter.lua",package ="texor")
    pandoc_opt<-c("-s",
                  "--resource-path",abs_file_path,
                  "--lua-filter",bib_filter,
                  "--lua-filter",image_filter,
                  "--lua-filter",code_block_filter,
                  "--lua-filter",knitr_filter)
    #output_format<-"markdown-simple_tables-multiline_tables-pipe_tables"
    output_format<-"markdown-simple_tables-pipe_tables"
    # This will generate a markdown file with YAML headers.
    rmarkdown::pandoc_convert(input_file,from = "latex",to= output_format,options=pandoc_opt,output = md_file,citeproc = TRUE,verbose = TRUE)
}

#' generate rmarkdown file in output folder
#'
#' @param markdown_file relative path along with name of the markdown file
#' @param volume volume number (int)
#' @param issue issue number (int)
#'
#' @return
#' @export
#'
#' @examples
generate_rmd<-function(markdown_file,volume, issue){
    metadata <- rmarkdown::yaml_front_matter(markdown_file)
    metadata$abstract<- metadata$author[2]
    #metadata$emails<-metadata$author[3:length(metadata$author)]
    metadata$author <- lapply(
            strsplit(metadata$address, "\\\n", fixed = TRUE),
            function(person) {
                author <- list(
                    name = person[1],
                    affiliation = person[2]
                )
                if(any(orcid <- grepl("^ORCiD:", person))) {
                    author$orcid <- sub("^ORCiD: ", "", person[orcid])
                }
                if(any(email <- grepl("^email:", person))) {
                    author$email <- sub("^email:", "", person[email])
                }
                #if(!is.na(metadata$emails)){
                #    author$email <- metadata$emails[2+person]
                #}
                fields <- logical(length(person))
                fields[1:2] <- TRUE
                if(any(address <- !(fields | orcid | email))) {
                    author$address <- person[address]
                }
                author
            }
        )
    metadata$address <- NULL

    # article metadata from DESCRIPTION
    article_metadata <- if(file.exists(file.path(dirname(markdown_file), "DESCRIPTION"))) {
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
        issue_month <- if(issue_year < 2022) issue * 6 else issue * 3
        list(
            slug = basename(dirname(markdown_file)),
            online = as.Date(paste(volume + 2008, issue_month, "01"), format = "%Y %m %d")
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
        preview = 'preview.png',
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

    input_file=basename(markdown_file)
    template_path=paste(find.package('texor'),'extdata/rmd-style-markdown.template',sep ='/')
    output_file_name=paste(dirname(markdown_file),"/output/",toString(tools::file_path_sans_ext(input_file)),".Rmd",sep="")
    dir.create(dirname(output_file_name),showWarnings = F)
    xfun::write_utf8(
        c("---", yaml::as.yaml(front_matter), "---", article_body),
        output_file_name)
}


