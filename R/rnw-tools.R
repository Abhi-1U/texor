#' @title Sweave to RMarkdown
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_to_rmd <- function(input_file, front_matter_type = "vignettes") {
    if (!pandoc_version_check()) {
        warning(paste0("pandoc version too old, current-v : ",rmarkdown::pandoc_version()," required-v : >=2.17"))
        return(FALSE)
    }
    dir <- dirname(input_file)
    if(!dir.exists(dir)) {
        stop("Directory does not exist")
    }
    dir <- xfun::normalize_path(dir)
    date <- Sys.Date()

    # Stage 01: pre process beofre using part of texor::latex_to_web()
    # Step 01: Convert Rnw to knitr and tex
    knitr::Sweave2knitr(input_file)
    input_file <- gsub("[.]([^.]+)$", "-knitr.\\1", input_file)
    output_file <- gsub(".Rnw", ".tex", input_file)
    if(!file.exists(input_file)) {
        stop("knitr file not created")
    }
    patch_rnw_abstract(input_file)

    # PHINNEY: donnot compile the knitr file to save time
    # knitr::knit(input = input_file, output = output_file) # it will print as warning/highlight
    # if(!file.exists(output_file)) {
    #     stop("tex file not created")
    # }
    # Step 02: Separate knitr file to code chunks and tex
    part_file_path <- rnw_remove_code_chunk(input_file)
    md_code_file_path <- part_file_path$md_file_path
    rnw_file_path <- part_file_path$rnw_file_path

    # Step 03: only keep the body of tex file (\document)
    rnw_read_body(rnw_file_path)

    # Step 04: renme original .tex file to .tex.bak
    # PHINNEY: donnot compile the knitr file to save time
    # file.rename(output_file, paste0(output_file, ".bak"))

    # Stage 02: Convert tex to Markdown (part of texor::latex_to_web())
    # TODO: We just use texor::latex_to_web(dir, log_steps = TRUE, temp_mode = FALSE,
    #                                       auto_wrapper = TRUE, interactive_mode = FALSE) for now
    interactive_mode = FALSE
    auto_wrapper = TRUE
    web_dir = FALSE
    compile_rmd_in_temp = !interactive_mode
    # Step 01: Include Meta-fix style file
    wrapper <- get_wrapper_type(dir,
                                auto_wrapper = auto_wrapper,
                                interactive_mode = interactive_mode) #wrapper file name
    file_name <- get_texfile_name(dir)
    include_style_file(dir)
    wrapper <- get_wrapper_type(dir, auto_wrapper = auto_wrapper)

    # PHINNEY: patch for self-defined macros
    wrapper_auto_sty(input_file)


    rebib::aggregate_bibliography(dir)

    patch_code_env(dir)
    patch_table_env(dir)
    data <- handle_figures(dir, file_name)
    patch_equations(dir)
    # Step - 6 : patch figure environments to figure
    patch_figure_env(dir)
    # Step - 7 : Convert to markdown + find package
    #            references
    meta <- pre_conversion_statistics(dir)
    convert_to_markdown(dir)


    # Stage 03: Post process after convert to markdown

    # Step 01: patch for R code
    md_file_path <- paste0(dir, "/RJwrapper.md")
    rnw_patch_inline_code(md_file_path)
    rnw_patch_code_chunk(md_file_path, md_code_file_path)

    # Step 02: patch for vignette entry
    if(front_matter_type == "vignettes") {
        rnw_patch_vignette_entry(md_file_path, input_file)
    }


    # Stage 04
    # Step - 9 : generate R markdown file with
    #             metadata from DESCRIPTION, tex file
    #             and file path
    # Note : the below function will work on any article, However ideally it needs a
    # folder structure similar to RJournal style /YYYY-ZZ/YYYY-MMM where
    # YYYY is the year, ZZ is the Journal issue number and MMM is the DOI
    # referral(unique article number).

    rnw_generate_rmd(dir,web_dir = web_dir, interactive_mode = interactive_mode, front_matter_type = front_matter_type)
    # post_data <- yaml::read_yaml(paste0(dir,"/post-conversion-meta.yaml"))

    return(TRUE)
}


#' @title Remove code chunks from Rnw file
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_remove_code_chunk <- function(input_file) {
    dir <- dirname(input_file)
    if(!dir.exists(dir)) {
        stop("Directory does not exist")
    }
    dir <- xfun::normalize_path(dir)

    md_file_path <- paste(toString(tools::file_path_sans_ext(input_file)),
                     "-part1.md", sep = "")
    input_file_path <- paste(dir, basename(input_file), sep = "/")
    md_file_path <- xfun::normalize_path(md_file_path)
    rnw_file_path <- gsub(".Rnw", "-part2.tex", input_file)

    sweave_code_reader <- system.file(
        "sweave_code_reader.lua", package = "texor")
    sweave_code_remove <- system.file(
        "sweave_code_remove.lua", package = "texor")
    r_code_chunk_patcher <- system.file(
        "r_code_chunk_patcher.lua", package = "texor")
    pandoc_opt_code_chunk <- c("--resource-path", dir,
                    "-f", sweave_code_reader,
                    "--lua-filter", r_code_chunk_patcher)
    pandoc_opt_other <- c("--resource-path", dir,
                    "-f", sweave_code_remove)
    markdown_output_format <- "markdown-simple_tables-pipe_tables-fenced_code_attributes"

    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = markdown_output_format,
                              options = pandoc_opt_code_chunk,
                              output = md_file_path,
                              verbose = TRUE)
    rmarkdown::pandoc_convert(input_file_path,
                              from = "latex",
                              to = "latex",
                              options = pandoc_opt_other,
                              output = rnw_file_path,
                              verbose = TRUE)

    if (!file.exists(md_file_path)) {
        stop("Markdown part file not created")
    }
    if (!file.exists(rnw_file_path)) {
        stop("Rnw part file not created")
    }

    return(list(md_file_path = md_file_path, rnw_file_path = rnw_file_path))
}


#' @title Only keep body in tex file
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_read_body <- function(input_file) {
    if(!file.exists(input_file)) {
        stop("File does not exist")
    }

    file_content <- readLines(input_file)
    # delete \begin{document}, \end{document}, \usepackage{...}, \documentclass{...}
    file_content <- file_content[!grepl("\\\\usepackage(\\[.*\\])?\\{.*\\}", file_content)]
    file_content <- file_content[!grepl("\\\\documentclass(\\[.*\\])?\\{.*\\}", file_content)]
    file_content <- file_content[!grepl("\\\\begin\\{document\\}", file_content)]
    file_content <- file_content[!grepl("\\\\end\\{document\\}", file_content)]
    xfun::write_utf8(file_content, input_file)
    return(TRUE)

    latex_body_reader <- system.file(
        "latex_body_reader.lua", package = "texor")

    pandoc_opt <- c("--resource-path", dirname(input_file),
                               "-f", latex_body_reader)

    rmarkdown::pandoc_convert(input_file,
                              from = "latex",
                              to = "latex",
                              options = pandoc_opt,
                              output = input_file,
                              verbose = TRUE)
    return(TRUE)
}


#' @title Only keep body in tex file
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_patch_inline_code <- function(input_file_path) {
    if(!file.exists(input_file_path)) {
        stop("File does not exist")
    }
    file_content <- readLines(input_file_path)
    file_content <- gsub("\\\\Sexpr\\{(.*?)\\}", "`r \\1`", file_content)
    file_content <- gsub("\\\\verb\\|r (.*?)\\|", "`r \\1`", file_content)
    xfun::write_utf8(file_content, input_file_path)
    return(TRUE)
}



#' @title Only keep body in tex file
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_patch_code_chunk <- function(input_file_path, code_file_path) {
    if(!file.exists(input_file_path) || !file.exists(code_file_path)) {
        stop("File does not exist")
    }
    code_chunk_content <- readLines(code_file_path)
    chunks <- list()
    current_chunk <- NULL
    in_chunk <- FALSE
    for (line in code_chunk_content) {
        if (grepl("^```\\s*\\{r", line)) {
            # Start of a new chunk
            if (!is.null(current_chunk)) {
                # Save the previous chunk
                chunks <- c(chunks, list(current_chunk))
            }
            current_chunk <- line
            in_chunk <- TRUE
        } else if (grepl("^```$", line) && in_chunk) {
            # End of the current chunk
            current_chunk <- c(current_chunk, line)
            chunks <- c(chunks, list(current_chunk))
            current_chunk <- NULL
            in_chunk <- FALSE
        } else if (in_chunk) {
            # Inside a chunk
            current_chunk <- c(current_chunk, line)
        }
    }
    if (!is.null(current_chunk)) {
        # Save the last chunk
        chunks <- c(chunks, list(current_chunk))
    }

    chunk_index <- 1
    file_content <- readLines(input_file_path)
    modified_content <- lapply(file_content, function(line) {
        if (grepl("<!--R_CODE_CHUNK_PLACEHOLDER-->", line)) {
            if (chunk_index <= length(chunks)) {
                replacement <- paste(chunks[[chunk_index]], collapse = "\n")
                chunk_index <<- chunk_index + 1
                return(replacement)
            } else {
                return(line)
            }
        } else {
            return(line)
        }
    })

    modified_content <- unlist(modified_content, use.names = FALSE)
    xfun::write_utf8(modified_content, input_file_path)
    return(TRUE)
}

#' @title patch for vignette entry name
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
rnw_patch_vignette_entry <- function(md_file_path, rnw_file_path) {
    if(!file.exists(md_file_path) || !file.exists(rnw_file_path)) {
        stop("File does not exist")
    }
    md_content <- readLines(md_file_path)
    rnw_content <- readLines(rnw_file_path)

    # Extract the entry name from the Rnw file
    entry_name <- NULL
    depend_name <- NULL
    for (line in rnw_content) {
        if (grepl("^%\\\\VignetteIndexEntry", line)) {
            entry_name <- gsub("^%\\\\VignetteIndexEntry\\{(.*)\\}$", "\\1", line)
            break
        }
    }
    for (line in rnw_content) {
        if (grepl("^%\\\\VignetteDepends", line)) {
            depend_name <- gsub("^%\\\\VignetteDepends\\{(.*)\\}$", "\\1", line)
            break
        }
    }

    if (is.null(entry_name)) {
        entry_name <- basename(rnw_file_path)
        # stop("Vignette entry name not found")
    }

    # Add the entry name to the front yaml in md file
    entry_added <- FALSE
    modified_content <- vector("list", length(md_content))
    for (i in seq_along(md_content)) {
        line <- md_content[[i]]
        if (!entry_added && grepl("^---$", line)) {
            modified_content[[i]] <- c(line, paste0("VignetteIndexEntry: ", entry_name))
            if (!is.null(depend_name)) {
                modified_content[[i]] <- c(modified_content[[i]], paste0("VignetteDepends: ", depend_name))
            } else{
                modified_content[[i]] <- c(modified_content[[i]], paste0("VignetteDepends: ", ""))
            }
            entry_added <- TRUE
        } else {
            modified_content[[i]] <- line
        }
    }

    modified_content <- unlist(modified_content, use.names = FALSE)
    xfun::write_utf8(modified_content, md_file_path)
    return(TRUE)
}

#' @title patch for abstract syntax of Sweave
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
patch_rnw_abstract <- function(rnw_file_path) {
    if(!file.exists(rnw_file_path)) {
        stop("File does not exist")
    }
    rnw_content <- readLines(rnw_file_path)
    in_abstract <- FALSE
    abstract_start <- NULL
    abstract_end <- NULL
    modified_content <- list()

    for (i in seq_along(rnw_content)) {
        line <- rnw_content[i]
        # check in_abstract above to prevent modify the same line
        if (in_abstract && grepl("\\}$", line)) {
            in_abstract <- FALSE
            abstract_end <- i
            line <- sub("\\}$", "\\\\end{abstract}", line)
        }
        if (grepl("\\\\abstract\\{", line)) {
            in_abstract <- TRUE
            abstract_start <- i
            line <- sub("\\\\abstract\\{", "\\\\begin{abstract}", line)
        }
        modified_content[i] <- line
    }

    modified_content <- unlist(modified_content, use.names = FALSE)
    xfun::write_utf8(modified_content, rnw_file_path)
    return(TRUE)
}

#' @title patch for self-defined macros of LaTeX
#' @description 1
#' @param dir directory path
#' @note Use pandoc version greater than or equal to 2.17
#' @note 1
#'
#' @return RMarkdown file in the same folder
#'
#' @export
#' @examples
#' # Checking for pandoc version
#' # texor works with pandoc version >= 2.17
#' article_dir <- system.file("examples/article",
#'                  package = "texor")
wrapper_auto_sty <- function(rnw_file_path, wrapper_name = "RJwrapper.tex") {
    if (!file.exists(rnw_file_path)) {
        stop("File does not exist")
    }
    article_dir <- xfun::normalize_path(dirname(rnw_file_path))
    article_files <- list.files(article_dir, recursive = FALSE)
    sty_files <- article_files[grep(pattern = "[.]sty$", article_files)]
    sty_files <- sty_files[!grepl(pattern = "Metafix[.]sty$", sty_files)]
    rnw_content <- readLines(rnw_file_path)
    include_sty_files <- list()
    for (i in seq_along(rnw_content)) {
        line <- rnw_content[[i]]
        if (grepl("\\\\usepackage\\{", line)) {
            sty_file <- gsub(".*\\{(.*)\\}", "\\1", line)
            if (paste0(sty_file, ".sty") %in% sty_files) {
                include_sty_files <- c(include_sty_files, sty_file)
            }
        }
    }
    wrapper_path <- file.path(article_dir, wrapper_name)
    if (!file.exists(wrapper_path)) {
        stop("Wrapper file does not exist")
    }
    wrapper_content <- readLines(wrapper_path)
    modified_content <- list()
    # avoid duplicate sty files
    for (i in seq_along(wrapper_content)) {
        line <- wrapper_content[[i]]
        if (grepl("\\\\usepackage\\{", line)) {
            sty_file <- gsub(".*\\{(.*)\\}", "\\1", line)
            if (sty_file %in% include_sty_files) {
                include_sty_files <- include_sty_files[include_sty_files != sty_file]
            }
        }
    }
    # add sty file after \usepackage{Metafix}
    for (i in seq_along(wrapper_content)) {
        line <- wrapper_content[[i]]
        modified_content <- c(modified_content, line)
        if (grepl("\\\\usepackage\\{Metafix\\}", line)) {
            for (sty_file in include_sty_files) {
                modified_content <- c(modified_content, paste0("\\usepackage{", sty_file, "}"))
            }
        }
    }
    modified_content <- unlist(modified_content, use.names = FALSE)
    xfun::write_utf8(modified_content, wrapper_path)
    return(TRUE)
}
