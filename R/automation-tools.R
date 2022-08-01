texor_orchestrate <- function(article_dir) {
    old_wd <- getwd()
    setwd(article_dir)
    article_dirs <- list.dirs(recursive = FALSE)
    for (dir in article_dirs) {
        latex_to_web(dir)
    }
    print(article_dir)
    on.exit(setwd(old_wd), add = TRUE)
}

latex_to_web <- function(dir) {
    print(dir)
    # Step - 0 : Set working directory
    #            pdf directory
    # Step - 1 : Include Meta-fix style file
    include_style_file(dir)
    # Step - 2 : Manage Bibliography(ies)
    handle_bibliography(dir)
    # Step - 3 : Check for PDF and then convert
    #            PDF to PNG based on condition
    pdf_to_png(dir)
    # Step - 4 : patch code environments to verbatim
    patch_code_env(dir)
    # Step - 5 : patch custom table environments to table
    patch_table_env(dir)
    # Step - 6 : patch figure environments to figure
    patch_figure_env(dir)
    # Step - 7 : Check for Tikz images and pre-process
    #            it based on condition.
    if (article_has_tikz(dir)) {
        # Process tikz here
    }
    # Step - 8 : Convert to markdown + find package
    #            references
    convert_to_markdown(dir)
    # Step - 9 : Create a new directory and copy
    #            dependent files/folders
    copy_other_files(dir)
    # Step - 10 : generate R markdown file with
    #             metadata from DESCRIPTION, tex file
    #             and file path
    # Note : the below function wont work on any article as it needs a
    #folder structure similar to RJournal style /YYYY-ZZ/YYYY-MMM where
    #YYYY is the year, ZZ is the Journal issue number and MMM is the DOI
    # referral(unique article number)
    texor::generate_rmd(dir)
    # Step - 11 : produce html (using rj_web_article) format
    texor::produce_html(dir)
}
