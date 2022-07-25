texor_orchestrate <- function(article_dir) {
    old_wd <- getwd()
    setwd(article_dir)
    article_dirs <- list.dirs(recursive = FALSE)
    for (dir in article_dirs) {
        print(dir)
        # Step - 0 : Set working directory
        #            html directory
        #            pdf directory
        # Step - 1 : Include Meta-fix style file
        include_style_file(dir)
        # Step - 2 : Manage Bibliography(ies)
        rebib::handle_bibliography(dir)
        # Step - 3 : Check for PDF and then convert
        #            PDF to PNG based on condition
        pdf_to_png(dir)
        # Step - 4 : patch code environments to verbatim
        patch_code_env(dir)
        # Step - 5 : patch custom table environments to table
        patch_table_env(dir)
        # Step - 6 : Check for Tikz images and pre-process
        #            it based on condition.
        if (article_has_tikz(dir)) {
            # Process tikz here
        }
        # Step - 7 : Convert to markdown + find package
        #            references
        convert_to_markdown(dir)
        # Step - 8 : Create a new directory and copy
        #            dependent files/folders
        copy_other_files(dir)

        volume <- 0 # todo : a function to get volume number from path
        issue <- 0 # todo : a function to get issue number from path
        md_file_path <- gsub(".tex", ".md", paste(wd,
                                texor::get_wrapper_type(wd), sep = "/"))
        # the below function wont work on any article as it needs a folder structure
        # similar to RJournal style /YYYY-ZZ/YYYY-MMM where YYYY is the year,
        # ZZ is the Journal issue number and MMM is the DOI referral(unique article number)
        texor::generate_rmd(md_file_path, volume,issue) # 2 is volume,1 is issue no
        web_article_path <- gsub(".tex", ".Rmd", paste(wd, "web",
                                    texor::get_wrapper_type(wd), sep = "/"))
        texor::produce_html(web_article_path)
    }
    print(article_dir)
    on.exit(setwd(old_wd), add = TRUE)
}
