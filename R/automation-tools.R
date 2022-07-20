texor_orchestrate <- function(article_dir) {
    old_wd <- getwd()
    setwd(article_dir)
    article_dirs <-list.dirs(recursive = FALSE)
    for (dir in article_dirs) {
        print(dir)
        # Step - 0 : Set working directory and html directory
        old_wd <- getwd()
        setwd(dir)
        # Step - 1 : Include Meta-fix style file
        include_style_file(".")
        # Step - 2 : Manage Bibliography(ies)
        handle_bibliography(".")
        # Step - 3 : Check for PDF and then convert PDF to PNG based on condition
        pdf_to_png(".")
        # Step - 4 : Check for Tikz images and pre-process it based on condition
        if (article_has_tikz(".")) {
            # Process tikz here
        }
        # Step - 5 : Convert to markdown + find package references
        convert_to_markdown(".")
        # Step - 6 : Create a new directory and copy dependent files/folders
        copy_other_files(".")
        # Step - 7 : Go back a folder
        # setwd() one folder up
        # Step - 8 : generate rmarkdown with meta data and headers
        file_path <- "" # todo : a function to get the file path with file name
        volume <- 0 # todo : a function to get volume number from path
        issue <- 0 # todo : a function to get issue number from path
        generate_rmd(file_path, volume, issue)
        # Step - 9 : call rmarkdown to create HTML
        rmd_path <- "" #todo : a function to fetch rmd file path
        produce_html(rmd_path)
        # Step - 10 : Subroutine to create PDF version
        # todo : link some rmarkdown:: fun to generate PDF output
        # Step - 11 : reset working directory
        setwd(old_wd)
    }
    print(article_dir)
    on.exit(setwd(old_wd), add = TRUE)
}
