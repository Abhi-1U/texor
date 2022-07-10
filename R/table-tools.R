#' function to modify env and commands in TeX using GNU sed
#'
#'
#' changes are made to :
#' 1. table* environment to table environment
#' 2. \\multicolumn to \\multicolumnx
#' These are due to the pandoc's limitations and ease in conversion.
#'
#' \\multicolumnx is redefined in Metafix.sty as
#' \\renewcommand{\\multicolumnx}[3]{\\multicolumn{#1}{c}{#3}}
#' @param file_name name of the Tex file where modifications are to be made
#'
#' @return
#' @export
#'
#' @examples
#' texor::modify_table_environment("myfile.tex")
modify_table_environment <- function(file_name) {
    # currently this works only on current working directory
    # Begin part
    begin_command <- paste(
        "sed -i -e 's/\\(begin\\){table\\*}/\\1{table}/g '",
        file_name)
    system(begin_command)
    print("Changed \\begin{table*} to \\begin{table}")
    # end part
    end_command <- paste(
        "sed -i -e 's/\\(\\end\\){table\\*}/\\1{table}/g' ",
        file_name)
    system(end_command)
    print("Changed \\end{table*} to \\end{table}")
    # change the \multicolumn to \multicolumnx then metafix macro will fix it
    multicolumn_command <- paste(
        "sed -i -e 's/\\multicolumn/\\multicolumnx/g' ",
        file_name)
    system(multicolumn_command)
    print("changed \\multicolumn to \\multicolumnx")
}
