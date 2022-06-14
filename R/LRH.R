# Abhi-1U
#

#'Convert_To_Markdown :
#'  This Function converts the LaTeX file into a Markdown file using pandoc
#'  with the same file name in the same directory
#'
#' @param input_file_path : String Path for the Input File
#'
#' @return none :
#' @export
#'  "input_file_name.md"
#'
Convert_To_Markdown<- function(input_file_path){
    path=dirname(input_file_path)
    old_working_directory=getwd()
    setwd(path)
    input_file=basename(input_file_path)
    md_file=paste(toString(tools::file_path_sans_ext(input_file)),".md",sep="")
    rmarkdown::pandoc_convert(input_file, to= "markdown",output = md_file,citeproc = TRUE)
    setwd(old_working_directory)
}

#'Append_Markdown_Files :
#'   Appends the R-Markdown YML meta data like title,bibliography file etc..
#'   and generates a R-Markdown file from a normal Markdown file in a new
#'   output folder.
#' @param input_file_path : String path to the input Markdown File
#' @param title : String title of the document
#' @param bib_file : String path to the bib file
#' @param author : String name(s) of author(s)
#' @return none :
#' @export
#'   A new folder named output with the R-Markdown version of the input Markdown
#'
#'
Append_Markdown_Files <- function(input_file_path,author,title,bib_file){
    input_file=basename(input_file_path)
    md_file = file(input_file_path,open="rt")
    md_file_content=readLines(md_file)
    yml_header="---"
    author_string="author:"
    title_string="title:"
    sensitized_title=paste0('"', title, '"')
    sensitized_author=paste0('"',author,'"')
    newline_string="\n"
    bibliography_string="bibliography:"
    output_string="output:"
    output_spec="rjtools::rjournal_web_article"
    output_file_name=paste(dirname(input_file_path),"/output/",toString(tools::file_path_sans_ext(input_file)),".Rmd",sep="")
    dir.create(dirname(output_file_name),showWarnings = F)
    rmd_yml_additions= paste(yml_header,newline_string,title_string,sensitized_title,newline_string,author_string,sensitized_author,newline_string,bibliography_string,bib_file,newline_string,output_string,output_spec,newline_string, sep =" ")
    output_file = file(output_file_name, open="wt")
    writeLines(paste(rmd_yml_additions,""),con=output_file,useBytes = FALSE)
    writeLines(paste(yml_header,""),con=output_file,useBytes = FALSE)
    writeLines(paste(md_file_content,""),con=output_file,useBytes = FALSE)
    close.connection(md_file,type = "rt")
    close.connection(output_file,type = "wt")
}


#' Copy_Other_Files :
#'   Copies Supporting documents like images,pdf,bib files into the output
#'   folder for building the HTML version of the R-Markdown file.
#' @param from_path : String indicating base path for the working directory
#' @return none
#' @export
#'   copies dependency files into the output folder.
#'
Copy_Other_Files<-function(from_path){
    old_working_directory=getwd()
    setwd(from_path)
    dir_list=list.dirs(recursive = FALSE)
    possible_dirs=c("*_files" , "figures" , "images")
    target_dir=basename(dir_list[grep(paste(possible_dirs,collapse="|"),dir_list)])
    print(target_dir)
    dir.create(paste("output/",target_dir,sep=""),showWarnings = F)
    file.copy(list.files(target_dir, full.names = TRUE), paste("output/",target_dir,sep=""), recursive = TRUE)
    file_list=list.files(recursive = FALSE)
    extensions = c("*.png", "*.jpg", "*.bib","*.pdf","*.tex","*.R","*.bbl")
    target_files = unique(grep(paste(extensions,collapse="|"), file_list, value=TRUE))
    print(target_files)
    file.copy(target_files,to = "output/", copy.mode = T, recursive=FALSE,)
    setwd(old_working_directory)
}
#' Produce HTML
#'
#' @param input_file_path ; String path for the R-Markdown file
#'
#' @return none
#' @export HTML output
#'
#'
Produce_HTML<-function(input_file_path){
    rmarkdown::render(input = input_file_path,output_format = "rjtools::rjournal_web_article")
}

#' Title
#'
#' @param input_file_path
#'
#' @return
#' @export
#'
#' @examples
Pdf_To_Png<-function(input_file_path){
    path=dirname(input_file_path)
    old_working_directory=getwd()
    setwd(path)
    input_file=basename(input_file_path)
    png_file=paste(toString(tools::file_path_sans_ext(input_file)),".md",sep="")
    magick::image_convert(input_file, to= "markdown",output = png_file)
    setwd(old_working_directory)
}



