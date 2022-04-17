# Abhi-1U
#

#'Convert_To_Markdown :
#'  This Function converts the LaTeX file into a Markdown file using pandoc
#'  with the same file name in the same directory
#'
#' @param input_file : String Path for the Input File
#'
#' @return none :
#' @export
#'  "input_file_name.md"
#' TODO : Examples
Convert_To_Markdown<- function(input_file){
  old_working_directory=getwd()
  setwd(dirname(input_file))
  md_file_name=paste(toString(tools::file_path_sans_ext(input_file)),".md",sep="")
  rmarkdown::pandoc_convert(input_file, to= "markdown",output=md_file_name)
  setwd(old_working_directory)
}

#'Append_Markdown_Files :
#'   Appends the R-Markdown YML meta data like title,bibliography file etc..
#'   and generates a R-Markdown file from a normal Markdown file in a new
#'   output folder.
#' @param input_file : String path to the input Markdown File
#' @param title : String title of the document
#' @param bib_file : String path to the bib file
#'
#' @return none :
#' @export
#'   A new folder named output with the R-Markdown version of the input Markdown
#'    file
#' TODO : Examples
Append_Markdown_Files <- function(input_file,title,bib_file){
  old_working_directory=getwd()
  setwd(dirname(input_file))
  md_file = file(input_file,open="rt")
  md_file_content=readLines(md_file)
  yml_header="---"
  title_string="title:"
  newline_string="\n"
  bibliography_string="bibliography:"
  output_string="output:"
  output_path="rjtools::rjournal_web_article"
  dir.create("output",showWarnings = F)
  rmd_yml_additions= paste(yml_header,newline_string,title_string,title,newline_string,bibliography_string,bib_file,newline_string,output_string,output_path,newline_string, sep =" ")
  output_file = file(paste("output/",toString(tools::file_path_sans_ext(input_file)),".Rmd",sep=""), open="wt")
  writeLines(paste(rmd_yml_additions,""),con=output_file,useBytes = FALSE)
  writeLines(paste(yml_header,""),con=output_file,useBytes = FALSE)
  writeLines(paste(md_file_content,""),con=output_file,useBytes = FALSE)
  close.connection(md_file,type = "rt")
  close.connection(output_file,type = "wt")
  setwd(old_working_directory)
}


#' Copy_Other_Files :
#'   Copies Supporting documents like images,pdf,bib files into the output
#'   folder for building the HTML version of the R-Markdown file.
#' @param base_dir_path : String indicating base path for the working directory
#' @return none
#' @export
#'   copies dependency files into the output folder.
#'
#' TODO : Examples
Copy_Other_Files<-function(base_dir_path){
  old_working_directory=getwd()
  setwd(base_dir_path)
  dir_list=list.dirs(recursive = FALSE)
  target_dir=basename(dir_list[grep("*_files",dir_list)])
  dir.create(paste("output/",target_dir,sep=""),showWarnings = F)
  file.copy(list.files(target_dir, full.names = TRUE), paste("output/",target_dir,sep=""), recursive = TRUE)
  file_list=list.files(recursive = FALSE)
  extensions = c("*.png", "*.jpg", "*.bib")
  target_files = unique(grep(paste(extensions,collapse="|"), file_list, value=TRUE))
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
#' @examples
#' TODO
Produce_HTML<-function(input_file_path){
    rmarkdown::render(input = input_file_path)
}
