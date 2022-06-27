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
# Convert_To_Markdown<- function(input_file_path){
#     #Filter(input_file_path)
#     path=dirname(input_file_path)
#     old_working_directory=getwd()
#     setwd(path)
#     input_file=basename(input_file_path)
#     md_file=paste(toString(tools::file_path_sans_ext(input_file)),".md",sep="")
#     rmarkdown::pandoc_convert(input_file, to= "markdown",options=c("-s"),output = md_file,citeproc = TRUE,verbose = TRUE)
#     setwd(old_working_directory)
# }
#
# Filter<-function(input_file_path){
#     path=dirname(input_file_path)
#     old_working_directory=getwd()
#     setwd(path)
#     input_file=basename(input_file_path)
#     filter_path=paste(find.package('texor'),'extdata/pkg_filter.lua',sep ='/')
#     int_file=paste(toString(tools::file_path_sans_ext(input_file)),"-intermediate.tex",sep="")
#     rmarkdown::pandoc_convert(input_file,from = 'latex+raw_tex' ,to= "latex",options=c("-s",'--lua-filter',filter_path),output = int_file,citeproc = TRUE,verbose = TRUE)
#     setwd(old_working_directory)
# }

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
# Append_Markdown_Files <- function(input_file_path,author,title,bib_file){
#     print('A Deprecated alternative method: use texor::md_to_rmd()')
#     input_file=basename(input_file_path)
#     md_file = file(input_file_path,open="rt")
#     md_file_content=readLines(md_file)
#     yml_header="---"
#     author_string="author:"
#     title_string="title:"
#     sensitized_title=paste0('"', title, '"')
#     sensitized_author=paste0('"',author,'"')
#     newline_string="\n"
#     bibliography_string="bibliography:"
#     output_string="output:"
#     output_spec="rjtools::rjournal_web_article"
#     output_file_name=paste(dirname(input_file_path),"/output/",toString(tools::file_path_sans_ext(input_file)),".Rmd",sep="")
#     dir.create(dirname(output_file_name),showWarnings = F)
#     rmd_yml_additions= paste(yml_header,newline_string,title_string,sensitized_title,newline_string,author_string,sensitized_author,newline_string,bibliography_string,bib_file,newline_string,output_string,output_spec,newline_string, sep =" ")
#     output_file = file(output_file_name, open="wt")
#     writeLines(paste(rmd_yml_additions,""),con=output_file,useBytes = FALSE)
#     writeLines(paste(yml_header,""),con=output_file,useBytes = FALSE)
#     writeLines(paste(md_file_content,""),con=output_file,useBytes = FALSE)
#     close.connection(md_file,type = "rt")
#     close.connection(output_file,type = "wt")
# }

#' md_to_rmd
#'
#' @param input_file_path : The input file name along with path
#'
#' @return
#' @export A rmd file with same name to the output folder
#'
#' @examples
# md_to_rmd<-function(input_file_path){
#     input_file=basename(input_file_path)
#     template_path=paste(find.package('texor'),'extdata/rmd-style-markdown.template',sep ='/')
#     output_file_name=paste(dirname(input_file_path),"/output/",toString(tools::file_path_sans_ext(input_file)),".Rmd",sep="")
#     dir.create(dirname(output_file_name),showWarnings = F)
#     rmarkdown::pandoc_convert(input_file, to= "markdown",options=c("-s","--template",template_path),output = output_file_name,citeproc = TRUE,verbose = TRUE)
# }

#' Modify_YAML_Data
#'
#' @param input_file: The file whose YAML data has to be modified.
#' @param ... the YAML data that needs to be appended
#'
#' @return
#' @export
#'
#' @examples
# Modify_YAML_Data<-function(input_file,...){
#     input_lines <- readLines(input_file)
#     delimiters <- grep("^---\\s*$", input_lines)
#     if (!length(delimiters)) {
#         stop("unable to find yaml delimiters")
#     } else if (length(delimiters) == 1L) {
#         if (delimiters[1] == 1L) {
#             stop("cannot find second delimiter, first is on line 1")
#         } else {
#             # found just one set, assume it is *closing* the yaml matter;
#             # fake a preceding line of delimiter
#             delimiters <- c(0L, delimiters[1])
#         }
#     }
#     delimiters <- delimiters[1:2]
#     yaml_list <- yaml::yaml.load(input_lines[ (delimiters[1]+1):(delimiters[2]-1) ])
#     dots <- list(...)
#     yaml_list <- c(yaml_list[ setdiff(names(yaml_list), names(dots)) ], dots)
#     output_lines <- c(
#         if (delimiters[1] > 0) input_lines[1:(delimiters[1])],
#         strsplit(yaml::as.yaml(yaml_list), "\n")[[1]],
#         input_lines[ -(1:(delimiters[2]-1)) ]
#     )
#     writeLines(output_lines, con = input_file)
#     return(invisible(output_lines))
# }

#' Find_Bib_File
#'
#' @param path current working directory
#'
#' @return bib_file name as a string
#' @export
#'
#' @examples
# Find_Bib_File <- function(path){
#     old_working_directory=getwd()
#     setwd(path)
#     file_list=list.files(recursive = FALSE)
#     extensions = c("*.bib")
#     bib_file = unique(grep(paste(extensions,collapse="|"), file_list, value=TRUE))
#     print(bib_file)
#     return(bib_file)
# }

#' Include_Bib_Metadata
#'
#' @param input_file
#'
#' @return
#' @export Modified input file
#'
#' @examples
# Include_Bib_Metadata<- function(input_file){
#     bib_file=Find_Bib_File('.')
#     Modify_YAML_Data(input_file,bib_file=bib_file)
# }

#' Copy_Other_Files :
#'   Copies supporting documents like images,pdf,bib files into the output
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





