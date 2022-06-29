#' pdf_to_png
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export
#'
#' @examples
pdf_to_png<-function(article_dir){
    path=dirname(article_dir)
    old_working_directory=getwd()
    if(old_working_directory!=path){
        print('Working directory path is same')
    }
    else{
        setwd(path)
    }
    input_files=find_pdf_files('.')
    make_png_files('.',input_files)
    setwd(old_working_directory)
}

#' find pdf files
#'
#' @param article_dir path to the directory which contains tex article
#'
#' @return
#' @export
#'
#' @examples
find_pdf_files<-function(article_dir){
    print("Finding inclusive PDF files")
    file_list=list.files(article_dir,recursive = FALSE)
    extensions = c("*.pdf")
    pdf_files = unique(grep(paste(extensions,collapse="|"), file_list, value=TRUE))
    pdf_files_native<-c('RJwrapper.pdf','RJwrap.pdf','wrapper.pdf')
    filtered_pdf_files<-setdiff(pdf_files,pdf_files_native)
    if(identical(filtered_pdf_files, character(0)) ){
        print("Image : No PDF graphic files found !")
        return("NA")
    }
    else{
        print(paste("Image : Found",length(filtered_pdf_files),"PDF graphic files"))
        return(filtered_pdf_files)
    }
}


#' make png file
#'
#' @param article_dir path to the directory which contains tex article
#' @param input_files list of file names to be converted to png
#'
#' @return
#' @export
#'
#' @examples
make_png_files<-function(article_dir,input_files){
    if(input_files=="NA"){
        print("No files to convert")
        return('')
    }
    #library('pdftools')
    for(file in input_files){
        png_file=paste(toString(tools::file_path_sans_ext(file)),".png",sep="")
        pdftools::pdf_convert(file, dpi = 600,pages = 1,filenames = png_file)
    }
    print("made PNG graphics @ 600 dpi density")
}
