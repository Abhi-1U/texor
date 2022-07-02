manage_tikz_images<-function(article_dir){
    # checking for RJwrapper and fetching the file name for tex file
    wrapper_types=c('RJwrapper.tex','RJwrap.tex','wrapper.tex')
    wrapper_file=""
    for(w_type in wrapper_types){
        if(file.exists(file.path(article_dir, w_type))) {
            print(paste("Tikz-tools Stage 1 : Found ",w_type))
            wrapper_file=w_type
        }
    }
    file_name<-find_src_file(article_dir,w_type)
    if(!grepl(".tex$",file_name)){
        file_name<-paste0(file_name,".tex")
    }
    # extract tikz blocks as objects
    tikz_object=extract_embeded_tikz_image(article_dir,file_name)
    # isolate tikz into a template latex file
    tikz_template<-c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{tikz}",
        "\\begin{document}",
        "\\nopagecolor",
        tikz_object,
        "\\end{document}"
    )
    fileConn<-file("tikz.tex")
    writeLines(tikz_template, fileConn)
    close(fileConn)
    # convert the tex file into pdf
    dir.create('tikz',showWarnings = F)
    tikz_dir<-paste(article_dir,'tikz',sep='/')
    tex_build("tikz.tex",tikz_dir)
    # run pdf to png
    texor::make_png_files('.',c('tikz.pdf'))
    # copy over the file 
    
    # include tikz as pdf in the tex document
}

extract_embeded_tikz_image<-function(article_dir,file_name){
    print(paste("TKZ-S2 : extracting Tikz Code from ",file_name))
    src_file_data<- readLines(file.path(article_dir,file_name))
    fig_start<- which(grepl("^\\s*\\\\begin\\{figure", src_file_data))
    fig_end<- which(grepl("^\\s*\\\\end\\{figure", src_file_data))
    pre_fig<- src_file_data[seq_len(fig_start)-1]
    post_fig<-src_file_data[seq_len(fig_end)]
    fig_data<-setdiff(post_fig,pre_fig)
    tikz_start<- which(grepl("^\\s*\\\\begin\\{tikzpicture", fig_data))
    tikz_end<- which(grepl("^\\s*\\\\end\\{tikzpicture", fig_data))
    # if no tikz its probably a PDF or PNG image
    if(identical(tikz_start,integer(0))||identical(tikz_end,integer(0))){
        print("Not a tikz file")
        return()
    }
    # Return Tikz data for a single tikz image
    # will not work on multiple tikz images
    pre_tikz<- fig_data[seq_len(tikz_start-1)]
    post_tikz<-fig_data[seq_len(tikz_end)]
    tikz_data<-setdiff(post_tikz,pre_tikz)
    print(paste("Stage 4 : Extracted tikz image data from  ",file_name))
    return(tikz_data)
}
tex_build <- function(tex_file,
                      fileDir ,
                      engine = 'pdflatex',
                      ...){
  cwd <- getwd()
  on.exit({setwd(cwd)},add = TRUE)
  setwd(fileDir)
  temp_log    <- sprintf('%sDoc.log',stem)
  writeLines(tex_lines, con = temp_tex)
  tinytex::latexmk(file        = tex_file, 
                   engine      = engine,
                   engine_args = '-synctex=1',
                   clean       = FALSE,
                   ...)  
  log_lines <- readLines(temp_log)
  attr(log_lines,'error') <- grepl('error',log_lines[length(log_lines)])
  log_lines
}