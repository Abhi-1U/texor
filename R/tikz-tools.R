manage_tikz_images<-function(article_dir){
    old_wd<-getwd()
    if(getwd()!=article_dir){
        setwd(article_dir)
    }
    # checking for RJwrapper and fetching the file name for tex file
    wrapper_types=c('wrapper.tex','RJwrap.tex','RJwrapper.tex')
    wrapper_file=""
    for(w_type in wrapper_types){
        if(file.exists(file.path(article_dir, w_type))) {
            print(paste("Tikz-tools Stage 1 : Found ",w_type))
            wrapper_file=w_type
        }
    }
    file_name<-find_src_file(article_dir,wrapper_file)
    if(!grepl(".tex$",file_name)){
        file_name<-paste0(file_name,".tex")
    }
    # extract tikz blocks as objects
    tikz_object=extract_embeded_tikz_image(article_dir,file_name)
    # isolate tikz into a template latex file
    tikz_template<-c(
        "\\documentclass{standalone}",
        "\\usepackage{xcolor}",
        "\\usepackage{verbatim}",
        "\\usepackage{tikz}",
        "\\newcommand{\\CRANpkg}[1]{\\href{https://CRAN.R-project.org/package=#1}{\\pkg{#1}}}%",
        "\\newcommand{\\pkg}[1]{#1}",
        "\\newcommand{\\code}[1]{#1 }",
        "\\usetikzlibrary{fit}",
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
    file.copy('tikz.tex',tikz_dir,copy.mode=T,recursive=F)
    tinytex::latexmk('tikz/tikz.tex',engine = 'pdflatex')
    # run pdf to png
    texor::make_png_files('','./tikz/tikz.pdf')
    # copy over the file 
    texor::Copy_Other_Files('.')
    # include tikz as pdf in the tex document
    image_path<-'tikz/tikz.png'
    texor::inject_generated_image(article_dir,file_name,image_path)

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
    tikz_style<- which(grepl("^\\s*\\\\tikzstyle\\{", fig_data))
    # if no tikz its probably a PDF or PNG image
    if(identical(tikz_start,integer(0))||identical(tikz_end,integer(0))){
        print("Not a tikz file")
        return()
    }
    # Return Tikz data for a single tikz image
    # will not work on multiple tikz images
    pre_tikz<- fig_data[seq_len(tikz_style-1)]
    post_tikz<-fig_data[seq_len(tikz_end)]
    tikz_data<-setdiff(post_tikz,pre_tikz)
    print(paste("TKZ-S2 : Extracted tikz image data from  ",file_name))
    print(tikz_data)
    return(tikz_data)
}
inject_generated_image<-function(article_dir,file_name,image_path){
    tex_file <- readLines(file.path(article_dir, file_name))
    tikz_start <- which(grepl("^\\s*\\\\begin\\{tikzpicture", tex_file))
    pre_tikz<- tex_file[seq_len(tikz_start-1)]
    post_tikz<-setdiff(tex_file,pre_tikz)
    include_graphics<-paste("\\includegraphics[scale=1]{",image_path,"}",sep='')
    # write to original wrapper file
    write_file<-file(file_name,'w')
    writeLines(c(pre_tikz,include_graphics,"",post_tikz), write_file)
    close(write_file)
}
pre_process_tikz<-function(article_dir){
    wrapper_types=c('wrapper.tex','RJwrap.tex','RJwrapper.tex')
    wrapper_file=""
    for(w_type in wrapper_types){
        if(file.exists(file.path(article_dir, w_type))) {
            print(paste("Tikz-tools Stage 1 : Found ",w_type))
            wrapper_file=w_type
        }
    }
    file_name<-find_src_file(article_dir,wrapper_file)
    if(!grepl(".tex$",file_name)){
        file_name<-paste0(file_name,".tex")
    }
    abs_file_path<-tools::file_path_as_absolute(article_dir)
    input_file<-file_name
    latex_template<-system.file("extdata/latex.template", package = "texor")
    tikz_filter<-system.file("extdata/extract_tikz_filter.lua", package = "texor")
    pandoc_opt<-c(
                  "--resource-path",abs_file_path,
                  "--lua-filter",tikz_filter,
                  "--template",latex_template)
    input_format<-"latex+raw_tex"
    output_format<-"latex"
    out_file<-"outfile.tex" # temporary solution will change it later
    rmarkdown::pandoc_convert(input_file,from = input_format,to= output_format,options=pandoc_opt,output = out_file,verbose = TRUE)
}