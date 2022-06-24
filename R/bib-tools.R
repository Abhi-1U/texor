generate_bib_file<-function(article_dir){
    # checking for RJwrapper and fetching the file name for tex file
    if(file.exists(file.path(article_dir, "RJwrapper.tex"))) {
        file_name<-find_src_file(article_dir,"RJwrapper.tex")
        if(grepl(".tex$",file_name)){
            # read the bibliography from file_name
            bib_items<-extract_embeded_bibliography(article_dir,file_name)
        }
        else{
            # add the file extension
            file_name<-paste0(file_name,".tex")
        }
    }
    # checking for RJwrap and fetching the file name for tex file
    else{
        if(file.exists(file.path(article_dir, "RJwrap.tex"))) {
            find
        
        }
        # No file found in directory
        else{
            print('RJwrapper.tex or RJwrap not found !')
        }
    }
}

extract_embeded_bibliography<-function(article_dir,file_name){
    src_file_data<- readLines(file.path(article_dir,file_name))
    bbl_start<- which(grepl("^\\s*\\\\begin\\{thebibliography\\}", src_file_data))
    bbl_end<- which(grepl("^\\s*\\\\end\\{thebibliography\\}", src_file_data))
    pre_bbl<- src_file_data[seq_len(bbl_start)]
    post_bbl<-src_file_data[seq_len(bbl_end)]
    bbl_data<-setdiff(post_bbl,pre_bbl)
    bib_breakpoints<-which(grepl("^\\s*\\\\bibitem\\[", bbl_data))
    bib_items<-list()
    # creating chunks of bibliography entries
    for(i in 1:(length(bib_breakpoints)-1)){
        bib_items[length(bib_items)+1]<-list(bbl_data[(bib_breakpoints[i]-1):(bib_breakpoints[(i+1)]-1)])
        if(i==(length(bib_breakpoints)-1)){
            bib_items[length(bib_items)+1]<-list(bbl_data[(bib_breakpoints[(i+1)]-1):length(bbl_data)])    
        }
    }
    return(bib_items)
}
convert_bbl_to_bib<-function(bbl_data){

}
link_bibliography<-function(){

}
find_src_file<-function(article_dir,lookup_file){
    wrapper_file <- readLines(file.path(article_dir,lookup_file))
        article_start <- which(grepl("^\\s*\\\\begin\\{article\\}", wrapper_file))
        pre_marker<-wrapper_file[seq_len(article_start)]
        post_marker<- wrapper_file[seq_len(article_start)+1]
        source_line<-setdiff(post_marker,pre_marker)
        diff_patt<-c("\\input{","}")
        source_file<-setdiff(source_file,diff_pat)
        tex_file<- gsub("[[:space:]]", "",gsub("\\\\input\\{|\\}","",source_line))
    return(tex_file)
}

