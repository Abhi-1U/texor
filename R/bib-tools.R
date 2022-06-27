generate_bib_file<-function(article_dir){
    # checking for RJwrapper and fetching the file name for tex file
    if(file.exists(file.path(article_dir, "RJwrapper.tex"))) {
        print("Stage 1 : Found RJWrapper.tex")
        file_name<-find_src_file(article_dir,"RJwrapper.tex")
        if(grepl(".tex$",file_name)){
            # read the bibliography from file_name
            # if bib_file already exists!
            if(find_bib_file()==paste(toString(tools::file_path_sans_ext(file_name)),".bib",sep="")){
                print('Bib file exists')
                link_bibliography_line(article_dir,file_name)
            }else{
                print("Using parser to generate bibtex entries")
                bib_items<-extract_embeded_bibliography(article_dir,file_name)
                bibtex_data<-convert_bbl_to_bib(bib_items,article_dir,file_name)
                link_bibliography_line(article_dir,file_name)
            }
        }
        else{
            # add the file extension
            file_name<-paste0(file_name,".tex")
            print(paste("Found ",file_name))
            # read the bibliography from file_name
            # if bib_file already exists!
            if(find_bib_file()==paste(toString(tools::file_path_sans_ext(file_name)),".bib",sep="")){
                print('Bib file exists')
                link_bibliography_line(article_dir,file_name)
            }else{
                print("Using parser to generate bibtex entries")
                bib_items<-extract_embeded_bibliography(article_dir,file_name)
                bibtex_data<-convert_bbl_to_bib(bib_items,article_dir,file_name)
                link_bibliography_line(article_dir,file_name)
            }
        }
    }
    # checking for RJwrap and fetching the file name for tex file
    else{
        if(file.exists(file.path(article_dir, "RJwrap.tex"))) {
            file_name<-find_src_file(article_dir,"RJwrapper.tex")
            if(grepl(".tex$",file_name)){
                # read the bibliography from file_name
                # read the bibliography from file_name
                # if bib_file already exists!
                if(find_bib_file()==paste(toString(tools::file_path_sans_ext(file_name)),".bib",sep="")){
                    link_bibliography_line(article_dir,file_name)
                }else{
                    bib_items<-extract_embeded_bibliography(article_dir,file_name)
                    bibtex_data<-convert_bbl_to_bib(bib_items,article_dir,file_name)
                    link_bibliography_line(article_dir,file_name)
                }
            }
            else{
                # add the file extension
                file_name<-paste0(file_name,".tex")
                # read the bibliography from file_name
                # if bib_file already exists!
                if(find_bib_file()==paste(toString(tools::file_path_sans_ext(file_name)),".bib",sep="")){
                    link_bibliography_line(article_dir,file_name)
                }else{
                    bib_items<-extract_embeded_bibliography(article_dir,file_name)
                    bibtex_data<-convert_bbl_to_bib(bib_items,article_dir,file_name)
                    link_bibliography_line(article_dir,file_name)
                }
            }
        }
        # No file found in directory
        else{
            print('RJwrapper.tex or RJwrap not found !')
        }
    }
}

extract_embeded_bibliography<-function(article_dir,file_name){
    print(paste("Stage 4 : Bib file not found, hence extracting bibliography from ",file_name))
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
    print(paste("Stage 4 : Extracted bib items from ",file_name))
    return(bib_items)
}
convert_bbl_to_bib<-function(bib_items,article_dir,file_name){
    bib_tex_records<-list()
    for(item in bib_items){
        bib_record<-list()
        # if bib item closes on the first index
        print('Set 1')
        if(which(grepl("\\}$", item[[1]]))==which(grepl("^\\s*\\\\bibitem\\[", item[[1]]))){
            print('Set 2')
            start_idx<-which(grepl("^\\s*\\\\bibitem\\[", item[[1]]))# start_idx =1
            bib_record$unique_id<-str_split(str_split(gsub("\\\\bibitem\\[|\\]","",item[[1]][start_idx]),"\\{")[[1]][2],"\\}")[[1]][1]
            break_points<-which(grepl("^\\\\newblock", item[[1]]))
            # author_names
            # difference between start of identifier and authors
            if((break_points[1]-start_idx)==2){
                bib_record$author<-item[[1]][start_idx+1]
            }
            # difference between start of identifier and authors
            if((break_points[1]-start_idx)==3){
                bib_record$author<-paste(item[[1]][start_idx+1],item[[1]][start_idx+2])
            }
            # title extraction
            # difference between author names and title is 1 line
            if((break_points[2]-break_points[1])==1){
                bib_record$title<-gsub("\\\\newblock","",item[[1]][break_points[1]])
            }
            # difference between author names and title is 2 lines
            if((break_points[2]-break_points[1])==2){
                bib_record$title<-paste(gsub("\\\\newblock","",item[[1]][break_points[1]]),item[[1]][break_points[1]+1])
            }

            # journal ,volume ,number ,pages , year 
            # difference between title and journal details is 1 line
            if((break_points[3]-break_points[2])==1){
                journal_line<-which(grepl("^\\\\newblock\\\\emph{", item[[1]]))
                journal_info<-strsplit(item[[1]][which(grepl("\\\\emph\\{", item[[1]]))]," ")
                journal_start_idx<-which(grepl("\\\\emph\\{",journal_info[[1]]))
                journal_end_idx<-which(grepl("\\}",journal_info[[1]]))
                journal_name<-""
                for(i in journal_start_idx:journal_end_idx){
                    journal_name<-paste(journal_name,journal_info[[1]][i])
                }
                bib_record$journal<-gsub("\\}"," ",gsub("\\\\emph\\{"," ",journal_name))
                bib_record$volume<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+1])
                bib_record$number<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+2])
                bib_record$pages<-journal_info[[1]][j_end_idx+3]
                if(length(journal_info)>=(j_end_idx+3)){
                    bib_record$year<-journal_info[[1]][j_end_idx+4]
                }
            }
            # difference between title and journal details is 2 line
            if((break_points[3]-break_points[2])==2){
                journal_line<-which(grepl("^\\\\newblock\\\\emph{", item[[1]]))
                journal_info<-strsplit(item[[1]][which(grepl("\\\\emph\\{", item[[1]]))]," ")
                journal_start_idx<-which(grepl("\\\\emph\\{",journal_info[[1]]))
                journal_end_idx<-which(grepl("\\}",journal_info[[1]]))
                journal_name<-""
                for(i in journal_start_idx:journal_end_idx){
                    journal_name<-paste(journal_name,journal_info[[1]][i])
                }
                bib_record$journal<-gsub("\\}"," ",gsub("\\\\emph\\{"," ",journal_name))
                bib_record$volume<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+1])
                bib_record$number<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+2])
                bib_record$pages<-journal_info[[1]][j_end_idx+3]
                if(length(journal_info)>=(j_end_idx+3)){
                    bib_record$year<-journal_info[[1]][j_end_idx+4]
                }
                bib_record$year<-item[break_points[3]+1]
                
            } 


        }
        print('Set 3')
        # if bib item identifier is two lines long
        if((which(grepl("\\}$", item[[1]]))-1)==which(grepl("^\\s*\\\\bibitem\\[", item[[1]]))){
            start_idx<-which(grepl("\\}$", item[[1]]))
            unique_id<-gsub("\\}$","",str_split(item[[1]][start_idx],"\\{")[[1]][2])
            break_points<-which(grepl("^\\\\newblock", item[[1]]))
            # difference between start of identifier and authors
            if((break_points[1]-start_idx)==3){
                bib_record$author<-item[[1]][start_idx+2]
            }
            # difference between start of identifier and authors
            if((break_points[1]-start_idx)==4){
                bib_record$author<-paste(item[[1]][start_idx+2],item[[1]][start_idx+3])
            }
            # difference between author names and title is 1 line
            if((break_points[2]-break_points[1])==1){
                bib_record$title<-gsub("\\\\newblock","",item[[1]][break_points[1]])
            }
            # difference between author names and title is 2 lines
            if((break_points[2]-break_points[1])==2){
                bib_record$title<-paste(gsub("\\\\newblock","",item[[1]][break_points[1]]),item[[1]][break_points[1]+1])
            }
            # journal ,volume ,number ,pages , year 
            # difference between title and journal details is 1 line
            if((break_points[3]-break_points[2])==1){
                journal_line<-which(grepl("^\\\\newblock\\\\emph{", item[[1]]))
                journal_info<-strsplit(item[[1]][which(grepl("\\\\emph\\{", item[[1]]))]," ")
                journal_start_idx<-which(grepl("\\\\emph\\{",journal_info[[1]]))
                journal_end_idx<-which(grepl("\\}",journal_info[[1]]))
                journal_name<-""
                for(i in journal_start_idx:journal_end_idx){
                    journal_name<-paste(journal_name,journal_info[[1]][i])
                }
                bib_record$journal<-gsub("\\}"," ",gsub("\\\\emph\\{"," ",journal_name))
                bib_record$volume<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+1])
                bib_record$number<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+2])
                bib_record$pages<-journal_info[[1]][j_end_idx+3]
                if(length(journal_info)>=(j_end_idx+3)){
                    bib_record$year<-journal_info[[1]][j_end_idx+4]
                }
            }
            # difference between title and journal details is 2 line
            if((break_points[3]-break_points[2])==2){
                journal_line<-which(grepl("^\\\\newblock\\\\emph{", item[[1]]))
                journal_info<-strsplit(item[[1]][which(grepl("\\\\emph\\{", item[[1]]))]," ")
                journal_start_idx<-which(grepl("\\\\emph\\{",journal_info[[1]]))
                journal_end_idx<-which(grepl("\\}",journal_info[[1]]))
                journal_name<-""
                for(i in journal_start_idx:journal_end_idx){
                    journal_name<-paste(journal_name,journal_info[[1]][i])
                }
                bib_record$journal<-gsub("\\}"," ",gsub("\\\\emph\\{"," ",journal_name))
                bib_record$volume<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+1])
                bib_record$number<-gsub("\\\\penalty0"," ",journal_info[[1]][j_end_idx+2])
                bib_record$pages<-journal_info[[1]][j_end_idx+3]
                if(length(journal_info)>=(j_end_idx+3)){
                    bib_record$year<-journal_info[[1]][j_end_idx+4]
                }
                bib_record$year<-item[break_points[3]+1]
            }
            bib_tex_records[[length(bib_tex_records)+1]] <- list(bib_record)
        }
    input_file=basename(input_file)
    #template_path=paste(find.package('texor'),'extdata/rmd-style-markdown.template',sep ='/')
    output_file_name=paste(dirname(input_file),"/output/",toString(tools::file_path_sans_ext(input_file)),".bib",sep="")
    dir.create(dirname(output_file_name),showWarnings = F)
    xfun::write_utf8(
        c("---", yaml::as.yaml(list=(BibTex=bib_tex_records)), "---"),
        output_file_name)
        #yaml::as.yaml(list=(
        #      BibTex=bib_tex_records))
        return(bib_tex_records)
    }
}
link_bibliography_line<-function(article_dir,file_name){
    src_file_data<- readLines(file.path(article_dir,file_name))
    bib_exist<-FALSE
    for(line in src_file_data){
        if(grepl("^\\\\bibliography",line)){
            bib_exist<-TRUE   
            break
        }
    }
    if(bib_exist){
        print('\\bibliography{bib_file} exists!')
        bib_line<-""
    }
    else{
        bib_line<-paste("\\bibliography{",toString(tools::file_path_sans_ext(file_name)),"}",sep="")
    }
    # Backup original wrapper file
    write_file<-file(paste(file_name,".bk",sep=""))
    writeLines(src_file_data, write_file)
    close(write_file)
    # write to original wrapper file
    write_file<-file(file_name,'a')
    writeLines(c(bib_line), write_file,)
    close(write_file)
}
find_src_file<-function(article_dir,lookup_file){
    print(paste("Stage 2 : Looking for source file included in",lookup_file))
    wrapper_file <- readLines(file.path(article_dir,lookup_file))
    article_start <- which(grepl("^\\s*\\\\begin\\{article\\}", wrapper_file))
    pre_marker<-wrapper_file[seq_len(article_start)]
    post_marker<- wrapper_file[seq_len(article_start)+1]
    source_line<-setdiff(post_marker,pre_marker)
    diff_patt<-c("\\input{","}")
    intermediate<-setdiff(source_line,diff_patt)
    tex_file<- gsub("[[:space:]]", "",gsub("\\\\input\\{|\\}","",source_line))
    print(paste("Stage 2 : Found source file ",tex_file))
    return(tex_file)
}

find_bib_file<-function(){
    print("Stage 3 : Finding bib files")
    file_list=list.files(recursive = FALSE)
    extensions = c("*.bib")
    bib_file = unique(grep(paste(extensions,collapse="|"), file_list, value=TRUE))
    if(identical(bib_file, character(0)) ){
        print("Stage 3 : No Bib files found !")
        return("")
    }
    else{
        print(paste("Stage 3 : Found Bib file ",bib_file))
        return(bib_file)
    }
}