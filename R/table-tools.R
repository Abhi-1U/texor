modify_table_environment<-function(file_name){
    # currently this works only on current working directory
    # Begin part
    begin_command<-paste("sed -i -e 's/\\(begin\\){table\\*}/\\1{table}/g' ",file_name)
    system(begin_command)
    print('Changed \\begin{table*} to \\begin{table}')
    # end part
    end_command<- paste("sed -i -e 's/\\(\\end\\){table\\*}/\\1{table}/g' ",file_name)
    system(end_command)
    print('Changed \\end{table*} to \\end{table}')
    # change the \multicolumn to \multicolumnx then metafix macro will fix it
    multicolumn_command<-paste("sed -i -e 's/\\multicolumn/\\multicolumnx/g' ",file_name)
    system(multicolumn_command)
    print('changed \\multicolumn to \\multicolumnx')
}
