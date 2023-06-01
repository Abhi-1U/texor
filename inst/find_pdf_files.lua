--[[
find-pdf-files – extract list of pdf files paths.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

image_path_list = {}
old_session = false

function checkPDFextension(src)
  return src:match("%.pdf")
end

function Image(img)
    if(not checkPDFextension(img.src)) then
        return(img)
    end
    table.insert(image_path_list,img.src)
    if old_session then
        local file,err = io.open("pdf_image_source.txt",'a')
        if file then
            file:write(img.src .. "\n")
            file:close()
        else
            print("error:", err)
        end
    else
        local file,err = io.open("pdf_image_source.txt",'w')
        if file then
            file:write(img.src .. "\n")
            file:close()
        else
            print("error:", err)
        end
        old_session = true
    end
    return(img)
end
