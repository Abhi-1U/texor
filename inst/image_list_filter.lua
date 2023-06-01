--[[
image-list – extract list of image paths.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

image_path_list = {}
old_session = false

function removePDFExtensions(src)
  return src:gsub("%.pdf", "")
end

function checkPDFextension(src)
  return src:match("%.pdf")
end

function Image(img)
    if(checkPDFextension(img.src)) then
        local img_old_src = removePDFExtensions(img.src)
        local new_src = img_old_src .. ".png"
        img.src = new_src
    end
    table.insert(image_path_list,img.src)
    if old_session then
        local file,err = io.open("image_source.txt",'a')
        if file then
            file:write(img.src .. "\n")
            file:close()
        else
            print("error:", err)
        end
    else
        local file,err = io.open("image_source.txt",'w')
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
