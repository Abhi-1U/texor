--[[
image-filter – To correct image paths without extensions.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

function removePDFExtensions(src)
  return src:gsub("%.pdf", "")
end

function checkPDFextension(src)
  return src:match("%.pdf")
end

function GetFileExtension(src)
  return src:match("^.+(%..+)$")
end

function Image(img)
    local img_extension = GetFileExtension(img.src)
    if not img_extension then
        local new_src = img_old_src .. ".png"
        img.src = new_src
    end
    if(checkPDFextension(img.src)) then
        local img_old_src = removePDFExtensions(img.src)
        local new_src = img_old_src .. ".png"
        img.src = new_src
    end
    local old_attr = img.attributes[1]
    if old_attr == nil then
        -- Figure has no attributes
        img.attributes[1] = {"width", "100%"}
        img.attributes[2] = {"alt","graphic without alt text"}
    else
        -- Add labimg as plain block element
        attribute_1 = img.attributes
        if img.attributes[1][2]:match('%\\') then
            local width = tonumber(attribute_1[1][2]:match('%d+.%d+'))
            if(attribute_1[1][2]:match('%d+.%d+') == nil) then
                img.attributes[1] = {"width", "100%"}
                img.attributes[2] = {"alt","graphic without alt text"}
            else
                width_as_percent = tostring(width*100)
                img.attributes[1] = {"width", width_as_percent .. [[%]]}
                img.attributes[2] = {"alt","graphic without alt text"}
            end
        else
            --pass
        end
    end
    return img
end


