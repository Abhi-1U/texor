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
  print("No Extension")
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
    return img
  end
  return img
end
