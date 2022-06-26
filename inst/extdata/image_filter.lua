
function removePDFExtensions(s)
    return s:gsub("%.pdf", "")
end


function Image(img)
  local img_old_src = removePDFExtensions(img.src)
  local new_src = img_old_src .. ".png"
  local image = pandoc.Image(img.caption, new_src, img.title, img.attr)
  return image
end

