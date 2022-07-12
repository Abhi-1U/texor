

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
    local image = pandoc.Image(img.caption, new_src, img.title, img.attr)
    return image
  end
  return img
end