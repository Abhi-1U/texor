--[[
image-to-knitr-graphics-filter – change pandoc Image.to R-markdown knitr image
Copyright: © 2022 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]


--[[
Creates a new pandoc.RawInline with R-markdown spec image block
--]]
function KnitrBlock(img)
    local attr_construct='```{r ' .. pandoc.utils.stringify(img.attr.identifier):gsub("^fig:", "") ..', echo=FALSE , fig.cap="'.. pandoc.utils.stringify(img.caption) .. '"}\n'
    local knitr_command='knitr::include_graphics("'.. img.src ..'")'
    return pandoc.RawInline('markdown', attr_construct .. knitr_command..'\n```\n')
end



function Image(img)

  -- CodeBlock method
  local image = KnitrBlock(img)
  return image
end
