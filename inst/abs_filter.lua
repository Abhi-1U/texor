--[[
abs filter – remove \182 based unicode.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

function Str(el)
    if el.text:match('\182') then
        el.text = ""
    end
    return(el)
end
