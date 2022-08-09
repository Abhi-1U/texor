--[[
Math Filter – Fix Math environments to be MathJax safe
License:   MIT – see LICENSE file for details
--]]

--[[
Applies the filter to Math elements
--]]
function Math(el)
    str_math =  pandoc.utils.stringify(el.text)
    if el.mathtype == 'InlineMath' then
        if str_math:match'mbox' then
            local delimiter =  "[\\(" .. el.text .. "\\)]{.math .inline}"
            return(pandoc.RawInline('markdown', delimiter))
        end
    end
    return(el)
end
