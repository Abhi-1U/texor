--[[
Inline Math Filter – Supports nested math environment for MathjaX V3 compatibility
pandoc generated sample : $\mbox{$\mathbf B$}$
filter generated equivalent : [\(\mbox{$\mathbf B$}\)]{.math .inline}
Conversion type : LaTeX --> Markdown
License:   MIT – see LICENSE file for details
--]]

--[[
Applies the filter to Math elements
--]]
function Math (m)
  local left = m.mathtype == 'InlineMath' and '\\(' or '$$'
  local right = m.mathtype == 'InlineMath' and '\\)' or '$$'
  return pandoc.RawInline('markdown', left .. m.text .. right)
end
--function Math(el)
--    if el.mathtype == 'InlineMath' then
--        local math_data =  "[\\(" .. el.text .. "\\)]{.math .inline}"
--        return(pandoc.RawInline('markdown', math_data))
--    else
--        return(el)
--    end
--end
