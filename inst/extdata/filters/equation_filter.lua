--[[
Bookdown Math Equation  Filter – Add Bookdown style equation numbering and labels from LaTeX
format \label{eq:xyz} to bookdown format (\#eq:xyz)
pandoc generated sample : $\mbox{$\mathbf B$}$
filter generated equivalent : [\(\mbox{$\mathbf B$}\)]
Conversion type : LaTeX --> Markdown
Copyright: © 2022 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

--[[
Applies the filter to Math elements
--]]
function Math(el)
    if el.text:match('label') then
        --print("TRUE")
        local text = pandoc.utils.stringify(el.text)
        --label = string.gsub(text, "\\label{^\\{(.-)\\}$}", "%1")
        s, e, l =string.find(text,"\\label{(.-)}")
        --label = string.gsub(label, "",)
        print(l)
        el.text = text .. [[(\#]] .. l .. [[)]]
        --print(text)
    else
        --pass
    end
    local left = el.mathtype == 'InlineMath' and '\\(' or '$$'
    local right = el.mathtype == 'InlineMath' and '\\)' or '$$'
    return pandoc.RawInline('markdown', left .. el.text .. right)
    --print(el.text)
    --return(el)
end



--gsub("& ?= ?&?", "& =", x)
