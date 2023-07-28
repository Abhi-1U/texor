--[[
Bookdown Math Equation  Filter – Add Bookdown style equation numbering and labels from LaTeX
format \label{eq:xyz} to bookdown format (\#eq:xyz)
pandoc generated sample : $\mbox{$\mathbf B$}$
filter generated equivalent : [\(\mbox{$\mathbf B$}\)]
Conversion type : LaTeX --> Markdown
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]


--[[
Applies the filter to Math elements
--]]
--[[
Equation filter – tries to correct and fix the equations and references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

function Math(el)
    if el.mathtype == "DisplayMath" then
        if el.text:match('label') then
            local text = pandoc.utils.stringify(el.text)
            s, e, l = string.find(text,"\\label{(.-)}")
            -- Bookdown does not support . _ in equations hence substituting them as hyphen
            l = string.gsub(l, "%.", "-")
            l = string.gsub(l, "_", "-")
            l = string.gsub(l, " ", "-")
            if (not l:match("^eq:")) then
                l = "eq:" .. l
            end
            el.text = text .. [[  (\#]] .. l .. [[)  ]]
        else
            --pass
        end
        local left = el.mathtype == 'InlineMath' and '\\(' or '\n$$'
        local right = el.mathtype == 'InlineMath' and '\\)' or '$$'
        return pandoc.RawInline('markdown', left .. el.text .. right)
    else
        return el
    end
end

function Link(el)
    is_bkdwn = false
    local resource = el.target
    if resource:match("^www.") then
        resource = [[http://]] .. resource
        el.target = resource
    end
    if (el.target:match("^#eq:")) then
        l = el.target
        l = string.gsub(l, "%.", "-")
        l = string.gsub(l, "_", "-")
        l = string.gsub(l, " ", "-")
        el.content = l:gsub("#","")
        bkdown = [[\@ref(]] .. l:gsub("#","") .. [[)]]
        return pandoc.RawInline('markdown', bkdown)
    end
    return el
end


