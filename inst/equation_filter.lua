--[[
Bookdown Math Equation  Filter – Add Bookdown style equation numbering and labels from LaTeX
format \label{eq:xyz} to bookdown format (\#eq:xyz)
pandoc generated sample : $\mbox{$\mathbf B$}$
filter generated equivalent : [\(\mbox{$\mathbf B$}\)]
Conversion type : LaTeX --> Markdown
Copyright: © 2022 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

equation_labels = {}

--[[
Applies the filter to Math elements
--]]
function Math(el)
    if el.text:match('label') then
        --print("TRUE")
        local text = pandoc.utils.stringify(el.text)
        --label = string.gsub(text, "\\label{^\\{(.-)\\}$}", "%1")
        s, e, l =string.find(text,"\\label{(.-)}")
        table.insert(equation_labels,l)
        -- Bookdown does not support . _ in equations hence substituting them as hyphen
        l = string.gsub(l, "%.", "-")
        l = string.gsub(l, "_", "-")
        if (not l:match("^eq:")) then
            l = "eq:" .. l
        end
        --label = string.gsub(label, "",)
        --print(l)
        el.text = text .. [[  (\#]] .. l .. [[)  ]]
        --print(text)
    else
        --pass
    end
    local left = el.mathtype == 'InlineMath' and '\\(' or '\n$$'
    local right = el.mathtype == 'InlineMath' and '\\)' or '$$'
    return pandoc.RawInline('markdown', left .. el.text .. right)
    --print(el.text)
    --return(el)
end

function Link(el)
    is_bkdwn = false
    local resource = el.target
    if resource:match("^www.") then
        resource = [[http://]] .. resource
        el.target = resource
    end
    for _,label in pairs(equation_labels) do
        if ("#"..label) == el.target then
            print(label)
            print(el.target)
            local link_text = el.target
            link_text = string.gsub(link_text, "%.", "-")
            link_text = string.gsub(link_text, "_", "-")
            if (not link_text:match("^eq:")) then
                link_text = "eq:" .. link_text
            end
            label = string.gsub(label, "%.", "-")
            label = string.gsub(label, "_", "-")
            if (not label:match("^eq:")) then
                label = "eq:" .. label
            end
            el.target = link_text
            bkdwn = [[\@ref(]] .. label .. [[)]]
            is_bkdwn = true
            break
        end
    end
    if is_bkdwn then
        return pandoc.RawInline('markdown', bkdwn)
    else
        return(el)
    end
end

--gsub("& ?= ?&?", "& =", x)