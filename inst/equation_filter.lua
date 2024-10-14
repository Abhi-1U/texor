--[[
Equation filter – tries to correct and fix the equations and references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]
old_session = false
old_session_2 = false
function Math(el)
    if el.text:match('\\bm') then
        el.text= el.text:gsub('\\bm','\\mathbf')
    end
    if el.text:match('\\begin{tabular}') then
        el.text= el.text:gsub('\\begin{tabular}','\\begin{array}')

    end
    if el.text:match('\\end{tabular}') then
        el.text= el.text:gsub('\\end{tabular}','\\end{array}')
    end
    if el.mathtype == "DisplayMath" then
        if el.text:match('%$') then
            el.text= el.text:gsub('%$', '')
        end
        if el.text:match('\\label') then
            local text = pandoc.utils.stringify(el.text)
            s, e, l = string.find(text,"\\label{(.-)}")
            -- Bookdown does not support . _ in equations hence substituting them as hyphen
            if old_session then
                write_to_file('oldeqlabels.txt','a',l)

            else
               write_to_file('oldeqlabels.txt','w',l)
               old_session = true
            end
            if l:match("^eq:") then
                l = string.gsub(l,"^eq:","")
            end
            l = string.gsub(l, "%.", "-")
            l = string.gsub(l, "_", "-")
            l = string.gsub(l, " ", "-")
            l = string.gsub(l, ":","")
            l = string.gsub(l, "/","")
            l = string.gsub(l, ",","")
            if (not l:match("^eq:")) then
                l = "eq:" .. l
            end
            if old_session_2 then
                write_to_file('neweqlabels.txt','a',l)

            else
               write_to_file('neweqlabels.txt','w',l)
               old_session_2 = true
            end
            el.text = text .. [[   (\#]] .. l .. [[)]]
        else
            --pass
        end
        return {pandoc.Str("\n"),el,pandoc.Str("\n")}
        --return el
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
    return el
end

function write_to_file(filename, open_mode, content)
    local file,err = io.open(filename, open_mode)
    if file then
        file:write(content .. "\n")
        file:close()
    else
        print("error:", err)
    end
end


