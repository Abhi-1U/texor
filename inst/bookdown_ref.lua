--[[
Link Filter – tries to correct and fix references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

new_eq_labels = {}
new_tab_labels = {}
not_cached = true
fig_fuse = 0
function Link(el)
    -- check for fig fuse
    if (file_exists("fig_fuse.txt")) then
        for line in io.lines("fig_fuse.txt") do
            if line == "Blown" then
                fig_fuse =1
            end
        end
    end
    -- cache new equation labels in a table
    if file_exists('neweqlabels.txt') and not_cached then
        for line in io.lines("neweqlabels.txt") do
            table.insert(new_eq_labels, line)
        end
        not_cached = false
    end
    if file_exists('newtablabels.txt') and not_cached then
        for line in io.lines("newtablabels.txt") do
            table.insert(new_tab_labels, line)
        end
        not_cached = false
    end
    -- change the numbering of algorithm images
    if file_exists("algorithms.txt") then
        mini_iter = 1
        for line in io.lines("algorithms.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                el.content = pandoc.Str(tostring(mini_iter))
                break
            end
            mini_iter = mini_iter + 1
        end
    end
    -- change the numbering for lstlistings environment
    if file_exists("listings.txt") then
        mini_iter_x = 1
        for line in io.lines("listings.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                el.content = pandoc.Str(tostring(mini_iter_x))
                break
            end
            mini_iter_x = mini_iter_x + 1
        end
    end
    -- change the numbering of normal figures only in case of algorithms, listings
    -- where there is a need for renumbering
    if (file_exists("algorithms.txt") and file_exists("figs.txt")) then
        mini_iter_2 = 1
        for line in io.lines("figs.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                el.content = pandoc.Str(tostring(mini_iter_2))
                break
            end
            mini_iter_2 = mini_iter_2 + 1
        end
    end
    -- change figure references if new numbering scheme is used
    if (file_exists("fig_refs.txt")) then
        mini_iter_f = 1
        for line in io.lines("fig_refs.txt") do
            if ("#"..line) == (pandoc.utils.stringify(el.target)) then
                el.target = [[#fig:]]..sanitize_identifier(pandoc.utils.stringify(el.target))
                el.content = l:gsub("#","")
                bkdown = [[\@ref(fig:]] .. l:gsub("#","") .. [[)]]
                return pandoc.RawInline('markdown', bkdown)
            end
            mini_iter_f  = mini_iter_f + 1
        end
    end
    -- change numbering of tables only if widetables are present
    if (file_exists("tabs.txt")) then
        mini_iter_3 = 1
        for line in io.lines("tabs.txt") do
            if ("#"..line) == (pandoc.utils.stringify(el.target)) then
                el.target = [[#]]..new_tab_labels[mini_iter_3]
                break
            end
            mini_iter_3  = mini_iter_3 + 1
        end
    end
    -- change numbering of equations if they exist
    if (file_exists('oldeqlabels.txt') and file_exists('neweqlabels.txt')) then
        mini_iter_4 = 1
        for line in io.lines("oldeqlabels.txt") do
            if ("#"..line) == (pandoc.utils.stringify(el.target)) then
                el.target = [[#]]..new_eq_labels[mini_iter_4]
                break
            end
            mini_iter_4  = mini_iter_4 + 1
        end
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
    if el.attributes[1] ~= nil then
        if el.attributes[1][2] == "ref" then
            if ((el.target:match("^#fig:")) and (fig_fuse == 0 )) or (el.target:match("^#tab:")) or (el.target:match("^#table:")) then
                l = el.target
                if el.target:match("^#table:") then
                    l:gsub("#table:","#tab:")
                end
                el.content = l:gsub("#","")
                bkdown = [[\@ref(]] .. l:gsub("#","") .. [[)]]
                return pandoc.RawInline('markdown', bkdown)
            else
                return pandoc.RawInline('markdown', [[[]].. pandoc.utils.stringify(el.content) .. [[](]] .. el.target .. [[)]])
            end
        end
    else
        return el
    end
    return el
end

function sanitize_identifier(identifier)
    l = identifier
    l = string.gsub(l, "%.", "-")
    l = string.gsub(l, "_", "-")
    l = string.gsub(l, " ", "-")
    l = string.gsub(l,"#","")
    l = string.gsub(l,":","")
    return l
end

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end
