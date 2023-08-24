--[[
Link Filter – tries to correct and fix references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

new_eq_labels = {}
not_cached = true

function Link(el)
    -- cache new equation labels in a table
    if file_exists('neweqlabels.txt') and not_cached then
        for line in io.lines("neweqlabels.txt") do
            table.insert(new_eq_labels, line)
        end
        not_cached = false
    end
    -- change the numbering of algorithm images
    if file_exists("algorithms.txt") then
        mini_iter = 1
        for line in io.lines("algorithms.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                --print("#"..line)
                print("#"..line .. tostring(mini_iter))
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
                print("#"..line .. tostring(mini_iter_2))
                el.content = pandoc.Str(tostring(mini_iter_2))
                break
            end
            mini_iter_2 = mini_iter_2 + 1
        end
    end
    -- change numbering of tables only if widetables are present
    if (file_exists("tabs.txt")) then
        mini_iter_3 = 1
        for line in io.lines("tabs.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                --print("#"..line)
                print("#"..line .. tostring(mini_iter_3))
                el.content = pandoc.Str(tostring(mini_iter_3))
                break
            end
            mini_iter_3 = mini_iter_3 + 1
        end
    end
    -- change numbering of equations if they exist
    if (file_exists('oldeqlabels.txt') and file_exists('neweqlabels.txt')) then
        mini_iter_4 = 1
        for line in io.lines("oldeqlabels.txt") do
            if ("#"..line) == (pandoc.utils.stringify(el.target)) then
                print("#"..line .. tostring(mini_iter_4))
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

            return pandoc.RawInline('markdown', [[[]].. pandoc.utils.stringify(el.content) .. [[](]] .. el.target .. [[)]])
        end
    else
        return el
    end
    return el
end

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end
