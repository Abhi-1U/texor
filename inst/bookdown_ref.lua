--[[
Link Filter – tries to correct and fix references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]
function Link(el)
    -- change the numbering of algorithm images
    if (file_exists("algorithms.txt") and pandoc.utils.stringify(el.target):match("alg:")) then
        mini_iter = 1
        for line in io.lines("algorithms.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                --print("#"..line)
                el.content = pandoc.Str(tostring(mini_iter))
                break
            end
            mini_iter = mini_iter + 1
        end
        return(el)
    end
    -- change the numbering of normal figures only in case of algorithms,
    -- where there is a need for renumbering
    if (file_exists("algorithms.txt") and file_exists("figs.txt")) then
        mini_iter_2 = 1
        for line in io.lines("figs.txt") do
            if (("#"..line) == pandoc.utils.stringify(el.target)) then
                --print("#"..line)
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
                el.content = pandoc.Str(tostring(mini_iter_3))
                break
            end
            mini_iter_3 = mini_iter_3 + 1
        end
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
