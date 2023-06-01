--[[
Link Filter – tries to correct and fix references.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]
function Link(el)
    if el.attributes[1] ~= nil then
        if el.attributes[1][2] == "ref" then
            return pandoc.RawInline('markdown', [[[]].. pandoc.utils.stringify(el.content) .. [[](]] .. el.target .. [[)]])
        end
    else
        return(el)
    end
end
