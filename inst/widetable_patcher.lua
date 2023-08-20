--[[
WideTable Patcher – Adapts WideTable to the table framework
License:   MIT – see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]
old_session = false

is_table =0
is_wd_table =0
table_identifiers = {}
--[[
Applies the filter to Table elements
--]]
filtert = {
Table = function(el)
    is_table = 1
end
}
function Div(el)
    pandoc.walk_block(el,filtert)
    if is_table then
        if not table_identifiers then
            table.insert(table_identifiers,pandoc.utils.stringify(el.identifier))
            local file,err = io.open("tabs.txt",'w')
            if file then
                --print(el.identifier)
                file:write(pandoc.utils.stringify(el.identifier) .. "\n")
                file:close()
            else
                print("error:", err)
            end
        elseif (is_unique(pandoc.utils.stringify(el.identifier))) then
            table.insert(table_identifiers,pandoc.utils.stringify(el.identifier))
            if old_session then
                local file,err = io.open("tabs.txt",'a')
                if file then
                    --print(el.identifier)
                    file:write(pandoc.utils.stringify(el.identifier) .. "\n")
                    file:close()
                else
                    print("error:", err)
                end
            else
                local file,err = io.open("tabs.txt",'w')
                if file then
                    --print(el.identifier)
                    file:write(pandoc.utils.stringify(el.identifier) .. "\n")
                    file:close()
                else
                    print("error:", err)
                end
            old_session = true
            end
        else
            for i = 1,#el.content,1 do
    	        if el.content[i].tag == 'Table' then
    	            el.content[i].caption.long = pandoc.Str("widetable")
    	        end
    	    end
        end
    end
    is_table = 0
    is_wd_table = 0
    return el
end

function is_unique(el)
    for i = 1,#table_identifiers,1 do
        if el == table_identifiers[i] or el == "" then
    	    return false
    	 end
    end
    return true
end
