--[[
WideTable Patcher – Adapts WideTable to the table framework
License:   MIT – see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]
old_session = false
ignore_tables = {}
is_table =0
is_image =0
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
        if not table_identifiers and (pandoc.utils.stringify(el.content[i].caption.long) ~= "") then
            table.insert(table_identifiers,pandoc.utils.stringify(el.identifier))
            write_to_file("tabs.txt",'w',pandoc.utils.stringify(el.identifier))
            old_session = true
        elseif (is_unique(pandoc.utils.stringify(el.identifier))) then
            table.insert(table_identifiers,pandoc.utils.stringify(el.identifier))
            if old_session then
                write_to_file("tabs.txt",'a',pandoc.utils.stringify(el.identifier))
            else
                write_to_file("tabs.txt",'w',pandoc.utils.stringify(el.identifier))
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

function write_to_file(filename,open_mode,content)
    local file,err = io.open(filename,open_mode)
    if file then
        file:write(content .. "\n")
        file:close()
    else
        print("error:", err)
    end
end

function file_exists(name)
   local f=io.open(name,"r")
   if f~=nil then io.close(f) return true else return false end
end

