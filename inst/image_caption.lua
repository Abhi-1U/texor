--[[
Image numbering filter V3 - Numbering Images in caption
Warning: for numbering of Figure, presence of a label\identifier is necessary.
Any image inside 'alg/' folder is numbered as Algorithm
Alert: This filter may fail if there is no caption for the figure.
License:   MIT - see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
Pandoc : > 3.0
--]]
old_session_al = false
old_session_fg = false
old_session_wd = false
old_session_ls = false
-- Image counter variable
figures = 0
local_fig_count = 0
-- Algorithm counter variable
algorithms = 0
algs = 0
-- codeblock counter variable
codes = 0
listings = 0
-- widetables counter variable
wdtables = 0
is_alg = 0
-- temp variable to check for figure image
is_fig = 0
is_listing = 0
-- temp variable to check for widetable
is_wdtable = 0

tikz_syle = 0
-- para filter for tikz content
p_filter = {
    Para = function(el)
        string_el = pandoc.utils.stringify(el)
        if string_el:find('^(= %[)') then
            print("removing leftover tikz style")
            print(el.content)
            el.content = pandoc.Plain( pandoc.Space())
            print(el.content)
            return el
        end
        return el
    end
}
--[[
Applies the filter to Image elements
--]]
filter = {
 Image = function(el)
 	if el.src:match('alg/') then
 	    local_fig_count = local_fig_count+1
        is_alg = 1
        is_fig = 0
 	elseif el.src:match('lst/') then
 	    local_fig_count = local_fig_count+1
        is_listing = 1
        is_fig = 0
 	elseif el.src:match('tikz/') then
 	    local_fig_count = local_fig_count+1
        tikz_style=1
        is_fig = 1
        is_alg = 0
    else
        local_fig_count = local_fig_count+1
        is_fig = 1
        is_alg = 0
    end
 end,
 Para = function(el)
        string_el = pandoc.utils.stringify(el)
        if string_el:find('^(= %[)') then
            tikz_style = 1
        end
 end,
CodeBlock = function(el)
        is_code = 1
end,
Table = function(el)
        is_wdtable = 1
end
}

function Figure(el)
    local label = ""
    pandoc.walk_block(el,filter)
    if is_alg == 1 then
        if old_session_al then
            write_to_file("algorithms.txt",'a',pandoc.utils.stringify(el.identifier))
        else
            write_to_file("algorithms.txt",'w',pandoc.utils.stringify(el.identifier))
            old_session_al = true
        end
        algorithms = algorithms + 1
    	label = "Algorithm " .. tostring(algorithms) .. ":"
    	for i = 1,#el.content,1 do
    	    if el.content[i].tag == 'Image' and local_fig_count == 1 then
    	        print(label)
    	        -- leave it empty pandoc will handle it appropriately.
    	        el.content[i].attributes[2] = {}
    	    end
    	    if el.content[i].tag == 'Para' or el.content[i].tag == 'Plain' then
    	        if el.content[i].content[1].tag ~= "Image" then
    	            -- remove any leftover string from algorithm Images
    	            el.content[i] = pandoc.Space()
    	        end
    	    end
    	 end
    end

    if is_code == 1 and is_listing==1 and is_alg == 0 then
        if old_session_ls then
            write_to_file("listings.txt",'a',pandoc.utils.stringify(el.identifier))
        else
            write_to_file("listings.txt",'w',pandoc.utils.stringify(el.identifier))
            old_session_ls = true
        end
        listings = listings + 1
    	label = "Listing " .. tostring(listings) .. ":"
    end

    if is_fig == 1 and is_alg == 0 then
    	figures = figures + 1
    	label = "Figure " .. tostring(figures) .. ":"
    	if old_session_fg then
    	    write_to_file("figs.txt",'a',pandoc.utils.stringify(el.identifier))
        else
            write_to_file("figs.txt",'w',pandoc.utils.stringify(el.identifier))
            old_session_fg = true
    	end
        if tikz_style == 1 then
            for i = 1,#el.content,1 do
    	        if el.content[i].tag == 'Para' or el.content[i].tag == 'Plain' then
    	            if el.content[i].content[1].tag ~= "Image" then
    	                -- remove any leftover string from tikz Images
    	                el.content[i] = pandoc.Space()
    	            end
    	        end
            end
            for i = 1,#el.content,1 do
                if el.content[i].tag == 'Image' and local_fig_count == 1 then
    	            -- leave alt text empty pandoc will handle it appropriately.
    	            print(label)
    	            el.content[i].attributes[2] = {}
                end
	        end
            tikz_style = 0
        end
    end
    if is_code == 1 and is_fig == 0 and is_alg == 0 then
        figures = figures + 1
    	label = "Figure " .. tostring(figures) .. ":"
    	if old_session_fg then
    	    write_to_file("figs.txt",'a',pandoc.utils.stringify(el.identifier))
        else
            write_to_file("figs.txt",'w',pandoc.utils.stringify(el.identifier))
            old_session_fg = true
        end
    end
    --using table for grid content as in subfigures
    if is_wdtable == 1 and is_code == 0 and is_fig == 1 and is_alg == 0 then
        for i = 1,#el.content,1  do
            if el.content[i].tag == 'Table' then
                el.content[i].caption.long = pandoc.Str("widetable")
            end
        end

    	label = "Figure " .. tostring(figures) .. ":"
    end
    local caption = el.caption
    if not caption then
      -- Figure has no caption, just add the label
      caption = {pandoc.Str(label)}
    else
      caption = {pandoc.Str(label),pandoc.Space()}
    end
    if el.caption then
        el.caption.long[1].content = caption .. el.caption.long[1].content
    end
    is_fig = 0
    is_alg = 0
    is_code = 0
    is_wdtable = 0
    local_fig_count = 0
    return el
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
