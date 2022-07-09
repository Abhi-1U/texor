--[[
reinstate-tikz-filter – reinstate tikz image and styleset data in R-markdown custom RawBlock.
Note: A supplementary function to read and store caption and label will also be required in
      any programming language.
      One for R is here : https://github.com/Abhi-1U/texor/blob/master/R/tikz-tools.R#L116-#L145
Copyright: © 2022 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]


--[[
Reads tikz related data in top-down sequence
--]]
function read_tikz()
    local data_file,err = io.open("tikz_temp_data.txt",'r')
    if data_file then
        -- read all lines of the file
        local tikz_content = data_file:read "*a"
        return tikz_content
    else
        print("error:", err)
        return "nil"
    end
end

--[[
Reads tikz related meta data in top-down sequence
--]]
function read_meta()
    local meta_file,err = io.open("tikz_temp_meta.txt",'r')
    if meta_file then
        -- read all lines of the file
        local meta_content = meta_file:read "*a"
        return meta_content
    else
        print("error:", err)
        return "Nil"
    end
end

--[[
Creates a new pandoc.RawInline with placeholder
--]]
local function markdown_tikz_block(data,metadata)
    local attr="```{tikz, fig.cap = 'Funky tikz', fig.ext = 'png'}\n"
    return pandoc.RawBlock('markdown', attr..data .. "\n" .. metadata .. "\n```")
end

--[[
Match RawBlocks for the specific tikz commands
--]]
function Div(el)
    -- If there is tikzstyle first then it will be read along with the tikz image
    --if el.text:match'^\\{SetTikz}' then
        -- Reinstate the initial tikzset or tikzstyle data
        --tikz_content=read_tikz()
        -- return tikz content as a RawBlock which will be later treated as tikz in R-markdown
        --return markdown_tikz_block(tikz_content)
    --end
    -- Alternatively if there is only tikzpicture part then this command will be invoked
    if el.classes[1] == "StikzImage" then
        -- Reinstate the actual data (data gets read and removed from the temp_file)
        tikz_content=read_tikz()
        meta_content=read_meta()
        -- return tikz content as a RawBlock which will be later treated as tikz in R-markdown
        return markdown_tikz_block(tikz_content,"")
    end
end
