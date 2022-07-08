--[[
extract-tikz-filter – extract tikz image and styleset in an orderly fashion.
Note: In pandoc use --from as latex+raw_tex 
Copyright: © 2022 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]


--[[
Appends tikz related data in top-down sequence
--]]
function store_tikz(tikz_data)
    local file,err = io.open("tikz_temp_data.txt",'a')
    if file then
        file:write(tikz_data)
        file:write("\n")
        file:close()
    else
        print("error:", err) 
    end
end

--[[
Creates a new pandoc.RawInline with placeholder
--]]
local function latex_placeholder_replacement(text)
  return pandoc.RawBlock('latex', text)
end

--[[
Match RawBlocks for the specific tikz commands
--]]
function RawBlock(el)
  -- First Read the tikzset or tikzstyle data
  if el.text:match'^\\tikzset' or el.text:match'^\\tikzstyle' then
    local tikz_dat=el.text
    -- Store the initial tikzset or tikzstyle data
    store_tikz(tikz_dat)
    -- return a placeholder replacement which will be later treated as div in markdown 
    return latex_placeholder_replacement("\\begin{SetTikz}\\n\\end{SetTikz}")
  end
  -- In the second pass Read the tikzpicture part
  if el.text:match'^\\begin{tikzpicture}' then
    local tikz_dat=el.text
    -- Store the actual data (data gets appended to the temp_file)
    store_tikz(tikz_dat)
    -- return a placeholder replacement which will be later treated as div in markdown   
    return latex_placeholder_replacement("\\begin{StikzImage}\\n\\end{StikzImage}")
  end
end
