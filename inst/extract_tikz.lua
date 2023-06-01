--[[
extract-tikz-filter – extract tikz image and styleset in an orderly fashion.
Note: In pandoc use --from as latex+raw_tex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

tikz_set_count = 0

--[[
Appends tikz related data in top-down sequence
--]]
function store_tikz(tikz_data, pos)
    local file,err = io.open("tikz_style_data.yaml",'a')
    if file then
        file:write("image: " .. pos .. "\n")
        file:write(tikz_data)
        file:write("\n")
        file:write("image-end: " .. pos .. "\n")
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
  if el.text:match'^\\tikzset' or  el.text:match'\\tikzset' then
    local tikz_dat=el.text
    tikz_set_count = tikz_set_count + 1
    --Store the initial tikzset or tikzstyle data
    store_tikz(tikz_dat, tikz_set_count)
    --return a placeholder replacement which will be later treated as div in markdown
    return latex_placeholder_replacement("\\begin{SetTikz}\\n\\end{SetTikz}")
  end
  -- In the second pass Read the tikzpicture part
  if el.text:match'^\\begin{tikzpicture}' or el.text:match'\\begin{tikzpicture}' then
    return el
  end
end

