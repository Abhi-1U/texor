--[[
Table filter – Adds Table Numbering (ignores widetables)
License:   MIT – see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]

-- Table counter variable
tables = 0
old_session_5 =false

--[[
Applies the filter to table elements
--]]
function Table(el)
    tables = tables + 1

    -- Table Numbering to be appended
    local link_string = pandoc.utils.stringify(el.identifier)
    local label = pandoc.Inlines([[(\\#]] .."tab:T".. tostring(tables) .. [[)]])

    -- original caption
    local caption = el.caption.long
    if not caption[1] then
      -- Table has no caption, just add the label
      caption = pandoc.Blocks{label}
      if (pandoc.utils.stringify(el.caption.long) == "") then
          tables = tables - 1
          caption[1].content = pandoc.Space()
      end
    elseif caption[1].tag == 'Plain' or caption[1].tag == 'Para' then
      -- skip numbering widetables
      if pandoc.utils.stringify(caption[1].content) == "widetable" then
          tables = tables - 1
        caption[1].content = pandoc.Space()
      else
         -- Prepend label to paragraph
        label:extend{pandoc.Str '', pandoc.Space()}
        caption[1].content = label .. caption[1].content
        if old_session_5 then
            write_to_file('newtablabels.txt','a',"tab:T".. tostring(tables))
        else
            write_to_file('newtablabels.txt','w',"tab:T".. tostring(tables))
            old_session_5 = true
        end
      end
    else
      -- Add label as plain block element
    end
    el.caption.long = caption
    return el
end

function sanitize_identifier(identifier)
  local l = identifier
  l = string.gsub(l, "%.", "-")
  l = string.gsub(l, "_", "-")
  l = string.gsub(l, " ", "-")
  l = string.gsub(l,"#","")
  l = string.gsub(l,":","")
  return l
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


