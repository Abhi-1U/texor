--[[
Table filter – Adds Table Numbering (ignores widetables)
License:   MIT – see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]

-- Table counter variable
tables = 0


--[[
Applies the filter to table elements
--]]
function Table(el)
    tables = tables + 1
    -- Table Numbering to be appended
    local label = pandoc.Inlines("Table " .. tostring(tables))
    -- original caption
    local caption = el.caption.long
    if not caption[1] then
      -- Table has no caption, just add the label
      caption = pandoc.Blocks{label}
      if (pandoc.utils.stringify(el.caption.long) == "") then
          print("widetable")
          tables = tables - 1
          caption[1].content = pandoc.Space()
      end
    elseif caption[1].tag == 'Plain' or caption[1].tag == 'Para' then
      -- skip numbering widetables
      if pandoc.utils.stringify(caption[1].content) == "widetable" then
          --print("widetable")
          tables = tables - 1
        caption[1].content = pandoc.Space()
      else
         -- Prepend label to paragraph
         label:extend{pandoc.Str ':', pandoc.Space()}
        caption[1].content = label .. caption[1].content
      end
    else
      -- Add label as plain block element
    end
    el.caption.long = caption
    return el
end

