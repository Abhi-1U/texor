--[[
CodeBlock filter – Convert a Div block into a CodeBlock
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
    elseif caption[1].tag == 'Plain' or caption[1].tag == 'Para' then
      -- Prepend label to paragraph
      label:extend{pandoc.Str ':', pandoc.Space()}
      caption[1].content = label .. caption[1].content
    else
      -- Add label as plain block element
      label:extend{pandoc.Str ':', pandoc.Space()}
      caption:insert(1, pandoc.Plain(label))
    end
    el.caption.long = caption
    return el
end
