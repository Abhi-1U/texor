--[[
Image numbering filter - Numbering Images in caption
License:   MIT - see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]

-- Image counter variable
figures = 0
--[[
Applies the filter to Image elements
--]]
function Image(el)
    figures = figures + 1
    -- Figure Numbering to be appended
    local label = "Figure " .. tostring(figures)
    -- original caption
    local caption = pandoc.utils.stringify(el.caption)
    if not caption then
      -- Figure has no caption, just add the label
      caption = label
    else
      caption = label .. " " .. caption
    end
    el.caption = caption
    -- centering the figure
    local classes = el.classes
    if not classes[1] then
        classes = {"center"}
    else
        classes = {"center"}
    end
    el.classes = classes
    local width_attr = "width=100%"
    local old_attr = el.attributes
    print(el.attr)
    if not old_attr[1] then
      -- Figure has no attributes
      old_attr = pandoc.Blocks{width_attr}
    else
      -- Add label as plain block element
      --table.insert(old_attr[1],1, pandoc.Plain(width_attr))
      new_attr = pandoc.Attr(el.attr.identifier, el.classes, {{"max-width","100%"}})
    end
    new_attr = pandoc.Attr(el.attr.identifier, el.classes, {{"max-width","100%"}})
    print(new_attr)
    el.attr = new_attr
    return el
end

function print_r(arr, indentLevel)
    local str = ""
    local indentStr = "#"

    if(indentLevel == nil) then
        print(print_r(arr, 0))
        return
    end

    for i = 0, indentLevel do
        indentStr = indentStr.."\t"
    end

    for index,value in pairs(arr) do
        if type(value) == "table" then
            str = str..indentStr..index..": \n"..print_r(value, (indentLevel + 1))
        else
            str = str..indentStr..index..": "..value.."\n"
        end
    end
    return str
end
