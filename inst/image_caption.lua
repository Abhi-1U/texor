--[[
Image numbering filter - Numbering Images in caption
License:   MIT - see LICENSE file for details
adapted from: Albert Krewinkel implementation
original License: CC0
--]]

-- Image counter variable
figures = 0
-- Algorithm counter variable
algorithms = 0
--[[
Applies the filter to Image elements
--]]
function Image(el)
    local label = ""
    if el.src:match('alg/') then
        algorithms = algorithms + 1
        -- Figure Numbering to be appended
        label = "Algorithm " .. tostring(algorithms)
    else
        figures = figures + 1
        -- Figure Numbering to be appended
        label = "Figure " .. tostring(figures)
    end
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
    local old_attr = el.attributes[1]
    if old_attr == nil then
      -- Figure has no attributes
      el.attributes[1] = {"width", "100%"}
    else
      -- Add label as plain block element
      attribute_1 = el.attributes
      if el.attributes[1][2]:match('%\\') then
        local width = tonumber(attribute_1[1][2]:match('%d+.%d+'))
        if(attribute_1[1][2]:match('%d+.%d+') == nil) then
           el.attributes[1] = {"width",[[100%]]}
        else
            el.attributes[1] = {"width",tostring(width*100)..[[%]]}
        end
      else
          --pass
      end
    end
    return el
end

function print_r(arr, indentLevel)
    local str = ""
    local indentStr = "#"

    if(indentLevel == nil) then
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
