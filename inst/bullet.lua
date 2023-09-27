

function BulletList(el)
    for i = 1,#el.content,1 do
        el.content[i][1] = el.content[i][1].content
    end
    return el
end

function OrderedList(el)
    for i = 1,#el.content,1 do
        el.content[i][1] = el.content[i][1].content
    end
    return el
end


function DefinitionList(el)
    for i = 1,#el.content,1 do
        new_dl ={}
        for k,v in pairs(el.content[i]) do
            if (v[1] ~= nil and k ~= 1 ) then
                el.content[i][k][1] = pandoc.Plain(el.content[i][k][1][1].content)
            end
        end
    end
    return el
end
