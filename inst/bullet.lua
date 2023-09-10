

function BulletList(el)
    for i = 1,#el.content,1 do
        el.content[i][1] = el.content[i][1].content
    end
    return el
end
