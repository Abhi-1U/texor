function Str(el)
    if el.text:match('\182') then
        el.text = ""
    end
    return(el)
end
