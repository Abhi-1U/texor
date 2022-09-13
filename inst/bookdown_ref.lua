function Link(el)
    if el.attributes[1] ~= nil then
        if el.attributes[1][2] == "ref" then
            print([[\@ref(]] .. el.target:gsub("^%#","") .. [[)]])
            return pandoc.RawInline('markdown', [[\@ref(]] .. el.target:gsub("^%#","") .. [[)]])
        end
    else
        return(el)
    end
end
