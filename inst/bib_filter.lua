--[[
bib-filter – remove embedded bibliography from LaTeX file.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

function Div(el)
    if el.classes[1] == 'thebibliography' then
        return { }
    end
end
