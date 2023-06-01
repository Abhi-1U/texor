--[[
R code filter – adds a param for R language for code highlighting in generated Rmarkdown.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]

function CodeBlock(block)
    block.classes = {'r'}
    return block
end
