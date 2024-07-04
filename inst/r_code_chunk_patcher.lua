--[[
R code filter – adds a param for R language for code chunks in generated Rmarkdown.
Note: In pandoc use --from as latex
Copyright: © 2024 Yinxiang Huang
License:   MIT – see LICENSE file for details
--]]

function CodeBlock(block)
  block.classes = {"{r "..block.attributes["attributes"].."}"}
  return block
end
