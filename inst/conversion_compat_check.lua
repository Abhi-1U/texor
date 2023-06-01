--[[
conversion-metdata – Generate a yaml report of metadata about file after conversion.
Note: In pandoc use --from as latex
Copyright: © 2023 Abhishek Ulayil
License:   MIT – see LICENSE file for details
--]]
-- counts environments in a document
tables = 0
figure = 0
maths = 0
cites = 0
inlinemath = 0
displaymath = 0
codeblock = 0
inlinecode = 0
links = 0
equation_references = 0
figure_references = 0
table_references = 0
words = 0
characters = 0
characters_and_spaces = 0
process_anyway = false

wordcount = {
  Str = function(el)
    -- we don't count a word if it's entirely punctuation:
    if el.text:match("%P") then
        words = words + 1
    end
    characters = characters + utf8.len(el.text)
    characters_and_spaces = characters_and_spaces + utf8.len(el.text)
  end,

  Cite = function(el)
     cites = cites + 1
  end,

  Space = function(el)
    characters_and_spaces = characters_and_spaces + 1
  end,

  Code = function(el)
    inlinecode = inlinecode + 1
  end,

  CodeBlock = function(el)
    codeblock = codeblock + 1
  end,
  InlineMath = function(el)
    inlinemath = inlinemath + 1
  end,
  DisplayMath = function(el)
    displaymath = displaymath + 1
  end,
  Math = function(el)
    maths = maths + 1
  end,

  Image = function(el)
    figure = figure + 1
  end,

  Table = function(el)
    tables = tables + 1
  end
}

-- check if the `wordcount` variable is set to `process-anyway`
function Meta(meta)
  if meta.wordcount and (meta.wordcount=="process-anyway"
    or meta.wordcount=="process" or meta.wordcount=="convert") then
      process_anyway = true
  end
end

function Pandoc(el)
    -- skip metadata, just count body:
    pandoc.walk_block(pandoc.Div(el.blocks), wordcount)
    local file,err = io.open("post-conversion-meta.yaml",'w')
    if file then
        file:write("table: " .. tables .. "\n")
        file:write("figure: " .. figure .. "\n")
        file:write("code: " .. "\n")
        file:write("    block: " .. codeblock .. "\n")
        file:write("    inline: " .. inlinecode .. "\n")
        file:write("text: " .. "\n")
        file:write("    words: " .. words .. "\n")
        file:write("    characters: " .. characters .. "\n")
        file:write("    characters_and_spaces: " .. characters_and_spaces .. "\n")
        file:write("citations: " .. cites .. "\n")
        file:write("math: " .. maths .. "\n")
        file:close()
    else
        print("error:", err)
    end
end
