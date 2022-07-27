-- counts environments in a document
table = 0
figure = 0
math = 0
inlinemath = 0
displaymath = 0
codeblock = 0
inlinecode = 0
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
    math = math + 1
  end,

  Image = function(el)
    figure = figure + 1
  end,

  Table = function(el)
    table = table + 1
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
    print("---")
    print("table: " .. table)
    print("figure: " .. figure)
    print("math: " .. math)
    print("codeblock:" .. codeblock)
    print("inlinecode:" .. inlinecode)
    print("---")
    print(words .. " words in body")
    print(characters .. " characters in body")
    print(characters_and_spaces .. " characters in body (including spaces)")
end
