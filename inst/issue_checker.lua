--[[
issue-checker – Generate a yaml report of errors in the file after conversion.
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

bm_usage = 0
boldmath_usage = 0
file = 0
--[[
Appends tikz related data in top-down sequence
--]]
function store_errors(err_data, err_text)
    if (file == 0) then
        io.open("potential_errors.yaml",'w'):close()
    end
    local file,err = io.open("potential_errors.yaml",'a')
    if file then
        file:write(err_data .. ": " .. err_text)
        file:write("\n")
        file:close()
    else
        print("error:", err)
    end
end



potential_errors = {
    InlineMath = function(el)
        if (el.text:match('\\bm')) then
            bm_usage = bm_usage + 1
        end
        if (el.text:match('\\boldmath')) then
            boldmath_usage = boldmath_usage + 1
        end
    end,

    DisplayMath = function(el)
        if (el.text:match('\\bm')) then
            bm_usage = bm_usage + 1
        end
        if (el.text:match('\\boldmath')) then
            boldmath_usage = boldmath_usage + 1
        end
    end,

    Math = function(el)
        if (el.text:match('\\bm')) then
            bm_usage = bm_usage + 1
        end
        if (el.text:match('\\boldmath')) then
            boldmath_usage = boldmath_usage + 1
        end
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
    pandoc.walk_block(pandoc.Div(el.blocks), potential_errors)
    store_errors("bm", bm_usage)
    file = file + 1
    store_errors("boldmath", boldmath_usage)

end
