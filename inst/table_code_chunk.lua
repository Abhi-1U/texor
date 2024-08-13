--- table-code-chunk.lua – converts tables into kable code chunk
---
--- Copyright: © 2024 Abhishek Ulayil
--- License: MIT – see LICENSE for details

-- Makes sure users know if their pandoc version is too old for this
-- filter.
PANDOC_VERSION:must_be_at_least '3.1'

-- Fuse for falling back to "simple markdown tables"
table_fuse = 0

table_count = 1
identifiers = {}
function write_to_file(filename,open_mode,content)
  local file,err = io.open(filename,open_mode)
  if file then
      file:write(content .. "\n")
      file:close()
  else
      print("error:", err)
  end
end

function wide_table_check(el)
    if (pandoc.utils.stringify(el.caption.long) == "") then
          return false
    end
    local caption = el.caption.long
    if caption[1].tag == 'Plain' or caption[1].tag == 'Para' then
        if pandoc.utils.stringify(caption[1].content) == "widetable" then
            return true
        end
    end
    return false
end

table_filter = {
    Image = function(el)
        element_matrix[1] = 1
    end,
    Table = function(el)
        element_matrix[2] = 1
    end,
    Link = function(el)
        element_matrix[3] = 1
    end,
    CodeBlock = function(el)
        element_matrix[4] = 1
    end,
    Math = function(el)
        element_matrix[5] = 1
    end
}

function Table(el)
  -- if table fuse is blown, fallback to normal tables for the rest of the doc
  if table_fuse == 1 then
      return el
  end
  element_matrix = {0,0,0,0,0}
  -- 1. Image
  -- 2. Table
  -- 3. Link
  -- 4. Code Block
  -- 5. Math Equation
  -- check for other components in the table
  pandoc.walk_block(el,table_filter)
  for i = 1,5,1 do
    if element_matrix[i] > 0 then
        table_count = table_count + 1
        table_fuse = 1
        return el
    end
  end
  -- wide table check
  if wide_table_check(el) then
      el.caption.long[1].content = ""
  end
  -- Header Data
  local data_file_name = [[table_data_]] .. table_count .. [[.csv]]
  local head = el.head.rows
  for i = 1,#head,1 do
    if head[i] ~= nil then
      local row_data = head[i]
      local cell_data = row_data.cells
      local row_text = ""
      for j = 1,#cell_data,1 do
        if j ~= #cell_data then
          row_text = row_text .. pandoc.utils.stringify(cell_data[j].contents) .. " ,"
        else
          row_text = row_text .. pandoc.utils.stringify(cell_data[j].contents)
        end
      end
      if (i == 1) then
        write_to_file(data_file_name,'w',row_text)
      else
        write_to_file(data_file_name,'a',row_text)
      end
    end
  end
  -- body content
  local content = el.bodies[1].body
  for i = 1,#content,1 do
    local row_data = content[i]
    local cell_data = row_data.cells
    local row_text = ""
    for j = 1,#cell_data,1 do
      if cell_data[j] ~= nil then
        if j ~= #cell_data then
          row_text = row_text .. pandoc.utils.stringify(cell_data[j].contents) .. " ,"
        else
          row_text = row_text .. pandoc.utils.stringify(cell_data[j].contents)
        end
      end
    end
    if row_text ~= "" then
      write_to_file(data_file_name,'a',row_text)
    end
  end
  local table_code_block = {}
  local patt = "^Table "..table_count .. ":"
  local caption =  pandoc.utils.stringify(el.caption)
  if caption:match(patt) then
    caption = caption:gsub(patt,"")
  end
  table.insert(table_code_block, [[```{r ]] .. [[table-]]..table_count..[[, echo = FALSE, results = 'asis'}]] .. string.char(10))
  table.insert(table_code_block, [[table_]]..table_count..[[_data <- read.csv("]] .. data_file_name .. [[")]] .. string.char(10))
  if wide_table_check(el) then
    table.insert(table_code_block, [[knitr::kable(table_]]..table_count..[[_data)]] .. string.char(10))
  else
    table.insert(table_code_block, [[knitr::kable(table_]]..table_count..[[_data, caption="]] .. caption .. [[")]] .. string.char(10))
  end
  table.insert(table_code_block, [[```]])
  table_count = table_count + 1
  return pandoc.RawInline('markdown', pandoc.utils.stringify(table_code_block))
end

function sanitize_identifier(identifier)
  local l = identifier
  l = string.gsub(l, "%.", "-")
  l = string.gsub(l, "_", "-")
  l = string.gsub(l, " ", "-")
  l = string.gsub(l,"#","")
  l = string.gsub(l,":","")
  return l
end
