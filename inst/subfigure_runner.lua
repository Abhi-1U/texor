-- subfigure_runner.lua
--
-- Pandoc wrapper around subfigure_to_tabular.lua.
-- The actual transformation is performed by convert() in the
-- subfigure_to_tabular module.

local script_dir = PANDOC_SCRIPT_FILE:match("^(.*)[/\\]") or "."
local convert = dofile(script_dir .. "/subfigure_to_tabular.lua")


local function get_ref_commands()
  local value = os.getenv("TEXOR_SUBFIGURE_REF_COMMANDS")

  if not value or value == "" then
    return nil
  end

  local commands = {}

  for command in value:gmatch("[^,%s]+") do
    table.insert(commands, command)
  end

  return commands
end


function Pandoc(doc)

  local input = PANDOC_STATE.input_files[1]
  local output = os.getenv("TEXOR_SUBFIGURE_OUTPUT")

  if not input then
    error("No input file supplied to pandoc")
  end

  if not output or output == "" then
    error("TEXOR_SUBFIGURE_OUTPUT is not set")
  end

  -- Read the original LaTeX file.
  local f = assert(io.open(input, "rb"))
  local text = f:read("*a")
  f:close()

  -- Run the user's conversion function.
  local ref_commands = get_ref_commands()

  local converted

  if ref_commands then
    converted = convert(text, {
      refCommands = ref_commands
    })
  else
    converted = convert(text)
  end

  -- Write the converted LaTeX.
  local out = assert(io.open(output, "wb"))
  out:write(converted)
  out:close()

  -- Return the Pandoc document unchanged.
  -- Pandoc is being used here only as the Lua host.
  return doc
end
