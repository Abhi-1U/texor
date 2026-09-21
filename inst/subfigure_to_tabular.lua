--[[
  subfigure_to_tabular.lua

  Rewrites `\begin{figure}...\end{figure}` blocks that contain `subcaption`-style
  `\begin{subfigure}...\end{subfigure}` environments into an equivalent figure
  that lays the images out with a `tabular` (an image row followed by a
  caption row, repeated once per row of subfigures) instead of subfigure
  boxes. Input and output are both LaTeX; ordinary figures without subfigures
  pass through completely untouched.

  In addition to rewriting the figures themselves, `convert` rewrites
  `\ref{...}` (and, optionally, other cross-referencing commands) elsewhere
  in the document that point at an individual subfigure's `\label`, so that
  what used to render as e.g. "1a" via the `subcaption` counter keeps
  rendering as "1a" after the subfigures have been flattened into a plain
  tabular.

  Usage:
      local convert = require("subfigure_to_tabular")
      local new_tex, subrefs = convert(old_tex)

      -- to also rewrite \autoref/\cref alongside \ref:
      local new_tex = convert(old_tex, { refCommands = {"ref", "autoref", "cref", "Cref"} })
]]

local lpeg = require("lpeg")
local P, R, S, C, Cc, Cg, Ct, Cs =
  lpeg.P, lpeg.R, lpeg.S, lpeg.C, lpeg.Cc, lpeg.Cg, lpeg.Ct, lpeg.Cs

----------------------------------------------------------------------
-- low-level lexical helpers
----------------------------------------------------------------------

local hspace   = S(" \t")^0
local anyspace = S(" \t\r\n")^0

-- A LaTeX command name: backslash + letters, not a prefix of a longer name
-- (so cmd("label") does not accidentally match inside \labelformat).
local function cmd(name)
  return P("\\" .. name) * -R("az", "AZ")
end

-- {...} with correctly balanced nested braces, captured *with* the
-- surrounding braces so callers can decide whether to keep or strip them.
-- G matches "{" then any number of (a nested "{...}" group | any single
-- non-brace character), then "}", with the whole span captured.
local braced = P{
  "G",
  G = C("{" * ((lpeg.V("G") + (1 - S("{}")))^0) * "}")
}

-- [...] with correctly balanced nested brackets, captured with the brackets.
local bracketed = P{
  "G",
  G = C("[" * ((lpeg.V("G") + (1 - S("[]")))^0) * "]")
}

-- Optional [...] argument that always yields exactly one capture (the
-- bracketed string, or boolean false if absent) so it composes cleanly
-- inside Cg/Ct.
local optArg = bracketed + Cc(false)

local function unbrace(s)
  if not s then return nil end
  return s:sub(2, -2)
end

local function trim(s)
  return (s:gsub("^%s+", ""):gsub("%s+$", ""))
end

----------------------------------------------------------------------
-- individual LaTeX commands we care about
----------------------------------------------------------------------

local includegraphics =
  Ct( cmd("includegraphics") * hspace
    * Cg(optArg, "opts") * hspace
    * Cg(braced, "path") )

local caption =
  Ct( cmd("caption") * hspace
    * Cg(optArg, "shortopt") * hspace
    * Cg(braced, "text") )

local label =
  Ct( cmd("label") * hspace
    * Cg(braced, "text") )

----------------------------------------------------------------------
-- \begin{subfigure}[pos]{width} ... \end{subfigure}
----------------------------------------------------------------------

local subfigBegin = P("\\begin") * hspace * P("{subfigure}")
local subfigEnd   = P("\\end")   * hspace * P("{subfigure}")

-- Scan the inside of one subfigure, pulling out the graphic/caption/label
-- wherever they appear and skipping everything else one character at a time.
local subfigInner =
  ( Cg(includegraphics, "graphic")
  + Cg(caption, "caption")
  + Cg(label, "label")
  + (1 - subfigEnd) )^0

local subfigure =
  Ct( subfigBegin
    * anyspace * Cg(optArg, "pos")
    * anyspace * Cg(braced, "width")
    * subfigInner
    * subfigEnd )

----------------------------------------------------------------------
-- \begin{figure}[pos] <preamble> <subfigure>+ <postamble> \end{figure}
----------------------------------------------------------------------

-- Matches \end{figure} or \end{figure*}. The star is accepted on either
-- side of \end here regardless of what \begin used (this is a boundary
-- heuristic to stop scans, not a validator of well-formed LaTeX), which
-- also means a mismatched \begin{figure}...\end{figure*} in already-broken
-- source won't make the scanner run away looking for a closer that never
-- comes.
local figEnd = P("\\end") * hspace * P("{figure") * (P("*") + P(true)) * P("}")

-- Raw span up to (but not including) the next occurrence of `stopper`.
local function upTo(stopper)
  return C((1 - stopper)^0)
end

-- One (separator, subfigure) pair. For the first pair, `sep` is everything
-- between `\begin{figure}[...]` and the first subfigure (typically just
-- `\centering`); for later pairs it is the text between two subfigures,
-- which `isRowBreak` below inspects to decide on row grouping.

local subfigItem = Ct( Cg(upTo(subfigBegin + figEnd), "sep") * Cg(subfigure, "env") )

-- One or more such pairs. Because this is `^1` rather than `^0`, an
-- ordinary figure with no subfigures simply fails to match here, and the
-- top-level driver leaves it untouched.
local subfigItems = Ct(subfigItem^1)

local figureWithSubfigs =
  Ct( P("\\begin") * hspace * P("{figure") * Cg(C(P("*") + P(true)), "star") * P("}")
    * hspace * Cg(optArg, "figopts")
    * Cg(subfigItems, "items")
    * Cg(upTo(figEnd), "post")
    * figEnd )

----------------------------------------------------------------------
-- rendering: table capture -> new LaTeX string
----------------------------------------------------------------------

local ABC = {"a","b","c","d","e","f","g","h","i","j","k","l","m",
             "n","o","p","q","r","s","t","u","v","w","x","y","z"}

-- Populated by `render` as a side effect of flattening each figure, and
-- consumed by `rewriteRefs` afterwards. Keyed by a subfigure's own label
-- text; each value is { parent = <enclosing figure's label text>,
-- letter = <"a"/"b"/...> }. Reset at the top of every `convert` call.
local subrefs = {}


local function isRowBreak(sep)
  if sep:find("\r?\n[ \t]*\r?\n") then return true end
  if sep:find("\\\\", 1, true) then return true end
  if sep:find("\\newline") then return true end
  return false
end

-- Given a raw matched `\label{...}` command (or nil), return just the
-- trimmed label text inside the braces (or nil).
local function labelTarget(labelCmd)
  if not labelCmd then return nil end
  local inner = labelCmd:match("^\\label(%b{})$")
  return inner and trim(unbrace(inner)) or nil
end

local function render(fig)
  -- 1. group the flat list of subfigures into rows
  local rows, row = {}, {}
  for i, item in ipairs(fig.items) do
    local isBreak = (i > 1) and isRowBreak(item.sep)
    if isBreak then
      table.insert(rows, row)
      row = {}
    end
    table.insert(row, item.env)
  end
  if #row > 0 then table.insert(rows, row) end

  local ncols = 0
  for _, r in ipairs(rows) do ncols = math.max(ncols, #r) end

  -- 2. pull the overall \caption / \label out of whatever followed the
  --    last subfigure (order-independent, plain substring search).
  local overallCaption = fig.post:match("\\caption%b{}")
  local overallLabel   = fig.post:match("\\label%b{}")
  local parentLabelText = labelTarget(overallLabel)

  -- 3. build the tabular body: one image row + one caption row per row
  local letter = 1
  local lines = {}
  for _, r in ipairs(rows) do
    local imgCells, capCells = {}, {}
    for c = 1, ncols do
      local sub = r[c]
      if sub then
        local width = unbrace(sub.width)
        local path  = sub.graphic and unbrace(sub.graphic.path) or ""
        table.insert(imgCells,
          ("\\includegraphics{%s}"):format(path))

        local letterStr = ABC[letter] or tostring(letter)
        local tag     = "(" .. letterStr .. ")"
        local capText = sub.caption and unbrace(sub.caption.text) or ""
        local capLbl  = sub.label and trim(unbrace(sub.label.text)) or nil
        local capLine = trim(tag .. " " .. capText)
        if capLbl then
          capLine = capLine .. "\\label{" .. capLbl .. "}"
          -- Only recordable if the enclosing figure has its own label to
          -- repoint the reference at; otherwise leave \ref{capLbl} alone
          -- (it will still resolve, just to the plain figure number).
          if parentLabelText then
            subrefs[capLbl] = { parent = parentLabelText, letter = letterStr }
          end
        end
        table.insert(capCells, capLine)
        letter = letter + 1
      else
        -- pad short rows so every row has the same column count
        table.insert(imgCells, "")
        table.insert(capCells, "")
      end
    end
    table.insert(lines, table.concat(imgCells, " & ") .. " \\\\")
    table.insert(lines, table.concat(capCells, " & ") .. " \\\\")
  end

  -- 4. whatever preceded the first subfigure (e.g. \centering), if any
  local pre = trim(fig.items[1].sep)

  local out = {}
  table.insert(out, "\\begin{figure" .. fig.star .. "}" .. (fig.figopts or ""))
  table.insert(out, pre ~= "" and pre or "\\centering")
  table.insert(out, "\\begin{tabular}{" .. string.rep("c", ncols) .. "}")
  for _, l in ipairs(lines) do table.insert(out, l) end
  table.insert(out, "\\end{tabular}")
  if overallCaption then table.insert(out, overallCaption) end
  if overallLabel   then table.insert(out, overallLabel) end
  table.insert(out, "\\end{figure" .. fig.star .. "}")

  return table.concat(out, "\n")
end

----------------------------------------------------------------------
-- top-level driver: rewrite every matching figure, leave everything else
-- (including ordinary figures without subfigures) byte-for-byte identical
----------------------------------------------------------------------

local rewrite = Cs(((figureWithSubfigs / render) + P(1))^0)

----------------------------------------------------------------------
-- second pass: repoint document-wide \ref{...}-style commands that
-- targeted a subfigure's own \label at the enclosing figure's \label
-- instead, with the subfigure's letter appended literally, e.g.
-- \ref{fig:sub-a} (which used to render "1a" via subcaption) becomes
-- \ref{fig:whole}a (which renders "1" from the \ref plus a literal "a").
----------------------------------------------------------------------

-- Alternation of cmd(name) for each name in `names`.
local function altCmd(names)
  local pat
  for _, name in ipairs(names) do
    pat = pat and (pat + cmd(name)) or cmd(name)
  end
  return pat
end

-- Build a Cs(...) pattern that rewrites every `\<name>{label}` (for any
-- `name` in `names`) whose `label` is a tracked subfigure label, and
-- leaves every other command/character byte-for-byte untouched.
local function makeRefRewriter(names)
  local refCmdPat = altCmd(names)
  local refCall = Ct( Cg(C(refCmdPat), "cmd") * Cg(hspace, "sp") * Cg(braced, "arg") )
  return Cs(((refCall / function(t)
    local target = trim(unbrace(t.arg))
    local mapping = subrefs[target]
    if mapping then
      return t.cmd .. "{" .. mapping.parent .. "}" .. mapping.letter
    else
      return t.cmd .. t.sp .. t.arg
    end
  end) + P(1))^0)
end

-- Precompiled for the common case (only \ref needs rewriting), so a plain
-- `convert(text)` call doesn't pay for rebuilding this grammar every time.
local defaultRefRewriter = makeRefRewriter({ "ref" })

----------------------------------------------------------------------
-- public entry point
----------------------------------------------------------------------

-- convert(text [, opts]) -> new_text, subrefs
--   opts.refCommands: list of command names (without the backslash) to
--     scan for and repoint when their argument is a tracked subfigure
--     label. Defaults to {"ref"}. Add "autoref", "cref", "Cref", etc. if
--     your document uses those instead of/alongside plain \ref.
local function convert(text, opts)
  opts = opts or {}
  subrefs = {}

  local out = rewrite:match(text)

  local refRewriter = defaultRefRewriter
  if opts.refCommands then
    refRewriter = makeRefRewriter(opts.refCommands)
  end
  out = refRewriter:match(out)

  return out, subrefs
end

return convert
