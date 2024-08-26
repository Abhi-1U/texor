local P, S, C, Ct, V, Cs =
  lpeg.P, lpeg.S, lpeg.C, lpeg.Ct, lpeg.V, lpeg.Cs

local newline = P"\r"^-1 * P"\n"
local blankline = (S(" \t")^0 * newline)
local space = S(" \t")^0
local algo_start = P"\\begin{algorithm}"
local algo_end = P"\\end{algorithm}"
local label = space * P"\\label{" * C((1 - P"}")^1) * P"}"

local algo_counter = 0

-- Grammar
G = P{ "Pandoc",
    Pandoc = Ct(V"Block"^0) / pandoc.Pandoc;
    Block = blankline^0
          * ( V"AlgorithmBlock"
            + V"Para");
    Para = (1 - algo_start)^1
            * newline^0
            / function(text)
              return pandoc.RawBlock("latex", text)
            end;
    AlgorithmBlock = algo_start
                     * C((1 - algo_end)^0)
                     * algo_end
                     / function(content)
                        return pandoc.RawBlock("latex", "\\Algorithmplaceholder{}")
                     end;
}

function Reader(input, reader_options)
    return lpeg.match(G, tostring(input))
end
