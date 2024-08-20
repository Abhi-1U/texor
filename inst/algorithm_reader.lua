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
            * newline^0;
    AlgorithmBlock = algo_start
                     * C((1 - algo_end)^0)
                     * algo_end
                     / function(content)
                        local lbl = nil
                        local lines = {}
                        for line in content:gmatch("[^\r\n]+") do
                            local match = lpeg.match(label, line)
                            if match then
                                lbl = match
                            else
                                table.insert(lines, line)
                            end
                        end

                        if not lbl then
                          algo_counter = algo_counter + 1
                          lbl = "algo-" .. algo_counter
                        end

                        local cleaned_content = table.concat(lines, "\n")
                        local div_start = "::::{#" .. lbl .. "}\n```{.algorithm}\n"
                        local div_end = "\n```\n::::"
                        return pandoc.RawBlock("markdown", div_start .. "\\begin{algorithm}\n" .. cleaned_content .. "\n\\end{algorithm}" .. div_end)
                     end;
}

function Reader(input, reader_options)
    return lpeg.match(G, tostring(input))
end
