-- A sample custom reader that just remove latex header and footer.


-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blankline = spacechar^0 * newline
local docbegin = P("\\begin{document}")
local docend = P("\\end{document}")

-- Grammar
G = P{ "Pandoc",
    Pandoc = (P(1) - docbegin)^0
             * docbegin
             * blankline^0
             * C((P(1) - docend)^0)
             * blankline^0
             * docend
             * (P(1) - docend)^0
             / function(body)
                 return pandoc.Pandoc({pandoc.RawBlock("latex", body)})
               end;
}


function Reader(input, reader_options)
    return lpeg.match(G, tostring(input))
end