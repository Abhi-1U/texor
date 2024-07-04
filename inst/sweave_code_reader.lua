-- A sample custom reader that just read Sweave code chunks.


-- For better performance we put these functions in local variables:
local P, S, R, Cf, Cc, Ct, V, Cs, Cg, Cb, B, C, Cmt =
  lpeg.P, lpeg.S, lpeg.R, lpeg.Cf, lpeg.Cc, lpeg.Ct, lpeg.V,
  lpeg.Cs, lpeg.Cg, lpeg.Cb, lpeg.B, lpeg.C, lpeg.Cmt

local whitespacechar = S(" \t\r\n")
local spacechar = S(" \t")
local newline = P"\r"^-1 * P"\n"
local blankline = spacechar^0 * newline
local codeblockstart = P"<<"
                        * spacechar^0 -- Ignore spaces
                        * C((P(1) - P(spacechar^0 * P">>="))^0) -- Capture attributes between << and >>=
                        * spacechar^0 -- Ignore spaces
                        * ">>="

-- Grammar
G = P{ "Pandoc",
    Pandoc = Ct(V"Block"^0) / pandoc.Pandoc;
    Block = blankline^0
          * ( V"CodeBlock"
            + V"Para");
    Para = P(P(1) - codeblockstart)^1
            * newline^0;
    CodeBlock = codeblockstart
                * blankline
                * C((1 - (newline * P"@"))^0) -- Capture codes between << >>= and @
                * newline
                * P"@"
                / function(attributes, code) -- Return a CodeBlock with the captured attributes and codes
                    return pandoc.CodeBlock(code, {attributes = attributes})
                  end;
}


function Reader(input, reader_options)
    return lpeg.match(G, tostring(input))
end