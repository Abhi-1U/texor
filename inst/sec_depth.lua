-- Borrowed from rjtools

function Header (h)
    if h.level < 6 then
        h.level = h.level+1
    else
        -- pass
    end
    return h
end
