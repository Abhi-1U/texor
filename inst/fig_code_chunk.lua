--- fig-code-chunk.lua – transforms Figure element into Rmarkdown style
--- figure chunk.
--- Copyright: © 2024 Abhishek Ulayil
--- License: MIT – see LICENSE for details

-- Makes sure users know if their pandoc version is too old for this
-- filter.
PANDOC_VERSION:must_be_at_least '3.1'

old_session = false
function write_to_file(filename,open_mode,content)
    local file,err = io.open(filename,open_mode)
    if file then
        file:write(content .. "\n")
        file:close()
    else
        print("error:", err)
    end
end

-- extracts image paths and widths
filter_image = {
    Image = function(img)
        table.insert(figure_src,img.src)
    local img_attr = img.attributes[1]
    local width = "100%"
    if img_attr == nil then
    width = "100%"
    else
        attribute_1 = img.attributes
    if img.attributes[1][2]:match('%\\') then
    width = tonumber(attribute_1[1][2]:match('%d+.%d+'))
    -- if width is defined but not parsed by pandoc
    if(attribute_1[1][2]:match('%d+.%d+') == nil) then
    width = "100%"
    -- if width is defined but is in raw form like [width=0.35\linewidth] in LaTeX
    else
        width = tostring(width*100) .. "%"
    end
    -- if width is defined and parsed as percentage by pandoc
    elseif(attribute_1[1][2]:match('%d%%') ~= nil) then
    width = attribute_1[1][2]
    -- default width handling at 100%
    else
        width = "100%"
    end
    end
    table.insert(figure_width,width)
    end
}

filter_check = {
    Image = function(el)
        if el.src:match('alg/') then
    local_fig_count = local_fig_count+1
    is_alg = 1
    is_fig = 0
    elseif el.src:match('lst/') then
    local_fig_count = local_fig_count+1
    is_listing = 1
    is_fig = 0
    elseif el.src:match('tikz/') then
    local_fig_count = local_fig_count+1
    tikz_style=1
    is_fig = 1
    is_alg = 0
    else
        local_fig_count = local_fig_count+1
    is_fig = 1
    is_alg = 0
    end
    end,
    Para = function(el)
        string_el = pandoc.utils.stringify(el)
    if string_el:find('^(= %[)') then
    tikz_style = 1
    end
    end,
    CodeBlock = function(el)
        is_code = 1
    end,
    Table = function(el)
        is_wdtable = 1
    end
}


function Figure(fig)
    -- temp variable to check for algorithm image
    is_alg = 0
    -- temp variable to check for figure image
    is_fig = 0
    is_listing = 0
    -- temp variable to check for widetable
    is_wdtable = 0
    is_code = 0
    tikz_syle = 0
    local_fig_count = 0
    pandoc.walk_block(fig,filter_check)
    -- Checks for code blocks or tables within the Figure environment
    -- falls back to default handling.
    if (is_code == 1) or (is_wdtable == 1) then
        return fig
    end
    figure_src = {}
    figure_width = {}
    -- identifier % label
    if old_session then
        write_to_file("fig_refs.txt",'a',pandoc.utils.stringify(fig.identifier))
    else
        write_to_file("fig_refs.txt",'w',pandoc.utils.stringify(fig.identifier))
        old_session = true
    end
    local identifier = sanitize_identifier(fig.identifier)
    -- caption % need to stringify
    local caption = pandoc.utils.stringify(fig.caption.long)
    -- alt % default alt text for images
    local alt = "graphic without alt text"
    -- alignment % default center
    local alignment = "center"
    pandoc.walk_block(fig,filter_image)
    local_raw_block_table = {}
    local attr_construct= [[```{r ]] .. identifier ..[[, echo=FALSE , fig.cap="]] .. caption
    .. [[", fig.alt="]].. alt .. [[",fig.show='hold', fig.align="]].. alignment..[["}]] .. string.char(10)
    -- if single image and width defined
    if #figure_src >= 1 then
        attr_construct= [[```{r ]] .. identifier ..[[, echo=FALSE , fig.cap="]] .. caption
        .. [[", fig.alt="]] .. alt .. [[", fig.show='hold', fig.align="]].. alignment ..[[", out.width="]]
        .. figure_width[1] .. [["}]] .. string.char(10)
    end
    table.insert(local_raw_block_table,attr_construct)
    local knitr_command='knitr::include_graphics(c('
    for i = 1,#figure_src,1 do
        if i ~= 1 then
            knitr_command= knitr_command .. [[,]]
        end
    knitr_command= knitr_command .. [["]] .. figure_src[i] .. [["]]
    end
    knitr_command = knitr_command ..'))' .. string.char(10)
    table.insert(local_raw_block_table,knitr_command)
    local attr_end = [[```]]
    table.insert(local_raw_block_table,attr_end)
    -- Markdown Rawblock to replace the existing Figure element
    knitr_block = pandoc.RawInline('markdown', pandoc.utils.stringify(local_raw_block_table))
    return knitr_block
end

function sanitize_identifier(identifier)
    l = identifier
    l = string.gsub(l, "%.", "-")
    l = string.gsub(l, "_", "-")
    l = string.gsub(l, " ", "-")
    l = string.gsub(l,"#","")
    l = string.gsub(l,":","")
    return l
end
