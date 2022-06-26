function CodeBlock(block)
   return pandoc.CodeBlock(block.content,block.attributes)
end

ENV_CLASSES = { 'example', 'example*', 'Sinput', 'Soutput', 'Sin','Sout','Scode'}

 

local function markdown(s)
  return pandoc.RawBlock('markdown', s)
end

 

local function get_env_class(elem)
    if elem.classes then
        for i = 1, #elem.classes do
            for j = 1, #ENV_CLASSES do
                if elem.classes[i] == ENV_CLASSES[j] then 
                    return ENV_CLASSES[j] 
                end
            end
         end
    else
        error('function has_class used on an element of type ' ..elem.t .. ' that cannot have classes.') 
    end 
end

 
function Div(el)
  local env_class = get_env_class(el) -- nil if there is none
  if env_class then -- false if env_class is nil
    return { markdown('```R\n'), el, markdown('\n```') }
  end
end


--function Div(el)
--    if el.classes[1] == 'Schunk' and el.classes[1] == 'Sinput' or el.classes[1] == 'Soutput' then
        --local contentElements = pandoc.readBlock(el.content)
--        return {CodeBlock(el)}
--    end
--end 
