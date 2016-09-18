local bidi = require('bidi')
local serpent  = require('serpent') -- luarocks install serpent

-- Simple example of using luabidi module.

local text = {0x06CC, 0x06C1} -- "یہ" U+06CC U+06C1

local reordered_text = bidi.get_visual_reordering(text)

-- hex representation
for i,v in ipairs(reordered_text) do
  reordered_text[i] = string.format("U+%04X", v)
end

for i,v in ipairs(text) do
  text[i] = string.format("U+%04X", v)
end

print("Original codepoints (in logical order): " .. serpent.line(text,{comment = false}))
print("Visual reordering: " .. serpent.line(reordered_text,{comment = false}))
