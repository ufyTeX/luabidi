local bidi = require('bidi')
local serpent  = require('serpent') -- luarocks install serpent

-- Example of using the types exported from luabidi module.

local codepoints = {0x06CC, 0x06C1} -- "یہ" U+06CC U+06C1


local types = bidi.codepoints_to_types(codepoints)
local pair_types = bidi.codepoints_to_pair_types(codepoints)
local pair_values = bidi.codepoints_to_pair_values(codepoints)
local dir = 1 -- RTL

local para = bidi.Paragraph.new(types, pair_types, pair_values, dir)

local linebreaks = { #codepoints + 1 }
local levels = para:getLevels(linebreaks)
local reordering = para:getReordering(linebreaks)

print("Resolved Levels: " .. serpent.line(levels, {comment = false}))
print("Reordering: " .. serpent.line(reordering, {comment = false}))

