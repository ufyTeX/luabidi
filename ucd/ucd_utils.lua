-- patterns
local comment =  "^#.*$"
local empty = "^$"
local level =  "^@Levels:%s+(.*)"
local reorder = "^@Reorder:%s+(.*)"
local data = "^([%S%s]+);%s*(%d+)$"


local BidiTestReader, BidiCharacterTestReader = {}, {}

-- Provides an iterator for tests cases in the BidiTest.txt file.
--
-- Function takes an optional `limit` argument that can be used to
-- limit the number of test cases.
function BidiTestReader.reader(limit)
  return coroutine.wrap(function()
    local levels, reordering, types
    local line_no = 0
    local test_case_no = 0

    for line in io.lines("ucd/BidiTest.txt") do
      line_no = line_no + 1
      if not string.find(line, comment) and
        not string.find(line, empty) then
        if string.find(line,level) then
          local level_str = string.match(line, level)
          levels = {}
          for l in string.gmatch(level_str, "%S+") do
            table.insert(levels, tonumber(l) or l)
          end
        elseif string.find(line,reorder) then
          local reorder_str = string.match(line, reorder)
          reordering = {}
          for r in string.gmatch(reorder_str, "%S+") do
            table.insert(reordering, tonumber(r) or r)
          end
        else
          local input, bitset = string.match(line,data)
          types = {}
          for t in string.gmatch(input, "%S+") do
            table.insert(types, t)
          end
          local test_case = {
            line_no = line_no,
            levels = levels,
            reordering = reordering,
            types = types,
            bitset = tonumber(bitset)
          }
          test_case_no = test_case_no + 1
          coroutine.yield(test_case)
          if limit and test_case_no == limit then return end
        end
      end
    end
  end)
end


local function numberstohex(tbl)
  local hex = {}
  for i,v in ipairs(tbl) do
    hex[i] = string.format("%04X",v)
  end

  return hex
end

-- Provides an iterator for tests cases in the BidiCharacterTest.txt file.
--
-- Function takes an optional `limit` argument that can be used to
-- limit the number of test cases.
function BidiCharacterTestReader.reader(limit)
  return coroutine.wrap(function()
    local codepoints, dir, resolvedEmbeddingLevel, resolvedLevels, visualOrdering
    local line_no = 0
    local test_case_no = 0

    for line in io.lines("ucd/BidiCharacterTest.txt") do
      line_no = line_no + 1
      if not string.find(line, comment) and
        not string.find(line, empty) then
        local fn = 0
        for field in string.gmatch(line, "([%w%s]+);?") do
          fn = fn + 1
          if fn == 1 then -- A sequence of hexadecimal code point values separated by space
            codepoints = {}
            -- setmetatable(codepoints, { __serialize = numberstohex })
            for cp in string.gmatch(field, "%S+") do
              table.insert(codepoints, tonumber(cp, 16))
            end
          elseif fn == 2 then -- A value representing the paragraph direction (0,1 or 2)
            dir = tonumber(field)
          elseif fn == 3 then -- The resolved paragraph embedding level
            resolvedEmbeddingLevel = tonumber(field)
          elseif fn == 4 then -- A list of resolved levels; characters removed in rule X9 are indicated with an 'x'
            resolvedLevels = {}
            for l in string.gmatch(field, "%S+") do
              table.insert(resolvedLevels, tonumber(l) or l)
            end
          elseif fn == 5 then -- list of indices showing the resulting visual ordering from left to right; characters with a resolved level of 'x' are skipped
            visualOrdering = {}
            for o in string.gmatch(field, "%S+") do
              table.insert(visualOrdering, tonumber(o))
            end
          else
            error("Parse error on line " .. line_no)
          end
        end
        local test_case = {
          line_no = line_no,
          codepoints = codepoints,
          dir = dir,
          resolvedEmbeddingLevel = resolvedEmbeddingLevel,
          resolvedLevels = resolvedLevels,
          visualOrdering = visualOrdering
        }
        test_case_no = test_case_no + 1
        coroutine.yield(test_case)
        if limit and test_case_no == limit then return end
      end
    end
  end)
end

return {
  BidiTestReader = BidiTestReader,
  BidiCharacterTestReader = BidiCharacterTestReader
}
