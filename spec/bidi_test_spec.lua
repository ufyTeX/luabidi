local ucdn = require("ucdn")
local bidi = require("bidi")
local ucd_utils = require("ucd_utils")
local serpent = require("serpent")

local bidiClass = {
  ["AL" ]         = ucdn.UCDN_BIDI_CLASS_AL,
  ["AN" ]         = ucdn.UCDN_BIDI_CLASS_AN,
  ["B"  ]         = ucdn.UCDN_BIDI_CLASS_B,
  ["BN" ]         = ucdn.UCDN_BIDI_CLASS_BN,
  ["CS" ]         = ucdn.UCDN_BIDI_CLASS_CS,
  ["EN" ]         = ucdn.UCDN_BIDI_CLASS_EN,
  ["ES" ]         = ucdn.UCDN_BIDI_CLASS_ES,
  ["ET" ]         = ucdn.UCDN_BIDI_CLASS_ET,
  ["L"  ]         = ucdn.UCDN_BIDI_CLASS_L,
  ["NSM"]         = ucdn.UCDN_BIDI_CLASS_NSM,
  ["ON" ]         = ucdn.UCDN_BIDI_CLASS_ON,
  ["R"  ]         = ucdn.UCDN_BIDI_CLASS_R,
  ["S"  ]         = ucdn.UCDN_BIDI_CLASS_S,
  ["WS" ]         = ucdn.UCDN_BIDI_CLASS_WS,
  ["LRO"]         = ucdn.UCDN_BIDI_CLASS_LRO,
  ["RLO"]         = ucdn.UCDN_BIDI_CLASS_RLO,
  ["LRE"]         = ucdn.UCDN_BIDI_CLASS_LRE,
  ["RLE"]         = ucdn.UCDN_BIDI_CLASS_RLE,
  ["PDF"]         = ucdn.UCDN_BIDI_CLASS_PDF,
  ["LRI"]         = ucdn.UCDN_BIDI_CLASS_LRI,
  ["RLI"]         = ucdn.UCDN_BIDI_CLASS_RLI,
  ["FSI"]         = ucdn.UCDN_BIDI_CLASS_FSI,
  ["PDI"]         = ucdn.UCDN_BIDI_CLASS_PDI
}


local removeClasses = {
  [ucdn.UCDN_BIDI_CLASS_LRO]  = true,
  [ucdn.UCDN_BIDI_CLASS_RLO] = true,
  [ucdn.UCDN_BIDI_CLASS_RLE] = true,
  [ucdn.UCDN_BIDI_CLASS_LRE] = true,
  [ucdn.UCDN_BIDI_CLASS_PDF] = true,
  [ucdn.UCDN_BIDI_CLASS_BN]  = true,
}

local function filterOrder(types, order)
  local no = {}
  for _,o in ipairs(order) do
    if not removeClasses[types[o]] then table.insert(no, o) end
  end
  return no
end

-- shift order by 1 to account for Lua’s indexing
local function shiftOrder(order)
  local shifted = {}
  for i,v in ipairs(order) do
    shifted[i] = v + 1
  end
  return shifted
end

local function table_str(t)
  return serpent.line(t, {comment = false, compact = true})
end

local function getAllLevels(types, levels)
  local allLevels = {}
  for i, v in ipairs(levels) do
    if not removeClasses[types[i]] then
      table.insert(allLevels, v)
    else
      table.insert(allLevels, 'x')
    end
  end
  return allLevels
end

local function run_bidi_test_case(test_case)
  for i = 0,2 do
    if bit32.band(test_case.bitset,bit32.lshift(1,i)) == 0 then goto continue1 end


    local types, pairValues, pairTypes = {}, {}, {}
    for j, v in ipairs(test_case.types) do
      types[j] = bidiClass[v]

      -- We ignore the bracketing part of the algorithm.
      pairValues[j] = 0
      pairTypes[j] = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_NONE
    end

    local level = i - 1
    local par = bidi.Paragraph.new(types, pairTypes, pairValues, level)
    -- Test levels
    local levels = par:getLevels({ #types + 1 })
    for j,l in ipairs(test_case.levels) do
      if l == "x" then
        -- continue
      else
        if l ~= levels[j] then
          print(table_str(test_case))
          print(string.format("%s:%d:levels: got %s; want %s",table_str(test_case.types),level,table_str(levels),table_str(test_case.levels)))
          os.exit(1)
        end
      end
    end

    -- Test ordering
    local order = par:getReordering({ #types + 1})
    local gotOrder = filterOrder(types,order)
    local wantOrder = shiftOrder(test_case.reordering)

    if table_str(gotOrder) ~= table_str(wantOrder) then
      print(table_str(test_case))
      print(string.format("level: %d, got order: %s, want %s, original, %s", level, table_str(gotOrder), table_str(wantOrder), table_str(order)))
      os.exit(1)
    end
    ::continue1::
  end
end

local function run_bidi_char_test_case(test_case)
  local level = test_case.dir
  if level == 2 then level = nil end -- Fixing mismatch in test file and algorithm

  local types = bidi.codepoints_to_types(test_case.codepoints)
  local pairValues = bidi.codepoints_to_pair_values(test_case.codepoints)
  local pairTypes = bidi.codepoints_to_pair_types(test_case.codepoints)

  -- print(table_str(types))
  -- print(table_str(pairTypes))
  -- print(table_str(pairValues))

  local par = bidi.Paragraph.new(types, pairTypes,  pairValues, level)

  -- Test resolved level
  if par.paragraphEmbeddingLevel ~= test_case.resolvedEmbeddingLevel then
    print(table_str(test_case))
    print(string.format("level: got %d, want: %d", par.paragraphEmbeddingLevel, test_case.resolvedEmbeddingLevel))
    os.exit(1)
  end

  -- Test levels
  local levels = par:getLevels({ #types + 1 })
  local allLevels = getAllLevels(types,levels)
  if table_str(allLevels) ~= table_str(test_case.resolvedLevels) then
    print(table_str(test_case))
    print(string.format("resolved levels: got: %s", table_str(allLevels)))
    os.exit(1)
  end

  -- Test ordering
  local order = par:getReordering({ #types + 1})
  local gotOrder = filterOrder(types,order)
  local wantOrder = shiftOrder(test_case.visualOrdering)

  if table_str(gotOrder) ~= table_str(wantOrder) then
    print(table_str(test_case))
    print(string.format("reordering: got: %s, want: %s", table_str(order), table_str(wantOrder)))
    os.exit(1)
  end
end

-- performs the tests in BidiTest.txt.
-- See http://www.unicode.org/Public/UCD/latest/ucd/BidiTest.txt.
describe("Test cases in BidiTest.txt file #ucd #biditest", function()
  it("should pass", function()
    print("running tests in BidiTest.txt file...")
    for test_case in ucd_utils.BidiTestReader.reader() do
      run_bidi_test_case(test_case)
    end
  end)
end)

-- performs the tests in BidiTest.txt.
-- See http://www.unicode.org/Public/UCD/latest/ucd/BidiCharacterTest.txt.
describe("Test cases in BidiCharacterTest.txt file #ucd #bidichartest", function()
  it("should pass", function()
    print("running tests in BidiCharacterTest.txt file...")
    for test_case in ucd_utils.BidiCharacterTestReader.reader() do
      run_bidi_char_test_case(test_case)
    end
  end)
end)

describe("Single test case #ucd #single #biditest", function()
  it("should pass", function()
    local test_case = {bitset=7,levels={1,1,1},line_no=67849,reordering={2,1,0},types={"R","ES","R"}}
    run_bidi_test_case(test_case)
  end)
end)

describe("Single test case #ucd #single #bidichartest", function()
  it("should pass", function()
    local test_case = {codepoints={97,40,98,41,41},dir=1,line_no=48719,resolvedEmbeddingLevel=1,resolvedLevels={2,2,2,2,1},visualOrdering={4,0,1,2,3}}
    run_bidi_char_test_case(test_case)
  end)
end)
