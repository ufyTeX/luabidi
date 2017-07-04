-----------
-- Lua port of the reference implementation of the Unicode
-- Bidirectional Algorithm (UAX #9).
--
-- * [Source on Github](https://github.com/deepakjois/luabidi)
--
-- @author Deepak Jois <<deepak.jois@gmail.com>>
-- @copyright 2016
-- @license MIT
-- @module bidi
local ucdn = require("ucdn")
local bracket = require("bidi.bracket")

-- This implementation is a port based on the reference implementation found at:
-- http://www.unicode.org/Public/PROGRAMS/BidiReferenceJava/
--
-- described in Unicode Bidirectional Algorithm (UAX #9).
--
-- Input:
-- There are two levels of input to the algorithm, since clients may prefer to
-- supply some information from out-of-band sources rather than relying on the
-- default behavior.
--
-- - Bidi class array
-- - Bidi class array, with externally supplied base line direction
--
-- Output:
-- Output is separated into several stages:
--
--  - levels array over entire paragraph
--  - reordering array over entire paragraph
--  - levels array over line
--  - reordering array over line
--
-- Note that for conformance to the Unicode Bidirectional Algorithm,
-- implementations are only required to generate correct reordering and
-- character directionality (odd or even levels) over a line. Generating
-- identical level arrays over a line is not required. Bidi explicit format
-- codes (LRE, RLE, LRO, RLO, PDF) and BN can be assigned arbitrary levels and
-- positions as long as the rest of the input is properly reordered.
--
-- As the algorithm is defined to operate on a single paragraph at a time, this
-- implementation is written to handle single paragraphs. Thus rule P1 is
-- presumed by this implementation-- the data provided to the implementation is
-- assumed to be a single paragraph, and either contains no 'B' codes, or a
-- single 'B' code at the end of the input. 'B' is allowed as input to
-- illustrate how the algorithm assigns it a level.
--
-- Also note that rules L3 and L4 depend on the rendering engine that uses the
-- result of the bidi algorithm. This implementation assumes that the rendering
-- engine expects combining marks in visual order (e.g. to the left of their
-- base character in RTL runs) and that it adjusts the glyphs used to render
-- mirrored characters that are in RTL runs so that they render appropriately.

--- Paragraph contains the state of a paragraph.
-- @type Paragraph
local Paragraph = {}
Paragraph.__index = Paragraph

local L   = ucdn.UCDN_BIDI_CLASS_L
local LRE = ucdn.UCDN_BIDI_CLASS_LRE
local LRO = ucdn.UCDN_BIDI_CLASS_LRO
local R   = ucdn.UCDN_BIDI_CLASS_R
local AL  = ucdn.UCDN_BIDI_CLASS_AL
local RLE = ucdn.UCDN_BIDI_CLASS_RLE
local RLO = ucdn.UCDN_BIDI_CLASS_RLO
local PDF = ucdn.UCDN_BIDI_CLASS_PDF
local EN  = ucdn.UCDN_BIDI_CLASS_EN
local ES  = ucdn.UCDN_BIDI_CLASS_ES
local ET  = ucdn.UCDN_BIDI_CLASS_ET
local AN  = ucdn.UCDN_BIDI_CLASS_AN
local CS  = ucdn.UCDN_BIDI_CLASS_CS
local NSM = ucdn.UCDN_BIDI_CLASS_NSM
local BN  = ucdn.UCDN_BIDI_CLASS_BN
local B   = ucdn.UCDN_BIDI_CLASS_B
local S   = ucdn.UCDN_BIDI_CLASS_S
local WS  = ucdn.UCDN_BIDI_CLASS_WS
local ON  = ucdn.UCDN_BIDI_CLASS_ON
local LRI = ucdn.UCDN_BIDI_CLASS_LRI
local RLI = ucdn.UCDN_BIDI_CLASS_RLI
local FSI = ucdn.UCDN_BIDI_CLASS_FSI
local PDI = ucdn.UCDN_BIDI_CLASS_PDI

local TYPE_MIN = ucdn.UCDN_BIDI_CLASS_L
local TYPE_MAX = ucdn.UCDN_BIDI_CLASS_PDI

local typenames = {
  "L",
  "LRE",
  "LRO",
  "R",
  "AL",
  "RLE",
  "RLO",
  "PDF",
  "EN",
  "ES",
  "ET",
  "AN",
  "CS",
  "NSM",
  "BN",
  "B",
  "S",
  "WS",
  "ON",
  "LRI",
  "RLI",
  "FSI",
  "PDI"
}

local implicitEmbeddingLevel = -1

local resolvePairedBrackets = bracket.resolvePairedBrackets

-- Throw exception if type array is invalid.
local function validateTypes(types)
  if type(types) ~= "table"  then
    error "types is invalid"
  elseif #types == 0 then
    error "types is empty"
  else
    for i,v in ipairs(types) do
      if v < TYPE_MIN or v > TYPE_MAX then
        error("illegal type value at "..i..": "..v)
      end
    end

    for i = 1, #types-1 do
      if types[i] == ucdn.UCDN_BIDI_CLASS_B then
        error("B type before end of paragraph at index: "..i)
      end
    end
  end
end

-- Throw exception if paragraph embedding level is invalid. Special
-- allowance for implicitEmbeddinglevel so that default processing of the
-- paragraph embedding level as implicit can still be performed when
-- using this API.
local function validateParagraphEmbeddingLevel(paragraphEmbeddingLevel)
  if paragraphEmbeddingLevel ~= 0 and
    paragraphEmbeddingLevel ~= 1 and
    paragraphEmbeddingLevel ~= implicitEmbeddingLevel then
    error("illegal paragraph embedding level: "..paragraphEmbeddingLevel)
  end
end

-- Throw exception if line breaks array is invalid.
local function validateLineBreaks(linebreaks, textLength)
  local prev = 0
  for i = 1, #linebreaks do
    local next_ = linebreaks[i]
    if next_ <= prev then error("bad linebreak: "..next_.."at index: "..i) end
    prev = next_
  end

  if prev ~= textLength + 1 then error("last linebreak must be at "..textLength) end
end

-- Throw exception if pairTypes array is invalid
local function validatePbTypes(pairTypes)
  if type(pairTypes) ~= "table" then
    error "pairTypes is invalid"
  elseif #pairTypes == 0 then
    error "pairTypes is empty"
  else
    for i,v in ipairs(pairTypes) do
      if v < ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_OPEN or v > ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_NONE then
        error("illegal pairType value at "..i..": "..v)
      end
    end
  end
end

-- Throw exception if pairValues array is invalid or doesn't match pairTypes in length
-- Unfortunately there's little we can do in terms of validating the values themselves
local function validatePbValues(pairValues, pairTypes)
  if type(pairValues) ~= "table" then
    error "pairValues is invalid"
  elseif #pairValues == 0 then
    error "pairValues is empty"
  elseif #pairTypes ~= #pairValues then
    error "pairTypes is different length from pairValues"
  end
end

-- Return true if the type is considered a whitespace type for the line
-- break rules.
local function isWhitespace(biditype)
  if biditype == LRE
    or biditype == RLE
    or biditype == LRO
    or biditype == RLO
    or biditype == PDF
    or biditype == LRI
    or biditype == RLI
    or biditype == FSI
    or biditype == PDI
    or biditype == BN
    or biditype == WS then
    return true
  else
    return false
  end
end

-- Set levels from start up to (but not including) limit to newLevel.
local function setLevels(levels, newLevel, len)
  for i = 1, len do
    levels[i] = newLevel
  end
end

-- Return true if the type is one of the types removed in X9.
local function isRemovedByX9(bidiType)
  if bidiType == LRE
    or bidiType == RLE
    or bidiType == LRO
    or bidiType == RLO
    or bidiType == PDF
    or bidiType == BN then
    return true
  else
    return false
  end
end

-- Return the strong type (L or R) corresponding to the level.
local function typeForLevel(level)
  if bit32.band(level, 1) == 0 then return L else return R end
end

--- Initialize a new paragraph.
--
-- Initialize a new paragaph using several arrays of direction and other types
-- and an externally supplied paragraph embedding level.
--
-- @param types Bidi\_Class property (directional codes) for each character in the
-- original string. Codes must correspond to the values in the
-- [luaucdn](http://deepakjois.github.io/luaucdn) module.
--
-- This can be generated from the original input text using
-- `codepoints_to_types` function.
--
-- @param pairTypes t Bidi\_Paired\_Bracket\_Type property  for each character
-- in the original string. Codes must correspond to the values in the
-- [luaucdn](http://deepakjois.github.io/luaucdn) module.
--
-- This can be generated from the original input text using
-- `codepoints_to_pair_types` function.
--
-- @param pairValues array of unique integers identifying which pair of
-- brackets (or canonically equivalent set) a bracket character belongs to. For
-- example in the string `[Test(s)>` the characters `(` and `)` would share one
-- value and `[` and `>` share another (assuming that `]` and `>` are
-- canonically equivalent).  Characters that have Bidi\_Paired\_Bracket\_Type `n` (None)
-- may always get a single value like 0.
--
-- This can be generated from the input text using `codepoints_to_pair_values`
-- function.
--
-- @param[opt=-1] paragraphEmbeddingLevel The embedding level may be  `0`(LTR), `1`(RTL) or `-1`(auto).
-- `-1` means apply the default algorithm (rules P2 and P3).
function Paragraph.new(types, pairTypes, pairValues, paragraphEmbeddingLevel)
  validateTypes(types)
  validatePbTypes(pairTypes)
  validatePbValues(pairValues, pairTypes)
  if paragraphEmbeddingLevel == nil then paragraphEmbeddingLevel = implicitEmbeddingLevel end
  validateParagraphEmbeddingLevel(paragraphEmbeddingLevel)

  local para = {}
  setmetatable(para, Paragraph)

  local initialTypes = {}
  local resultTypes = {}
  for i = 1, #types do
    initialTypes[i] = types[i]
    resultTypes[i] = types[i]
  end


  para.initialTypes = initialTypes
  para.paragraphEmbeddingLevel = paragraphEmbeddingLevel
  para.pairTypes = pairTypes
  para.pairValues = pairValues
  para.resultTypes = resultTypes

  para:runAlgorithm()

  return para
end

function Paragraph:length()
  return #self.initialTypes
end

-- The algorithm. Does not include line-based processing (Rules L1, L2).
-- These are applied later in the line-based phase of the algorithm.
function Paragraph:runAlgorithm()
  -- Preprocessing to find the matching isolates
  self:determineMatchingIsolates()

  -- 1) determining the paragraph level
  -- Rule P1 is the requirement for entering this algorithm.
  -- Rules P2, P3.
  -- If no externally supplied paragraph embedding level, use default.
  if self.paragraphEmbeddingLevel == implicitEmbeddingLevel then
    self.paragraphEmbeddingLevel = self:determineParagraphEmbeddingLevel(1, self:length())
  end

  -- Initialize result levels to paragraph embedding level.
  self.resultLevels = {}
  setLevels(self.resultLevels, self.paragraphEmbeddingLevel, self:length())

  -- 2) Explicit levels and directions
  -- Rules X1-X8.
  self:determineExplicitEmbeddingLevels()

  -- Rule X9.
  -- We do not remove the embeddings, the overrides, the PDFs, and the BNs
  -- from the string explicitly. But they are not copied into isolating run
  -- sequences when they are created, so they are removed for all
  -- practical purposes.

  -- Rule X10.
  -- Run remainder of algorithm one isolating run sequence at a time
  local sequences = self:determineIsolatingRunSequences()

  for _,sequence in ipairs(sequences) do
    -- 3) resolving weak types
    -- Rules W1-W7.
    sequence:resolveWeakTypes()

    -- 4a) resolving paired brackets
    -- Rule N0
    resolvePairedBrackets(sequence)

    -- 4b) resolving neutral types
    -- Rules N1-N3.
    sequence:resolveNeutralTypes()

    -- 5) resolving implicit embedding levels
    -- Rules I1, I2.
    sequence:resolveImplicitLevels()

    -- Apply the computed levels and types
    sequence:applyLevelsAndTypes()
  end

  -- Assign appropriate levels to 'hide' LREs, RLEs, LROs, RLOs, PDFs, and
  -- BNs. This is for convenience, so the resulting level array will have
  -- a value for every character.
  self:assignLevelsToCharactersRemovedByX9()
end

-- Determines the matching PDI for each isolate initiator and vice versa.
--
-- Definition BD9.
--
-- At the end of this function:
--
--  - The member variable matchingPDI is set to point to the index of the
--    matching PDI character for each isolate initiator character. If there is
--    no matching PDI, it is set to the length of the input text. For other
--    characters, it is set to -1.
--  - The member variable matchingIsolateInitiator is set to point to the
--    index of the matching isolate initiator character for each PDI character.
--    If there is no matching isolate initiator, or the character is not a PDI,
--    it is set to -1.
function Paragraph:determineMatchingIsolates()
  -- Index of matching PDI for isolate initiator characters. For other
  -- characters, the value of matchingPDI will be set to -1. For isolate
  -- initiators with no matching PDI, matchingPDI will be set to the length of
  -- the input string.
  self.matchingPDI = {}

  -- Index of matching isolate initiator for PDI characters. For other
  -- characters, and for PDIs with no matching isolate initiator, the value of
  -- matchingIsolateInitiator will be set to -1.
  self.matchingIsolateInitiator = {}

  for i = 1, self:length() do
    self.matchingIsolateInitiator[i] = -1
  end

  for i = 1, self:length() do
    self.matchingPDI[i] = -1
    local t = self.resultTypes[i]
    if t == LRI or t == RLI or t == FSI then
      local depthCounter = 1
      for j = i+1, self:length() do
        local u = self.resultTypes[j]
        if u == LRI or u == RLI or u == FSI then
          depthCounter = depthCounter + 1
        elseif u == PDI then
          depthCounter = depthCounter - 1
          if depthCounter == 0 then
            self.matchingPDI[i] = j
            self.matchingIsolateInitiator[j] = i
            break
          end
        end
      end

      if self.matchingPDI[i] == -1 then self.matchingPDI[i] = self:length() + 1 end
    end
  end
end

-- Reports the resolved paragraph direction of the substring limited by the
-- given range [startIndex, endIndex].
--
-- Determines the paragraph level based on rules P2, P3. This is also used
-- in rule X5c to find if an FSI should resolve to LRI or RLI.
function Paragraph:determineParagraphEmbeddingLevel(startIndex, endIndex)
  local strongType = -1 -- unknown

  -- Rule P2
  local i = startIndex
  while i <= endIndex do
    local t = self.resultTypes[i]
    if t == L or t == AL or t == R then
      strongType = t
      break
    elseif t == FSI or t == LRI or t == RLI then
      i = math.min(self.matchingPDI[i], self:length()) -- skip over to the matching PDI
      assert(i <= endIndex)
    end
    i = i + 1
  end

  -- Rule P3
  if strongType == -1 then
    return 0
  elseif strongType == L then
    return 0
  else -- AL, R
    return 1
  end
end

local MAX_DEPTH = 125

-- This stack will store the embedding levels and override and isolated
-- statuses
local directionalStatusStack = {}
directionalStatusStack.__index = directionalStatusStack

function directionalStatusStack.new()
  local stack = {
    stackCounter = 0,
    embeddingLevelStack = {},
    overrideStatusStack = {},
    isolateStatusStack = {}
  }

  setmetatable(stack, directionalStatusStack)

  return stack
end

function directionalStatusStack:empty()
  self.stackCounter = 0
end

function directionalStatusStack:push(level, overrideStatus, isolateStatus)
  self.stackCounter = self.stackCounter + 1
  self.embeddingLevelStack[self.stackCounter] = level
  self.overrideStatusStack[self.stackCounter] = overrideStatus
  self.isolateStatusStack[self.stackCounter] = isolateStatus
end

function directionalStatusStack:pop()
  self.stackCounter = self.stackCounter - 1
end

function directionalStatusStack:depth()
  return self.stackCounter
end

function directionalStatusStack:lastEmbeddingLevel()
  return self.embeddingLevelStack[self.stackCounter]
end

function directionalStatusStack:lastDirectionalOverrideStatus()
  return self.overrideStatusStack[self.stackCounter]
end

function directionalStatusStack:lastDirectionalIsolateStatus()
  return self.isolateStatusStack[self.stackCounter]
end

-- Determine explicit levels using rules X1 - X8
function Paragraph:determineExplicitEmbeddingLevels()
  local stack = directionalStatusStack.new()
  local overflowIsolateCount, overflowEmbeddingCount, validIsolateCount = 0, 0, 0

  -- Rule X1
  stack:empty()
  stack:push(self.paragraphEmbeddingLevel, ON, false)

  for i = 1, self:length() do
    local t = self.resultTypes[i]

    -- Rules X2, X3, X4, X5, X5a, X5b, X5c
    if t == RLE or
      t == LRE or
      t == RLO or
      t == LRO or
      t == RLI or
      t == LRI or
      t == FSI then
      local isIsolate = t == RLI or t == LRI or t == FSI
      local isRTL = t == RLE or t == RLO or t == RLI
      -- override if this is an FSI that resolves to RLI
      if t == FSI then
        isRTL = self:determineParagraphEmbeddingLevel(i+1, math.min(self.matchingPDI[i], self:length())) == 1
      end

      if isIsolate then
        self.resultLevels[i] = stack:lastEmbeddingLevel()
        if stack:lastDirectionalOverrideStatus() ~= ON then
          self.resultTypes[i] = stack:lastDirectionalOverrideStatus()
        end
      end

      local newLevel
      if isRTL then
        -- least greater odd
        newLevel = bit32.bor(stack:lastEmbeddingLevel() + 1, 1)
      else
        -- least greater even
        newLevel = bit32.band(stack:lastEmbeddingLevel() + 2, bit32.bnot(1))
      end

      if newLevel <= MAX_DEPTH and overflowIsolateCount == 0 and overflowEmbeddingCount == 0 then
        if isIsolate then validIsolateCount = validIsolateCount + 1 end

        -- Push new embedding level, override status, and isolated status.
        -- No check for valid stack counter, since the level check suffices.
        if t == LRO then
          stack:push(newLevel, L, isIsolate)
        elseif t == RLO then
          stack:push(newLevel, R, isIsolate)
        else
          stack:push(newLevel, ON, isIsolate)
        end

        -- Not really part of the spec
        if  not isIsolate then
          self.resultLevels[i] = newLevel
        end
      else
        -- This is an invalid explicit formatting character,
        -- so apply the "Otherwise" part of rules X2-X5b.
        if isIsolate then
          overflowIsolateCount = overflowIsolateCount + 1
        else -- not isIsolate
          if overflowIsolateCount == 0 then overflowEmbeddingCount = overflowEmbeddingCount + 1 end
        end
      end

      -- Rule X6a
    elseif t == PDI then
      if overflowIsolateCount > 0 then
        overflowIsolateCount = overflowIsolateCount - 1
      elseif validIsolateCount == 0 then
        -- do nothing
      else
        overflowEmbeddingCount = 0
        while not stack:lastDirectionalIsolateStatus() do
          stack:pop()
        end
        stack:pop()
        validIsolateCount = validIsolateCount - 1
      end
      self.resultLevels[i] = stack:lastEmbeddingLevel()

      -- Rule X7
    elseif t == PDF then
      -- Not really part of the spec
      self.resultLevels[i] = stack:lastEmbeddingLevel()

      if overflowIsolateCount > 0 then
        -- do nothing
      elseif overflowEmbeddingCount > 0 then
        overflowEmbeddingCount = overflowEmbeddingCount - 1
      elseif not stack:lastDirectionalIsolateStatus() and stack:depth() >= 2 then
        stack:pop()
      else
        -- do nothing
      end

    elseif t == B then
      -- Rule X8.
      --
      -- These values are reset for clarity, in this implementation B can only
      -- occur as the last code in the array.
      stack:empty()
      overflowIsolateCount = 0
      overflowEmbeddingCount = 0
      validIsolateCount = 0
      self.resultLevels[i] = self.paragraphEmbeddingLevel
    else
      self.resultLevels[i] = stack:lastEmbeddingLevel()
      if stack:lastDirectionalOverrideStatus() ~= ON then
        self.resultTypes[i] = stack:lastDirectionalOverrideStatus()
      end
    end
  end
end

local IsolatingRunSequence = {}
IsolatingRunSequence.__index = IsolatingRunSequence

-- Rule X10, second bullet: Determine the start-of-sequence (sos) and
-- end-of-sequence (eos) types, either L or R, for each isolating run sequence.
function IsolatingRunSequence.new(para, inputIndexes)
  local seq = {}
  setmetatable(seq, IsolatingRunSequence)

  seq.paragraph = para

  -- indexes to the original string
  seq.indexes = inputIndexes

  -- type of each character using the index
  seq.types = {}
  for i = 1, #seq.indexes do
    seq.types[i] = para.resultTypes[seq.indexes[i]]
  end

  -- assign level, sos and eos
  seq.level = para.resultLevels[seq.indexes[1]]

  local prevChar = seq.indexes[1] - 1
  while prevChar >= 1 and isRemovedByX9(para.initialTypes[prevChar]) do
    prevChar = prevChar - 1
  end

  local prevLevel = prevChar >= 1 and para.resultLevels[prevChar] or para.paragraphEmbeddingLevel
  seq.sos = typeForLevel(math.max(prevLevel, seq.level))

  local lastType = seq.types[#seq.types]
  local succLevel
  if lastType == LRI or lastType == RLI or lastType == FSI then
    succLevel = para.paragraphEmbeddingLevel
  else
    local limit = seq.indexes[#seq.indexes] + 1 -- the first character after the end of run sequence
    while limit <= para:length() and isRemovedByX9(para.initialTypes[limit]) do
      limit = limit + 1
    end
    succLevel = limit <= para:length() and para.resultLevels[limit] or para.paragraphEmbeddingLevel
  end

  seq.eos = typeForLevel(math.max(succLevel, seq.level))
  return seq
end

-- Resolving bidi paired brackets  Rule N0
function IsolatingRunSequence:resolvePairedBrackets()
  resolvePairedBrackets(self)
end

-- Resolving weak types Rules W1-W7.
--
-- Note that some weak types (EN, AN) remain after this processing is
-- complete.
function IsolatingRunSequence:resolveWeakTypes()
  -- on entry, only these types remain
  self:assertOnly({ L, R, AL, EN, ES, ET, AN, CS, B, S, WS, ON, NSM, LRI, RLI, FSI, PDI })

  -- Rule W1.
  -- Changes all NSMs.
  local precedingCharacterType = self.sos
  for i = 1, #self.types do
    local t = self.types[i]
    if t == NSM then
      self.types[i] = precedingCharacterType
    else
      -- if t == LRI or t == RLI or t == FSI or t == PDI then precedingCharacterType = ON end
      precedingCharacterType = t
    end
  end

  -- Rule W2.
  -- EN does not change at the start of the run, because sos != AL.
  for i = 1, #self.types do
    if self.types[i] == EN then
      local j = i - 1
      while j >= 1 do
        local t = self.types[j]
        if t == L or t == R or t == AL then
          if t == AL then self.types[i] = AN end
          break
        end
        j = j - 1
      end
    end
  end

  -- Rule W3.
  for i = 1, #self.types do
    if self.types[i] == AL then self.types[i] = R end
  end

  -- Rule W4.
  -- Since there must be values on both sides for this rule to have an
  -- effect, the scan skips the first and last value.
  --
  -- Although the scan proceeds left to right, and changes the type
  -- values in a way that would appear to affect the computations
  -- later in the scan, there is actually no problem. A change in the
  -- current value can only affect the value to its immediate right,
  -- and only affect it if it is ES or CS. But the current value can
  -- only change if the value to its right is not ES or CS. Thus
  -- either the current value will not change, or its change will have
  -- no effect on the remainder of the analysis.

  for i = 2, #self.types - 1 do
    if self.types[i] == ES or self.types[i] == CS then
      local prevSepType = self.types[i - 1]
      local succSepType = self.types[i + 1]
      if prevSepType == EN and succSepType == EN then
        self.types[i] = EN
      elseif self.types[i] == CS and prevSepType == AN and succSepType == AN then
        self.types[i] = AN
      end
    end
  end

  do
    -- Rule W5.
    local i = 1
    while i <= #self.types do
      if self.types[i] == ET then
        -- locate end of sequence
        local runstart = i
        local runlimit = self:findRunLimit(runstart, #self.types, {ET})
        -- check values at ends of sequence
        local t = runstart == 1 and self.sos or self.types[runstart - 1]

        if t ~= EN then t = runlimit == #self.types and self.eos or self.types[runlimit + 1] end

        if t == EN then self:setTypes(runstart, runlimit, EN) end

        -- continue at end of sequence
        i = runlimit
      end
      i = i + 1
    end

  end

  -- Rule W6.
  for i = 1, #self.types do
    local t = self.types[i]
    if t == ES or t == ET or t == CS then self.types[i] = ON end
  end

  -- Rule W7
  for i = 1, #self.types do
    if self.types[i] == EN then
      -- set default if we reach start of run
      local prevStrongType = self.sos
      local j = i - 1
      while j >= 1 do
        local t = self.types[j]
        if t == L or t == R then -- // AL's have been changed to R
          prevStrongType = t
          break
        end
        j = j - 1
      end
      if prevStrongType == L then self.types[i] = L end
    end
  end

end

-- 6) resolving neutral types Rules N1-N2.
function IsolatingRunSequence:resolveNeutralTypes()

  self:assertOnly({ L, R, EN, AN, B, S, WS, ON, RLI, LRI, FSI, PDI })

  local i = 1
  while i <= #self.types do
    local t = self.types[i]
    if t == WS or t == ON or t == B or t == S or t == RLI or t == LRI or t == FSI or t == PDI then
      local runstart = i
      local runlimit = self:findRunLimit(runstart, #self.types, {B, S, WS, ON, RLI, LRI, FSI, PDI})

      -- determine effective types at ends of run
      local leadingType
      local trailingType

      -- Note that the character found can only be L, R, AN, or
      -- EN
      if (runstart == 1) then
        leadingType = self.sos
      else
        leadingType = self.types[runstart - 1]
        if leadingType == AN or leadingType == EN then leadingType = R end
      end

      if runlimit == #self.types then
        trailingType = self.eos
      else
        trailingType = self.types[runlimit + 1]
        if trailingType == AN or trailingType == EN then trailingType = R end
      end

      local resolvedType
      if leadingType == trailingType then
        -- Rule N1
        resolvedType = leadingType
      else
        -- Rule N2.
        -- Notice the embedding level of the run is used, not
        -- the paragraph embedding level.
        resolvedType = typeForLevel(self.level)
      end
      self:setTypes(runstart, runlimit, resolvedType)

      -- skip over run of (former) neutrals
      i = runlimit
    end
    i = i + 1
  end

end

-- 7) resolving implicit embedding levels Rules I1, I2.
function IsolatingRunSequence:resolveImplicitLevels()
  -- on entry, only these types can be in resultTypes
  self:assertOnly({L, R, EN, AN})

  self.resolvedLevels = {}
  setLevels(self.resolvedLevels, self.level, #self.types)

  if bit32.band(self.level, 1) == 0 then -- even level
    for i = 1, #self.types do
      local t = self.types[i]
      -- Rule I1
      if t == L then
        -- no change
      elseif t == R then
        self.resolvedLevels[i] = self.resolvedLevels[i] + 1
      else -- t == AN || t == EN
        self.resolvedLevels[i] = self.resolvedLevels[i] + 2
      end
    end
  else -- odd level
    for i = 1, #self.types do
      local t = self.types[i]
      -- Rule I2
      if t == R then
        -- no change
      else -- t == L || t == AN || t == EN
        self.resolvedLevels[i] = self.resolvedLevels[i] + 1
      end
    end
  end
end

-- Applies the levels and types resolved in rules W1-I2 to the
-- resultLevels array.
function IsolatingRunSequence:applyLevelsAndTypes()
  for i = 1, #self.indexes do
    local originalIndex = self.indexes[i]
    self.paragraph.resultTypes[originalIndex] = self.types[i]
    self.paragraph.resultLevels[originalIndex] = self.resolvedLevels[i]
  end
end

-- Return the limit of the run consisting only of the types in validSet
-- starting at index. This checks the value at index, and will return
-- index if that value is not in validSet.
function IsolatingRunSequence:findRunLimit(index, limit, validSet)
  while index <= limit do
    local t = self.types[index]
    for i = 1, #validSet do
      if t == validSet[i] then
        index = index + 1
        goto continue1
      end
    end
    do return index - 1 end
    ::continue1::
  end
  return limit
end

-- Set types from start up to (but not including) limit to newType.
function IsolatingRunSequence:setTypes(start, limit, newType)
  for i = start, limit do
    self.types[i] = newType
  end
end

-- Algorithm validation. Assert that all values in types are in the
-- provided set.
function IsolatingRunSequence:assertOnly(codes)
  for i = 1, #self.types do
    local t = self.types[i]
    for j = 1, #codes do
      if t == codes[j] then goto continue2 end
    end
    error("invalid bidi code "..typenames[t+1].." present in assertOnly at position "..self.indexes[i])
    ::continue2::
  end
end


-- returns an array of level runs. Each level run is described as an array of
-- indexes into the input string.
--
-- Determines the level runs. Rule X9 will be applied in determining the
-- runs, in the way that makes sure the characters that are supposed to be
-- removed are not included in the runs.
function Paragraph:determineLevelRuns()
  -- temporary array to hold the run
  local temporaryRun = {}
  -- temporary array to hold the list of runs
  local allRuns = {}
  local numRuns = 0

  local currentLevel = -1
  local runLength = 0
  for i = 1, self:length() do
    if not isRemovedByX9(self.initialTypes[i]) then
      if self.resultLevels[i] ~= currentLevel then -- we just encountered a new run
        -- Wrap up last run
        if currentLevel >= 0 then
          numRuns = numRuns + 1
          allRuns[numRuns] = temporaryRun
          temporaryRun = {}
        end

        -- Start new run
        currentLevel = self.resultLevels[i]
        runLength = 0
      end
      runLength = runLength + 1
      temporaryRun[runLength] = i
    end
  end

  -- Wrap up the final run, if any
  if runLength ~= 0 then
    numRuns = numRuns + 1
    allRuns[numRuns] = temporaryRun
  end

  return allRuns
end

-- Definition BD13. Determine isolating run sequences.
function Paragraph:determineIsolatingRunSequences()
  local levelRuns = self:determineLevelRuns()
  local numRuns = #levelRuns

  -- Compute the run that each character belongs to
  local runForCharacter = {}
  for runNumber = 1, numRuns do
    for i = 1, #levelRuns[runNumber] do
      local characterIndex = levelRuns[runNumber][i]
      runForCharacter[characterIndex] = runNumber
    end
  end

  local sequences = {}
  local numSequences = 0
  local currentRunSequence
  for i = 1, #levelRuns do
    local firstCharacter = levelRuns[i][1]
    if self.initialTypes[firstCharacter] ~= PDI or self.matchingIsolateInitiator[firstCharacter] == -1 then
      currentRunSequence = {}
      local currentRunSequenceLength = 0
      local run = i
      repeat
        for j = 1, #levelRuns[run] do
          currentRunSequence[currentRunSequenceLength + j] = levelRuns[run][j]
        end
        currentRunSequenceLength = currentRunSequenceLength + #levelRuns[run]

        local lastCharacter = currentRunSequence[currentRunSequenceLength]
        local lastType = self.initialTypes[lastCharacter]
        if (lastType == LRI or lastType == RLI or lastType == FSI) and self.matchingPDI[lastCharacter] ~= self:length() + 1 then
          run = runForCharacter[self.matchingPDI[lastCharacter]]
        else
          break
        end
      until false
      numSequences = numSequences + 1
      sequences[numSequences] = IsolatingRunSequence.new(self,currentRunSequence)
    end
  end
  return sequences
end

-- Assign level information to characters removed by rule X9. This is for
-- ease of relating the level information to the original input data. Note
-- that the levels assigned to these codes are arbitrary, they're chosen so
-- as to avoid breaking level runs.
function Paragraph:assignLevelsToCharactersRemovedByX9()
  for i = 1, #self.initialTypes do
    local t = self.initialTypes[i]
    if t == LRE or t == RLE or t == LRO or t == RLO or t == PDF or t == BN then
      self.resultTypes[i] = t
      self.resultLevels[i] = -1
    end
  end

  -- now propagate forward the levels information (could have
  -- propagated backward, the main thing is not to introduce a level
  -- break where one doesn't already exist).

  if self.resultLevels[1] == -1 then self.resultLevels[1] = self.paragraphEmbeddingLevel end
  for i = 2, #self.initialTypes do
    if self.resultLevels[i] == -1 then self.resultLevels[i] = self.resultLevels[i-1] end
  end


  -- Embedding information is for informational purposes only
  -- so need not be adjusted.
end

--
-- Output
--

--- Return levels array breaking lines at offsets in linebreaks (Rule L1).
--
-- The returned levels array contains the resolved level for each bidi code
-- passed to the constructor.
--
-- @param linebreaks The linebreaks array must include at least one value. The values must be
-- in strictly increasing order (no duplicates) between 1 and the length of
-- the text, inclusive. The last value must be the length of the text.
function Paragraph:getLevels(linebreaks)

  -- Note that since the previous processing has removed all
  -- P, S, and WS values from resultTypes, the values referred to
  -- in these rules are the initial types, before any processing
  -- has been applied (including processing of overrides).
  --
  -- This example implementation has reinserted explicit format codes
  -- and BN, in order that the levels array correspond to the
  -- initial text. Their final placement is not normative.
  -- These codes are treated like WS in this implementation,
  -- so they don't interrupt sequences of WS.

  validateLineBreaks(linebreaks, self:length())

  local result = {} -- will be returned to caller
  for i,v in ipairs(self.resultLevels) do
    result[i] = v
  end

  -- don't worry about linebreaks since if there is a break within
  -- a series of WS values preceding S, the linebreak itself
  -- causes the reset.
  for i = 1, #result do
    local t = self.initialTypes[i]
    if t == B or t == S then
      -- Rule L1, clauses one and two.
      result[i] = self.paragraphEmbeddingLevel

      -- Rule L1, clause three.
      local j = i - 1
      while j >= 1 do
        if isWhitespace(self.initialTypes[j]) then -- including format codes
          result[j] = self.paragraphEmbeddingLevel
        else
          break
        end
        j = j - 1
      end
    end
  end

  -- Rule L1, clause four.
  local start = 1
  for i = 1, #linebreaks do
    local limit = linebreaks[i]
    for j = limit - 1, start, -1 do
      if isWhitespace(self.initialTypes[j]) then -- including format codes
        result[j] = self.paragraphEmbeddingLevel
      else
        break
      end
    end
    start = limit
  end

  return result
end

-- Return reordering array for a given level array. This reorders a single
-- line. The reordering is a visual to logical map. For example, the
-- leftmost char is string.charAt(order[0]). Rule L2.
local function computeReordering(levels)
  local lineLength = #levels

  local result = {}

  -- initialize order
  for i = 1, lineLength do
    result[i] = i
  end

  -- locate highest level found on line.
  -- Note the rules say text, but no reordering across line bounds is
  -- performed, so this is sufficient.
  local highestLevel = 0
  local lowestOddLevel = MAX_DEPTH + 2
  for i = 1, lineLength do
    local level = levels[i]
    if level > highestLevel then highestLevel = level end
    if bit32.band(level, 1) ~= 0 and level < lowestOddLevel then lowestOddLevel = level end
  end

  for level = highestLevel, lowestOddLevel, -1 do
    local i = 1
    while i <= lineLength do
      if levels[i] >= level then
        -- find range of text at or above this level
        local start = i
        local limit = i + 1
        while limit <= lineLength and levels[limit] >= level do
          limit = limit + 1
        end

        -- reverse run
        local j = start
        local k = limit - 1
        while j < k do
          local temp = result[j]
          result[j] = result[k]
          result[k] = temp
          j = j + 1
          k = k - 1
        end

        -- skip to end of level run
        i = limit
      end
      i = i + 1
    end
  end

  return result
end

-- Return multiline reordering array for a given level array. Reordering
-- does not occur across a line break.
local function computeMultilineReordering(levels, linebreaks)
  local result = {}

  local start = 1
  for i = 1, #linebreaks do
    local limit = linebreaks[i]

    local templevels = {}
    for j = 1, limit - start do
      templevels[j] = levels[start + j - 1]
    end

    local temporder = computeReordering(templevels)
    for j = 1, #temporder do
      result[start + j - 1] = temporder[j] + start - 1
    end

    start = limit
  end

  return result
end

--- Return reordering array breaking lines at offsets in linebreaks.
--
-- The reordering array maps from a visual index to a logical index. Lines
-- are concatenated from left to right. So for example, the fifth character
-- from the left on the third line is
--
--
--     para:getReordering(linebreaks)[linebreaks[1] + 4]
--
--
-- (`linebreaks[1]` is the position after the last character of the second
-- line, which is also the index of the first character on the third line,
-- and adding four gets the fifth character from the left).
--
-- @param linebreaks The linebreaks array must include at least one value. The values must be
-- in strictly increasing order (no duplicates) between 1 and the length of
-- the text, inclusive. The last value must be the length of the text.
function Paragraph:getReordering(linebreaks)
  validateLineBreaks(linebreaks, self:length())

  local levels = self:getLevels(linebreaks)
  return computeMultilineReordering(levels, linebreaks)
end

local bidi = {
  Paragraph = Paragraph,
}

--- Helper Functions
--
-- Helper functions to generate arrays for the `bidi.Paragraph` type.
-- @section

--- Generate Bidi\_Class property (directional codes) for each codepoint in the
-- input array.
--
-- Codes will correspond to the values in the
-- [luaucdn](http://deepakjois.github.io/luaucdn) module.
--
-- @param codepoints list of codepoints in the original input string.
function bidi.codepoints_to_types(codepoints)
  local types = {}
  for i,cp in ipairs(codepoints) do
    types[i] = ucdn.get_bidi_class(cp)
  end

  return types
end

--- Generate Bidi\_Paired\_Bracket\_Type property for each codepoint
-- in the input array.
--
-- Codes must correspond to the values in the
-- [luaucdn](http://deepakjois.github.io/luaucdn) module.
--
-- @param codepoints list of codepoints in the original input string.
function bidi.codepoints_to_pair_types(codepoints)
  local pairTypes = {}
  for i,cp in ipairs(codepoints) do
    pairTypes[i] = ucdn.paired_bracket_type(cp)
  end

  return pairTypes
end

--- Generate an array of unique integers identifying which pair of brackets a
-- bracket character belongs to.
--
-- For example in the string `[Test(s)>` the characters `(` and `)` would share one
-- value and `[` and `>` share another (assuming that `]` and `>` are
-- canonically equivalent).  Characters that have Bidi\_Paired\_Bracket\_Type `n` (None)
-- may always get a single value like 0.
--
-- @param codepoints list of codepoints in the original input string.
function bidi.codepoints_to_pair_values(codepoints)
  local pairValues = {}
  for i,cp in ipairs(codepoints) do
    local pair_type = ucdn.paired_bracket_type(cp)
    if pair_type == ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_OPEN then
      local dc = ucdn.compat_decompose(cp)
      pairValues[i] = #dc > 0 and dc[1] or cp
    elseif pair_type == ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_CLOSE then
      local dc = ucdn.compat_decompose(cp)
      local paired_cp =  #dc > 0 and dc[1] or cp
      pairValues[i] = ucdn.paired_bracket(paired_cp)
    else
      pairValues[i] = 0
    end
  end

  return pairValues
end

-- list of types that should be removed from final reordered output.
local removeClasses = {
  [ucdn.UCDN_BIDI_CLASS_LRO]  = true,
  [ucdn.UCDN_BIDI_CLASS_RLO] = true,
  [ucdn.UCDN_BIDI_CLASS_RLE] = true,
  [ucdn.UCDN_BIDI_CLASS_LRE] = true,
  [ucdn.UCDN_BIDI_CLASS_PDF] = true,
  [ucdn.UCDN_BIDI_CLASS_BN]  = true,
}

local function filter_order(types, order)
  local no = {}
  for _,o in ipairs(order) do
    if not removeClasses[types[o]] then table.insert(no, o) end
  end
  return no
end

--- Generate a visual reordering of codepoints after applying the Unicode
-- Bidirectional Algorithm.
--
-- This function can be directly called with the list of codepoints in the
-- original string. It does the heavy lifting of calling all the other helper
-- functions, applying sensible defaults, and removing unneeded characters from
-- the visually re-ordered string.
--
-- @param codepoints list of codepoints in the original input string.
--
-- @param[opt=nil] dir The externally supplied direction – either `'ltr'`, `'rtl'` or `nil` (for auto).
--
-- @param[opt=nil] linebreaks offsets in the codepoints array where line breaks must be applied.
--
-- When not provided, the default value is an array with a single offset beyond
-- the range of the input text.
--
-- The values in the linebreaks must be instrictly increasing order (no
-- duplicates) between 1 and the length of the text, inclusive. The last value
-- must be the length of the text.
function bidi.get_visual_reordering(codepoints, dir, linebreaks)
 if dir  == nil then dir = -1  -- auto
 elseif string.lower(dir) == 'ltr' then dir = 0
 elseif string.lower(dir) == 'rtl' then dir = 1 else error "Invalid value for dir. Must be one of 'ltr', 'rtl' or nil" end

  if linebreaks == nil then linebreaks = { #codepoints + 1} end
  local types = bidi.codepoints_to_types(codepoints)
  local pair_types = bidi.codepoints_to_pair_types(codepoints)
  local pair_values = bidi.codepoints_to_pair_values(codepoints)

  local para = Paragraph.new(types, pair_types, pair_values, dir)
  local reordering = filter_order(types, para:getReordering(linebreaks))

  local reordered = {}
  for i,v in ipairs(reordering) do
    reordered[i] = codepoints[v]
  end

  return reordered
end

--- Constants
--
--  @section

--- _Embedding levels_ are numbers that indicate how deeply the text is nested,
--  and the default direction of text on that level. The minimum embedding level
--  of text is zero, and the maximum explicit depth is 125, a value
--  exported as MAX_DEPTH.
--
-- @field bidi.MAX_DEPTH
bidi.MAX_DEPTH = MAX_DEPTH

--- Bidi_Class property.
-- Enumerated constants representing [Bidi_Class values](http://www.unicode.org/reports/tr44/#Bidi_Class_Values).
-- These values are currently exactly equivalent to their corresponding values in [luaucdn](https://ufytex.github.io/luaucdn)
-- @section

--- Left\_To_Right
-- @field bidi.BIDI_CLASS_L
bidi.BIDI_CLASS_L = L

--- Left\_To_Right\_Embedding
-- @field bidi.BIDI_CLASS_LRE
bidi.BIDI_CLASS_LRE = LRE

--- Left\_To_Right\_Override
-- @field bidi.BIDI_CLASS_LRO
bidi.BIDI_CLASS_LRO = LRO

--- Right\_To_Left
-- @field bidi.BIDI_CLASS_R
bidi.BIDI_CLASS_R = R

--- Arabic_Letter
-- @field bidi.BIDI_CLASS_AL
bidi.BIDI_CLASS_AL = AL

--- Right\_To_Left\_Embedding
-- @field bidi.BIDI_CLASS_RLE
bidi.BIDI_CLASS_RLE = RLE

--- Right\_To_Left\_Override
-- @field bidi.BIDI_CLASS_RLO
bidi.BIDI_CLASS_RLO = RLO

--- Pop\_Directional_Format
-- @field bidi.BIDI_CLASS_PDF
bidi.BIDI_CLASS_PDF = PDF

--- European_Number
-- @field bidi.BIDI_CLASS_EN
bidi.BIDI_CLASS_EN = EN

--- European_Separator
-- @field bidi.BIDI_CLASS_ES
bidi.BIDI_CLASS_ES = ES

--- European_Terminator
-- @field bidi.BIDI_CLASS_ET
bidi.BIDI_CLASS_ET = ET

--- Arabic_Number
-- @field bidi.BIDI_CLASS_AN
bidi.BIDI_CLASS_AN = AN

--- Common_Separator
-- @field bidi.BIDI_CLASS_CS
bidi.BIDI_CLASS_CS = CS

--- Nonspacing_Mark
-- @field bidi.BIDI_CLASS_NSM
bidi.BIDI_CLASS_NSM = NSM

--- Boundary_Neutral
-- @field bidi.BIDI_CLASS_BN
bidi.BIDI_CLASS_BN = BN

--- Paragraph_Separator
-- @field bidi.BIDI_CLASS_B
bidi.BIDI_CLASS_B = B

--- Segment_Separator
-- @field bidi.BIDI_CLASS_S
bidi.BIDI_CLASS_S = S

--- White_Space
-- @field bidi.BIDI_CLASS_WS
bidi.BIDI_CLASS_WS = WS

--- Other_Neutral
-- @field bidi.BIDI_CLASS_ON
bidi.BIDI_CLASS_ON = ON

--- Left\_To_Right\_Isolate
-- @field bidi.BIDI_CLASS_LRI
bidi.BIDI_CLASS_LRI = LRI

--- Right\_To_Left\_Isolate
-- @field bidi.BIDI_CLASS_RLI
bidi.BIDI_CLASS_RLI = RLI

--- First\_Strong_Isolate
-- @field bidi.BIDI_CLASS_FSI
bidi.BIDI_CLASS_FSI = FSI

--- Pop\_Directional_Isolate
-- @field bidi.BIDI_CLASS_PDI
bidi.BIDI_CLASS_PDI = PDI

return bidi


