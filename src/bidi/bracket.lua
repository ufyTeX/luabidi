local ucdn = require("ucdn")
local LinkedList = require("bidi.LinkedList")

-- This file contains a port of the reference implementation of the
-- Bidi Parentheses Algorithm:
-- http://www.unicode.org/Public/PROGRAMS/BidiReferenceJava/BidiPBAReference.java
--
-- The implementation in this file covers definitions BD14-BD16 and rule N0
-- of UAX#9.
--
-- Some preprocessing is done for each character before data is passed to this
-- algorithm:
--  - opening and closing brackets are identified
--  - a bracket pair type, like '(' and ')' is assigned a unique identifier that
--    is identical for the opening and closing bracket. It is left to do these
--    mappings.
--  - The BPA algorithm requires that bracket characters that are canonical
--    equivalents of each other be able to be substituted for each other.
--    It is the responsibility of the caller to do this canonicalization.
--
-- In implementing BD16, this implementation departs slightly from the "logical"
-- algorithm defined in UAX#9. In particular, the stack referenced there
-- supports operations that go beyond a "basic" stack. An equivalent
-- implementation based on a linked list is used here.

local L  = ucdn.UCDN_BIDI_CLASS_L
local R  = ucdn.UCDN_BIDI_CLASS_R
local ON = ucdn.UCDN_BIDI_CLASS_ON
local AL = ucdn.UCDN_BIDI_CLASS_AL
local EN = ucdn.UCDN_BIDI_CLASS_EN
local AN = ucdn.UCDN_BIDI_CLASS_AN
local NSM = ucdn.UCDN_BIDI_CLASS_NSM

local N = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_NONE
local O = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_OPEN
local C = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_CLOSE


local BracketPairer = {}
BracketPairer.__index = BracketPairer

function BracketPairer.new()
  local bp = {}
  setmetatable(bp, BracketPairer)

  return bp
end


-- reports whether characters at given positions form a matching bracket pair.
function BracketPairer:matchOpener(pairValues, opener, closer)
  return pairValues[self.indexes[opener]] == pairValues[self.indexes[closer]]
end

local MAX_PAIRING_DEPTH = 63

-- locates matching bracket pairs according to BD16.
--
-- This implementation uses a linked list instead of a stack, because, while
-- elements are added at the front (like a push) they are not generally removed
-- in atomic 'pop' operations, reducing the benefit of the stack archetype.
function BracketPairer:locateBrackets(pairTypes, pairValues)

  -- traverse the run
  for ich, index in ipairs(self.indexes) do
    -- look at the bracket type for each character
    if pairTypes[index] == N then -- default - non paired
      -- continue scanning
    elseif self.codesIsolatedRun[ich] ~= ON then
      -- continue scanning
    elseif pairTypes[index] == O then -- opening bracket found, note location
      -- check for stack overflow
      if self.openers:getCount() == MAX_PAIRING_DEPTH then
        self.pairPositions = {}
        return
      end
      self.openers:pushFront(ich)
    elseif pairTypes[index] == C then -- closing bracket found
      -- see if there is a match
      if self.openers:getCount() == 0 then goto continue2 end
      local iter = self.openers:getHead()
      local count = 0
      while iter ~= nil do
        count = count + 1
        local opener = iter:getData()
        if self:matchOpener(pairValues, opener, ich) then
          table.insert(self.pairPositions, { opener = opener, closer = ich })
          for _ = count, 1, -1 do
            self.openers:popFront()
          end
          break
        end
        iter = iter:getNext()
      end
      table.sort(self.pairPositions, function(a, b) return a.opener < b.opener end)
      -- if we get here, the closing bracket matched no openers
      -- and gets ignored
    end
    ::continue2::
  end
end

-- Bracket pairs within an isolating run sequence are processed as units so
-- that both the opening and the closing paired bracket in a pair resolve to
-- the same direction.
--
-- N0. Process bracket pairs in an isolating run sequence sequentially in
-- the logical order of the text positions of the opening paired brackets
-- using the logic given below. Within this scope, bidirectional types EN
-- and AN are treated as R.
--
-- Identify the bracket pairs in the current isolating run sequence
-- according to BD16. For each bracket-pair element in the list of pairs of
-- text positions:
--
-- a Inspect the bidirectional types of the characters enclosed within the
-- bracket pair.
--
-- b If any strong type (either L or R) matching the embedding direction is
-- found, set the type for both brackets in the pair to match the embedding
-- direction.
--
-- o [ e ] o -> o e e e o
--
-- o [ o e ] -> o e o e e
--
-- o [ NI e ] -> o e NI e e
--
-- c Otherwise, if a strong type (opposite the embedding direction) is
-- found, test for adjacent strong types as follows: 1 First, check
-- backwards before the opening paired bracket until the first strong type
-- (L, R, or sos) is found. If that first preceding strong type is opposite
-- the embedding direction, then set the type for both brackets in the pair
-- to that type. 2 Otherwise, set the type for both brackets in the pair to
-- the embedding direction.
--
-- o [ o ] e -> o o o o e
--
-- o [ o NI ] o -> o o o NI o o
--
-- e [ o ] o -> e e o e o
--
-- e [ o ] e -> e e o e e
--
-- e ( o [ o ] NI ) e -> e e o o o o NI e e
--
-- d Otherwise, do not set the type for the current bracket pair. Note that
-- if the enclosed text contains no strong types the paired brackets will
-- both resolve to the same level when resolved individually using rules N1
-- and N2.
--
-- e ( NI ) o -> e ( NI ) o

-- map character's directional code to strong type as required by rule N0
function BracketPairer:getStrongTypeN0(ich)
  local code = self.codesIsolatedRun[ich]

  -- in the scope of N0, number types are treated as R
  if code == EN or code == AN or code == AL or code == R then
    return R
  elseif code == L then
    return L
  else
    return ON
  end
end

-- determine which strong types are contained inside a Bracket Pair
function BracketPairer:classifyPairContent(pairedLocation, dirEmbed)
  local dirOpposite = ON
  for ich = pairedLocation.opener + 1, pairedLocation.closer - 1 do
    local dir = self:getStrongTypeN0(ich)
    if dir == ON then goto continue1 end
    if dir == dirEmbed then
      return dir
    end -- type matching embedding direction found
    dirOpposite = dir
    ::continue1::
  end


  -- return ON if no strong type found, or class opposite to dirEmbed
  return dirOpposite
end

-- determine which strong types are present before a Bracket Pair
function BracketPairer:classBeforePair(pairedLocation)
  for ich = pairedLocation.opener - 1, 1, -1 do
    local dir = self:getStrongTypeN0(ich)
    if dir ~= ON then return dir end
  end

  return self.sos
end

-- Implement rule N0 for a single bracket pair
function BracketPairer:assignBracketType(pairedLocation,dirEmbed)
  -- rule "N0, a", inspect contents of pair
  local dirPair = self:classifyPairContent(pairedLocation, dirEmbed)

  -- dirPair is now L, R, or N (no strong type found)

  -- the following logical tests are performed out of order compared to
  -- the statement of the rules but yield the same results
  if dirPair == ON then return end

  if dirPair ~= dirEmbed then
    -- case "c": strong type found, opposite - check before (c.1)
    dirPair = self:classBeforePair(pairedLocation)
    if dirPair == dirEmbed or dirPair == ON then
      -- no strong opposite type found before - use embedding (c.2)
      dirPair = dirEmbed
    end
  end

  -- else: case "b", strong type found matching embedding,
  -- no explicit action needed, as dirPair is already set to embedding
  -- direction

  -- set the bracket types to the type found
  self.codesIsolatedRun[pairedLocation.opener] = dirPair
  self.codesIsolatedRun[pairedLocation.closer] = dirPair

  for i = pairedLocation.opener + 1, pairedLocation.closer - 1 do
    local index = self.indexes[i]
    if self.paragraph.initialTypes[index]  == NSM  then
      self.codesIsolatedRun[i] = dirPair
    else
      break
    end
  end

  for i = pairedLocation.closer + 1, #self.indexes do
    local index = self.indexes[i]
    if self.paragraph.initialTypes[index]  == NSM  then
      self.codesIsolatedRun[i] = dirPair
    else
      break
    end
  end
end

-- this implements rule N0 for a list of pairs
function BracketPairer:resolveBrackets(dirEmbed)
  for _, pair in ipairs(self.pairPositions) do
    self:assignBracketType(pair, dirEmbed)
  end
end

-- runs the paired bracket part of the UBA algorithm
local function resolvePairedBrackets(sequence)
  local dirEmbed = bit32.band(sequence.level, 1) == 1 and R or L

  local bp = BracketPairer.new()
  bp.sos = sequence.sos
  bp.openers = LinkedList.new()
  bp.codesIsolatedRun = sequence.types
  bp.indexes = sequence.indexes
  bp.paragraph = sequence.paragraph
  bp.pairPositions = {}

  bp:locateBrackets(sequence.paragraph.pairTypes, sequence.paragraph.pairValues)
  bp:resolveBrackets(dirEmbed)
end

return {
  resolvePairedBrackets = resolvePairedBrackets
}
