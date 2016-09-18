local bidi = require('bidi')
local ucdn = require('ucdn')

describe("bidi module", function()

  local L = ucdn.UCDN_BIDI_CLASS_L
  local B = ucdn.UCDN_BIDI_CLASS_B

  local O = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_OPEN
  local C = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_CLOSE
  local N = ucdn.UCDN_BIDI_PAIRED_BRACKET_TYPE_NONE

  describe("bidi.Paragraph class", function()

    describe("initialization",function()
      local validTypes = {L, L, L}
      local validPairTypes = {N, O, C}
      local validPairValues = { 0, 1, 1 }

      it("does not raise an error with valid arguments", function()
        bidi.Paragraph.new(validTypes, validPairTypes, validPairValues)
        assert.has_no.errors(function()  end)
      end)

      it("raises an error if types argument is not a table", function()
        assert.has_error(function() bidi.Paragraph.new(nil, validPairTypes, validPairValues)  end)
      end)

      it("raises an error if types argument is empty", function()
        assert.has_error(function() bidi.Paragraph.new({}, validPairTypes, validPairValues)  end)
      end)

      it("raises an error if types contains invalid values", function()
        assert.has_error(function() bidi.Paragraph.new({ 23 }, validPairTypes, validPairValues)  end)
      end)

      it("raises an error if types argument contains a paragraph separator before the last element", function()
        assert.has_error(function() bidi.Paragraph.new({ L, B, L }, validPairTypes, validPairValues)  end)
      end)

      it("raises an error if pairTypes argument is not a table", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes,nil, validPairValues)  end)
      end)

      it("raises an error if pairTypes argument is empty", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes, {}, validPairValues)  end)
      end)

      it("raises an error if pairTypes contains invalid values", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes, { N, 3, O, C }, validPairValues)  end)
      end)

      it("raises an error if pairValues argument is not a table", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes, validPairTypes, nil)  end)
      end)

      it("raises an error if pairValues argument is empty", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes, validPairTypes, {})  end)
      end)

      it("raises an error if pairValues does not match pairTypes in length", function()
        assert.has_error(function() bidi.Paragraph.new(validTypes, validPairTypes, {1,1})  end)
      end)

      it("raises an error if the linebreaks array is invalid", function()
        assert.has_error(function()
          local para = bidi.Paragraph.new(validTypes, validPairTypes, validPairValues)
          para:getReordering({1, 0})
        end)
      end)
    end)
  end)

end)
