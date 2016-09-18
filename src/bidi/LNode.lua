---------------------------------------------------------------
-- A doubly linked list Node
--
-- @classmod LNode
-- @author jose@josellausas.com
---------------------------------------------------------------

local LNode  = {}
LNode.__index = LNode

---------------------------------------------------------------
-- Creates a new node containing the data
-- @constructor
--
-- @param data **(any)** The data
-- @return **(Node)** New Node
---------------------------------------------------------------
function LNode.new(data)
  local self = {}
  self.next = nil
  self.prev = nil
  -- Weak table references
  self.ref  = {}
  setmetatable(self.ref, { __mode = 'v' })
  self.ref.data = data

    setmetatable(self, LNode)
    return self
end

function LNode:setData(data)
	self.ref.data = data
end

---------------------------------------------------------------
-- Returns the next node
--
-- @return **(Node)** The next node (can be nil)
---------------------------------------------------------------
function LNode:getNext()
	return self.next
end

---------------------------------------------------------------
-- Returns the previous node. Can be nil
--
-- @return **(Node)** The previous node
---------------------------------------------------------------
function LNode:getPrev()
	return self.prev
end

---------------------------------------------------------------
-- Returns the data
--
-- @return **(Node)** The data contained by this node
---------------------------------------------------------------
function LNode:getData()
	return self.ref.data
end


return LNode
