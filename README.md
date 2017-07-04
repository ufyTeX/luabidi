# luabidi
Lua implementation of the Unicode Bidirectional Algorithm, as specified in [UAX #9](http://unicode.org/reports/tr9/).

## Installing

```
luarocks install luabidi
```

## Documentation

* [API Docs](http://ufytex.github.io/luabidi/)

## Quickstart

```lua
local bidi = require('bidi')
local serpent  = require('serpent') -- luarocks install serpent

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
print("Visual reordering: " .. serpent.line(reordered_text,{comment = false})) -- should be { "U+06C1", "U+06CC"}
```
More sample code in the [examples](https://github.com/ufytex/luabidi/tree/master/example) folder.

## Development

#### Building

```
luarocks make
```

#### Testing and Linting
In order to make changes to the code and run the tests, the following dependencies need to be installed:

* [Busted](http://olivinelabs.com/busted/) – `luarocks install busted`
* [luacheck](http://luacheck.readthedocs.org) – `luarocks install luacheck`

Run the test suite:
```
make spec
```

Lint the codebase:
```
make lint
```

## Contact
[Open a Github issue][luabidi-issues], or email me at <deepak.jois@gmail.com>.

[luabidi-issues]: https://github.com/ufytex/luabidi/issues

