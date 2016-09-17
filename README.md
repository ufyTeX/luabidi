# luabidi
Lua implementation of the Unicode Bidirectional Algorithm, as specified in [UAX #9](http://unicode.org/reports/tr9/).

## Installing

```
luarocks install luabidi
```

## Documentation

* [API Docs](http://deepakjois.github.io/luabidi/api/)

_Add usage examples here_

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

[luabidi-issues]: https://github.com/deepakjois/luabidi/issues

