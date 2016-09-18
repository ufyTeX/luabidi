package = "luabidi"
version = "0.0.1-0"
source = {
  url = "https://github.com/deepakjois/luabidi",
  tag = "v0.0.1"
}
description = {
  summary = "Lua implementation of the Unicode Bidirectional Algorithm",
  homepage = "https://github.com/deepakjois/luabidi",
  license = "MIT",
  maintainer = "Deepak Jois <deepak.jois@gmail.com>"
}
dependencies = {
  "lua ~> 5.2",
  "luaucdn >= 0.0.2"
}
build = {
  type = "builtin",
  modules = {
    bidi = "src/bidi.lua"
  }
}