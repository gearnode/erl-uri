% erl-uri changelog

# Next Version
## Misc
- Use result tuples for parsing and decoding, instead of signaling errors. For
  example, `uri:parse/1` now returns either `{ok, URI}` or `{error, Reason}`
  instead of either return a URI or signaling an error.

# 1.1.0
## Features
- Do not serialize empty queries and fragments.

# 1.0.0
First public version.
