### Elm Package Architecture

This project is an Elm package. Packages are reusable sets of modules designed to be consumed by Elm applications or other packages.

Care should be taken with the exposed API for an Elm package.  Some modules will be listed as public in the elm.json.  For the modules that are listed as public, all exposed declarations (values or types or whatever), should have succinct, well-written doc comments.  The modules themselves should also have doc comments.  Doc-comments can contain markdown and in some cases it can be useful to include an example or two.

It is common to have a toplevel module namespace.  For exmaple, if we're dealing with Markdown, we'll likely have a Markdown module.

It's also common to put types that are needed internally in an {namespace}/Internal.elm file or an {namespace}/Internal/*.elm directory.  These files can expose whatever they wish, and it is likely beneficial to do so to enable testing.


Key reading:
- Well‑formed Elm code guidance: `file://guides/well-formed-elm-code`


