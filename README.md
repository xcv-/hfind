hfind
==========

Filtering and pruning by name already works!

To run it,
```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal build
dist/build/hfind  # prints usage
dist/build/hfind src -if '$name =~ m/\.hs/'
```

_Note_: -L has the same meaning as in the regular `find` command (follow symlinks)
