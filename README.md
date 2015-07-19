pipes-find
==========

First working executable already available!

To run it,
```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal build
dist/build/hfind/hfind .
dist/build/hfind/hfind -L .
```

/Note/: -L has the same meaning as in the regular `find` command (follow symlinks)
