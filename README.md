pipes-find
==========

A primitive find already works!

To run it,
```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal repl
> :set -XOverloadedStrings
> find  $ unsafeAbsDir "/absolute/directory/with/trailing/slash/"
> findL $ unsafeAbsDir "/absolute/directory/with/trailing/slash/"
```

The difference between `find` and `findL` is that the former does not follow
any symlinks.
