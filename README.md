hfind
=====

Filtering and pruning by already works!

To run it,
```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal build
dist/build/hfind/hfind  # prints usage
```

Some examples:
--------------

Find all files with `hs` extension in the `src` directory, following symlinks (`-L`).
```
$ dist/build/hfind/hfind -L src -if '$type == "f" && $name =~ m|\.hs$|'

/home/[...]/hfind/src/System/Posix/Find/Combinators.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Builtins.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Context.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Eval.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Parser.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Predicate.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Types.hs
/home/[...]/hfind/src/System/Posix/Find/Ls.hs
/home/[...]/hfind/src/System/Posix/Find/Types.hs
/home/[...]/hfind/src/System/Posix/Find.hs
/home/[...]/hfind/src/System/Posix/Text/Path.hs
```

Find all `.hs` files in src such that there is a directory with the same name
except the `.hs` extension in the same directory
```
$ dist/build/hfind/hfind src -if '$name =~ m|(.*)\.hs$| && isdir "$parentpath/$1"'

/home/[...]/hfind/src/System/Posix/Find.hs
```

For a list of builtins, see [Builtins.hs](src/System/Posix/Find/Lang/Builtins.hs)
