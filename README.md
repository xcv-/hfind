hfind
=====

Filtering and pruning by already works!

To run it,
```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal build
dist/build/hfind/hfind -h  # prints usage
```

Some examples:
--------------

Find all files with the `.hs` extension in the `src` directory, following symlinks (`-L`).
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

/home/[...]/hfind/src/System/Posix/Lang/Types.hs
/home/[...]/hfind/src/System/Posix/Find.hs
```

For a list of builtins, see [Builtins.hs](src/System/Posix/Find/Lang/Builtins.hs)


TODO:
-----

- more detailed error messages with context, like GHC (in the second argument of...)
- actually run commands, and make `print` a built-in special case that's the default
- review scope semantics
- evaluate performance (specially EvalT)
- allow running the entire process asynchronously (chunked, customizable)
- size/time literals
- add a `--dry-run` flag that runs the command in a pure monad (not the
  predicate, that would be impossible due to harmless I/O in builtins like
  `stat`), assuring the lack of side-effects (return a list of outputs to
  print?)
