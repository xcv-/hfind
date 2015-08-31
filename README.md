hfind
=====

Filtering and pruning already works!

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
/home/[...]/hfind/src/System/Posix/Find/Lang/Error.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Eval.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Parser.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Predicate.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Types/AST.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Types/Value.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Types.hs
/home/[...]/hfind/src/System/Posix/Find/Types.hs
/home/[...]/hfind/src/System/Posix/Find/Walk.hs
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

Find all regular files in `dist/` that don't have a dot in the name, it starts
with a lowercase letter. Plus, the owner group must have the same execution
permission as the rest of the users.

```
$ dist/build/hfind/hfind dist \
    -if '$type == "f"'                         \
    -if 'not ($name =~ m/\./)'                 \
    -if '$name =~ m/^[a-z]/'                   \
    -if '$perms =~ m/... ..(.) ..\1/x'

/home/[...]/hfind/dist/build/hfind/hfind
/home/[...]/hfind/dist/build/spec/spec
/home/[...]/hfind/dist/setup-config
```

List libraries built by ghc >=7.10 in `dist/` with executable permissions
(naive check)
```
$ me=$UID we=$GID dist/build/hfind/hfind dist -if '
       $type == "f"
    && $name =~ m/lib.*-ghc(\d+)\.(\d+)\.\d+\.so/
    && scope ( $perms =~ m/..(.) ..(.) ..(.)/x
             && (  ($1 == "x" && $me == tostr $ownerid)
                || ($2 == "x" && $we == tostr $groupid)
                ||  $3 == "x"
                )
             )
    && ($1 == "7" && readint $2 >= 10 || readint $1 > 7)'

/home/[...]/hfind/dist/build/libHShfind-0.1.0.0-48aLcpNTab3EiWEDW5Bwk0-ghc7.10.2.so
/home/[...]/hfind/dist/build/libHShfind-0.1.0.0-D1R5LGKLZgw0oun3dxLEOZ-ghc7.10.2.so
/home/[...]/hfind/dist/build/libHShfind-0.1.0.0-HrfH6DGM3FjLRrBEMCUfBr-ghc7.10.2.so
```

Note that captures inside `not (...)` or `(... || ...)`, won't be available
from the outside (`...` is not necessarily true), so these can be used as a
form of scoping. This can also be done explicitly with `scope (...)`.

Also, if a `$var` itself does not exist, it is first desugared to `var $_currentnode`
and if this fails as well, `$var` is looked up in the system environment.

For a list of builtin functions, see [Builtins.hs](src/System/Posix/Find/Lang/Builtins.hs)


Error reporting:
----------------

Some nice error reporting has been added. For example,

Analyzer error:
```
$ dist/build/hfind/hfind src -if '$name =~ m|(.*)\.hs$| && isdir "$parentpath/$2"'

Error at "string literal" (line 1, column 1): Variable $2 not found
    In a regex capture variable: $2
    In a string interpolation: "$parentpath/$2"
    In a function application: isdir "$parentpath/$2"
    In a conjunction (&&): $name =~ m|(.*)\.hs$| && isdir "$parentpath/$2"

Defined variables:
    $_currentnode

Active regex: "(.*)\\.hs$"
```

Runtime error:
```
$ dist/build/hfind/hfind src -if '$name =~ m|(.*)\.hs$| && owner "$parentpath/$1" == $USER'

Error at "argument #3" (line 1, column 26): Type error: 'fsnode' expected, but found 'string'
    In a function application: owner "$parentpath/$1"
    In operator (==): owner "$parentpath/$1" == $USER
    In a conjunction (&&): $name =~ m|(.*)\.hs$| && owner "$parentpath/$1" == $USER

Variable dump:
    $_currentnode = "/home/[...]/hfind/src/System/Posix/Find/Combinators.hs"

Active match: "(.*)\\.hs$"
    $0 = "Combinators.hs"
    $1 = "Combinators"
```


TODO:
-----

- **done** more detailed error messages with context, like GHC (in the second argument of...)
- **done** review scope semantics
- actually run commands, and make `print` a built-in special case that's the default
- size/time literals
- evaluate performance (specially EvalT)
- allow running the entire process asynchronously (chunked, customizable)
- add a `--dry-run` flag that runs the command in a pure monad (not the
  predicate, that would be impossible due to harmless I/O in builtins like
  `stat`), assuring the lack of side-effects (return a list of outputs to
  print?)
- add a compatibility adapter command interface to traditional find
- document everything (after the design is settled), plus a small tutorial on
  how to actually use it
