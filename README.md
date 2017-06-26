hfind
=====
```
stack build
./hfind -h  # prints usage
```

Motivation:
-----------

Other than a fun project, this is intended to be a saner version of the
traditional find command. It actually has a simple expression language instead
of hacking the command-line arguments into one. This alone makes it more
flexible because some external processes (test, etc) can be replaced with a
simple built-in like `isdir` or `exists`. Captured groups in regular
expressions can be referenced further in the pipeline, `stat` creates an
object that can be queried and compared and anywhere an string is expected
it is possible to interpolate any variable (including from the environment,
which are treated like regular variables).

At some point it might even be able to build a compatibility layer that allows
`hfind` to masquerade as `find`.


Some examples:
--------------

- Walk over all files under the home directory but do nothing (not even printing).
```
$ ./hfind ~ -nop
```

- Walk over all files under the home directory skipping hidden directories and
  printing with a custom format
```
$ ./hfind ~ -prune '$hidden' -print 'found $name somewhere in $HOME'

found somefile1 somewhere in /home/user
found somefile2 somewhere in /home/user
...
```

Equivalent find command:
```
$ find ~ -name '.*' -prune -o -printf "found %f somewhere in $HOME"
```

- Finding git repositories (following symlinks, `-L`)
```
$ ./hfind -L ~ -prune '$hidden' -if '$type == "d" and isdir "$path/.git"'

/home/[...]/[...]/repo1
/home/[...]/[...]/[...]/repo2
...
```

Equivalent find command:
```
$ find -L ~ -name '.*' -prune -o -type d -exec test -d '{}/.git' \;
```

- Find all files with the `.hs` extension in the `src` directory, following symlinks.
```
$ ./hfind -L src -if '$type == "f" and $name =~ m|\.hs$|'

/home/[...]/hfind/src/System/HFind/Combinators.hs
/home/[...]/hfind/src/System/HFind/Expr/Baker.hs
/home/[...]/hfind/src/System/HFind/Expr/Bakers/FromAST.hs
/home/[...]/hfind/src/System/HFind/Expr/Bakers/Fused.hs
/home/[...]/hfind/src/System/HFind/Expr/Bakers.hs
/home/[...]/hfind/src/System/HFind/Expr/Builtins.hs
/home/[...]/hfind/src/System/HFind/Expr/Error.hs
/home/[...]/hfind/src/System/HFind/Expr/Eval.hs
/home/[...]/hfind/src/System/HFind/Expr/Parser.hs
/home/[...]/hfind/src/System/HFind/Expr/Types/AST.hs
/home/[...]/hfind/src/System/HFind/Expr/Types/Value.hs
/home/[...]/hfind/src/System/HFind/Expr/Types.hs
/home/[...]/hfind/src/System/HFind/Path.hs
/home/[...]/hfind/src/System/HFind/Types.hs
/home/[...]/hfind/src/System/HFind/Walk.hs
```

Equivalent find command:
```
$ ./hfind -L src -type f -name '*.hs'
```

- Find all `.hs` files in src such that there is a directory with the same name
except the `.hs` extension in the same directory and print their first line.
```
$ ./hfind src -if '$name =~ m|(.*)\.hs$|'  \
    -let 'module_name=$1'                  \
    -if 'isdir "$parentpath/$module_name"' \
    -print '-- $path --'                   \
    -exec head -n2 '$path'

-- /home/[...]/hfind/src/System/HFind/Expr/Bakers.hs --
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
-- /home/[...]/hfind/src/System/HFind/Expr/Types.hs --
module System.HFind.Expr.Types
  ( module X
```

Equivalent find command: *(none)*


- Find all regular files in `.stack-work/` that don't have a dot in the name
  and start with a lowercase letter. Plus, the owner group must have the same
  execution permission as the rest of the users.

```
$ ./hfind .stack-work \
    -if '$type == "f"'                         \
    -if 'not ($name =~ m/\./)'                 \
    -if '$name =~ m/^[a-z]/'                   \
    -if '$perms =~ m/... ..(.) ..\1/x'

/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/build/hfind/hfind
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/setup-config
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/stack-build-cache
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/stack-cabal-mod
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/stack-config-cache
/home/[...]/hfind/.stack-work/install/i386-linux/lts-7.1/8.0.1/bin/hfind
```

Equivalent find command: *(?)*

List libraries built by ghc >=8.0.1 in `.stack-work/` with executable permissions
(naive check)
```
$ me=$UID we=$GID ./hfind .stack-work                     \
     -if '$type == "f"'                                   \
     -if '$name =~ m/lib.*-ghc(\d+)\.(\d+)\.(\d+)\.so/'   \
     -let 'major = readint $1'                            \
     -let 'minor = readint $2'                            \
     -let 'patch = readint $3'                            \
     -if '$perms =~ m/..(.) ..(.) ..(.)/x
             and ( ($1 == "x" and $me == "$ownerid")
                or ($2 == "x" and $we == "$groupid")
                or  $3 == "x"
                )'                                        \
     -if '$major*10000 + $minor*100 + $patch >= 80001'

/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.24.0.0/build/libHShfind-0.1.0.0-D8PMEILeDsh4KwcA6t7Cuq-ghc8.0.1.so
/home/[...]/hfind/.stack-work/install/i386-linux/lts-7.1/8.0.1/lib/i386-linux-ghc-8.0.1/hfind-0.1.0.0-D8PMEILeDsh4KwcA6t7Cuq/libHShfind-0.1.0.0-D8PMEILeDsh4KwcA6t7Cuq-ghc8.0.1.so
```

Equivalent find command: *(none)*

Note that captures inside `not (...)` or `(... or ...)`, won't be available
from the outside (`...` is not necessarily true), so these can be used as a
form of scoping. This can also be done explicitly with `scope (...)`.

Also, if a `$var` itself does not exist, it is first desugared to `var $_currentnode`
and if this fails as well, `$var` is looked up in the system environment.

For a list of builtin functions, see [Builtins.hs](src/System/Posix/Find/Expr/Builtins.hs)


Error reporting:
----------------

Some nice error reporting has been added. For example,

Analyzer error:
```
$ ./hfind src -if '$name =~ m|(.*)\.hs$| and isdir "$parentpath/$2"'

Error at "string literal" (line 1, column 1): Variable $2 not found
    In a regex capture variable: $2
    In a string interpolation: "$parentpath/$2"
    In a function application: isdir "$parentpath/$2"
    In a conjunction (and): $name =~ m|(.*)\.hs$| and isdir "$parentpath/$2"

Defined variables:

Active regex: (.*)\.hs$
```

Type error:
```
$ ./hfind src -if '$name =~ m|(.*)\.hs$| and owner "$parentpath/$1" == $USER'

Error at "argument #3" (line 1, column 26): Type error: 'fsnode' expected, but found 'string'
    In a function application: owner "$parentpath/$1"
    In operator (==): owner "$parentpath/$1" == $USER
    In a conjunction (and): $name =~ m|(.*)\.hs$| and owner "$parentpath/$1" == $USER

Defined variables:

Active regex: (.*)\.hs$
```

Runtime error:
```
$ ./hfind src -if '$name =~ m|(.*)\.hs$| and owner (stat "$parentpath/$1") == $USER'

Error at "argument #3" (line 1, column 34): File/Directory not found: '/[...]/hfind/src/System/HFind/Combinators'
    In a function application: stat "$parentpath/$1"
    In a function application: owner (stat "$parentpath/$1")
    In operator (==): owner (stat "$parentpath/$1") == $USER
    In a conjunction (and): $name =~ m|(.*)\.hs$| and owner (stat "$parentpath/$1") == $USER

Variable dump:
    $_current = "/[...]/hfind/src/System/HFind/Combinators.hs"

Active match: (.*)\.hs$
    $0 = "Combinators.hs"
    $1 = "Combinators"
```

TODO:
-----

- **done** more detailed error messages with context, like GHC (in the second argument of...)
- **done** review scope semantics
- **done** actually run commands, and make `print` a built-in special case that's the default
- **done** let-bindings
- **done** static typing
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
