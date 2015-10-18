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
$ ./hfind -L ~ -prune '$hidden' -if '$type == "d" && isdir "$path/.git"'

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
$ ./hfind -L src -if '$type == "f" && $name =~ m|\.hs$|'

/home/[...]/hfind/src/System/Posix/Find/Combinators.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Baker.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Builtins.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Error.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Eval.hs
/home/[...]/hfind/src/System/Posix/Find/Lang/Interp.hs
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

Equivalent find command:
```
$ ./hfind -L src -type f -name '*.hs'
```

- Find all `.hs` files in src such that there is a directory with the same name
except the `.hs` extension in the same directory and print their first line.
```
$ ./hfind src -if '$name =~ m|(.*)\.hs$| && isdir "$parentpath/$1"' \
    -print '-- $path --'                                            \
    -exec head -n2 '$path'

-- /home/[...]/hfind/src/System/Posix/Find/Lang/Types.hs --
module System.Posix.Find.Lang.Types
-- /home/[...]/hfind/src/System/Posix/Find.hs --
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
```

Equivalent find command: *(none)*


- Find all regular files in `.stack-work/` that don't have a dot in the name,
  it starts with a lowercase letter. Plus, the owner group must have the same
  execution permission as the rest of the users.

```
$ ./hfind .stack-work \
    -if '$type == "f"'                         \
    -if 'not ($name =~ m/\./)'                 \
    -if '$name =~ m/^[a-z]/'                   \
    -if '$perms =~ m/... ..(.) ..\1/x'

/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/build/hfind/hfind
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/build/spec/spec
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/setup-config
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/stack-build-cache
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/stack-cabal-mod
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/stack-config-cache
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/stack-test-built
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/stack-test-success
/home/[...]/hfind/.stack-work/install/i386-linux/lts-3.4/7.10.2/bin/hfind
```

Equivalent find command: *(?)*

List libraries built by ghc >=7.10 in `.stack-work/` with executable permissions
(naive check)
```
$ me=$UID we=$GID ./hfind .stack-work -if '
       $type == "f"
    && $name =~ m/lib.*-ghc(\d+)\.(\d+)\.\d+\.so/
    && scope ( $perms =~ m/..(.) ..(.) ..(.)/x
             && (  ($1 == "x" && $me == tostr $ownerid)
                || ($2 == "x" && $we == tostr $groupid)
                ||  $3 == "x"
                )
             )
    && ($1 == "7" && readint $2 >= 10 || readint $1 > 7)'

/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/build/libHShfind-0.1.0.0-1bOTNpdR5JrL3TUNaCEMap-ghc7.10.2.so
/home/[...]/hfind/.stack-work/dist/i386-linux/Cabal-1.22.4.0/build/libHShfind-0.1.0.0-BI81816hJ183CedkmYUeim-ghc7.10.2.so
/home/[...]/hfind/.stack-work/install/i386-linux/lts-3.4/7.10.2/lib/i386-linux-ghc-7.10.2/hfind-0.1.0.0-BI81816hJ183CedkmYUeim/libHShfind-0.1.0.0-BI81816hJ183CedkmYUeim-ghc7.10.2.so
```

Equivalent find command: *(none)*

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
$ ./hfind src -if '$name =~ m|(.*)\.hs$| && isdir "$parentpath/$2"'

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
$ ./hfind src -if '$name =~ m|(.*)\.hs$| && owner "$parentpath/$1" == $USER'

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
- **done** actually run commands, and make `print` a built-in special case that's the default
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
