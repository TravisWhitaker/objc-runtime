# objc-runtime

Tools for writing Haskell bindings for Objective-C APIs.

## Building

Stack seems to not support the `--extra-framework-dirs` option, so you'll likely
need to build with cabal.

You'll need to tell cabal where to find your frameworks with something like
this:

```bash
$ cabal configure --extra-framework-dirs=/System/Library/Frameworks
```

You'll need to start GHCi with something like this:

```bash
$ cabal repl --ghc-option=-fobject-code
```
