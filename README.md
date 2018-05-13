# objc-runtime

Tools for writing Haskell bindings for Objective-C APIs.

## Building

You'll need to tell cabal/stack where to find your frameworks with something
like this:

```bash
$ cabal configure --extra-framework-dirs=/System/Library/Frameworks
```
