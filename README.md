# objc-runtime

Tools for writing Haskell bindings for Objective-C APIs. This has only been
tested with Clang/LLVM's implementation of Objective C on macOS. In principle
this should work with GNUStep too, but I haven't tested it yet.

## On The State of This Library

I've only tested this with small parts of the Cocoa API. Important
missing features:

- A technique for dealing with Objective-C functions that accept or
  return structs.
- Test suite, ideally including x86 and Aarch64.
- A tutorial.

## Building

Stack does not support the `--extra-framework-dirs` option, so you'll need to
build with cabal.

You'll need to tell cabal where to find your system frameworks:

```bash
$ cabal configure --extra-framework-dirs=/System/Library/Frameworks
```

And you'll need to start GHCi with `-fobject-code`.

```bash
$ cabal repl --ghc-option=-fobject-code
```

## Using This library

`objc-runtime` works by using Template Haskell to generate code that
calls the Objective-C runtime (e.g. `objc_getClass`, `objc_msgSend`,
etc.) to send messages to objects. As an example, let's bind some of
the `NSString` messages. First, we'll make a Haskell type for the
`NSString` class:

```Haskell
{-# LANGUAGE DataKinds
           , GeneralizedNewtypeDeriving
           , UndecidableInstances
           , TemplateHaskell
           #-}

newtype NSString = NSString (SomeNSObject "NSString")
                 deriving (NSObject)

$(requireLinkedClass "NSString")
```

Almost all Object-C classes may be represented as a `newtype` wrapper
around `SomeNSObject`. Since class pointers are looked up by name at
runtime, `SomeNSObject` keeps the class' name as a `Symbol`. This is
the only information needed to implement the `alloc`, `init`, and
`new` methods in the `NSObject` typeclass, so the default method
implementations should work for any Objective-C class.

The `requireLinkedClass` splice provides a handy check to ensure that
your Haskell libraries and executables are linked against the required
frameworks correctly. Because of the way GHC interacts with the macOS
linking and loading process, it can be a bit tricky to ensure that
your programs will be able to find the Objective-C classes and methods
they need at runtime. See the [GHC, macOS, Linking and
Loading](#ghc-macos-linking-and-loading] section below.

`NSString` has a class method called `string`, which returns the empty
`NSString`. Class methods may be wrapped like so:

```Haskell
string :: IO NSString
string = $(mkSendClassMsg [t| NSString |] "string" [t| IO NSString |])
```

`mkSendClassMsg` is an expression splice that takes a `Q Type` of the
Haskell type that wrapps the class, a method selector string, and
finally a `Q Type` of the desired type of the spliced Haskell
expression. Class method argument and return types must either:

- Have an `NSObject` instance, or
- Be a foreign marshallable type, e.g. `Int`, `Float`, `Double`, `Ptr`, etc., or
- If the type of concern is the return value, an `IO` returning a type
  satisfying the above conditions.

Here's `initWithBytes`, an instance method:

```Haskell
initWithBytes :: NSString -> Ptr () -> CSize -> CULong -> IO NSString
initWithBytes = $(mkSendInstMsg "initWithBytes:length:encoding:"
                                [t| NSString
                                 -> Ptr ()
                                 -> CSize
                                 -> CULong
                                 -> IO NSString |]
                 )
```

Instance methods should take the receiving class as their first
argument. `mkSendInstMsg` is an expression splice that takes a method
selector string and a `Q Type` of the desired type of the
method. Instance method argument and return types must satisfy the
same rules as class method types.

In most cases, providing a higher-level Haskell API on top of your
Objective-C API is useful. For example, rather than working with
`CString` directly, a Haskell user probably expects to be able to
convert `NSString` values to and from `ByteString` and `Text`. the
`Cocoa.Foundation.NSString` module in the `cocoa` package provides an
example of such a higher-level API. Here's a simpler example that
converts ASCII `ByteString` values to `NSString` values:

```Haskell
asciiByteStringToNSString :: ByteString -- ^ Strict ByteString of ASCII Text.
                          -> IO NSString
asciiByteStringToNSString bs =
    -- toForeignPtr is a low-level function from Data.ByteString.Internal. It
    -- allows us to get access to memory region backing the ByteString. Since
    -- ByteStrings can be sliced without copying, we need to use the address
    -- offset and slice length as well..
    let (bsfp, off, len) = toForeignPtr bs
    in do
        -- Allocate a new ByteString with alloc from the NSObject class:
        newNSString <- alloc
        -- Call our Objective-C function:
        withForeignPtr (bsfp `plusForeignPtr` off) $ \p ->
            initWithBytes newNSString p (fromIntegral len) 1
```

The magic number 1 in this example indicates to `initWithBytes` that
the `ByteString` is ASCII encoded; in practice you should probably use
`Text` instead.

TODO: dynamically allocated class/method example, NSApplication delegate?

## GHC, macOS, Linking and Loading

TL;DR - Use `requireLinkedClass` on each of your `NSObject` instances,
keep Objective-C-calling Haskell code in Haskell libraries,
dynamically link your executables against those libraries (e.g. with
`--ghc-option=-dynamic`), and everything should "just work."

Since macOS Sierra, there is an arbitrary upper limit on the load
command sizes parsed by the dynamic loader (see [GHC Bug
#14444](https://ghc.haskell.org/trac/ghc/ticket/14444). To work around
this, GHC always passes the `-dead_strip_dylibs` flag to the linker on
macOS. When linking against Haskell and C functions, this is
fine. However, because Objective-C classes and methods are dynamically
looked-up at runtime, no explicit references to the classes and
methods end up in executables and dylibs. Because of this,
`-dead_strip_dylibs` tends to cause the linker to remove all
references to frameworks.

There are several potential workarounds for this. One would be to use
some scheme to dynamically load frameworks on our own. Another would
be to use Objective-C symbol mangling and attempt to resolve the
symbols that way, although given the dynamic-to-a-fault nature of
Objective-C I'm not sure this would work in all cases.

This library provides a relatively low-impact solution in the form of
a Template Haskell splice, `requireLinkedClass`. This splice generates
references to `_OBJC_CLASS_$_<classname>` data section symbols present
in each object file that provides an Objective C class. Unfortunately,
statically linking Haskell executables thwarts this mechanism, since
the linker will (correctly) conclude that these symbols aren't really
necessary and remove them from the resulting excutable. This is solved
by simply linking executables dynamically with GHC's `-dynamic` flag.

If you're aware of a better way of doing this, please open an issue
(or better yet, a PR!) in this repo's issue tracker.
