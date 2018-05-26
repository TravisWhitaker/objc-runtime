{-# LANGUAGE DataKinds
           , FlexibleContexts
           , KindSignatures
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           #-}

module Language.ObjC.NSObject (
    NSObject(..)
  , SomeNSObject(..)
  ) where

import Control.Exception

import Control.Monad

import Data.Coerce

import Data.Int

import Data.Proxy

import Data.Word

import GHC.TypeLits

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Language.ObjC.Class
import Language.ObjC.ObjCException

-- | Class for all Objective-C objects. Only the 'ClassName' type family must be
--   defined for an instance of this class. Most Objective-C classes may be
--   represented as newtypes around the 'SomeNSObject' type (which will provide
--   an instance of this class).
class ( Coercible a Id
      , Coercible Id a
      , KnownSymbol (ClassName a)
      ) => NSObject a where
    type ClassName a :: Symbol

    -- | Allocates an object without 'init'.
    alloc :: IO a
    alloc = do
        cls <- getClass (symbolVal (Proxy :: Proxy (ClassName a)))
        mth <- getMethod "alloc"
        res <- sendClassMsg mth cls
        -- Not actually sure what to do here. It's probably bad if we call
        -- objc_release on an uninitialized object, but there's no way for
        -- 'init' to tell if the objc_release finalizer has been installed yet
        -- or not.
        coerce <$> newId res

    -- | Perform type-specific object initialization.
    init :: a -> IO a
    init o = do
        mth <- getMethod "init"
        res <- sendIdMsg mth (coerce o)
        coerce <$> newId res

    -- | Allocate and perform type-specific object initialization. Should be
    --   equivalent to @'alloc' >>= 'init'@, but types are free to override
    --   this.
    new :: IO a
    new = do
        cls <- getClass (symbolVal (Proxy :: Proxy (ClassName a)))
        mth <- getMethod "new"
        res <- sendClassMsg mth cls
        -- Assuming that 'new' always has the same retain count behavior as
        -- 'init', i.e. is set to one.
        coerce <$> newId res

-- | A value of this type is an Objective-C object of the named class. Most
--   Objective-C classes may be represented as a newtype around this type. For
--   example, defining:
--
--   > {-# LANGUAGE DataKinds, GeneralizedNewtypeDeriving #-}
--   > newtype NSString = NSString (SomeNSObject "NSString")
--   >                  deriving (NSObject)
--
--   will provide 'alloc', 'init', and 'new' for the @NSString@ class.
newtype SomeNSObject (n :: Symbol) = SNSO Id

instance KnownSymbol n => NSObject (SomeNSObject n) where
    type ClassName (SomeNSObject n) = n
