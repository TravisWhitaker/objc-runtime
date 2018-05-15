{-# LANGUAGE DataKinds
           , FlexibleContexts
           , KindSignatures
           , ScopedTypeVariables
           , TemplateHaskell
           , TypeFamilies
           #-}

module Language.ObjC.NSObject where

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

    init :: a -> IO a
    init o = do
        mth <- getMethod "init"
        res <- sendIdMsg mth (coerce o)
        coerce <$> newId res

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

newtype Id = Id (ForeignPtr ())

idToFP :: Id -> ForeignPtr a
idToFP = castForeignPtr . coerce

fpToId :: ForeignPtr a -> Id
fpToId = coerce . castForeignPtr

type Class = Ptr ()

type Method = Ptr ()

getClass :: String -> IO Class
getClass n = withCString n $ \cn -> do
    cp <- objc_getClass cn
    when (cp == nullPtr) (throwIO ObjCClassDoesNotExist)
    pure cp

getMethod :: String -> IO Method
getMethod n = withCString n $ \cn -> do
    mp <- sel_getUid cn
    when (mp == nullPtr) (throwIO ObjCMethodDoesNotExist)
    pure mp

sendClassMsg :: Method -> Class -> IO (Ptr ())
sendClassMsg mp cp = objc_msgSend cp mp

sendIdMsg :: Method -> Id -> IO (Ptr ())
sendIdMsg mp tid =
    withForeignPtr (idToFP tid) $ \tp ->
    objc_msgSend tp mp

-- | For objects passed to us with a retain count set to 1, e.g. the result of
--   'new'.
newId :: Ptr a -> IO Id
newId = coerce $ newForeignPtr objc_release_addr

-- | For objects passed to us with a retain count set to 0, e.g. any function
--   call that would normally expect to be in an autorelease pool/block.
newRetainedId :: Ptr a -> IO Id
newRetainedId p = objc_retain p >>= newId

foreign import ccall objc_retain :: Ptr a -> IO (Ptr a)

foreign import ccall objc_release :: Ptr a -> IO ()

foreign import ccall "&objc_release" objc_release_addr :: FunPtr (Ptr a -> IO())

foreign import ccall objc_getClass :: CString -> IO (Ptr a)

foreign import ccall sel_getUid :: CString -> IO (Ptr a)

foreign import ccall objc_msgSend :: Ptr a -> Ptr a -> IO (Ptr a)
