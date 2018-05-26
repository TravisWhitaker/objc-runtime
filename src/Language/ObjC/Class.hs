{-# LANGUAGE TemplateHaskell #-}

module Language.ObjC.Class where

import Control.Exception

import Control.Monad

import Data.Coerce

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.ObjC.ObjCException

-- | A 'ForeignPtr' to some Objective-C object.
newtype Id = Id (ForeignPtr ())

idToFP :: Id -> ForeignPtr a
idToFP = castForeignPtr . coerce

fpToId :: ForeignPtr a -> Id
fpToId = coerce . castForeignPtr

-- | For objects passed to us with a retain count set to 1, e.g. the result of
--   'new'.
newId :: Ptr a -> IO Id
newId = coerce $ newForeignPtr objc_release_addr

-- | For objects passed to us with a retain count set to 0, e.g. any function
--   call that would normally expect to be in an autorelease pool/block.
newRetainedId :: Ptr a -> IO Id
newRetainedId p = objc_retain p >>= newId

-- | An Objective-C class pointer.
newtype Class = Class (Ptr ())

classPtr :: Class -> Ptr ()
classPtr (Class p) = p

-- | Lookup a class pointer by name.
getClass :: String -> IO Class
getClass n = withCString n $ \cn -> do
    cp <- objc_getClass cn
    when (cp == nullPtr) (throwIO ObjCClassDoesNotExist)
    pure (Class cp)

-- | An Objective-C method pointer (selector).
newtype Method = Method (Ptr ())

methodPtr :: Method -> Ptr ()
methodPtr (Method p) = p

-- | Look up a method pointer (selector) by name.
getMethod :: String -> IO Method
getMethod n = withCString n $ \cn -> do
    mp <- sel_registerName cn
    when (mp == nullPtr) (throwIO ObjCMethodDoesNotExist)
    pure (Method mp)

-- | Call a method with no arguments on a class object. This is only really
--   useful for 'NSObject'.
sendClassMsg :: Method -> Class -> IO (Ptr ())
sendClassMsg (Method mp) (Class cp) = objc_msgSend cp mp

-- | Call a method with no arguments on an object. This is really only useful
--   for 'NSObject'.
sendIdMsg :: Method -> Id -> IO (Ptr ())
sendIdMsg (Method mp) tid =
    withForeignPtr (idToFP tid) $ \tp ->
    objc_msgSend tp mp

-- | A dynamic class declaration.
newtype ClassDecl = ClassDecl (Ptr ())

-- | Start a new dynamic class declaration.
newClassDecl :: Class  -- ^ Metaclass.
             -> String -- ^ New class name.
             -> Int    -- ^ Bytes reserved for indexed IVars, typically 0.
             -> IO ClassDecl
newClassDecl (Class cp) cn eb = withCString cn $ \cnp ->
    ClassDecl <$> objc_allocateClassPair cp cnp (fromIntegral eb)

-- | Register a class with the runtime.
registerClassDecl :: ClassDecl -> IO Class
registerClassDecl (ClassDecl p) = do
    objc_registerClassPair p
    pure (Class p)

-- | Dispose of a dynamically declared class.
disposeClass :: Class -> IO ()
disposeClass (Class cp) = objc_disposeClassPair cp

-- | A dynamic method declaration.
data MethodDecl a = MethodDecl {
    implPtr :: (FunPtr a)
  , typeExp :: String
  }

newtype MethImplUniq = MethImplUniq Int

-- | Uncurry a Haskell type into its return type and argument types.
argRet :: Type -> (Type, [Type])
argRet = go id
    where go as (AppT (AppT ArrowT a) r) = go ((a:) . as) r
          go as r                        = (r, as [])

typeEnc :: Type -> (String -> String)
typeEnc (TupleT 0) = ('v':)
typeEnc (ConT t)
    | t == ''CChar   = ('c':)
    | t == ''CInt    = ('i':)
    | t == ''CShort  = ('s':)
    | t == ''CLong   = ('l':)
    | t == ''CLLong  = ('q':)
    | t == ''CUChar  = ('C':)
    | t == ''CUInt   = ('I':)
    | t == ''CUShort = ('S':)
    | t == ''CULong  = ('L':)
    | t == ''CULLong = ('Q':)
    | t == ''CFloat  = ('f':)
    | t == ''CDouble = ('d':)
    | t == ''CBool   = ('B':)
    | t == ''CString = ('*':)
    | t == ''Id      = ('@':)
    | t == ''Class   = ('#':)
    | t == ''Method  = (':':)
typeEnc (AppT (ConT p) t)
    | p == ''Ptr = ('^':) . typeEnc t
    | otherwise  = ('?':)
typeEnc _ = ('?':)

retTypeEnc :: Type -> (String -> String)
retTypeEnc t@(AppT (ConT i) r)
    | i == ''IO = typeEnc r
    | otherwise = typeEnc t
retTypeEnc t = typeEnc t

methodTypeEnc :: Type -> Q (String -> String)
methodTypeEnc t@(AppT (AppT ArrowT a) r) =
    let (r, as) = argRet t
    in pure ((retTypeEnc r) . foldr (.) id (map typeEnc as))
methodTypeEnc _ = fail "methodTypeEnc: Method must be a function!"

mkGetMethodDecl :: Q Type
                -> Q Exp
mkGetMethodDecl mtq = do
    mt <- mtq
    mwcount <- getQ :: Q (Maybe MethImplUniq)
    let wsuf = case mwcount of
                   Nothing               -> 0
                   Just (MethImplUniq wc) -> if wc < 0 then 0 else (wc + 1)
    wmn <- newName ("wrapper" ++ show wsuf)
    putQ (MethImplUniq wsuf)
    let fd = ForeignD (ImportF CCall
                               Safe
                               "wrapper"
                               wmn
                               mt
                      )
    addTopDecls [fd]
    [| \mimpl -> MethodDecl ($(varE wmn) mimpl)
                            $((methodTypeEnc mt <*> pure []) >>= stringE) |]

addMethod :: ClassDecl -> String -> MethodDecl a -> IO Bool
addMethod (ClassDecl cp) mn (MethodDecl mp te) = do
    (Method mnp) <- getMethod mn
    withCString te $ \tep ->
        (/= 0) <$> class_addMethod cp mnp mp tep

foreign import ccall objc_retain :: Ptr a -> IO (Ptr a)

foreign import ccall objc_release :: Ptr a -> IO ()

foreign import ccall "&objc_release" objc_release_addr :: FunPtr (Ptr a -> IO())

foreign import ccall objc_getClass :: CString -> IO (Ptr a)

foreign import ccall sel_registerName :: CString -> IO (Ptr a)

foreign import ccall objc_msgSend :: Ptr a -> Ptr a -> IO (Ptr a)

foreign import ccall objc_allocateClassPair :: Ptr a
                                            -> CString
                                            -> CSize
                                            -> IO (Ptr a)

foreign import ccall objc_registerClassPair :: Ptr a -> IO ()

foreign import ccall objc_disposeClassPair :: Ptr a -> IO ()

foreign import ccall class_addMethod :: Ptr a
                                     -> Ptr b
                                     -> FunPtr c
                                     -> CString
                                     -> IO CUChar
