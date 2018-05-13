{-# LANGUAGE TemplateHaskell #-}

module Language.ObjC.MsgSend where

import Control.Monad

import Data.Coerce

import Data.Int

import Data.List.NonEmpty

import Data.Proxy

import Data.Word

import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.StablePtr

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Language.ObjC.NSObject

-- | Whether or not a type is 'Id' or has an 'NSObject' instance.
isNSObject :: Type -> Q Bool
isNSObject t
    | t == ConT ''Id = pure True
    | otherwise      = isInstance ''NSObject [t]

-- | > NSObject a => idToPtr a  = Ptr ()
--   >               idToPtr Id = Ptr ()
--   >               idToPtr a  = a
idToPtr :: Type -> Q Type
idToPtr t = do
    isNSO <- isNSObject t
    if isNSO
    then [t| Ptr () |]
    else pure t

-- | Haskell return type to @objc_msgSend@ return type.
retTyToObjcMsgTy :: Type -> Q Type
retTyToObjcMsgTy t@(AppT (ConT c) r)
    | c == ''IO = AppT (ConT ''IO) <$> idToPtr r
    | otherwise = idToPtr t
retTyToObjcMsgTy t = idToPtr t

-- | Haskell function type to @objc_msgSend@ function type.
funTyToObjcMsgTy :: Type -> Q Type
funTyToObjcMsgTy (AppT (AppT ArrowT x) y) = do
    x' <- idToPtr x
    y' <- funTyToObjcMsgTy y
    pure $ AppT (AppT ArrowT x') y'
funTyToObjcMsgTy y = retTyToObjcMsgTy y

-- | Adds class and method pointers to the desired message type.
funTyToClassMsgTy :: Type -> Q Type
funTyToClassMsgTy t = [t| Id -> Id -> $(pure t) |]

-- | Adds the method pointer after the first argument type. Throws an error if
--   the provided type isn't a function type, as instance messages must at least
--   be a function of the instance.
funTyToInstMsgTy :: Type -> Q Type
funTyToInstMsgTy (AppT (AppT ArrowT inst) r) = do
    isNSO <- isNSObject inst
    when (not isNSO) $
        fail "instance messages must be a function of the instance."
    [t| $(pure inst) -> Id -> $(pure r) |]

-- | List of fresh names, one for each argument in a function type. Useful for
--   generating lambdas.
funTyNames :: Type -> Q [Name]
funTyNames (AppT (AppT ArrowT x) y) = (:) <$> (newName "a") <*> funTyNames y
funTyNames _                        = pure []

-- | Haskell function type to @objc_msgSend@ wrapper. Maps types like this:
--
--   > NSObject b => a -> b -> IO c
--
--   to wrappers like this:
--
--   > \w a b -> withForeignPtr b (\b' -> w a b')
--
--   And types like this:
--
--   > (NSObject a, NSObject c) => a -> b -> IO c
--
--   to wrapper like this:
--
--   > \w a b -> withForeignPtr a (\a' -> w a' b >>= (coerce . newRetainedId))
wrapperExp :: Type -> Q Exp
wrapperExp ty = do
    wn   <- newName "w"
    ftns <- funTyNames ty
    let ps = fmap VarP (wn:ftns)
    rhs <- fpWrap (wn:|[]) ftns ty
    pure $ LamE ps rhs
    where fpWrap :: NonEmpty Name -> [Name] -> Type -> Q Exp
          fpWrap us [] (AppT (AppT ArrowT x) y) =
              fail "wrapperEnv: Ran out of names!"
          fpWrap us (n:ns) (AppT (AppT ArrowT x) y) = do
           isNSO <- isNSObject x
           if isNSO
           then do
               pn  <- newName "p"
               [e| withForeignPtr $(varE n)
                                  (\ $(varP pn) -> $(fpWrap (pn <| us) ns y)) |]
           else do
               fpWrap (n <| us) ns y
          fpWrap us [] y = do
              isNSO <- isNSObject y
              if isNSO
              then [| $(pure ((callWrap us))) >>= (coerce . newRetainedId) |]
              else pure (callWrap us)
          fpWrap _ (_:_) _ = fail "wrapperEnv: Leftover names!"
          callWrap (w :| []) = VarE w
          callWrap (a :| (a':as)) = AppE (callWrap (a' :| as)) (VarE a)

-- | @objc_msgSend@ function type to objc_msgSend import. For now we just always
--   use @objc_msgSend@, going to break if we return a struct by value.
msgSendDec :: Type -> Q Exp
msgSendDec t = do
    msn <- newName "objc_msgSend"
    let fd = ForeignD (ImportF CCall
                               Safe
                               "objc_msgSend"
                               msn
                               t
                      )
    addTopDecls [fd]
    [| $(varE msn) |]

-- | Send a message to a class.
sendClassMsg :: Q Type -- ^ Class type.
             -> String -- ^ ObjC message name.
             -> Q Type -- ^ Desired message type.
             -> Q Exp
sendClassMsg cltq msn mstq = do
    clt <- cltq
    mst <- mstq
    clmt <- funTyToClassMsgTy mst
    ocmt <- funTyToObjcMsgTy clmt
    wexp <- wrapperExp clmt
    ocms <- msgSendDec ocmt
    [| let cls = getClass (symbolVal (Proxy :: Proxy (ClassName $(pure clt))))
           mth = getMethod $(pure (LitE (StringL msn)))
       in $(pure wexp) $(pure ocms) <*> cls <*> mth |]

-- | Send a message to an object.
sendInstMsg :: String -- ^ ObjC message name.
            -> Q Type -- ^ Desired message type.
            -> Q Exp
sendInstMsg msn mstq = do
    mst <- mstq
    inmt <- funTyToInstMsgTy mst
    ocmt <- funTyToObjcMsgTy inmt
    wexp <- wrapperExp inmt
    ocms <- msgSendDec ocmt
    [| let mth = getMethod $(pure (LitE (StringL msn)))
       in flip ($(pure wexp) $(pure ocms)) <*> mth |]
