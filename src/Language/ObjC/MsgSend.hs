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

-- | Whether or not a type is 'Id', has an 'NSObject' instance, or is 'IO'
--   returning one of those.
isReturnedNSObject :: Type -> Q Bool
isReturnedNSObject (AppT (ConT c) r)
    | c == ''IO = isNSObject r
    | otherwise = pure False
isReturnedNSObject t = isNSObject t

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
              isNSO <- isReturnedNSObject y
              if isNSO
              then [| $(pure ((callWrap us))) >>= newRetainedId |]
              else pure (callWrap us)
          fpWrap _ (_:_) _ = fail "wrapperEnv: Leftover names!"
          callWrap (w :| []) = VarE w
          callWrap (a :| (a':as)) = AppE (callWrap (a' :| as)) (VarE a)

classWrapperExp :: Type -> Q Exp
classWrapperExp ty = do
    wn   <- newName "objc_msgSend_wrapper"
    gcn  <- newName "objc_getClass_wrapper"
    gmn  <- newName "objc_getUid_wrapper"
    cn   <- newName "cls"
    mn   <- newName "mth"
    ftns <- funTyNames ty
    let ps = fmap VarP (wn:gcn:gmn:ftns)
    rhs <- fpWrap (gcn, gmn) (wn,cn,mn) [] ftns ty
    pure $ LamE ps rhs
    where fpWrap :: (Name, Name) -- class,method getters
                 -> (Name, Name, Name) -- wrapper, class, method
                 -> [Name] -- used up names
                 -> [Name] -- fresh names
                 -> Type   -- fun type
                 -> Q Exp
          fpWrap gs fs us [] (AppT (AppT ArrowT x) y) =
              fail "classWrapperEnv: Ran out of names!"
          fpWrap gs fs us (n:ns) (AppT (AppT ArrowT x) y) = do
           isNSO <- isNSObject x
           if isNSO
           then do
               pn  <- newName "ptr"
               [e| withForeignPtr $(varE n)
                                  (\ $(varP pn) -> $(fpWrap gs
                                                            fs
                                                            (pn:us)
                                                            ns
                                                            y)) |]
           else do
               fpWrap gs fs (n:us) ns y
          fpWrap (gcn,gmn) (wn,cn,mn) us [] y = do
              isNSO <- isReturnedNSObject y
              if isNSO
              then [| do $(varP cn) <- $(varE gcn)
                         $(varP mn) <- $(varE gmn)
                         $(pure (callWrap (wn,cn,mn) us))
                             >>= (coerce . newRetainedId) |]
              else [| do $(varP cn) <- $(varE gcn)
                         $(varP mn) <- $(varE gmn)
                         $(pure (callWrap (wn,cn,mn) us)) |]
          fpWrap _ _ (_:_) _ _ = fail "wrapperEnv: Leftover names!"
          callWrap (wn,cn,mn) [] = AppE (AppE (VarE wn) (VarE cn))
                                        (VarE mn)
          callWrap fn (a:as)     = AppE (callWrap fn as) (VarE a)

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
mkSendClassMsg :: Q Type -- ^ Class type.
               -> String -- ^ ObjC message name.
               -> Q Type -- ^ Desired message type.
               -> Q Exp
mkSendClassMsg cltq msn mstq = do
    clt <- cltq
    mst <- mstq
    clmt <- funTyToClassMsgTy mst
    ocmt <- funTyToObjcMsgTy clmt
    wexp <- classWrapperExp mst
    ocms <- msgSendDec ocmt
    [| let cls = coerce <$>
                   getClass (symbolVal (Proxy :: Proxy (ClassName $(pure clt))))
           mth = coerce <$> (getMethod $(pure (LitE (StringL msn))))
       in $(pure wexp) $(pure ocms) cls mth |]

-- | Send a message to an object.
mkSendInstMsg :: String -- ^ ObjC message name.
              -> Q Type -- ^ Desired message type.
              -> Q Exp
mkSendInstMsg msn mstq = do
    mst <- mstq
    inmt <- funTyToInstMsgTy mst
    ocmt <- funTyToObjcMsgTy inmt
    wexp <- wrapperExp inmt
    ocms <- msgSendDec ocmt
    [| let mth = coerce <$> (getMethod $(pure (LitE (StringL msn))))
       in mth >>= flip ($(pure wexp) $(pure ocms)) |]
