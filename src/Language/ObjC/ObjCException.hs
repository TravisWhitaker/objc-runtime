{-# LANGUAGE DeriveAnyClass #-}

module Language.ObjC.ObjCException where

import Control.Exception

-- | Exceptions from interacting with the Objective C runtime can only occur if
--   a Haskell wrapper is inconsistent with the underlying Objective C API.
data ObjCException = ObjCClassDoesNotExist
                   | ObjCMethodDoesNotExist
                   deriving (Show, Exception)
