{-# LANGUAGE MagicHash #-}

module Data.Word16.Arithmetic where

import GHC.Exts
import GHC.Prim
import GHC.Word

addWithCarry :: Word16 -> Word16 -> (Word16, Bool)
addWithCarry (W16# x#) (W16# y#) =
  let z# = plusWord# x# y#
  in (W16# (narrow16Word# z#)
     , isTrue# (gtWord# z# 65535##))

subWithBorrow :: Word16 -> Word16 -> (Word16, Bool)
subWithBorrow (W16# x#) (W16# y#) =
  (W16# (narrow16Word# (minusWord# x# y#))
  , isTrue# (gtWord# y# x#))

