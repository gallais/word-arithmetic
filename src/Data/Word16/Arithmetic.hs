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

shiftRight :: Word16 -> (Word16, Bool)
shiftRight (W16# x#) =
  let d# = int2Word# 2#
      q# = x# `quotWord#` d#
      r# = x# `remWord#`  d#
  in (W16# q#, isTrue# (eqWord# r# 1##))

shiftLeft :: Word16 -> (Word16, Bool)
shiftLeft (W16# x#) =
  (W16# (narrow16Word# (timesWord# x# (int2Word# 2#)))
  , isTrue# (gtWord# x# 32767##))
