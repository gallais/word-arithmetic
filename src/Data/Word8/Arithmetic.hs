{-# LANGUAGE MagicHash #-}

module Data.Word8.Arithmetic where

import GHC.Exts
import GHC.Prim
import GHC.Word

addWithCarry :: Word8 -> Word8 -> (Word8, Bool)
addWithCarry (W8# x#) (W8# y#) =
  let z# = plusWord# x# y#
  in (W8# (narrow8Word# z#)
     , isTrue# (gtWord# z# 255##))

subWithBorrow :: Word8 -> Word8 -> (Word8, Bool)
subWithBorrow (W8# x#) (W8# y#) =
  (W8# (narrow8Word# (minusWord# x# y#))
  , isTrue# (gtWord# y# x#))

shiftRight :: Word8 -> (Word8, Bool)
shiftRight (W8# x#) =
  let d# = int2Word# 2#
      q# = x# `quotWord#` d#
      r# = x# `remWord#`  d#
  in (W8# q#, isTrue# (eqWord# r# 1##))

shiftLeft :: Word8 -> (Word8, Bool)
shiftLeft (W8# x#) =
  (W8# (narrow8Word# (timesWord# x# (int2Word# 2#)))
  , isTrue# (gtWord# x# 127##))
