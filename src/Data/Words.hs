{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Words where

import GHC.Word

class Included a b where
  embed :: a -> b

instance Included Word8 Word16 where
  embed (W8# w) = W16# w
