{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.TL.I64
  ( I64,
  )
where

import Data.Aeson
import Data.Text (pack, unpack)

-- | 64 bit integer that encodes to json strings instead of json reals
newtype I64 = I64 {unI64 :: Int}
  deriving newtype (Show, Read, Eq, Num, Ord, Real, Bounded)

instance FromJSON I64 where
  parseJSON =
    withText "64 bit integer encoded as string" $
      pure . I64 . read . unpack

instance ToJSON I64 where
  toJSON = String . pack . show . unI64
