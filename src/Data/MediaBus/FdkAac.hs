-- | A MediaBus adapter for the Frauenhofer AAC encoder/decoder.
-- This module reexports the high-level encoding and decoding functions.
module Data.MediaBus.FdkAac
  ( module Data.MediaBus.FdkAac.Encoder
  , module Data.MediaBus.FdkAac.Conduit.Encoder
  , isFdkAacLogSource
  ) where

import Data.MediaBus.FdkAac.Conduit.Encoder
import Data.MediaBus.FdkAac.Encoder
import Control.Monad.Logger (LogSource)
import Data.MediaBus.FdkAac.InternalLogging ( myLogSource )

-- | A predicate for log messages from this library, based on the 'LogSource'
isFdkAacLogSource :: LogSource -> Bool
isFdkAacLogSource = (==) myLogSource