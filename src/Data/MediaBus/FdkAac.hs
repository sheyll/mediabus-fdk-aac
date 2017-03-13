-- | A MediaBus adapter for the Frauenhofer AAC encoder/decoder.
-- This module reexports the high-level encoding and decoding functions.
module Data.MediaBus.FdkAac
  ( module Data.MediaBus.FdkAac.Encoder
  , module Data.MediaBus.FdkAac.Conduit.Encoder
  ) where

import Data.MediaBus.FdkAac.Conduit.Encoder
import Data.MediaBus.FdkAac.Encoder
