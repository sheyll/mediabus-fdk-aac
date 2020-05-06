{-# LANGUAGE RankNTypes #-}

-- | This module defines a conduit to encode linear audio into AAC.
module Data.MediaBus.FdkAac.Conduit.Encoder
  ( encodeLinearToAacC
  ) where

import Conduit
import Control.Monad.Logger
import Data.MediaBus
import Data.MediaBus.FdkAac.Encoder
import Data.Tagged
import Data.Typeable
import GHC.TypeLits

--  * Conduit Based 'Stream' Encoding
-- | A conduit that receives signed 16 bit, mono or stereo, 'Raw' 'Audio' and
-- yields AAC encoded frames.
-- When a 'Start' event is received, the delay lines will be flushed.
-- The timestamps and sequence numbers of the input will be ignored.
-- The output sequence numbers start from zero and increase
-- monotonic. 'Start' events don't lead to resetting neither sequence numbers
-- nor timestamps.
-- The timestamps start at the encoder delay and are increased by 'frameSize'
-- and the type signature requires that the timestamp unit for 'Frame's has the
-- same sampling rate as the encoded audio.
encodeLinearToAacC
  :: forall channels r aot i m.
     ( MonadLoggerIO m
     , MonadThrow m
     , MonadResource m
     , KnownChannelLayout channels
     , KnownRate r
     , Typeable r
     , Show (AacAotProxy aot)
     , KnownNat (GetAacAot aot)
     , Show i
     , CanBeSample (Pcm channels S16)
     , Show (Pcm channels S16)
     )
  => AacEncoderConfig r channels aot
  -> Conduit (SyncStream i () (Audio r channels (Raw S16))) m (SyncStream i (AacEncoderInfo r channels aot) (Audio r channels (Aac aot)))
encodeLinearToAacC aacCfg = do
  (Tagged enc, info) <- lift $ aacEncoderAllocate aacCfg
  runReaderC enc (encodeThenFlush info)
  where
    encodeThenFlush info = do
      awaitForever go
      aacs <- lift flushAacEncoder
      Prelude.mapM_ yieldNextFrame aacs
      where
        go (MkStream (Next f)) = do
          aacs <- lift (encodeLinearToAac f)
          Prelude.mapM_ yieldNextFrame aacs
        go (MkStream (Start (MkFrameCtx fi _ _ _))) = do
          lift flushAacEncoder >>= Prelude.mapM_ yieldNextFrame
          let outStart = MkFrameCtx fi () () info
          yieldStartFrameCtx outStart
