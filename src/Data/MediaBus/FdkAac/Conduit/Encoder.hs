{-# LANGUAGE RankNTypes #-}

-- | This module defines a conduit to encode linear audio into AAC.
module Data.MediaBus.FdkAac.Conduit.Encoder
  ( encodeLinearToAacC
  ) where

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.State.Strict as State
import Data.MediaBus
import Data.MediaBus.FdkAac.Encoder
import Data.String
import Data.Tagged
import Data.Typeable
import GHC.TypeLits


--  * Conduit Based 'Stream' Encoding
-- | A conduit that receives signed 16 bit, mono or stereo, 'Raw' 'Audio' and
-- yields AAC encoded frames. The timestamps of the input frames will be
-- regarded such that gaps in the timestamps are reflected in the generated
-- frames. The sequence numbering will be
encodeLinearToAacC
  :: forall channels r aot i s t m.
     ( MonadLoggerIO m
     , MonadThrow m
     , MonadResource m
     , KnownChannelLayout channels
     , KnownRate r
     , Typeable r
     , Show (AacAotProxy aot)
     , KnownNat (GetAacAot aot)
     , Show i
     , Show t
     , Typeable t
     , LocalOrd t
     , Show s
     , Typeable s
     , Num t
     , Num s
     , CanBeSample (Pcm channels S16)
     , Show (Pcm channels S16)
     , Integral t
     )
  => AacEncoderConfig r channels aot
  -> Conduit (Stream i s (Ticks r t) () (Audio r channels (Raw S16))) m (Stream i s (Ticks r t) (AacEncoderInfo r channels aot) (Audio r channels (Aac aot)))
encodeLinearToAacC aacCfg = do
  (Tagged enc, info) <- lift $ aacEncoderAllocate aacCfg
  (_res, _st, ()) <-
    prefixLogsC (show enc ++ " - ") $
    runRWSC
      enc
      (MkAacEncSt @r @channels @aot 0)
      (evalStateC
         (initialReframerState :: ReframerSt s (Ticks r t))
         (encodeThenFlush info))
  return ()
  where
    encodeThenFlush info = do
      awaitForever go
      aacs <- lift $ lift flushAacEncoder
      Prelude.mapM_ yieldAacFrame aacs
      where
        go (MkStream (Next f)) = do
          let reframeFrame = f & framePayload %~ getDurationTicks
          $logDebug (fromString ("in: " ++ show f))
          pushRes <- lift $ pushFrame reframeFrame
          case pushRes of
            Nothing -> return ()
            Just er -> do
              $logInfo
                (fromString
                   ("frame timing problem: " ++ show er ++ ", frame: " ++
                    show reframeFrame))
              lift $ State.get >>= $logInfoSH
              aacs <- lift $ lift flushAacEncoder
              Prelude.mapM_ yieldAacFrame aacs
              lift $
                pushStartFrame
                  (reframeFrame ^. frameTimestamp + reframeFrame ^. framePayload)
          aacs <-
            lift $ lift $
            encodeLinearToAac (view framePayload f)
          Prelude.mapM_ yieldAacFrame aacs
        go start@(MkStream (Start (MkFrameCtx fi ft fs _fp))) = do
          lift $ pushStartFrame ft
          lift $ $logDebug (fromString ("in: " ++ show start))
          let outStart = MkFrameCtx fi ft fs info
          lift $ $logDebug (fromString ("out: " ++ show outStart))
          yieldStartFrameCtx outStart
        yieldAacFrame fc =
          let wantDur = getDurationTicks fc
          in when (wantDur > 0) $ do
               frm <- lift $ generateFrame wantDur
               when (frm ^. framePayload < wantDur) $ do
                 $logInfo
                   (fromString
                      ("encoder generated more output than input, input: " ++
                       show (frm ^. framePayload) ++
                       " < output: " ++
                       show wantDur))
                 availDur <- lift nextFrameAvailableDuration
                 $logInfo
                   (fromString
                      ("samples enqueuend in the encoder delay lines: " ++
                       show availDur))
                 rfst <- lift $ State.get
                 $logInfoSH rfst
               let outFrm = frm & framePayload .~ fc
               $logDebug (fromString ("out: " ++ show outFrm))
               yieldNextFrame outFrm
