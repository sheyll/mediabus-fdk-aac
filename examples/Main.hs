module Main
  ( main,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.MediaBus
import Data.MediaBus.FdkAac.Conduit.Encoder (encodeLinearToAacC)
import Data.MediaBus.FdkAac.Encoder
  ( Aac,
    AacAot (HighEfficiency),
    AacEncoderInfo,
    aacEncoderConfig,
  )
import Data.Proxy (Proxy (Proxy))
import Data.Time (NominalDiffTime)

main :: IO ()
main = do
  void (encodeSecondsOfSilence @Mono 10)
  void (encodeSecondsOfSilence @Stereo 10)

encodeSecondsOfSilence ::
  forall c.
  ( CanBeSample (Pcm c S16),
    KnownChannelLayout c,
    Show (Pcm c S16),
    CanBeBlank (Pcm c S16)
  ) =>
  NominalDiffTime ->
  IO
    [ Stream
        SrcId32
        SeqNum64
        (Ticks64 (Hz 16000))
        (AacEncoderInfo (Hz 16000) c 'HighEfficiency)
        [Frame SeqNum64 (Ticks64 (Hz 16000)) (Audio (Hz 16000) c (Aac 'HighEfficiency))]
    ]
encodeSecondsOfSilence t =
  runStdoutLoggingT $
    runResourceT $
      runConduit $
        replicateM_ 5 (yieldNextFrame (MkFrame () () pcmAudioSeconds))
          .| traceShowC 1 "pcm"
          .| encodeLinearToAacC encoderConfig
          .| setSequenceNumberAndTimestampC
          .| traceShowC 1 "aac"
          .| aggregateDurationC 2
          .| setSequenceNumberAndTimestampC
          .| traceShowSink 1 "aac-aggregated"
  where
    pcmAudioSeconds :: Audio (Hz 16000) c (Raw S16)
    pcmAudioSeconds = blankFor t

    encoderConfig =
      aacEncoderConfig (Proxy :: Proxy (Audio (Hz 16000) c (Aac 'HighEfficiency)))
