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

main :: IO ()
main = do
  void (encodeOneSeconfOfSilence @Mono)
  void (encodeOneSeconfOfSilence @Stereo)

encodeOneSeconfOfSilence ::
  forall c.
  ( CanBeSample (Pcm c S16),
    KnownChannelLayout c,
    Show (Pcm c S16),
    CanBeBlank (Pcm c S16)
  ) =>
  IO
    [ Stream
        SrcId32
        SeqNum64
        (Ticks64 (Hz 48000))
        (AacEncoderInfo (Hz 48000) c 'HighEfficiency)
        (Audio (Hz 48000) c (Aac 'HighEfficiency))
    ]
encodeOneSeconfOfSilence =
  runStdoutLoggingT $
    runResourceT $
      runConduit $
        yieldNextFrame (MkFrame () () pcmAudioOneSecond)
          .| traceShowC 1 "raw"
          .| encodeLinearToAacC encoderConfig
          .| traceShowC 1 "encoded"
          .| setSequenceNumberAndTimestampC
          .| traceShowSink 1 "timestamped and sequenced"
  where
    pcmAudioOneSecond :: Audio (Hz 48000) c (Raw S16)
    pcmAudioOneSecond = blankFor 1

    encoderConfig = aacEncoderConfig (Proxy :: Proxy (Audio (Hz 48000) c (Aac 'HighEfficiency)))
