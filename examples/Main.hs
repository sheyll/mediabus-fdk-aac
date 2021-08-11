module Main
  ( main,
  )
where

import Conduit
import Control.Monad
import Control.Monad.Logger
import Data.Conduit
import Data.Conduit.List (consume)
import Data.MediaBus
import Data.MediaBus.FdkAac

main :: IO ()
main = void encodeOneSeconfOfSilence

encodeOneSeconfOfSilence ::
  IO
    [ Stream
        SrcId32
        SeqNum64
        (Ticks64 (Hz 48000))
        (AacEncoderInfo (Hz 48000) Mono 'HighEfficiency)
        (Audio (Hz 48000) Mono (Aac 'HighEfficiency))
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

    pcmAudioOneSecond :: Audio (Hz 48000) Mono (Raw S16)
    pcmAudioOneSecond = blankFor 1

    encoderConfig = aacEncoderConfig aacHe48KhzMono
