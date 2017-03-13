module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Logger
import Data.Conduit
import Data.MediaBus
import Data.MediaBus.FdkAac

main :: IO ()
main = void encodeOneSeconfOfSilence

encodeOneSeconfOfSilence
  :: IO [Stream SrcId32
                SeqNum16
                (Ticks64 (Hz 48000))
                (AacEncoderInfo (Hz 48000) Stereo 'HighEfficiency)
                (Audio (Hz 48000) Stereo (Aac 'HighEfficiency))]
encodeOneSeconfOfSilence =
  runStdoutLoggingT $
  runConduitRes $
        yieldNextFrame (MkFrame 0 0 pcmAudioOneSecond)
     .| encodeLinearToAacC encoderConfig
     .| traceShowSink 1 "Example Trace"

  where
    pcmAudioOneSecond :: Audio (Hz 48000) Stereo (Raw S16)
    pcmAudioOneSecond = blankFor 1
    encoderConfig = aacEncoderConfig aacHe48KhzStereo
