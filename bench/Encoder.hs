module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Logger
import Data.Conduit
import Data.MediaBus
import Data.MediaBus.FdkAac
import Data.Conduit.List
import Data.Time.Clock
import Data.Typeable
import Criterion.Main
import Control.Lens
import Data.Vector.Storable as V
import Data.Word
import GHC.TypeLits

main :: IO ()
main =
  defaultMain
    [bgroupFromTestDuration 1000 16000, bgroupFromTestDuration 1000 48000]

bgroupFromTestDuration :: NominalDiffTime -> Int -> Benchmark
bgroupFromTestDuration testdur samplesPerSecond =
  bgroup
    (show testdur Prelude.++ " ptime=20ms " Prelude.++ show samplesPerSecond Prelude.++
     " Hz") $
  let frameCount = ceiling (testdur / ptime)
      ptime = 20 / 1000
  in [ bgroup
         "AAC HE"
         [ bench "Stereo" $
           nfIO $
           encodeNFrames (aacEncoderConfig aacHe16KhzStereo) $
           testFrames testdur frameCount
         , bench "Mono" $
           nfIO $
           encodeNFrames (aacEncoderConfig aacHe16KhzMono) $
           testFrames testdur frameCount
         ]
     , bgroup
         "AAC LC"
         [ bench "Stereo" $
           nfIO $
           encodeNFrames (aacEncoderConfig aacLc16KhzStereo) $
           testFrames testdur frameCount
         , bench "Mono" $
           nfIO $
           encodeNFrames (aacEncoderConfig aacLc16KhzMono) $
           testFrames testdur frameCount
         ]
     ]

encodeNFrames
  :: forall r channels aot.
     ( CanBeSample (Pcm channels S16)
     , CanBeBlank (Pcm channels S16)
     , Show (Pcm channels S16)
     , KnownRate r
     , KnownChannelLayout channels
     , Show (AacAotProxy aot)
     , KnownNat (GetAacAot aot)
     , Typeable r
     )
  => AacEncoderConfig r channels aot
  -> [Stream SrcId32 SeqNum16 (Ticks r Word64) () (Audio r channels (Raw S16))]
  -> IO [Stream SrcId32 SeqNum16 (Ticks r Word64) (AacEncoderInfo r channels aot) (Audio r channels (Aac aot))]
encodeNFrames cfg pcms =
  runStdoutLoggingT $
  runConduitRes $ sourceList pcms .| encodeLinearToAacC cfg .| consume

testFrames
  :: (KnownRate r, IsPcmValue (Pcm channels S16))
  => NominalDiffTime
  -> Int
  -> [Stream SrcId32 SeqNum16 (Ticks r Word64) () (Audio r channels (Raw S16))]
testFrames frameDuration frames =
  [ MkStream
    (Next
       (MkFrame
          (fromIntegral x * (nominalDiffTime # frameDuration))
          (fromIntegral x)
          (blankFor frameDuration)))
  | x <- [0 .. frames]
  ]
