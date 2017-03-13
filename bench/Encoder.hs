module Main
  ( main
  ) where

import Control.Lens
import Control.Monad.Logger
import Criterion.Main
import Data.Conduit
import Data.Conduit.List
import Data.MediaBus
import Data.MediaBus.FdkAac.Encoder
import Data.Typeable
import Data.Vector.Storable as V
import Data.Word
import GHC.TypeLits

main :: IO ()
main =
  defaultMain
    [ bgroupFromTestDuration 1000 16000
    , bgroupFromTestDuration 1000 48000
    ]

bgroupFromTestDuration :: Int -> Int -> Benchmark
bgroupFromTestDuration testdur samplesPerSecond =
  bgroup
    (show testdur Prelude.++ "ms ptime=20ms " Prelude.++
     show samplesPerSecond Prelude.++
     " Hz") $
  let frameCount = testdur `div` ptime
      frameSamples = ptime * (samplesPerSecond `div` 1000)
      ptime = 20
  in [ bgroup
         "AAC HE"
         [ bench "Stereo" $ nfIO $
           encodeNFrames (aacEncoderConfig aacHe16KhzStereo) $
           testFrames frameSamples frameCount
         , bench "Mono" $ nfIO $ encodeNFrames (aacEncoderConfig aacHe16KhzMono) $
           testFrames frameSamples frameCount
         ]
     , bgroup
         "AAC LC"
         [ bench "Stereo" $ nfIO $
           encodeNFrames (aacEncoderConfig aacLc16KhzStereo) $
           testFrames frameSamples frameCount
         , bench "Mono" $ nfIO $ encodeNFrames (aacEncoderConfig aacLc16KhzMono) $
           testFrames frameSamples frameCount
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
  runStdoutLoggingT $ runConduitRes $ sourceList pcms .| encodeLinearToAacC cfg .|
  consume

testFrames
  :: IsPcmValue (Pcm channels S16)
  => Int
  -> Int
  -> [Stream SrcId32 SeqNum16 (Ticks r Word64) () (Audio r channels (Raw S16))]
testFrames samplesPerFrame frames =
  [ MkStream
    (Next
       (MkFrame
          (fromIntegral $ x * samplesPerFrame)
          (fromIntegral x)
          (pcmMediaBuffer . mediaBufferVector #
           V.replicate samplesPerFrame blank)))
  | x <- [0 .. frames]
  ]
