module Main
  ( main,
  )
where

import Control.Monad.Logger
import Criterion.Main
import Data.Conduit
import Data.Conduit.List
import Data.MediaBus
import Data.MediaBus.FdkAac
import Data.Time.Clock
import Data.Typeable
import Data.Word
import GHC.TypeLits

main :: IO ()
main = defaultMain [bgroupFromTestDuration 1]

bgroupFromTestDuration :: NominalDiffTime -> Benchmark
bgroupFromTestDuration testdur =
  bgroup (show testdur Prelude.++ " ptime=20ms") $
    let frameCount = ceiling (testdur / ptime)
        ptime = 20 / 1000
     in [ bgroup
            "AAC HE 16kHz"
            [ bench "Stereo" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacHe16KhzStereo) $
                    testFrames ptime frameCount,
              bench "Mono" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacHe16KhzMono) $
                    testFrames ptime frameCount
            ],
          bgroup
            "AAC LC 16kHz"
            [ bench "Stereo" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacLc16KhzStereo) $
                    testFrames ptime frameCount,
              bench "Mono" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacLc16KhzMono) $
                    testFrames ptime frameCount
            ],
          bgroup
            "AAC HE 48kHz"
            [ bench "Stereo" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacHe48KhzStereo) $
                    testFrames ptime frameCount,
              bench "Mono" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacHe48KhzMono) $
                    testFrames ptime frameCount
            ],
          bgroup
            "AAC LC 48kHz"
            [ bench "Stereo" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacLc48KhzStereo) $
                    testFrames ptime frameCount,
              bench "Mono" $
                nfIO $
                  encodeNFrames (aacEncoderConfig aacLc48KhzMono) $
                    testFrames ptime frameCount
            ]
        ]

encodeNFrames ::
  forall r channels aot.
  ( CanBeSample (Pcm channels S16),
    CanBeBlank (Pcm channels S16),
    Show (Pcm channels S16),
    KnownRate r,
    KnownChannelLayout channels,
    Show (AacAotProxy aot),
    KnownNat (GetAacAot aot),
    Typeable r
  ) =>
  AacEncoderConfig r channels aot ->
  [SyncStream SrcId32 () (Audio r channels (Raw S16))] ->
  IO [Stream SrcId32 SeqNum64 (Ticks r Word64) (AacEncoderInfo r channels aot) (Audio r channels (Aac aot))]
encodeNFrames cfg pcms =
  runStdoutLoggingT $
    runConduitRes $
      sourceList pcms .| encodeLinearToAacC cfg .| setSequenceNumberAndTimestampC
        .| consume

testFrames ::
  (KnownRate r, IsPcmValue (Pcm channels S16)) =>
  NominalDiffTime ->
  Int ->
  [SyncStream SrcId32 () (Audio r channels (Raw S16))]
testFrames frameDuration frames =
  Prelude.replicate frames $
    MkStream (Next (MkFrame () () (blankFor frameDuration)))
