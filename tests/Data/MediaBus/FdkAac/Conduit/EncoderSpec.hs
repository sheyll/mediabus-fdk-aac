module Data.MediaBus.FdkAac.Conduit.EncoderSpec (spec) where

import Conduit
import Control.Monad.State
import Control.Lens ((#))
import Control.Monad.Logger
import Data.Conduit.List (consume)
import Data.MediaBus
import Data.MediaBus.FdkAac
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Word
import Test.Hspec
import Test.QuickCheck
import System.Random

mkTestInputs ::
  Bool ->
  Int ->
  [Int] ->
  [Stream () Word32 (Ticks (Hz 16000) Word32) () (Audio (Hz 16000) Mono (Raw S16))]
mkTestInputs isStartFrame randomSeed =
  reverse
    . snd
    . snd
    . foldl
      ( \((ts0, sn0), (rnd, acc0)) len ->
          ( (ts0 + fromIntegral len, sn0 + 1),
            let (rnd', nxt) = mkTestPacket sn0 ts0 (fromIntegral len) rnd
            in (rnd', nxt : acc0)
          )
      )
      ((0, 0), (mkStdGen randomSeed, [mkTestStartPacket | isStartFrame]))
  where
    mkTestPacket sn ts len rnd =
      let
        (xxx, rnd') = runState (replicateM len (state random)) rnd
      in (rnd', MkStream
          ( Next
              ( MkFrame
                  ts
                  sn
                  (pcmMediaBuffer . mediaBufferVector # V.fromListN len
                    (fromInteger <$> xxx))
              )
          ))
    mkTestStartPacket = MkStream (Start (MkFrameCtx () 0 0 ()))

spec :: Spec
spec = do
  it "encodes a single input frame with n*1024 samples (mono, 16kHz) to an output frame with the same total duration" $
    property $ \(Small n) seed ->
      n > 0 ==> ioProperty $ do
        let input = mkTestInputs False seed [n * 1024]
        output <-
          runNoLoggingT
            . runResourceT
            . runConduit
            $ ( yieldMany input
                  .| assumeSynchronizedC
                  .| encodeLinearToAacC exampleConfig
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                  .| consume
              )
        return (sum (map getDuration output) === getDuration input)
  it "encodes a single input frame with 1024 + 512 samples (mono, 16kHz) to an output frame with the same total duration" $
     do
        let input = mkTestInputs False 1337 [1024 + 512]
        output <-
          runNoLoggingT
            . runResourceT
            . runConduit
            $ ( yieldMany input
                  .| assumeSynchronizedC
                  .| encodeLinearToAacC exampleConfig
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                  .| consume
              )
        sum (map getDuration output) `shouldBe` getDuration input
  it "encodes two input frames with 1024 and 512 samples (mono, 16kHz) to an output frame with the same total duration" $
     do
        let input = mkTestInputs False 1337 [1024, 512]
        output <-
          runNoLoggingT
            . runResourceT
            . runConduit
            $ ( yieldMany input
                  .| assumeSynchronizedC
                  .| encodeLinearToAacC exampleConfig
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                  .| consume
              )
        sum (map getDuration output) `shouldBe` getDuration input
  it "encodes a single input frame with n samples (mono, 16kHz) to an output frame with the same total duration" $
    property $ \(n :: Word16) seed ->
      n > 0 ==> ioProperty $ do
        let input = mkTestInputs False seed [fromIntegral n]
        output <-
          runNoLoggingT
            . runResourceT
            . runConduit
            $ ( yieldMany input
                  .| assumeSynchronizedC
                  .| encodeLinearToAacC exampleConfig
                  .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                  -- .| traceShowC 1 "~~~~~~~~~~~~~~~~~~~~~~ SPEC 0004"
                  .| consume
              )
        return (sum (map getDuration output) === getDuration input)

exampleConfig :: AacEncoderConfig (Hz 16000) Mono 'LowComplexity
exampleConfig = aacEncoderConfig (Proxy @(Audio (Hz 16000) Mono (Aac 'LowComplexity)))