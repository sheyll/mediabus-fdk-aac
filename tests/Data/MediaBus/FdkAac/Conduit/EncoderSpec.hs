module Data.MediaBus.FdkAac.Conduit.EncoderSpec (spec) where

import Conduit
import Control.Lens ((#))
import Control.Monad.Logger
import Control.Monad.State
import Data.Conduit.List (consume)
import Data.MediaBus
import Data.MediaBus.FdkAac
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Word
import System.Random
import Test.Hspec
import Test.QuickCheck

mkTestInputs ::
  Bool ->
  Int ->
  [Int] ->
  [Stream () Word32 (Ticks (Hz 16000) Word32) () (Audio (Hz 16000) Mono (Raw S16))]
mkTestInputs generateStartFrame randomSeed =
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
      ((0, 0), (mkStdGen randomSeed, [mkTestStartPacket | generateStartFrame]))
  where
    mkTestPacket sn ts len rnd =
      let (xxx, rnd') = runState (replicateM len (state random)) rnd
       in ( rnd',
            MkStream
              ( Next
                  ( MkFrame
                      ts
                      sn
                      ( pcmMediaBuffer . mediaBufferVector
                          # V.fromListN
                            len
                            (fromInteger <$> xxx)
                      )
                  )
              )
          )
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
  it "it does not flush when a start frame is received in the begining" $
    do
      let input = mkTestInputs True 1337 [234]
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
      map isStartFrame output `shouldBe` [True, False, False, False]
  it "it does flush after it encoded some complete frames" $
    do
      let input = mkTestInputs True 1337 [1024] ++ mkTestInputs True 1337 [1024]
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
      map isStartFrame output `shouldBe` [True, False, False, False, True, False]
  it "it does not flush when a start frame is received when it has not received any payload frames since the last start" $
    do
      let inputDoubleStart =
            mkTestInputs False 1337 [1024]
              ++ mkTestInputs True 1337 []
              ++ mkTestInputs True 1337 []
              ++ mkTestInputs True 1337 []
              ++ mkTestInputs True 1337 [1024]
      output <-
        runNoLoggingT
          . runResourceT
          . runConduit
          $ ( yieldMany inputDoubleStart
                .| assumeSynchronizedC
                .| encodeLinearToAacC exampleConfig
                .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                .| consume
            )
      map isStartFrame output `shouldBe` [False, False, False, True, True, True, True, False]
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
  it "encodes a single input frame (mono, 16kHz) to output frames with the same total duration" $
    property $ \(n :: Word16) seed ->
      n > 5 ==> ioProperty $ do
        let input = mkTestInputs False seed [fromIntegral n]
        output <-
          runNoLoggingT
            . runResourceT
            . runConduit
            $ ( yieldMany input
                  .| assumeSynchronizedC
                  .| encodeLinearToAacC exampleConfig
                  .| consume
              )
        return (sum (map getDuration output) === getDuration input)
  it "encodes all input frames (mono, 16kHz) to output frames with the same total duration" $
    property $ \(ns' :: [Word16]) seed ->
      let ns = map (`div` 16) . filter (> 0) $ ns'
       in not (null ns) ==> ioProperty $ do
            let input = mkTestInputs False seed (fromIntegral <$> ns)
            output <-
              runNoLoggingT
                . runResourceT
                . runConduit
                $ ( yieldMany input
                      .| assumeSynchronizedC
                      .| encodeLinearToAacC exampleConfig
                      .| consume
                  )
            return (sum (map getDuration output) === getDuration input)
  it "encodes segmented input frames (mono, 16kHz) to aggregated output frames with the same total duration" $
    do
      let input = mkTestInputs False 31337 [1024 * 32]
      output <-
        runNoLoggingT
          . runResourceT
          . runConduit
          $ ( yieldMany input
                .| segmentC 1
                .| forgetSegmentationC
                .| assumeSynchronizedC
                .| encodeLinearToAacC exampleConfig
                .| aggregateDurationC 1
                .| setSequenceNumberAndTimestampC @_ @(Hz 16000) @_ @Word32 @Word64
                .| consume
            )
      replicateM_ 10 (liftIO (putStrLn "******************************************************************************************"))
      sum (map getDuration output) `shouldBe` getDuration input

exampleConfig :: AacEncoderConfig (Hz 16000) Mono 'LowComplexity
exampleConfig = aacEncoderConfig (Proxy @(Audio (Hz 16000) Mono (Aac 'LowComplexity)))

startEachSegmentC ::
  (Monad m) =>
  (s -> t -> FrameCtx i' s t p') ->
  ConduitT (Stream i s t p (Segment c)) (Stream i' s t p' c) m ()
startEachSegmentC mkFrameCtx = do
  evalStateC False $
    awaitForever $ \case
      MkStream (Start fctx) -> do
        hadStart <- get
        unless hadStart $ do
          put True
          yieldStartFrameCtx (mkFrameCtx (_frameCtxSeqNumRef fctx) (_frameCtxTimestampRef fctx))
      MkStream (Next frm) -> do
        hadStart <- get
        unless hadStart $ do
          yieldStartFrameCtx (mkFrameCtx (_frameSeqNum frm) (_frameTimestamp frm))
        yieldNextFrame frm .| forgetSegmentationC
        put False
