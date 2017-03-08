{-# LANGUAGE RankNTypes #-}

-- | This module defines a conduit to encode linear audio into AAC.
module Data.MediaBus.FdkAac.Encoder
  ( Aac (..)
  , AacAot(..)
  , AacAotProxy(..)
  , type GetAacAot
  , getAacAot
  , mkAacHe
  , mkAacLc
  , aacEncoderConfig
  , AacEncoderInfo(..)
  , aacEncoderInfoDelay
  , aacEncoderInfoFrameSize
  , aacEncoderInfoAudioSpecificConfig
  , encodeLinearToAac
  , flushAacEncoder
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import Data.Maybe
-- import Control.Monad.Trans.Resource
import Data.MediaBus
import Data.MediaBus.FdkAac.EncoderFdkWrapper
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.TypeLits
import Text.Printf

-- * Core Aac Types
-- | Type level symbol to indicate that the AAC audio coding is used for the
-- respective media. This is a phantom type to index the 'Audio' data family.
data Aac (aot :: AacAot)

-- | AAC aot sub-types
data AacAot
  = LowComplexity -- ^ corresponds to audio object type @2@
  | HighEfficiency -- ^ corresponds to audio object type @5@
  deriving (Show)

-- | Convert the 'AacAot' to an ISO/IEC 14496-3 Audio Object Type value.
type family GetAacAot (c :: AacAot) :: Nat where
  GetAacAot 'LowComplexity = 2
  GetAacAot 'HighEfficiency = 5

-- | Convert the 'AacAot' to an ISO/IEC 14496-3 Audio Object Type value.
getAacAot
  :: forall aot c proxy.
     (Integral aot, KnownNat (GetAacAot c))
  => proxy c -> aot
getAacAot _ =
  let px :: Proxy (GetAacAot c)
      px = Proxy
  in fromInteger $ natVal px

-- | This can be used to get a @proxy aot@ value (with @proxy ~ Aac@) for
-- 'LowComplexity' AAC, as needed by e.g. 'aacEncoderConfig'.
mkAacLc :: Proxy 'LowComplexity
mkAacLc = Proxy

-- | This can be used to get a @proxy aot@ value for 'HighEfficiency' AAC, as
-- needed by e.g. 'aacEncoderConfig'.
mkAacHe :: Proxy 'HighEfficiency
mkAacHe = Proxy

-- | Internal helper for showing an 'AacAot'
data AacAotProxy (aot :: AacAot) =
  MkAacAotProxy

instance Show (AacAotProxy 'LowComplexity) where
  showsPrec _ _ = showString "LOW-COMPLEXITY"

instance Show (AacAotProxy 'HighEfficiency) where
  showsPrec _ _ = showString "HIGH-EFFICIENCY"

-- | The media type for AAC encoded audio data.
data instance  Audio r c (Aac aot) = MkAacFrame{aacFrameMediaData
                                                :: !(V.Vector Word8),
                                                aacFrameSize :: !Word64}

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         Show (Audio r c (Aac aot)) where
  showsPrec d MkAacFrame {aacFrameMediaData, aacFrameSize} =
    showParen
      (d > 10)
      (showString "AAC-FRAME: " .
       showsPrec 11 (MkShowMedia :: MediaDescription (Audio r c (Aac aot))) .
       showString ", FRAME-SIZE: " .
       shows aacFrameSize .
       showString ", FRAME-DATA: " .
       showsPrec 11 aacFrameMediaData)

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         Show (MediaDescription (Audio r c (Aac aot))) where
  showsPrec d _ =
    showParen
      (d > 10)
      (showsPrec 11 (MkAacAotProxy :: AacAotProxy aot) . showChar ' ' .
       showsPrec 11 (MkRateProxy :: RateProxy r) .
       showChar ' ' .
       showsPrec 11 (MkChannelLayoutProxy :: ChannelLayoutProxy c))

-- * Encoder Configuration
-- | Create the 'Config' value needed to intialize the encoder from type-level
-- audio stream settings.
aacEncoderConfig
  :: (KnownRate rate, KnownChannelLayout channels, KnownNat (GetAacAot aot))
  => proxy1 rate -> proxy2 channels -> proxy3 aot -> Config
aacEncoderConfig pr pch pco =
  simpleConfig
    (getAacAot pco)
    (numberOfChannels pch)
    (fromInteger (rateVal pr))

-- * Encoder Initialization
-- | The encoding specific information, created __by the encoder__ during
-- initialization.
data AacEncoderInfo = MkAacEncoderInfo
  { _aacEncoderInfoDelay :: !Word32
  -- ^ Number of samples that the output will be delayed through the encoding
  -- mechanism (e.g. aot delay lines)
  , _aacEncoderInfoFrameSize :: !Word32
  -- ^ Number of samples that are encoded into each frame.
  , _aacEncoderInfoAudioSpecificConfig :: !(V.Vector Word8)
  -- ^ The audio specific configuration /box/ as specified in ISO/IEC 14496-3
  -- section 1.6, that the encoder generates.
  }

makeLenses ''AacEncoderInfo

-- * Stateful Encoding
-- | The internal state for 'encodeLinearToAac'
newtype AacEncSt (rate :: Rate) channels (aot :: AacAot) = MkAacEncSt
  { _numberOfDelayedSamples :: Word64
  -- ^ The number of input samples currently buffered in the delay lines of the encoder.
  } deriving (Show)

makeLenses ''AacEncSt

-- | Type alias for the internal RWS monad of the aac encoder.
type AacEncT rate channels aot m a = RWST Encoder () (AacEncSt rate channels aot) m a

-- | Convert linear audio to AAC encoded audio with the given encoder settings.
-- This is stateful, in order to initialize the required state, create a
-- 'Config' using 'aacEncoderConfig' and call 'create'.
encodeLinearToAac
  :: ( KnownRate rate
     , KnownChannelLayout channels
     , CanBeSample (Pcm channels S16)
     , MonadIO m
     )
  => Audio rate channels (Raw S16)
  -> AacEncT rate channels aot m (Either EncodeFailure [Audio rate channels (Aac aot)])
encodeLinearToAac !aIn = do
  let dIn = V.unsafeCast (aIn ^. mediaBuffer . mediaBufferVector)
  encodeAllInput dIn []

encodeAllInput
  :: (MonadIO m)
  => V.Vector Word16
  -> [Audio rate channels (Aac aot)]
  -> AacEncT rate channels aot m (Either EncodeFailure [Audio rate channels (Aac aot)])
encodeAllInput dIn acc
  | V.length dIn <= 0 = return (Right [])
  | otherwise = do
    e <- ask
    eres <- liftIO $ encode e dIn
    join <$> mapM (encodeLeftOverInput acc) eres

encodeLeftOverInput
  :: (MonadIO m)
  => [Audio rate channels (Aac aot)]
  -> EncodeResult
  -> AacEncT rate channels aot m (Either EncodeFailure [Audio rate channels (Aac aot)])
encodeLeftOverInput acc MkEncodeResult { encodeResultLeftOverInput
                                       , encodeResultSamples
                                       , encodeResultConsumedFrames
                                       } = do
  numberOfDelayedSamples += encodeResultConsumedFrames
  delayed <- use numberOfDelayedSamples
  let acc' = maybe acc (\es -> MkAacFrame es encoded : acc) encodeResultSamples
      encoded =
        if isJust encodeResultSamples
          then delayed
          else 0
            --  TODO logging
  liftIO $
    printf
      "Samples consumed: %d, delayed: %d, encoded: %d\n"
      encodeResultConsumedFrames
      delayed
      encoded
  numberOfDelayedSamples -= encoded
  case encodeResultLeftOverInput of
    Nothing -> return (Right (reverse acc'))
    Just rest -> encodeAllInput rest acc'

-- | Flush any left over input and then also flush delay lines of the AAC encoder.
flushAacEncoder
  :: (MonadIO m)
  => AacEncT r ch co m (Either EncodeFailure [Audio r ch (Aac co)])
flushAacEncoder = do
  e <- ask
  eres <- liftIO $ flush e
  d <- use numberOfDelayedSamples
  liftIO $ printf "Flushing %d samples\n" d
  join <$> mapM (encodeLeftOverInput []) eres


-- | A conduit wrapper for 'encodeLinearToAac'
-- encodeLinearToAacC
--   :: (MonadIO m, Num t, Num s)
--   => Conduit (Stream i s t p' c) m (Stream i s t AacInit Macrame)
-- encodeLinearToAacC = bracketP
{-
GHCI:

:l Data.MediaBus.FdkAac.Encoder
Right enc <- create (aacEncoderConfig (Proxy :: Proxy (Hz 48000)) (Proxy :: Proxy Mono) mkAacHe)
let pcm1 = (pcmMediaBuffer . mediaBufferVector # V.replicate 2047 0) :: Audio (Hz 8000) Mono (Raw S16)
runRWST (replicateM 10 (encodeLinearToAac pcm1) >> flushAacEncoder >> replicateM 10 (encodeLinearToAac pcm1)) enc (MkAacEncSt 0 :: AacEncSt (Hz 8000) Mono 'HighEfficiency )


-}
