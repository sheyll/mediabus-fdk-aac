-- | This module defines a conduit to encode linear audio into AAC.
module Data.MediaBus.Audio.FdkAac.Encoder
  (
  ) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import qualified Foreign.C.Types as C

import Data.Maybe
-- import Control.Monad.Trans.Resource
import Data.MediaBus
import Data.MediaBus.Audio.FdkAac.EncoderFdkWrapper
import Data.Monoid
import Data.Proxy
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.TypeLits
import Text.Printf

-- * Core Aac Types
-- | Type level symbol to indicate that the AAC audio coding is used for the
-- respective media. This is a phantom type to index the 'Audio' data family.
data Aac (aot :: AacCodec)

-- | AAC codec sub-types
data AacCodec
  = LowComplexity -- ^ corresponds to audio object type @2@
  | HighEfficiency -- ^ corresponds to audio object type @5@
  deriving (Show)

-- | The media type for AAC encoded audio data.
data instance  Audio r c (Aac aot) = MkAacFrame{_aacFrameMediaData
                                                :: !(V.Vector Word8),
                                                _aacFrameSize :: !Word64}

-- * Encoder Configuration
-- | Create the 'Config' value needed to intialize the encoder from type-level
-- audio stream settings.
aacEncoderConfig
  :: ( KnownRate rate
     , KnownChannelLayout channels
     , KnownNat (GetAacCodecAOT codec)
     )
  => proxy1 rate -> proxy2 channels -> proxy3 codec -> Config
aacEncoderConfig pr pch pco =
  simpleConfig
    (getAacCodecAOT pco)
    (numberOfChannels pch)
    (fromInteger (rateVal pr))

-- | Convert the 'AacCodec' to an ISO/IEC 14496-3 Audio Object Type value.
type family GetAacCodecAOT (c :: AacCodec) :: Nat where
  GetAacCodecAOT 'LowComplexity = 2
  GetAacCodecAOT 'HighEfficiency = 5

-- | Convert the 'AacCodec' to an ISO/IEC 14496-3 Audio Object Type value.
getAacCodecAOT
  :: forall aot c proxy.
     (Integral aot, KnownNat (GetAacCodecAOT c))
  => proxy c -> aot
getAacCodecAOT _ =
  let px :: Proxy (GetAacCodecAOT c)
      px = Proxy
  in fromInteger $ natVal px

-- | This can be used to get a @proxy codec@ value (with @proxy ~ Aac@) for
-- 'LowComplexity' AAC, as needed by e.g. 'aacEncoderConfig'.
mkAacLc :: Proxy 'LowComplexity
mkAacLc = Proxy

-- | This can be used to get a @proxy codec@ value for 'HighEfficiency' AAC, as
-- needed by e.g. 'aacEncoderConfig'.
mkAacHe :: Proxy 'HighEfficiency
mkAacHe = Proxy

-- * Encoder Initialization
-- | The encoding specific information, created __by the encoder__ during
-- initialization.
data AacEncoderInfo = MkAacEncoderInfo
  { _aacEncoderInfoDelay :: !Word32
  -- ^ Number of samples that the output will be delayed through the encoding
  -- mechanism (e.g. codec delay lines)
  , _aacEncoderInfoMaxFrameSize :: !Word32
  -- ^ Number of samples that are encoded into each frame.
  , _aacEncoderInfoAudioSpecificConfig :: !(V.Vector Word8)
  -- ^ The audio specific configuration /box/ as specified in ISO/IEC 14496-3
  -- section 1.6, that the encoder generates.
  }

makeLenses ''AacEncoderInfo

-- * Stateful Encoding
-- | The internal state for 'encodeLinearToAac'
newtype AacEncSt (rate :: Rate) channels (codec :: AacCodec) = MkAacEncSt
  { _leftOvers :: Maybe (V.Vector Word16)
  -- ^ The unencoded left over input from a previous encoding step.
  }

makeLenses ''AacEncSt

-- | Type alias for the internal RWS monad of the aac encoder.
type AacEncT rate channels codec m a = RWST Encoder () (AacEncSt rate channels codec) m a

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
  -> AacEncT rate channels codec m (Either EncodeFailure (Maybe (Audio rate channels (Aac codec))))
encodeLinearToAac !aIn = do
  mOldIn <- leftOvers <<.= Nothing
  let dIn =
        fromMaybe mempty mOldIn <>
        V.unsafeCast (aIn ^. mediaBuffer . mediaBufferVector)
  e <- ask
  eres <- liftIO $ encode e dIn
  mapM putBackLeftOverInputAndReturnOutput eres
  where
    {-
If the type signature for putBackLeftOverInputAndReturnOutput on line 136 is omitted, GHC panics with:

/home/sven/DEV_LBM/lbm-talk-flow/mediabus-fdk-aac/src/Data/MediaBus/Audio/FdkAac/Encoder.hs:124:8: error:ghc: panic! (the 'impossible' happened)
  (GHC version 8.0.2 for x86_64-unknown-linux):
	No skolem info: k_ataY[sk]

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

    -}
    putBackLeftOverInputAndReturnOutput :: Monad m => EncodeResult -> AacEncT rate channels codec m (Maybe (Audio rate channels (Aac codec)))
    putBackLeftOverInputAndReturnOutput MkEncodeResult { encodeResultLeftOverInput
                                                       , encodeResultSamples
                                                       , encodeResultConsumedFrames
                                                       } = do
      leftOvers .= encodeResultLeftOverInput
      return $ do
        encodedSamples <- encodeResultSamples
        return (MkAacFrame encodedSamples encodeResultConsumedFrames)

-- | Flush the delay lines of the encoder.
flushAacEncoder
  :: (MonadIO m)
  => AacEncT r ch co m (Either EncodeFailure [Audio r ch (Aac co)])
flushAacEncoder = undefined
-- | A conduit wrapper for 'encodeLinearToAac'
-- encodeLinearToAacC
--   :: (MonadIO m, Num t, Num s)
--   => Conduit (Stream i s t p' c) m (Stream i s t AacInit Macrame)
-- encodeLinearToAacC = bracketP
