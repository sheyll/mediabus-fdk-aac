{-# LANGUAGE RankNTypes #-}

-- | This module defines a type-safe, high-level interface to encode linear
--   audio into AAC. This interface is still relatively low-level.
--   For an easy to use, high-level interface please consult
--   'Data.MediaBus.FdkAac.Conduit.Encoder'.
module Data.MediaBus.FdkAac.Encoder
  ( aacFrameSize
  , aacFrameMediaData
  , Aac
  , aacHe48KhzStereo
  , aacHe16KhzStereo
  , aacHe48KhzMono
  , aacHe16KhzMono
  , aacLc48KhzStereo
  , aacLc16KhzStereo
  , aacLc48KhzMono
  , aacLc16KhzMono
  , aacEncoderConfig
  , AacEncoderInfo(..)
  , aacEncoderInfoId
  , aacEncoderInfoDelay
  , aacEncoderInfoFrameSize
  , aacEncoderInfoAudioSpecificConfig
  , type AacEncoderConfig
  , type AacEncoderContext
  , AacAot(..)
  , AacAotProxy(..)
  , type GetAacAot
  , aacEncoderAllocate
  , type AacEncT
  , AacEncSt(..)
  , numberOfDelayedSamples
  , encodeLinearToAac
  , flushAacEncoder
  ) where

import Conduit
import Control.DeepSeq
import Control.Lens
import Control.Monad.Logger
import Control.Monad.Trans.RWS.Strict
import Control.Monad.Trans.Resource
import Data.Kind
import Data.Maybe
import Data.MediaBus
import Data.MediaBus.FdkAac.EncoderFdkWrapper
import Data.Proxy
import Data.String
import Data.Tagged
import qualified Data.Vector.Storable as V
import Data.Word
import GHC.Generics (Generic)
import GHC.TypeLits
import Text.Printf

-- * MediaBus Aac Encoder
-- | This is the media type for AAC encoded audio data.
data Aac (aot :: AacAot)

data instance  Audio r c (Aac aot) = MkAacFrame{aacFrameMediaData
                                                :: !(V.Vector Word8),
                                                aacFrameSize :: !Word64}
                                   deriving (Generic)

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         IsMedia (Audio r c (Aac aot))

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         HasMedia (Audio r c (Aac aot)) (Audio r c (Aac aot)) where
  type MediaFrom (Audio r c (Aac aot)) = Audio r c (Aac aot)
  type MediaTo (Audio r c (Aac aot)) = Audio r c (Aac aot)
  media = iso id id

instance HasMediaBuffer (Audio r c (Aac aot)) (Audio r c (Aac aot)) where
  type MediaBufferFrom (Audio r c (Aac aot)) = MediaBuffer Word8
  type MediaBufferTo (Audio r c (Aac aot)) = MediaBuffer Word8
  mediaBuffer =
    lens
      (MkMediaBuffer . aacFrameMediaData)
      (\aacFrame -> \(MkMediaBuffer v) -> aacFrame {aacFrameMediaData = v})

instance (KnownRate r) =>
         HasDuration (Audio r c (Aac aot)) where
  getDuration MkAacFrame {aacFrameSize} =
    from nominalDiffTime # (MkTicks aacFrameSize :: Ticks r Word64)
  getDurationTicks MkAacFrame {aacFrameSize} =
    convertTicks (MkTicks aacFrameSize :: Ticks r Word64)

instance NFData (Audio r c (Aac aot))

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         Show (Audio r c (Aac aot)) where
  showsPrec d MkAacFrame {aacFrameMediaData, aacFrameSize} =
    showParen
      (d > 10)
      (showString "aac frame: " .
       showsPrec 11 (MkShowMedia :: MediaDescription (Audio r c (Aac aot))) .
       showString ", samples: " .
       shows aacFrameSize .
       showString ", data: " .
       showsPrec 11 (V.length aacFrameMediaData) . showString " bytes starting with: " .
       showsPrec 11 (V.take 16 aacFrameMediaData))

instance (KnownRate r, KnownChannelLayout c, Show (AacAotProxy aot)) =>
         Show (MediaDescription (Audio r c (Aac aot))) where
  showsPrec d _ =
    showParen
      (d > 10)
      (showsPrec 11 (MkAacAotProxy :: AacAotProxy aot) . showChar ' ' .
       showsPrec 11 (MkRateProxy :: RateProxy r) .
       showChar ' ' .
       showsPrec 11 (MkChannelLayoutProxy :: ChannelLayoutProxy c))

-- ** Aac Configuration
-- | Create the 'Config' value needed to intialize the encoder from type-level
-- audio stream settings.
aacEncoderConfig
  :: forall rate channels aot proxy.
     (KnownRate rate, KnownChannelLayout channels, KnownNat (GetAacAot aot))
  => proxy (Audio rate channels (Aac aot)) -> AacEncoderConfig rate channels aot
aacEncoderConfig _pa =
  let pr :: Proxy rate
      pr = Proxy
      pch :: Proxy channels
      pch = Proxy
      pco :: AacAotProxy aot
      pco = MkAacAotProxy
  in Tagged $
     simpleConfig
       (getAacAot pco)
       (numberOfChannels pch)
       (fromInteger (rateVal pr))

-- *** Pre-defined Aac media Proxies
-- | 48 kHz Stereo 'HighEfficiency'
aacHe48KhzStereo :: Proxy (Audio (Hz 48000) Stereo (Aac 'HighEfficiency))
aacHe48KhzStereo = Proxy

-- | 16 kHz Stereo 'HighEfficiency'
aacHe16KhzStereo :: Proxy (Audio (Hz 16000) Stereo (Aac 'HighEfficiency))
aacHe16KhzStereo = Proxy

-- | 48 kHz Mono 'HighEfficiency'
aacHe48KhzMono :: Proxy (Audio (Hz 48000) Mono (Aac 'HighEfficiency))
aacHe48KhzMono = Proxy

-- | 16 kHz Mono 'HighEfficiency'
aacHe16KhzMono :: Proxy (Audio (Hz 16000) Mono (Aac 'HighEfficiency))
aacHe16KhzMono = Proxy

-- | 48 kHz Stereo 'LowComplexity'
aacLc48KhzStereo :: Proxy (Audio (Hz 48000) Stereo (Aac 'LowComplexity))
aacLc48KhzStereo = Proxy

-- | 16 kHz Stereo 'LowComplexity'
aacLc16KhzStereo :: Proxy (Audio (Hz 16000) Stereo (Aac 'LowComplexity))
aacLc16KhzStereo = Proxy

-- | 48 kHz Mono 'LowComplexity'
aacLc48KhzMono :: Proxy (Audio (Hz 48000) Mono (Aac 'LowComplexity))
aacLc48KhzMono = Proxy

-- | 16 kHz Mono 'LowComplexity'
aacLc16KhzMono :: Proxy (Audio (Hz 16000) Mono (Aac 'LowComplexity))
aacLc16KhzMono = Proxy

-- | The encoding specific information, created __by the encoder__ during
-- initialization.
data AacEncoderInfo (rate :: Rate) (channels :: Type) (aot :: AacAot) = MkAacEncoderInfo
  { _aacEncoderInfoDelay :: !Word64
  -- ^ Number of samples that the output will be delayed through the encoding
  -- mechanism (e.g. aot delay lines)
  , _aacEncoderInfoFrameSize :: !Word64
  -- ^ Number of samples that are encoded into each frame.
  , _aacEncoderInfoAudioSpecificConfig :: !(V.Vector Word8)
  -- ^ The audio specific configuration /box/ as specified in ISO/IEC 14496-3
  -- section 1.6, that the encoder generates.
  , _aacEncoderInfoId :: String
  -- ^ Identification string for one specific encoder, derived from the natice
  -- C pointer in 'encoderHandle'.
  } deriving Generic

instance NFData (AacEncoderInfo r c a)

instance ( KnownRate r
         , KnownChannelLayout c
         , Show (AacAotProxy aot)
         , KnownNat (GetAacAot aot)
         ) =>
         Show (AacEncoderInfo r c aot) where
  showsPrec d MkAacEncoderInfo { _aacEncoderInfoDelay
                               , _aacEncoderInfoFrameSize
                               , _aacEncoderInfoAudioSpecificConfig
                               , _aacEncoderInfoId
                               } =
    showParen
      (d > 10)
      (showString _aacEncoderInfoId . showString " " .
       showsPrec 11 (MkShowMedia :: MediaDescription (Audio r c (Aac aot))) .
       showParen
         True
         (showString "encoding delay: " . showsPrec 11 _aacEncoderInfoDelay .
          showString ", samples per frame: " .
          showsPrec 11 _aacEncoderInfoFrameSize .
          showString ", audio specific config: " .
          showsPrec 11 _aacEncoderInfoAudioSpecificConfig))

-- | Tag a 'Config' with the compile time configuration parameters.
type AacEncoderConfig rate channels aot = Tagged '( rate, channels, aot) Config

-- | Tag an 'Encoder' with the compile time configuration parameters.
type AacEncoderContext rate channels aot = Tagged '( rate, channels, aot) Encoder

-- ** Audio Object Types
-- | Type level symbol to indicate that the AAC audio coding is used for the
-- respective media. This is a phantom type to index the 'Audio' data family.
data AacAot
  = LowComplexity -- ^ corresponds to audio object type @2@
  | HighEfficiency -- ^ corresponds to audio object type @5@
  deriving (Show)

instance Show (AacAotProxy 'LowComplexity) where
  showsPrec _ _ = showString "low-complexity"

instance Show (AacAotProxy 'HighEfficiency) where
  showsPrec _ _ = showString "high-efficiency"

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

-- | Proxy for showing an 'AacAot'
data AacAotProxy (aot :: AacAot) =
  MkAacAotProxy

-- * Encoder Initialization
-- | Allocate a new encoder instance in a 'ResourceT' monad such that it will
-- automatically be released and the freed.
aacEncoderAllocate
  :: ( KnownRate rate
     , KnownChannelLayout channels
     , Show (AacAotProxy aot)
     , KnownNat (GetAacAot aot)
     , MonadIO m
     , MonadResource m
     , MonadThrow m
     , MonadLoggerIO m
     )
  => AacEncoderConfig rate channels aot
  -> m (AacEncoderContext rate channels aot, AacEncoderInfo rate channels aot)
aacEncoderAllocate (Tagged cfg) = do
  let destroyAndLog logger enc =
        runLoggingT
          (do $logDebug (fromString (printf "%s destroying" (show enc)))
              res <- liftIO (destroy enc)
              case res of
                AacEncOk ->
                  $logInfo (fromString (printf "%s destroyed" (show enc)))
                other ->
                  $logError
                    (fromString
                       (printf
                          "%s failed to destroy: %s"
                          (show enc)
                          (show other))))
          logger
  logger <- askLoggerIO
  (rkey, eres) <- allocate (create cfg) (Prelude.mapM_ (destroyAndLog logger))
  case eres of
    Left err -> do
      $logError (fromString (printf "encoder allocation failed: %s" (show err)))
      release rkey
      throwM err
    Right enc@MkEncoder {encoderDelay, frameSize, audioSpecificConfig} -> do
      let i =
            MkAacEncoderInfo
            { _aacEncoderInfoDelay = fromIntegral encoderDelay
            , _aacEncoderInfoFrameSize = fromIntegral frameSize
            , _aacEncoderInfoAudioSpecificConfig = audioSpecificConfig
            , _aacEncoderInfoId = show enc
            }
      $logInfo (fromString (printf "allocated: %s" (show i)))
      return (Tagged enc, i)

-- * Stateful Encoding

-- | Type alias for the internal RWS monad of the aac encoder.
type AacEncT rate channels aot m a = RWST Encoder () (AacEncSt rate channels aot) m a

-- | The internal state for 'encodeLinearToAac'
newtype AacEncSt (rate :: Rate) channels (aot :: AacAot) = MkAacEncSt
  { _numberOfDelayedSamples :: Word64
  -- ^ The number of input samples currently buffered in the delay lines of the encoder.
  } deriving (Show)

-- | An internal 'Iso' for the 'AacEncSt'
numberOfDelayedSamples :: Iso' (AacEncSt (rate :: Rate) channels (aot :: AacAot)) Word64
numberOfDelayedSamples = iso _numberOfDelayedSamples MkAacEncSt

-- | Convert linear audio to AAC encoded audio with the given encoder settings.
encodeLinearToAac
  :: ( KnownRate rate
     , KnownChannelLayout channels
     , KnownRate rate
     , Show (AacAotProxy aot)
     , CanBeSample (Pcm channels S16)
     , MonadIO m
     , MonadThrow m
     , MonadLogger m
     )
  => Audio rate channels (Raw S16)
  -> AacEncT rate channels aot m [Audio rate channels (Aac aot)]
encodeLinearToAac !aIn =
  let dIn = V.unsafeCast (aIn ^. mediaBuffer . mediaBufferVector)
  in encodeSampleVector dIn []

-- | Convert linear audio to AAC encoded audio with the given encoder settings,
-- from a raw sample vector.
-- If the sample vector contains more that the encoder can swallow at a time,
-- the encoder is repeatedly called until all input is consumed.
encodeSampleVector
  :: forall m channels rate aot.
     ( MonadLogger m
     , MonadIO m
     , MonadThrow m
     , KnownChannelLayout channels
     , KnownRate rate
     , Show (AacAotProxy aot)
     , V.Storable (Pcm channels S16)
     )
  => V.Vector Word16
  -> [Audio rate channels (Aac aot)]
  -> AacEncT rate channels aot m [Audio rate channels (Aac aot)]
encodeSampleVector dIn acc
  | V.length dIn <= 0 = return []
  | otherwise = do
    e <- ask
    eres <- liftIO $ encode e dIn
    case eres of
      Left err -> do
        $logError (fromString (printf "encoding failed: %s" (show err)))
        throwM err
      Right fr -> returnAccOrEncodeAgain acc fr

-- | Flush any left over input and then also flush delay lines of the AAC encoder.
flushAacEncoder
  :: ( MonadIO m
     , MonadLogger m
     , MonadThrow m
     , KnownChannelLayout channels
     , KnownRate r
     , Show (AacAotProxy aot)
     , V.Storable (Pcm channels S16)
     )
  => AacEncT r channels aot m [Audio r channels (Aac aot)]
flushAacEncoder = do
  d <- use numberOfDelayedSamples
  e <- ask
  $logDebug (fromString (printf "flushing %d samples" d))
  eres <- liftIO $ flush e
  case eres of
    Left err -> do
      $logError (fromString (printf "flushing failed: %s" (show err)))
      throwM err
    Right fr -> do
      $logInfo (fromString (printf "flushed frame: %s" (show fr)))
      returnAccOrEncodeAgain [] fr

-- | Internal function
returnAccOrEncodeAgain
  :: ( MonadLogger m
     , MonadIO m
     , MonadThrow m
     , KnownChannelLayout channels
     , KnownRate rate
     , Show (AacAotProxy aot)
     , V.Storable (Pcm channels S16)
     )
  => [Audio rate channels (Aac aot)]
  -> EncodeResult
  -> AacEncT rate channels aot m [Audio rate channels (Aac aot)]
returnAccOrEncodeAgain acc MkEncodeResult { encodeResultLeftOverInput
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
  numberOfDelayedSamples -= encoded
  case encodeResultLeftOverInput of
    Nothing -> return (reverse acc')
    Just rest -> encodeSampleVector rest acc'

makeLenses ''AacEncoderInfo
