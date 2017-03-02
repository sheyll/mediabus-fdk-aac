module Data.MediaBus.Audio.FdkAac.Encoder
    ( AacInit(..), aacInitDelay, aacInitFrameSize, aacInitAudioSpecificConfig, aacSink ) where

import           Data.MediaBus.Audio.FdkAac.EncoderFdkWrapper
import           Data.MediaBus
import           Data.Word
import qualified Data.Vector.Storable         as V
import           Text.Printf
import           Control.Lens

data AacInit = MkAacInit { _aacInitDelay :: !Word32
                         , _aacInitFrameSize :: !Word32
                         , _aacInitAudioSpecificConfig :: !(V.Vector Word8)
                         }

newtype AacFrame



makeLenses ''AacInit

aacSink :: (MonadResource m, MonadIO m, KnownNat sampleRate, Num t, Num s, HasSampleBuffer c, HasDuration c, GetSampleType c ~ S16 sampleRate)
        => Conduit (Stream i s t p' c) m (Stream i s t AacInit AacFrame)
aacSink =
