module Data.MediaBus.Audio.FdkAac.EncoderFdkWrapper
    ( AacEncErrorCode(..)
    , toAacEncErrorCode
    , simpleConfig
    , Config(..)
    , CreateFailure(..)
    , CreateFailedAt(..)
    , EncoderState(..)
    , create
    , EncodeResult(..)
    , EncodeFailure(..)
    , encode
    , destroy
    ) where

import           Data.MediaBus
import           Data.Monoid
import           Data.Word
import           Foreign.ForeignPtr           ( mallocForeignPtrBytes
                                              , withForeignPtr )
import           Foreign.C.Types
import qualified Language.C.Inline            as C
import qualified Data.Vector.Storable.Mutable as VM
import qualified Data.Vector.Storable         as V
import           Text.Printf
import           Data.Coerce

C.context (C.baseCtx <> C.vecCtx)

C.include "<stdio.h>"

C.include "<stdint.h>"

C.include "fdk-aac/aacenc_lib.h"

-- | Allocate a FDK-AAC encoder.
create :: Config -> IO (Either CreateFailure EncoderState)
create MkConfig{configModules,configChannels,configAot,configSampleRate,configBitRate,configBitRateMode,configBandwidth,configSbrMode,configSignallingMode,configChannelMode,configAfterburner} = do
    let confBufMaxLen = 255 :: CUInt
    confBufC <- mallocForeignPtrBytes (fromIntegral confBufMaxLen)
    ((encDelayC, confBufSizeC, frameSizeC, aacEncoderCfgErrorC, errorCodeC), hPtr) <- withForeignPtr confBufC $
                                                                                          \confBufP -> C.withPtrs $
                                                                                              \(encDelayP, confBufSizeP, frameSizeP, aacEncoderCfgErrorP, errorCodeP) ->
                                                                                                  [C.block| uintptr_t {
              AACENC_ERROR e = AACENC_OK;
              HANDLE_AACENCODER phAacEncoder;
              AACENC_InfoStruct pInfo;
              CHANNEL_MODE channelMode;
              *($(int* aacEncoderCfgErrorP)) = 13;

              switch ($(unsigned int configChannelMode)) {
                  case 1: channelMode = MODE_1;       break;
                  case 2: channelMode = MODE_2;       break;
                  case 3: channelMode = MODE_1_2;     break;
                  case 4: channelMode = MODE_1_2_1;   break;
                  case 5: channelMode = MODE_1_2_2;   break;
                  case 6: channelMode = MODE_1_2_2_1; break;
                 default: channelMode = 0;            break;
              }

              e = aacEncOpen(&phAacEncoder, $(unsigned int configModules), $(unsigned int configChannels));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 0;
                goto e0;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_AOT, (const UINT) $(unsigned int configAot));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 1;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_SBR_MODE, (const UINT) $(unsigned int configSbrMode));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 2;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_SAMPLERATE, (const UINT) $(unsigned int configSampleRate));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 3;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_CHANNELMODE, channelMode);
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 4;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_BITRATEMODE, (const UINT) $(unsigned int configBitRateMode));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 5;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_BITRATE, (const UINT) $(unsigned int configBitRate));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 6;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_BANDWIDTH, (const UINT) $(unsigned int configBandwidth));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 7;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_TRANSMUX, TT_MP4_RAW); // TODO extract from TRANSPORT_TYPE in FDK_audio.h
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 8;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_SIGNALING_MODE, (const UINT) $(unsigned int configSignallingMode));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 9;
                goto e1;
              }
              e = aacEncoder_SetParam(phAacEncoder, AACENC_AFTERBURNER, (const UINT) $(unsigned int configAfterburner));
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 10;
                goto e1;
              }
              e = aacEncEncode(phAacEncoder, NULL, NULL, NULL, NULL);
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 11;
                goto e1;
              }
              e = aacEncInfo(phAacEncoder, &pInfo);
              if (e != AACENC_OK) {
                *($(int* aacEncoderCfgErrorP)) = 12;
                goto e1;
              }

              for (unsigned int i = 0; i < pInfo.confSize && i < $(unsigned int confBufMaxLen); ++i) {
                 *($(unsigned char* confBufP) + i) = pInfo.confBuf[i];
              }
              *($(unsigned int* confBufSizeP)) = pInfo.confSize;
              *($(unsigned int* encDelayP))    = pInfo.encoderDelay;
              *($(unsigned int* frameSizeP))   = pInfo.frameLength;
              return ((uintptr_t) phAacEncoder);

              e1:
                 if (AACENC_OK != aacEncClose(&phAacEncoder)) {
                    printf("Failed to free the AAC Encoder.\n");
                 }

              e0:
                 *($(int* errorCodeP)) = e;
                 return (uintptr_t)NULL;
            } |]
    if hPtr == 0
        then return (Left MkCreateFailure { createFailureErrorCode = toAacEncErrorCode errorCodeC
                                          , createFailureAt = toEnum (fromIntegral aacEncoderCfgErrorC)
                                          })
        else do
            let ascVM = VM.unsafeFromForeignPtr0 confBufC
                                                 (fromIntegral confBufSizeC)
            asc <- V.freeze ascVM
            let outSize = fromIntegral (768 * configChannels)
            outV <- VM.new outSize
            return $
                Right $
                    MkEncoderState { encoderHandle = hPtr
                                   , channelCount = configChannels
                                   , encoderDelay = fromIntegral encDelayC
                                   , frameSize = fromIntegral frameSizeC
                                   , unsafeOutBuffer = outV
                                   , audioSpecificConfig = coerce asc
                                   }

data Config = MkConfig { configModules        :: !CUInt
                       , configChannels       :: !CUInt
                       , configAot            :: !CUInt
                       , configSampleRate     :: !CUInt
                       , configBitRate        :: !CUInt
                       , configBitRateMode    :: !CUInt
                       , configBandwidth      :: !CUInt
                       , configSbrMode        :: !CUInt
                       , configSignallingMode :: !CUInt
                       , configChannelMode    :: !CUInt
                       , configAfterburner    :: !CUInt
                       }
    deriving (Eq, Show)

simpleConfig :: Bool -> ChannelLayout -> Word32 -> Config
simpleConfig !highEfficiency !channels !sampleRate =
    let !configModules = 0x17
        !configChannels = case channels of
            SingleChannel -> 1
            ChannelPair -> 2
        !configAot = if highEfficiency then 5 else 2
        !configSampleRate = fromIntegral sampleRate
        !configBitRate = configChannelMode *
            round (fromIntegral configSampleRate *
                       ((if highEfficiency then 0.625 else 1.5) :: Double)) --
        !configBitRateMode = 0
        !configBandwidth = 8000
        !configSbrMode = fromIntegral (fromEnum highEfficiency)
        !configSignallingMode = if highEfficiency then 2 else 0
        !configChannelMode = case channels of
            SingleChannel -> 1
            ChannelPair -> 2
        !configAfterburner = 0
    in
        MkConfig { configModules
                 , configChannels
                 , configAot
                 , configSampleRate
                 , configBitRate
                 , configBitRateMode
                 , configBandwidth
                 , configSbrMode
                 , configSignallingMode
                 , configChannelMode
                 , configAfterburner
                 }

data CreateFailure = MkCreateFailure { createFailureErrorCode :: AacEncErrorCode
                                     , createFailureAt        :: CreateFailedAt
                                     }
    deriving (Show, Eq)

data CreateFailedAt = AacEncOpen
                    | AacEncSetAot
                    | AacEncSetSbrMode
                    | AacEncSetSampleRate
                    | AacEncSetChannelMode
                    | AacEncSetBitRateMode
                    | AacEncSetBitRate
                    | AacEncSetBandwidth
                    | AacEncSetTransMux
                    | AacEncSetSignalingMode
                    | AacEncSetAfterburner
                    | AacEncApplyConfig
                    | AacEncReadInfo
                    | AacEncUnknownError
    deriving (Eq, Show, Enum)

-- | Handle for a specific encoder, can be created with 'aacEncoderNew'.
data EncoderState = MkEncoderState { encoderHandle       :: !CUIntPtr
                                   , unsafeOutBuffer     :: !(VM.IOVector CUChar)
                                   , channelCount        :: !CUInt
                                   , encoderDelay        :: !Word32
                                   , frameSize           :: !Word32
                                   , audioSpecificConfig :: !(V.Vector Word8)
                                   }

-- | Encode Samples.
encode :: EncoderState
       -> V.Vector C.CShort
       -> IO (Either EncodeFailure EncodeResult)
encode MkEncoderState{encoderHandle,unsafeOutBuffer,channelCount} !vec = do
    ((numOutBytes, numInSamples, numAncBytes), retCode) <- C.withPtrs $
                                                               \(numOutBytesP, numInSamplesP, numAncBytesP) ->
                                                                   [C.block| int {
            AACENC_ERROR e;
            HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);

            /* Input buffer */
            AACENC_BufDesc inBuffDesc;
            INT inBuffIds[1]             = {IN_AUDIO_DATA};
            INT inBuffSizes[1]           = {$vec-len:vec * 2};
            INT inBuffElSizes[1]         = {2};
            void* inBuffBuffers[1]       = {$vec-ptr:(short *vec)};
            inBuffDesc.numBufs           = 1;
            inBuffDesc.bufs              = inBuffBuffers;
            inBuffDesc.bufferIdentifiers = inBuffIds;
            inBuffDesc.bufSizes          = inBuffSizes;
            inBuffDesc.bufElSizes        = inBuffElSizes;
            AACENC_InArgs inArgs         =
              { .numInSamples = $vec-len:vec
              , .numAncBytes = 0 };

            /* Ouput buffer */
            AACENC_BufDesc outBuffDesc;
            INT outBuffIds[1]             = {OUT_BITSTREAM_DATA};
            INT outBuffSizes[1]           = {$vec-len:unsafeOutBuffer};
            INT outBuffElSizes[1]         = {1};
            void* outBuffBuffers[1]       = {$vec-ptr:(unsigned char *unsafeOutBuffer)};
            outBuffDesc.numBufs           = 1;
            outBuffDesc.bufs              = outBuffBuffers;
            outBuffDesc.bufferIdentifiers = outBuffIds;
            outBuffDesc.bufSizes          = outBuffSizes;
            outBuffDesc.bufElSizes        = outBuffElSizes;
            AACENC_OutArgs outArgs;

            e = aacEncEncode (phAacEncoder, &inBuffDesc, &outBuffDesc,
                              &inArgs, &outArgs);
            *($(int* numOutBytesP))  = outArgs.numOutBytes;
            *($(int* numInSamplesP)) = outArgs.numInSamples;
            *($(int* numAncBytesP))  = outArgs.numAncBytes;

            return e;
         }|]
    if retCode /= 0
        then return $
            Left MkEncodeFailure { encodeFailureCode = toAacEncErrorCode retCode
                                 , encodeFailureNumOutBytes = fromIntegral numOutBytes
                                 , encodeFailureNumInSamples = fromIntegral numInSamples
                                 , encodeFailureNumAncBytes = fromIntegral numAncBytes
                                 }
        else do
            let !numInSamplesI = fromIntegral numInSamples
                !consumedFrames = fromIntegral numInSamplesI `div`
                    fromIntegral channelCount
                !leftOverInput = if numInSamplesI >= V.length vec
                                 then Nothing
                                 else let !inSliceLen = V.length vec -
                                              numInSamplesI
                                          !inSlice = V.slice numInSamplesI
                                                             inSliceLen
                                                             vec
                                      in
                                          Just inSlice
                !numOutBytesI = fromIntegral numOutBytes
            !encodedOutput <- if numOutBytesI == 0
                              then return Nothing
                              else do
                                  !outTooLarge <- V.freeze unsafeOutBuffer
                                  return $
                                      Just $
                                          V.force $
                                              V.slice 0 numOutBytesI outTooLarge
            return $
                Right MkEncodeResult { encodeResultConsumedFrames = consumedFrames
                                     , encodeResultLeftOverInput = coerce leftOverInput
                                     , encodeResultSamples = coerce encodedOutput
                                     }

data EncodeResult = MkEncodeResult { encodeResultConsumedFrames :: !Word64
                                   , encodeResultLeftOverInput  :: !(Maybe (V.Vector Word16))
                                   , encodeResultSamples        :: !(Maybe (V.Vector Word8))
                                   }

data EncodeFailure = MkEncodeFailure { encodeFailureCode         :: !AacEncErrorCode
                                     , encodeFailureNumOutBytes  :: !Word64
                                     , encodeFailureNumInSamples :: !Word64
                                     , encodeFailureNumAncBytes  :: !Word64
                                     }
    deriving (Eq)

instance Show EncodeFailure where
    show MkEncodeFailure{..} =
        printf "FDK AAC encode error: %s, numOutBytes:  %d, numInSamples: %d, numAncBytes: %d"
               (show encodeFailureCode)
               encodeFailureNumOutBytes
               encodeFailureNumInSamples
               encodeFailureNumAncBytes

-- TODO FLUSH!!!!!!!!!!!!!!!!!!!!!!
-- | Close an FDK-AAC encoder.
destroy :: EncoderState -> IO AacEncErrorCode
destroy MkEncoderState{encoderHandle} = do
    toAacEncErrorCode . fromIntegral <$>
        [C.block| int {
             HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);
             return aacEncClose(&phAacEncoder);
          } |]

data AacEncErrorCode = AacEncOk
                     | AacEncInvalidHandle
                     | AacEncMemoryError
                     | AacEncUnsupportedParameter
                     | AacEncInvalidConfig
                     | AacEncInitError
                     | AacEncInitAacError
                     | AacEncInitSbrError
                     | AacEncInitTpError
                     | AacEncInitMetaError
                     | AacEncEncoderError
                     | AacEncEncodeEof
                     | AacEncErrorOther CInt
    deriving (Eq, Show)

toAacEncErrorCode :: CInt -> AacEncErrorCode
toAacEncErrorCode e = case e of
    0x00 -> AacEncOk
    0x20 -> AacEncInvalidHandle
    0x21 -> AacEncMemoryError
    0x22 -> AacEncUnsupportedParameter
    0x23 -> AacEncInvalidConfig
    0x40 -> AacEncInitError
    0x41 -> AacEncInitAacError
    0x42 -> AacEncInitSbrError
    0x43 -> AacEncInitTpError
    0x44 -> AacEncInitMetaError
    0x60 -> AacEncEncoderError
    0x80 -> AacEncEncodeEof
    othr -> AacEncErrorOther othr
