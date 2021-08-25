-- | An internal module that tightly wraps around the Frauenhofer Development
-- Toolkit for AAC audio.
module Data.MediaBus.FdkAac.EncoderFdkWrapper
  ( AacEncErrorCode (..),
    toAacEncErrorCode,
    simpleConfig,
    Config (..),
    CreateFailure (..),
    CreateFailedAt (..),
    Encoder (..),
    create,
    EncodeResult (..),
    EncodeFailure (..),
    encode,
    flush,
    destroy,
  )
where

import Control.Exception
import Data.Coerce
import Data.Int
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM
import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr (mallocForeignPtrBytes, withForeignPtr)
import qualified Language.C.Inline as C
import Text.Printf
import UnliftIO

C.context (C.baseCtx <> C.vecCtx)

C.include "<stdio.h>"

C.include "<stdint.h>"

C.include "fdk-aac/aacenc_lib.h"

-- | Call into the native code to initialize an encoder context, if everything
-- works out great, an 'Encoder' is returned. Use 'destroy' to release the
-- resources associated with an 'Encoder'.
create :: Config -> IO (Either CreateFailure Encoder)
create
  config@MkConfig
    { configModules,
      configChannels,
      configAot,
      configSampleRate,
      configBitRate,
      configBitRateMode,
      configBandwidth,
      configSbrMode,
      configSignallingMode,
      configChannelMode,
      configAfterburner
    } = do
    let confBufMaxLen = 255 :: CUInt
    confBufC <- mallocForeignPtrBytes (fromIntegral confBufMaxLen)
    ((encDelayC, confBufSizeC, frameSizeC, aacEncoderCfgErrorC, errorCodeC), hPtr) <-
      withForeignPtr confBufC $ \confBufP ->
        C.withPtrs $ \(encDelayP, confBufSizeP, frameSizeP, aacEncoderCfgErrorP, errorCodeP) ->
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
              if ((const UINT) $(unsigned int configBandwidth) != 0) {
                e = aacEncoder_SetParam(phAacEncoder, AACENC_BANDWIDTH, (const UINT) $(unsigned int configBandwidth));
                if (e != AACENC_OK) {
                  *($(int* aacEncoderCfgErrorP)) = 7;
                  goto e1;
                }
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
              *($(unsigned int* encDelayP))    = pInfo.nDelay;
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
      then
        return
          ( Left
              MkCreateFailure
                { createFailureErrorCode = toAacEncErrorCode errorCodeC,
                  createFailureAt = toEnum (fromIntegral aacEncoderCfgErrorC),
                  createFailureInputConfig = config
                }
          )
      else do
        let ascVM = VM.unsafeFromForeignPtr0 confBufC (fromIntegral confBufSizeC)
        asc <- V.freeze ascVM
        let outSize = fromIntegral (768 * configChannels)
        outV <- VM.new outSize
        inBufRef <- newIORef 0
        return $
          Right $
            MkEncoder
              { encoderHandle = hPtr,
                channelCount = configChannels,
                encoderDelay = fromIntegral encDelayC,
                frameSize = fromIntegral frameSizeC,
                unsafeOutBuffer = outV,
                audioSpecificConfig = coerce asc,
                samplesInBufferRef = inBufRef
              }

-- | A subset of the possible encoder configuration parameters
data Config = MkConfig
  { configModules :: !CUInt,
    configChannels :: !CUInt,
    configAot :: !CUInt,
    configSampleRate :: !CUInt,
    configBitRate :: !CUInt,
    configBitRateMode :: !CUInt,
    -- | The audio frequency bandwidth to be considered
    -- when compressing audio, if @0@ the setting is
    -- not applied.
    configBandwidth :: !CUInt,
    configSbrMode :: !CUInt,
    configSignallingMode :: !CUInt,
    configChannelMode :: !CUInt,
    configAfterburner :: !CUInt
  }
  deriving (Eq, Show)

-- | Generate a 'Config' from three simple parameters
simpleConfig :: Word8 -> Int -> Word32 -> Config
simpleConfig !aot !channels !sampleRate =
  let !configModules = 0x17
      !configChannels = fromIntegral channels
      !highEfficiency = configAot == 5
      !configAot = fromIntegral aot
      !configSampleRate = fromIntegral sampleRate
      !configBitRate =
        configChannelMode
          * round
            ( fromIntegral configSampleRate
                * ( ( if highEfficiency
                        then 0.625
                        else 1.5
                    ) ::
                      Double --
                  )
            )
      !configBitRateMode = 0
      !configBandwidth = 0
      !configSbrMode = fromIntegral (fromEnum highEfficiency)
      !configSignallingMode =
        if highEfficiency
          then 2
          else 0
      !configChannelMode = fromIntegral channels
      !configAfterburner = 1
   in MkConfig
        { configModules,
          configChannels,
          configAot,
          configSampleRate,
          configBitRate,
          configBitRateMode,
          configBandwidth,
          configSbrMode,
          configSignallingMode,
          configChannelMode,
          configAfterburner
        }

-- | Description of the context in which the 'create' function failed.
data CreateFailure = MkCreateFailure
  { createFailureErrorCode :: AacEncErrorCode,
    createFailureAt :: CreateFailedAt,
    createFailureInputConfig :: Config
  }
  deriving (Show, Eq)

instance Exception CreateFailure

-- | This sum type narrows down the specific step that failed,
-- in the @inline-c@ wrapper code in the 'create' function.
data CreateFailedAt
  = AacEncOpen
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
data Encoder = MkEncoder
  { encoderHandle :: !CUIntPtr,
    unsafeOutBuffer :: !(VM.IOVector CUChar),
    channelCount :: !CUInt,
    encoderDelay :: !Word32,
    frameSize :: !Word32,
    audioSpecificConfig :: !(V.Vector Word8),
    -- | The number of input samples per channel inside the decoder buffer,
    --  that were not yet encoded.
    samplesInBufferRef :: !(IORef Word64)
  }

-- | This instance only shows the 'encoderHandle' as hex string.
instance Show Encoder where
  showsPrec d MkEncoder {encoderHandle} =
    showParen (d > 10) $
      showString (printf "fdk-aac-enc: %016X" (toInteger encoderHandle))

-- | Encode Samples.
encode :: Encoder -> V.Vector Word16 -> IO (Either EncodeFailure EncodeResult)
encode enc@MkEncoder {encoderHandle, unsafeOutBuffer} !vecW16 = do
  let vec = coerce vecW16
  toEncodeResult vec enc
    =<< C.withPtrs
      ( \(numOutBytesP, numInSamplesP, numAncBytesP) ->
          [C.block| int {
            AACENC_ERROR e;
            HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);


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
      )

-- | Encode the contents of the delay lines of the encoder.
flush :: Encoder -> IO (Either EncodeFailure EncodeResult)
flush enc@MkEncoder {encoderHandle, unsafeOutBuffer} =
  toEncodeResult mempty enc
    =<< C.withPtrs
      ( \(numOutBytesP, numInSamplesP, numAncBytesP) ->
          [C.block| int {
            AACENC_ERROR e;
            HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);


            AACENC_BufDesc inBuffDesc;
            inBuffDesc.numBufs           = 0;

            AACENC_InArgs inArgs =
              { .numInSamples = -1
              , .numAncBytes = 0 };

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
      } |]
      )

-- | Internal function
toEncodeResult ::
  V.Vector C.CShort ->
  Encoder ->
  ((C.CInt, C.CInt, C.CInt), CInt) ->
  IO (Either EncodeFailure EncodeResult)
toEncodeResult vec MkEncoder {unsafeOutBuffer, channelCount} ((numOutBytes, numInSamples, numAncBytes), retCode) =
  let retCode' = toAacEncErrorCode retCode
   in if retCode' == AacEncEncodeEof
        then return (Right MkEncodeResultEof)
        else
          if retCode' /= AacEncOk
            then
              return $
                Left
                  MkEncodeFailure
                    { encodeFailureCode = toAacEncErrorCode retCode,
                      encodeFailureNumOutBytes = fromIntegral numOutBytes,
                      encodeFailureNumInSamples = fromIntegral numInSamples,
                      encodeFailureNumAncBytes = fromIntegral numAncBytes
                    }
            else do
              let !numInSamplesI = fromIntegral numInSamples
                  !consumedFrames =
                    fromIntegral numInSamplesI `div` fromIntegral channelCount
                  !numOutBytesI = fromIntegral numOutBytes

              if numOutBytesI == 0
                then
                  return $
                    Right
                      MkEncodeResultNotFinished
                        { encodeResultConsumedSamplesPerChannel = consumedFrames
                        }
                else do
                  let !leftOverInput =
                        if numInSamplesI >= V.length vec
                          then Nothing
                          else
                            let !inSliceLen = V.length vec - numInSamplesI
                                !inSlice = V.slice numInSamplesI inSliceLen vec
                             in Just inSlice
                  !outTooLarge <- V.freeze unsafeOutBuffer
                  let !samples = V.force $ V.slice 0 numOutBytesI outTooLarge
                  return $
                    Right
                      MkEncodeResult
                        { encodeResultConsumedSamplesPerChannel = consumedFrames,
                          encodeResultLeftOverInput = coerce leftOverInput,
                          encodeResultSamples = coerce samples
                        }

-- | Result of 'encode'
data EncodeResult
  = MkEncodeResultEof
  | MkEncodeResultNotFinished
      { -- | Number of samples from the input
        -- that were processed by the encoder
        encodeResultConsumedSamplesPerChannel :: !Word64
      }
  | MkEncodeResult
      { -- | Number of samples from the input
        -- that were processed by the encoder
        encodeResultConsumedSamplesPerChannel :: !Word64,
        -- | The unprocessed
        -- rest of the input.
        -- Only if the input
        -- is larger than the
        -- encoders frame
        -- length a left over
        -- is returned.
        encodeResultLeftOverInput :: !(Maybe (V.Vector Word16)),
        -- | Encoded output. If less
        -- than the frame length
        -- samples have been
        -- encoded, then the result
        -- will be 'Nothing'
        -- otherwise it is the
        -- encoded AAC content
        -- representing the encded
        -- samples delayed by
        -- 'frameLength'.
        encodeResultSamples :: !(V.Vector Word8)
      }

instance Show EncodeResult where
  showsPrec _ MkEncodeResultEof = showString "encoder-EOF"
  showsPrec
    d
    MkEncodeResult
      { encodeResultConsumedSamplesPerChannel,
        encodeResultLeftOverInput,
        encodeResultSamples
      } =
      showParen (d > 10) $
        showString "encode result: processed input samples: "
          . shows encodeResultConsumedSamplesPerChannel
          . showString ", left over input samples: "
          . maybe (showString "n/a") (shows . V.length) encodeResultLeftOverInput
          . showString ", size of compressed output: "
          . (shows . V.length) encodeResultSamples
  showsPrec
    d
    MkEncodeResultNotFinished
      { encodeResultConsumedSamplesPerChannel
      } =
      showParen (d > 10) $
        showString "encode result: processed input samples: "
          . shows encodeResultConsumedSamplesPerChannel
          . showString " no output generated"

-- | Information about encoder when an 'encode' failure occured.
data EncodeFailure = MkEncodeFailure
  { encodeFailureCode :: !AacEncErrorCode,
    encodeFailureNumOutBytes :: !Word64,
    encodeFailureNumInSamples :: !Int16,
    encodeFailureNumAncBytes :: !Word64
  }
  deriving (Eq)

instance Exception EncodeFailure

instance Show EncodeFailure where
  show MkEncodeFailure {..} =
    printf
      "(FDK AAC encode error: %s, numOutBytes:  %d, numInSamples: %d, numAncBytes: %d)"
      (show encodeFailureCode)
      encodeFailureNumOutBytes
      encodeFailureNumInSamples
      encodeFailureNumAncBytes

-- | Close an FDK-AAC encoder.
destroy :: Encoder -> IO AacEncErrorCode
destroy MkEncoder {encoderHandle} =
  toAacEncErrorCode . fromIntegral
    <$> [C.block| int {
             HANDLE_AACENCODER phAacEncoder = (HANDLE_AACENCODER) $(uintptr_t encoderHandle);
             return aacEncClose(&phAacEncoder);
          } |]

-- | Error codes from the C-library, translated to this sum type
data AacEncErrorCode
  = AacEncOk
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

-- | Create an 'AacEncErrorCode' from the @int@ returned by the C-code.
toAacEncErrorCode :: CInt -> AacEncErrorCode
toAacEncErrorCode e =
  case e of
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
