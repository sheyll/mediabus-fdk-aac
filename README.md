# Frauenhofer ISO-14496-3 AAC FDK [mediabus](https://github.com/lindenbaum/mediabus) Integration

[![Build Status](https://travis-ci.org/lindenbaum/mediabus-fdk-aac.svg?branch=master)](https://travis-ci.org/lindenbaum/mediabus-fdk-aac)
[![Hackage](https://img.shields.io/hackage/v/mediabus-fdk-aac.svg)](http://hackage.haskell.org/package/mediabus-fdk-aac)

A wrapper around the Frauenhofer Development Kit (FDK) for AAC.

Currently only encoding is supported.

This library requires that this C-library including the header files is
installed and can by queried via `pkg-config`.


## Usage Example

The easiest way to use this package is via the `Conduit` interface.
An example is found in [examples/Main.hs](http://github.com/lindenbaum/mediabus-fdk-aac/blob/master/examples/Main.hs).

Encoding via the conduit interface basically boils down to:


```haskell

     .| encodeLinearToAacC (aacEncoderConfig aacHe48KhzStereo)

```

In this specific case `encodeLinearToAacC` has the type:


```haskell

encodeLinearToAacC ::  
     AacEncoderConfig (Hz 48000) Stereo 'HighEfficiency   -- The encoder cofiguration
  -> Conduit                                              -- The resulting Conduit
          (Stream SrcId32                                      -- Input: The Stream id is a 'SrcId32'
                  SeqNum16                                     --   16 bit frame sequence numbers
                  (Ticks64 (Hz 48000))                         --   64 bit frame timestamps at 48kHz sample rate
                  ()                                           --   no stream-info for raw audio
                  (Audio (Hz 48000) Stereo (Raw S16)))         --   linear signed 16bit stereo audio data

          (ResourceT (LoggingT IO))                            -- Monad: Logging and Resource management over IO

          (Stream SrcId32                                             -- Output:
                  SeqNum16
                  (Ticks64 (Hz 48000))               
                  (AacEncoderInfo (Hz 48000) Stereo 'HighEfficiency)  -- AAC Stream Info: Framelength and Audio Specific Config
                  (Audio (Hz 48000) Stereo (Aac 'HighEfficiency)))    -- AAC-HE audio data
```
