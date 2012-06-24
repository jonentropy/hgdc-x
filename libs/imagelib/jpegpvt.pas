unit JpegPvt ;

//
// Copyright (c) 1997,1998, 2001 Colosseum Builders, Inc.
// All rights reserved.
//
// Colosseum Builders, Inc. makes no warranty, expressed or implied
// with regards to this software. It is provided as is.
//
// See the README.TXT file that came with this software for restrictions
// on the use and redistribution of this file or send E-mail to
// info@colosseumbuilders.com
//

//
//  Title:  JPEG Definitions and Utility Functions
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//

Interface

const

  JPEG8BITMIDPOINTSAMPLEVALUE = 128 ;
  JPEG12BITMIDPOINTSAMPLEVALUE = 2048 ;
  JPEGMIDPOINTSAMPLEVALUE = JPEG8BITMIDPOINTSAMPLEVALUE ;

// Table B.5
  JPEGMAXHUFFMANTABLES = 4 ;
// Table B.4
  JPEGMAXQUANTIZATIONTABLES = 4 ;

// Table B.2
  JPEGMAXCOMPONENTSPERFRAME = 255 ;
// Table B.3
  JPEGMAXCOMPONENTSPERSCAN = 4 ;


  JPEGMINSAMPLEVALUE  = 0 ;
  JPEGMAX8BITSAMPLEVALUE = 255 ;
  JPEGMAX12BITSAMPLEVALUE = 4095 ;
  JPEGMAXSAMPLEVALUE = JPEGMAX8BITSAMPLEVALUE ;

// Table B.2
type
  JPEGSAMPLINGFREQUENCY = 1..4 ;

// Table B.5
  JPEGHUFFMANTABLEID = 0..3 ;
// Table B.4
  JPEGQUANTIZATIONTABLEID = 0..3 ;

// A.1.3
const
  JPEGSAMPLEWIDTH = 8 ;
  JPEGSAMPLESIZE = JPEGSAMPLEWIDTH * JPEGSAMPLEWIDTH ;

type
  COEFFICIENTINDEX = 0..JPEGSAMPLESIZE - 1 ;
  QUANTIZATIONPRECISION = 0..1 ;

// Datatype used to represent a sample value.

type
  JPEG8BITSAMPLE = JPEGMINSAMPLEVALUE..JPEGMAX8BITSAMPLEVALUE ;
  JPEG12BITSAMPLE = JPEGMINSAMPLEVALUE..JPEGMAX12BITSAMPLEVALUE ;
  JPEGSAMPLE = JPEG8BITSAMPLE  ;

// Table B.5
const
  JPEGMAXHUFFMANCODELENGTH = 16 ;
  JPEGMAXNUMBEROF8BITHUFFMANCODES = 256 ;
  JPEGMAXNUMBEROFHUFFMANCODES = JPEGMAXNUMBEROF8BITHUFFMANCODES ;
// B.2.3
  JPEGMAXDATAUNITSPERMCU = 10 ;

type
  JPEGHUFFMANFALUE = 0..JPEGMAXNUMBEROFHUFFMANCODES-1 ;

  JPEGQUANTIZATIONVALUE = 1..255 ;
  // Section B.2.3 Table B.3
  SUCCESSIVEAPPROXIMATION = 0..13 ;
  DATAUNITINDEX = 0..JPEGSAMPLEWIDTH-1 ;

  JPEGSUCCESSIVAPPROXIMATIONVALUE = 0..13 ;


const
// A.3.6 Figure A.6
// These values are the inverse of those shown in the
// JPEG standard.
  JpegZigZagInputOrder : Array [COEFFICIENTINDEX] Of Cardinal = (
     0,  1,  8, 16,  9,  2,  3, 10,
    17, 24, 32, 25, 18, 11,  4,  5,
    12, 19, 26, 33, 40, 48, 41, 34,
    27, 20, 13,  6,  7, 14, 21, 28,
    35, 42, 49, 56, 57, 50, 43, 36,
    29, 22, 15, 23, 30, 37, 44, 51,
    58, 59, 52, 45, 38, 31, 39, 46,
    53, 60, 61, 54, 47, 55, 62, 63
    ) ;

  JpegZigZagOutputOrder : Array [COEFFICIENTINDEX] Of Cardinal = (
     0,  1,  5,  6, 14, 15, 27, 28,
     2,  4,  7, 13, 16, 26, 29, 42,
     3,  8, 12, 17, 25, 30, 41, 43,
     9, 11, 18, 24, 31, 40, 44, 53,
    10, 19, 23, 32, 39, 45, 52, 54,
    20, 21, 33, 38, 46, 51, 55, 60,
    21, 34, 37, 47, 50, 56, 59, 61,
    35, 36, 48, 49, 57, 58, 62, 63
  ) ;

  SOF0 = $C0 ;
  SOF1 = $C1 ;
  SOF2 = $C2 ;
  SOF3 = $C3 ;
  // Start of Frame Markers, Differential Huffman Coding
  SOF5 = $C5 ;
  SOF6 = $C6 ;
  SOF7 = $C7 ;
  // Start of Frame Markers, Non-Differential Arithmetic Coding
  SOF9 = $C9 ;
  SOFA = $CA ;
  SOFB = $CB ;
  // Start of Frame Markers, Differential Arithmetic Coding
  SOFD = $CD ;
  SOFE = $CE ;
  SOFF = $CF ;
  // Other Markers
  DHT = $C4 ;
  DAC = $CC ;
  RST0 = $D0 ;
  RST1 = $D1 ;
  RST2 = $D2 ;
  RST3 = $D3 ;
  RST4 = $D4 ;
  RST5 = $D5 ;
  RST6 = $D6 ;
  RST7 = $D7 ;
  SOI = $D8 ;
  EOI = $D9 ;
  SOS = $DA ;
  DQT = $DB ;
  DNL = $DC ;
  DRI = $DD ;
  DHP = $DE ;
  EXP = $DF ;
  APP0 = $E0 ;
  APP1 = $E1 ;
  APP2 = $E2 ;
  APP3 = $E3 ;
  APP4 = $E4 ;
  APP5 = $E5 ;
  APP6 = $E6 ;
  APP7 = $E7 ;
  APP8 = $E8 ;
  APP9 = $E9 ;
  APPA = $EA ;
  APPB = $EB ;
  APPC = $EC ;
  APPD = $ED ;
  APPE = $EE ;
  APPF = $EF ;
  // C8, F0-FD, 01, 02-BF reserved
  SOFLS = $F7 ;
  LSE = $F8 ;
  COM = $FE ;
  SOB = $FF ;


Function McusRequired (length : Cardinal ; maxfrequency : JPEGSAMPLINGFREQUENCY) : Cardinal ;

implementation

//
//  Description:
//
//    This function determines the number of MCUs required to span either the
//    horizontal or vertical direction of an image.
//
//  Parameters:
//
//    length : The number of pixels for either the height or width.
//    maxfrequency : The maximum sampling frequency among all components for
//                     the height or width (must refer to the same diminension
//                     as the length parameter).
//
Function McusRequired (length : Cardinal ; maxfrequency : JPEGSAMPLINGFREQUENCY) : Cardinal ;
  Var
    mcusize : Cardinal ;
  Begin
  mcusize := JPEGSAMPLEWIDTH * maxfrequency ;
  result := (length + mcusize - 1) Div mcusize ;
  End ;


End.
