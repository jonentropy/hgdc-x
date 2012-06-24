unit JFIF ;
interface

//
// Copyright (c) 1999 Colosseum Builders, Inc.
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


//
//  JFIF Definitions
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  These definitions match version 1.02 of the
//  "JPEG File Interchange Format Specification" by Eric Hamilton
//
uses JpegPvt, SystemSpecific ;

type
  JFifHeader = Packed Record
    length : UBYTE2 ;
    identifier : Array [1..5] of Char ;
    version : Array [1..2] of UBYTE1 ;
    units : UBYTE1 ;
    xdensity : UBYTE2 ;
    ydensity : UBYTE2 ;
    xthumbnail : UBYTE1 ;
    ythumbnail : UBYTE1 ;
  // 3 * xthumbnail * ythumbnail bytes of thumbnail data follow.
    End ;


  JfifExtension = Packed Record
    length : UBYTE2 ;
    identifier : Array [0..4] of Char ;
    extension_code : UBYTE1 ;
    // Extension Data follows
    End ;

const
  JpegThumbnail = $10 ;
  OneByteThumbnail = $11 ;
  ThreeByteThumbnail = $13 ;

// Functions for YCbCr/RGB colorspace conversion
Function YCbCrToR (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;
Function YCbCrToG (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;
Function YCbCrToB (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;

Function RgbToY (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;
Function RgbToCb (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;
Function RgbToCr (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;

implementation

Function YCbCrToR (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp := Round (yy + 1.402 *  (Cr-128)) ;
  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

Function YCbCrToG (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp := Round (yy - 0.34414 * (cb-128) - 0.71414  * (cr-128)) ;

  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

Function YCbCrToB (yy, cb, cr : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp := Round (yy + 1.772 * (Cb-128)) ;
  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

Function RgbToY (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp := Round (0.299 * red  + 0.587 * green + 0.114 * blue) ;
  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

Function RgbToCb (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp := Round (- 0.1687 * red - 0.3313 * green + 0.5 * blue + 128) ;
  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

Function RgbToCr (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;
  var
    temp : LongInt ;
  Begin
  temp :=  Round (0.5 * red - 0.4187 * green - 0.0813 * blue + 128) ;
  if temp > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else if temp < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else
    Result := JPEGSAMPLE (temp) ;
  End ;

End.

