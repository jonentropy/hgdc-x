unit pngpvt;
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
//  Title:  PNG Encoder/Decoder private definitions
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//

interface

const
  PNGMAX8BITSAMPLEVALUE = 255 ;

// Length of the PNG signature.
  PngSignatureSize = 8 ;
  
const
  PngSignature = Chr (137) + Chr (80) + Chr (78) + Chr (71)
               + Chr (13) + Chr (10) + Chr (26) + Chr (10) ;

Type               
  TPngColorType = (Grayscale, invxxx1, RGB,
                   Palette, GrayscaleAlpha, invxxx5,
                   RGBAlpha) ;

// Filter type defintiions
  Type TPngFilterType = (FILTERNONE, FILTERSUB, FILTERUP,
                         FILTERAVERAGE, FILTERPAETH) ;

  TPngImageHeader = Packed Record
    width, height : Cardinal ;
    bitdepth, colortype, compressionmethod,
    filtermethod, interlacemethod : Byte ;
    End ;

  // Physical Layout of a cHRM chunk
  TPngChromaticitiesData = Packed Record
    whitepointx, whitepointy,
    redx, redy,
    greenx, greeny,
    bluex, bluey : Cardinal ;
    End ;

// Physical layout of a pPYs chucnk
  TPngPixelDimensions = Packed Record
    pixelsx, pixelsy : Cardinal ;
    units : Byte ;
    End ;

// Physical layout of a tIME chunk
  TPngTimeData = Packed Record
    year : Word ;
    month, day, hour, minute, second : Byte ;
    End ;

Function  PaethPredictor (left, above, upperleft : Byte) : Cardinal ;

implementation

//
//  Description:
//    Path predictor function defined in section 6.6 of the PNG standard.
//
//  Parameters:
//    left:  The pixel value for the value to the left of the current pixel.
//    above: The value for the pixel above the current pixel.
//    upperleft: The value for the pixel diagonally above and to the right
//                of the current pixel.
//
Function  PaethPredictor (left, above, upperleft : Byte) : Cardinal ;
  Var
    pp, pa, pb, pc : Integer ;
  Begin
  pp := left + above - upperleft ;
  if (pp > left) Then
    pa := pp - left
  else
    pa := left - pp ;
  if (pp > above) Then
    pb := pp - above
  else
    pb := above - pp ;
  if (pp > upperleft) Then
    pc := pp - upperleft
  else
    pc := upperleft - pp ;

  if (pa <= pb) And (pa <= pc) Then
    Result := left
  else if (pb <= pc) Then
    Result := above
  else
    Result := upperleft ;
  End ;
End.
