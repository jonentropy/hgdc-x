unit bitmapimage ;
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
//  Title: TBitmapImage Class Definitions
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//    The TBitmapImage class is intended to be a neutral intermediate format
//    for storing decompressed images. This class can manage 1, 2, 4, 8 or
//    24-bit images. For 24-bit images the data is stored as RGB triples
//    within the main data buffer. For all other types a color map is used
//    and the image data contains indices into the color map. Sample values are
//    assumed to be in the range 0..255.
//
//  Windows Notes:
//
//    For the sake of "efficiency" this class has been optimized for use on
//    the "Windows" family. The folling oddities are a result of
//    "windowsisms":
//
//    o The data for 24-bitmaps is stored in BGR order rather than RGB.
//      To change this for your system redefine "RedOffset", "GreenOffset",
//      and "BlueOffset".
//
//    o For whatever reason, Windows expects bitmaps to be stored bottom
//      up rather than top down. The first row in the bitmap data is the
//      bottom row in the image. To change behavoir this for your system
//      redefine the implementation of the [] operator.
//
//    o Windows expects the length of all image rows to be rounded up to the
//      nearest four bytes. To change this behavior redefine the value for
//     "RowRounding".
//
//  Debugging Notes:
//
//    Two methods for accessing pixel data within the image are implemented
//    by default range checking is only performed on rows. If the
//    preprocessor symbol CHECK_RANGE is defined then range check is
//    performed on columns as well.
//
//    While the abandonment of range checking here is contrary to the
//    principles followed elsewhere, this is a place where the
//    performance benefit is worth the lack of safety.
//

Interface

Uses SysUtils ;

type
  TBitmapImage = Class ;
  TBitmapImageCoder = Class ;

  PROGRESSDATA = Pointer ;
  PROGRESSFUNCTION = Procedure (
                        code : TBitmapImageCoder ;
			                  data : PROGRESSDATA ;
			                  currentpass : Cardinal ;
			                  passcount : Cardinal ;
                                          description : string ;
			                  progress : Cardinal ;
			                  var cancel : Boolean) ;

  Pixel = Packed Record
    Blue, Green, Red, Alpha : Byte ;
    End ;

//
// Description:
//
//   Common class for all encoders and decoders.
//
  TBitmapImageCoder = Class
    protected
      progress_function : PROGRESSFUNCTION ;
      progress_data : PROGRESSDATA ;

    public
      Constructor Create ; 
      Destructor Destroy ; Override ;
      Property ProgressFunction : PROGRESSFUNCTION read progress_function write progress_function ;
      Property ProgressData : PROGRESSDATA read progress_data write progress_data ;
      Procedure SetProgressFunction (func : PROGRESSFUNCTION ; data : PROGRESSDATA) ;
      End ;

//
// Description:
//
//  Common decoder class.
//
  TBitmapImageDecoder = Class (TBitmapImageCoder)
    Public
      Procedure readImageFile (filename : String ; image : TBitmapImage) ; Virtual ; Abstract ;
      Procedure updateImage ; Virtual ;
    End ;
//
// Description:
//
//  Common encoder class.
//
  TBitmapImageEncoder = Class (TBitmapImageCoder)
    Public
      Procedure writeImageFile (filename : String ; image : TBitmapImage) ; Virtual ; Abstract ;
    End ;

//
// Description:
//
//  The TBitmapImage class represents a bitmap image. This class is
//  implemented to allow it to be displayed easily on Windoze.
//
  APixel = Array of Pixel ;

  TBitmapImage = Class
    private
      image_width : Cardinal ;
      image_height : Cardinal ;
      image_size : Cardinal ;
      image_data : APixel ;

    public
      Constructor Create ;
      Destructor Destroy ; Override ;
      Procedure setSize (width, height : Cardinal) ;
      Procedure clearImage () ;
      Procedure mergeImage (image : TBitmapImage ; xpos, ypos : Integer) ;
      Property Pixels : APixel read image_data write image_data ;
      Property Width : Cardinal read image_width ;
      Property Height : Cardinal read image_height ;
      Procedure Assign (source : TBitmapImage) ;
      End ;

  EGraphicsException = Class (Exception)
    End ;

  EGraphicsAbort = class (EAbort)
    End ;

Implementation

Constructor TBitmapImageCoder.Create ;
  Begin
  Inherited Create ;
  progress_function := Nil ;
  progress_data := Nil ;
  End ;

Destructor TBitmapImageCoder.Destroy ;
  Begin
  progress_function := Nil ;
  progress_data := Nil ;
  Inherited Destroy ;
  End ;

Procedure TBitmapImageDecoder.updateImage ;
  Begin
  End ;

Constructor TBitmapImage.Create ;
  Begin
  image_size := 0 ;
  image_width := 0 ;
  image_height := 0 ;
  Inherited Create ;
  End ;

Destructor TBitmapImage.Destroy ;
  Begin
  Inherited Destroy ;
  End ;

Procedure TBitmapImage.setSize (width, height : Cardinal) ;
  Begin
  image_width := width ;
  image_height := height ;
  image_size := image_width * image_height ;
  SetLength (image_data, image_size) ;
  End ;

Procedure TBitmapImage.clearImage () ;
  Begin
  image_width := 0 ;
  image_height := 0 ;
  image_size := 0 ;
  SetLength (image_data, 0) ;
  End ;

Procedure TBitmapImage.mergeImage (image : TBitmapImage ; xpos, ypos : Integer) ;
  Begin
  End ;

Procedure TBitmapImage.Assign (source : TBitmapImage) ;
  Var
    ii : Cardinal ;
  Begin
  image_width := Source.image_width ;
  image_height := source.image_height ;
  image_size := source.image_size ;
  SetLength (image_data, High (image_data) + 1) ;
  for ii := 0 To High (image_data) Do
    image_data [ii] := source.image_data [ii] ;
  End ;

Procedure TBitmapImageCoder.SetProgressFunction (func : PROGRESSFUNCTION ; data : PROGRESSDATA) ;
  Begin
  progress_function := func ;
  progress_data := data ;
  End ;
End.
