unit imagetype;

interface

Uses
  bitmapimage ;

Type
  TImageType = (UnknownImage, BmpImage, JpegImage, PngImage, GifImage) ;

Function GetStreamImageType (Filename : String) : TImageType ;
Function ReadImage (Filename : String ; image : TBitmapImage ;
                    pfunction : PROGRESSFUNCTION ;
                    data : PROGRESSDATA) : TImageType ;


implementation

Uses
  Classes, sysutils, PngDecoder, JpegDecoder, BmpDecoder, GifDecoder ;

//
//  Description:
//
//    This function determines the type of image stored in a stream.
//
//  Parameters:
//    strm:  The image stream
//
//  Return Value:
//    An enumeration that identifies the stream type
//
Function GetStreamImageType (Filename : String) : TImageType ;
  Var
    strm : TFileStream ;
    buffer : Array [1..10] Of Byte ;
    count : Integer ;
    ii : Cardinal ;
  Begin
  strm := TFileStream.Create (filename, fmOpenRead Or fmShareDenyWrite) ;
  result := UnknownImage ;

  For ii := Low (Buffer) To High (buffer) Do
    buffer [ii] := 0 ;

  count := strm.read (buffer, sizeof (buffer)) ;
  If count < Sizeof (Buffer) Then
  Begin
    strm.Free;
    Exit ;
  End;

  If (Buffer [1] = 137)
      And (Buffer [2] = 80)
      And (Buffer [3] = 78)
      And (Buffer [4] = 71)
      And (Buffer [5] = 13)
      And (Buffer [6] = 10)
      And (Buffer [7] = 26)
      And (Buffer [8] = 10) Then
    Result := PngImage
  Else If (Buffer [1] = $FF)
           And (Buffer [2] = $D8)
           And (Buffer [3] = $FF)
           And (Buffer [4] = $E0)
           And (Buffer [7] = Ord ('J'))
           And (Buffer [8] = Ord ('F'))
           And (Buffer [9] = Ord ('I'))
           And (Buffer [10] = Ord ('F')) Then
    Result := JpegImage
  Else If (Buffer [1] = $FF)
           And (Buffer [2] = $D8)
           And (Buffer [3] = $FF)
           And (Buffer [4] = $E1)
           And (Buffer [7] = Ord ('E'))
           And (Buffer [8] = Ord ('x'))
           And (Buffer [9] = Ord ('i'))
           And (Buffer [10] = Ord ('f')) Then
    Result := JpegImage
  Else If (Buffer [1] = $FF)
           And (Buffer [2] = $D8) Then
    Result := JpegImage
  Else if (Buffer [1] = Ord ('G'))
          AND (Buffer [2] = Ord ('I'))
          AND (Buffer [3] = Ord ('F'))
          AND (Buffer [4] = Ord ('8'))
          AND ((Buffer [5] = Ord ('9')) OR (Buffer [5] = Ord ('7')))
          AND (Buffer [6] = Ord ('a')) Then
    Result := GifImage 
  Else if (Buffer [1] = Ord ('B')) And (Buffer [2] = Ord ('M')) Then
    Result := BmpImage ;

  strm.Free;
  End ;

Function ReadImage (Filename : String ; image : TBitmapImage ;
                    pfunction : PROGRESSFUNCTION ;
                    data : PROGRESSDATA) : TImageType ;
  var
    decoder : TBitmapImageDecoder ;
  Begin
  Result := GetStreamImageType (filename) ;
  Case Result Of
    Bmpimage:  decoder := TBmpDecoder.Create ;
    JpegImage: decoder := TJpegDecoder.Create ;
    PngImage:  decoder := TPngDecoder.Create ;
    GifImage:  decoder := TGifDecoder.Create ;
    Else Exit ;
    End ;
  Try
    decoder.ProgressFunction := pfunction ;
    decoder.ProgressData := data ;
    decoder.readImageFile (filename, image) ;
  Finally
    decoder.Destroy ;
    End ;
  End ;
End.
