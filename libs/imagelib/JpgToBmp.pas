Program JpgToBmp ;
  uses
    imagetype, bitmapimage, jpegdecoder, bmpencoder, sysutils ;
  Var
    image : TBitmapImage ;
    jpeg : TJpegDecoder ;
    bmp : TBmpEncoder ;
    input, output : String ;

  Procedure DecodeProgress (coder : TBitmapImageCoder ;
                            data : PROGRESSDATA ;
                            currentpass : Cardinal ;
                            passcount : Cardinal ;
                            description : String ;
                            progress : Cardinal ;
                            var cancel : Boolean) ;
    Begin
    Write (description, ' ', currentpass, '/', passcount, ' ', progress, '%                        ', chr (13)) ;
    End ;

  Procedure EncodeProgress (coder : TBitmapImageCoder ;
                            data : PROGRESSDATA ;
                            currentpass : Cardinal ;
                            passcount : Cardinal ;
                            description : String ;
                            progress : Cardinal ;
                            var cancel : Boolean) ;
    Begin
    Write (description, ' ', currentpass, '/', passcount, ' ', progress, '%                        ', chr (13)) ;
    End ;

  Procedure Usage ;
    Begin
    WriteLn ('Usage: ''' + ParamStr (0) + ''' input-jpg-file output-bmp-filename') ;
    End ;

  Begin
  if ParamCount <> 2 Then
    Begin
    Usage ;
    Exit ;
    End ;

  image := nil ;
  jpeg := nil ;
  bmp := nil ;

  try
    image := TBitmapImage.Create ;
    jpeg := TJpegDecoder.Create ;
    jpeg.SetProgressFunction (@DecodeProgress, Nil) ;

    input := ParamStr (1) ;
    if ExtractFileExt (input) = '' Then
      input := input + '.jpg' ;
    output := ParamStr (2) ;
    if ExtractFileExt (output) = '' Then
      output := output + '.bmp' ;

    bmp := TBmpEncoder.Create ;
    bmp.SetProgressFunction (@EncodeProgress, Nil) ;
    WriteLn ('JPEG Decoding') ;
    jpeg.ReadImageFile (input, image) ;
    WriteLn ;
    WriteLn ('BMP Encoding') ;
    bmp.WriteImageFile (output, image) ;
  finally
    image.Destroy ;
    jpeg.Destroy ;
    bmp.Destroy ;
    End ;
  End.
