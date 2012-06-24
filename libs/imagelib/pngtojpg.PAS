Program PngToJpg ;
  uses
    imagetype, bitmapimage, pngdecoder, jpegencoder, sysutils ;
  Var
    image : TBitmapImage ;
    jpeg : TJpegEncoder ;
    png : TPngDecoder ;
    ii : Cardinal ;
    input, output : string ;
    verbose : boolean ;
    progressive : boolean ;
    quality : integer ;
    grayscale : boolean ;

  Procedure Usage ;
    Begin
    WriteLn ('Usage: ''' + ExtractFileName (ParamStr (0)) + ''' [-flags-] input-png-file output-jpg-filename') ;
    WriteLn ('    -v     Verbose') ;
    WriteLn ('    -p     Progressive JPEG') ;
    WriteLn ('    -qN    JPEG Quality') ;
    WriteLn ('    -g     Grayscale Output') ;
    End ;

  Procedure DecodeProgress (coder : TBitmapImageCoder ;
                            data : PROGRESSDATA ;
                            currentpass : Cardinal ;
                            passcount : Cardinal ;
                            description : String ;
                            progress : Cardinal ;
                            var cancel : Boolean) ;
    Begin
    Write (description, ' ', currentpass, '/', passcount, ' ', progress, '%                            ', chr (13)) ;
    End ;

  Procedure EncodeProgress (coder : TBitmapImageCoder ;
                            data : PROGRESSDATA ;
                            currentpass : Cardinal ;
                            passcount : Cardinal ;
                            description : String ;
                            progress : Cardinal ;
                            var cancel : Boolean) ;
    Begin
    Write (description, ' ', currentpass, '/', passcount, ' ', progress, '%                     ', chr (13)) ;
    End ;

  Begin
  grayscale := false ;
  verbose := false ;
  progressive := false ;
  quality := 3 ;

  if ParamCount < 2 Then
    Begin
    Usage ;
    Exit ;
    End ;

  ii := 1 ;
  while ii <= ParamCount - 2 do
    Begin
    if ParamStr (ii)[1] <> '-' then
      Begin
      Usage ;
      Exit ;
      End ;
    Case ParamStr (II)[2] Of
    'v': verbose := true ;
    'p': progressive := true ;
    'g': grayscale := true ;
    'q':
        Begin
        If (Length (ParamStr (ii)) <> 3) Or (ParamStr (ii)[3] < '0') or (ParamStr (ii)[3] > '9') Then
          Begin
          Usage ;
          Exit ;
          End
        Else
          quality := Ord (ParamStr (ii)[3])  - Ord ('0') ;
        End ;
      End ;
    Inc (ii) ;
    End ;
  input := ParamStr (ii) ; Inc (ii) ;
  output := ParamStr (ii) ;

  if ExtractFileExt (input) = '' then
    input := input + '.png' ;

  if ExtractFileExt (output) = '' then
    output := output + '.jpg' ;

  image := nil ;
  jpeg := nil ;
  png := nil ;

  try
    image := TBitmapImage.Create ;
    jpeg := TJpegEncoder.Create ;
    jpeg.Quality := quality ;
    jpeg.setVerbose (verbose) ;
    jpeg.Progressive := progressive ;
    png := TPngDecoder.Create ;
    png.Verbose := verbose ;
    jpeg.SetProgressFunction (@EncodeProgress, Nil) ;
    png.SetProgressFunction (@DecodeProgress, Nil) ;
    WriteLn ('PNG Decode') ;
    png.ReadImageFile (input, image) ;
    WriteLn ;
    WriteLn ('JPEG Encode') ;
    jpeg.WriteImageFile (output, image) ;

  finally
    image.Destroy ;
    jpeg.Destroy ;
    png.Destroy ;
    End ;
  End.
