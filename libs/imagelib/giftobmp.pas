//
//  GIF to BMP Converter
//
//   Copyright (c) 2005 Colosseum Builders, Inc.
//   www.colosseumbuilders.com
//
//  Author: John M. Miano
//  Date:    June 28, A.D. 2005
//
//  Usage:
//
//     giftobmp [flags] input output
//
//    input => Input file name, defaults to .GIF extention if none specified.
//    output => Output file name, defaults to .BMP extension if none specified
//
//    flags
//      -v  Verbose, prints file structure information
//      -m Multi-image mode - Reads all images within the GIF file. It creates
//           files output.bmp, output_1.bmp, output_2.bmp ... output_n.bmp
//
//
Program GifToBas ;
  {%File 'giftobmp.bdsproj'}

uses
  bitmapimage,
  gifdecoder,
  bmpencoder,
  sysutils;

Var
    image : TBitmapImage ;
    bmp : TBmpEncoder ;
    gif: TGifDecoder ;
    ii : Cardinal ;
    input, output : string ;
    verbose : boolean ;
    basename : String ;
    filecount : Cardinal ;
    multiimagemode : Boolean ;

  Procedure Usage ;
    Begin
    WriteLn ('Usage: ''' + ExtractFileName (ParamStr (0)) + ''' [-flags-] input-png-file output-jpg-filename') ;
    WriteLn ('    -v     Verbose') ;
    WriteLn ('    -m     Multi-Image Mode') ;
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
  verbose := false ;
  multiimagemode := false ;

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
      'm': multiimagemode := true ;
      Else
        Begin
        Usage ;
        Exit ;
        end ;
      End ;
    Inc (ii) ;
    End ;
  input := ParamStr (ii) ; Inc (ii) ;
  output := ParamStr (ii) ;

  if ExtractFileExt (input) = '' then
    input := input + '.gif' ;

  basename := ExtractFileName (output) ;
  if ExtractFileExt (output) = '' then
    output := output + '.bmp' ;

  image := nil ;
  bmp := nil ;
  gif := nil ;

  try
    image := TBitmapImage.Create ;
    bmp := TBmpEncoder.Create ;
    gif := TGifDecoder.Create ;
    gif.Verbose := verbose ;
    gif.MultiImageMode := multiimagemode ;
    if Not verbose then
      Begin
      bmp.SetProgressFunction (@EncodeProgress, Nil) ;
      gif.SetProgressFunction (@DecodeProgress, Nil) ;
      End ;
    WriteLn ('GIF Decode') ;
    gif.ReadImageFile (input, image) ;
    WriteLn ;
    WriteLn ('BMP Encode') ;
    bmp.WriteImageFile (output, image) ;

    // Possibly read the remaining images in the file.
    filecount := 0 ;
    While gif.moreImages And MultiImageMode Do
      Begin
      Inc (filecount) ;
      output := basename + '_' + IntToStr (filecount) + '.bmp' ;
      WriteLn ('GIF Decode') ;
      gif.readNextImage (image) ;
      WriteLn ('BMP Encode') ;
      bmp.WriteImageFile (output, image) ;
      End ;
  finally
    image.Destroy ;
    bmp.Destroy ;
    gif.Destroy ;
    End ;
  End.
