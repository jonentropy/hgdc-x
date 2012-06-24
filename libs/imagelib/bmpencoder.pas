unit bmpencoder;
//
//  Title: Windows BMP Encoder
//
//  Author: John M. Miano
//
//  Date: 15-May-2001
//
//  Description:
//
//    This encoder creates 16, 24 and 32 BPP Windows BMP files.
//
//    Note that many applications cannot read the 16 and 32 BPP files
//    produced by this encoder. At this time, these include Microsoft
//    Photo Editor and Windows Paint in addition to other image
//    viewing applications.
//
//    While most applications cannot view them, I have encountered
//    no problems reading them using the WIN-32 API.
//

interface

Uses
  classes, bitmapimage ;

Type
  TBmpEncoder = Class (TBitmapImageEncoder)
    Private
      create_alpha_channel : Boolean ;
      bits_per_pixel : Cardinal ;
      Procedure callProgressFunction (percent : Cardinal) ;
      Procedure setBitsPerPixel (bpp : Cardinal) ;

    Public
      Constructor Create ;
      Procedure writeImage (strm : TStream ; image : TBitmapImage) ;
      Procedure writeImageFile (filename : String ; image : TBitmapImage) ; Override ;

      Property CreateAlphaChannel : Boolean
               read create_alpha_channel
               write create_alpha_channel ;
      Property BitsPerPixel : Cardinal
               read bits_per_pixel
               write setBitsPerPixel ;
    End ;

  EBmpError = Class (EGraphicsException) ;

implementation

uses
  sysutils, bmppvt ;

Const
  CHAR_BIT = 8 ;
  SIGNATURE = Ord ('B') Or (Ord ('M') Shl CHAR_BIT) ;

Constructor TBmpEncoder.Create ;
  Begin
  Inherited Create ;
  bits_per_pixel := 24 ;
  create_alpha_channel := true ;
  End ;


//
//  Description:
//
//    This function writes an image to a BMP stream.
//
//  Parameters:
//    strm:  The output stream
//    image:  The image to output
//
Procedure TBmpEncoder.writeImage (strm : TStream ; image : TBitmapImage) ;
  Var
    fileheader : BITMAPFILEHEADER ;
    infoheader : BITMAPV4HEADER ;
    rowwidth, datasize, spacerequired : Cardinal ;

  Procedure Write16BppData ;
    Var
      rowbuffer : Array Of Word ;
      II, JJ : Cardinal ;
      offset : Cardinal ;
    Begin
    SetLength (rowbuffer, rowwidth Div 2) ;
    Offset := 0 ;
    for II := 1 To image.Height Do
      Begin
      For JJ := 0 To image.Width - 1 Do
        Begin
        rowbuffer [jj] := ((image.Pixels [offset].blue  Shr 3) And   $1F)
                       Or ((image.Pixels [offset].green Shl 2) And  $3E0)
                       Or ((image.Pixels [offset].Red   Shl 7) And $7C00) ;
        Inc (offset) ;
        End ;
        strm.write (rowbuffer [0], rowwidth) ;
        callProgressFunction (ii * 100 Div image.Height) ;
      End ;
    End ;

  Procedure Write24BppData ;
    Var
      rowbuffer : Array of RGBTRIPLE ; // One Extra
      II, JJ : Integer ;
      Offset : Cardinal ;
    Begin
    // The one extra is to account for padding to round a multiple of
    // four bytes.
    SetLength (rowbuffer, image.Width + 1) ;
    // We store 24 BPP images from bottom up in order to
    // be compatibe with the LCD in viewers.
    For II := image.Height - 1 DownTo 0 Do
      Begin
      Offset := ii * image.Width ;
      For JJ := 0 To image.Width - 1 Do
        Begin
        Rowbuffer [jj].rgbtRed   := image.Pixels [offset].red ;
        Rowbuffer [jj].rgbtGreen := image.Pixels [offset].Green ;
        Rowbuffer [jj].rgbtBlue  := image.Pixels [offset].Blue ;
        Inc (offset) ;
        End ;
      strm.Write (rowbuffer [0], rowwidth) ;
      callProgressFunction (ii * 100 Div image.Height) ;
      End ;
    End ;
  Begin
  if (image.Width = 0) Or (image.Height = 0) Then
    Raise EBmpError.Create ('Empty Image') ;


  fileheader.bfType      := SIGNATURE ;
  fileheader.bfReserved1 := 0 ;
  fileheader.bfReserved2 := 0 ;

  infoheader.bV4Size          := sizeof (BITMAPV4HEADER) ;
  infoheader.bV4Width         := Image.Width ;
  infoheader.bV4Planes        := 1 ;
  infoheader.bV4BitCount      := bits_per_pixel ;
  infoheader.bV4XPelsPerMeter := 0 ;
  infoheader.bV4YPelsPerMeter := 0 ;
  infoheader.bV4ClrUsed       := 0 ;
  infoheader.bV4ClrImportant  := 0 ;
  if create_alpha_channel Then
    infoheader.bV4AlphaMask := $FF000000
  Else
    infoheader.bV4AlphaMask := 0 ;
  infoheader.bV4CSType := 0 ;
  infoheader.bV4Endpoints.ciexyzRed.ciexyzX := 0 ;
  infoheader.bV4Endpoints.ciexyzRed.ciexyzY := 0 ;
  infoheader.bV4Endpoints.ciexyzRed.ciexyzZ := 0 ;
  infoheader.bV4Endpoints.ciexyzGreen.ciexyzX := 0 ;
  infoheader.bV4Endpoints.ciexyzGreen.ciexyzY := 0 ;
  infoheader.bV4Endpoints.ciexyzGreen.ciexyzZ := 0 ;
  infoheader.bV4Endpoints.ciexyzBlue.ciexyzX := 0 ;
  infoheader.bV4Endpoints.ciexyzBlue.ciexyzY := 0 ;
  infoheader.bV4Endpoints.ciexyzBlue.ciexyzZ := 0 ;
  infoheader.bV4GammaRed   := 0 ;
  infoheader.bV4GammaGreen := 0 ;
  infoheader.bV4GammaBlue  := 0 ;

  // Row width rounded up to 4 bytes.
  rowwidth := (image.Width * (bits_per_pixel Div CHAR_BIT) + $3) And  Not $3 ;
  // Calulate the space required for the image.
  datasize := image.Height * rowwidth ;
  infoheader.bV4SizeImage := datasize ;

  Case bits_per_pixel Of
    16:
      Begin
      infoheader.bV4Size          := sizeof (BITMAPV4HEADER) ;
      infoheader.bV4Height        := -Image.Height ;
      infoheader.bV4RedMask       := $7C00 ;
      infoheader.bV4GreenMask     := $3E0 ;
      infoheader.bV4BlueMask      := $1F ;
      infoheader.bV4AlphaMask     := 0 ;
      infoheader.bV4V4Compression := BI_BITFIELDS ;
      End ;

    24:
      Begin
      infoheader.bV4Size          := sizeof (BITMAPINFOHEADER) ;
      infoheader.bV4Height        := Image.Height ;
      infoheader.bV4V4Compression := BI_RGB ;
      End ;

    32:
      Begin
      infoheader.bV4Size := sizeof (BITMAPV4HEADER) ;
      infoheader.bV4Height := -Image.Height ;
      infoheader.bV4RedMask := $FF0000 ;
      infoheader.bV4GreenMask := $FF00 ;
      infoheader.bV4BlueMask := $FF ;
      if create_alpha_channel Then
        infoheader.bV4AlphaMask := $FF000000
      Else
        infoheader.bV4AlphaMask := 0 ;
      infoheader.bV4V4Compression := BI_BITFIELDS ;
      End ;
    End ;

  spacerequired := sizeof (BITMAPFILEHEADER)
                + datasize
                + infoheader.bV4Size ;

  // Fill in the remaining header fields.
  fileheader.bfOffBits := sizeof (BITMAPFILEHEADER)
                       + infoheader.bV4Size ;
  fileheader.bfSize := spacerequired ;

  strm.write (fileheader, sizeof (BITMAPFILEHEADER)) ;
  strm.write (infoheader, infoheader.bV4Size) ;

  callProgressFunction (0) ;

  Case bits_per_pixel Of
    16:
      Write16BppData ;

    24:
      Write24BppData ;
    32:
      strm.write (image.Pixels [0], sizeof (image.Pixels [0]) * image.Height * image.Width) ;
    End ;
  callProgressFunction (100) ;
  End ;

Procedure TBmpEncoder.callProgressFunction (percent : Cardinal) ;
  Var
    abort : Boolean ;
  Begin
  If Assigned (progress_function) Then
    Begin
    abort := false ;
    progress_function (Self,
                       progress_data,
                       1,
                       1,
                       'BMP Encode',
                       percent,
                       abort) ;
    if (abort) Then
      Raise EGraphicsAbort.Create ('');
    End ;

  End ;

Procedure TBmpEncoder.setBitsPerPixel (bpp : Cardinal) ;
  Begin
  Case bpp Of
    16,24, 32: bits_per_pixel := bpp ;
    Else
      Raise EBmpError.Create ('Invalid Bit Depth') ;
    End ;
  End ;

Procedure TBmpEncoder.writeImageFile (filename : String ; image : TBitmapImage) ;
  Var
    strm : TFileStream ;
  Begin
  strm := TFileStream.Create (filename, fmCreate Or fmShareExclusive) ;
  try
    writeImage (strm, image) ;
  Finally
    strm.Destroy ;
    End ;
  End ;


End.
