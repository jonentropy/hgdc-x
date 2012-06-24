unit pngdecoder;
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
//  Title:  PNG Decoder class definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class is a decoder for PNG images
//

interface

Uses
  pnginputstream, bitmapimage, inflatedecoder, pngpvt ;

Type
  ColorPalette = Record
    Red, Green, Blue : Byte ;
    End ;

  TPngDecoder = Class (TBitmapImageDecoder)
    private
      verbose_flag : Boolean ;
      reading_pixel_data : Boolean ;  // Set to true while reading IDAT blocks

      // Image information from the header
      image_height : Cardinal ;
      image_width : Cardinal ;
      image_depth : Cardinal ;  // Bits per component vlaue
      image_color_type : TPngColorType ;
      image_compression_method : Cardinal ;
      image_filter_method : Cardinal ;
      image_interlace_method : Cardinal ;

      // Row input buffers
      row_buffers : Array [0..1] Of Array of Byte ;
      current_row_buffer : Cardinal ;
      row_buffer_width : Cardinal ;

      interlace_pass : Cardinal ;   // Current interlace pass
      file_gamma : Double ;             // Value from a gAMAchunk

      // Values used to parse the chunk ordering.
      data_read : Boolean ; // False until IDAT chunk read.
      palette_read : Boolean ; // False until PLTE chunk read.
      header_read : Boolean ; // False until IHDR chunk read.
      end_read : Boolean ; // False until IEND chunk read.

      inflate_decoder : TInflateDecoder ;

      background_gray : Cardinal ;
      background_red : Cardinal ;
      background_green : Cardinal ;
      background_blue : Cardinal ;

      palette_size : Cardinal ;
      color_palette : Array [Low (Byte)..High (Byte)] of ColorPalette ;

      previous_chunk_type : String ;

      // Function to read the PNG signature.
      Procedure readSignature (inputstream : TPngInputStream) ;

      // Functions for processing chunks
      Procedure processChunk (inputstream : TPngInputStream ; image : TBitmapImage) ;
      Procedure processHeader (inputstream : TPngInputStream ; image : TBitmapImage) ;
      Procedure processPalette (inputstream : TPngInputStream) ;
      Procedure processBackground (inputstream : TPngInputStream) ;
      Procedure processGamma (inputstream : TPngInputStream) ;
      Procedure processChromaticities (inputstream : TPngInputStream) ;
      Procedure processHistogram (inputstream : TPngInputStream) ;
      Procedure processPhysicalPixelDimensions (inputstream : TPngInputStream) ;
      Procedure processSignificantBits (inputstream : TPngInputStream) ;
      Procedure processTextualData (inputstream : TPngInputStream) ;
      Procedure processImageTime (inputstream : TPngInputStream) ;
      Procedure processTransparency (inputstream : TPngInputStream) ;
      Procedure processCompressedText (inputstream : TPngInputStream) ;

      Procedure callProgressFunction (description : String ; percent : Cardinal) ;

      // Function for reading pixel data for the image.
      Procedure readPixelData (inputstream : TPngInputStream ; image : TBitmapImage) ;
      Procedure copyNoninterlacedRowToImage (row : Cardinal ; Image : TBitmapImage);
      Procedure copyInterlacedRowToImage (row, rowwidth : Cardinal ; image : TBitmapImage) ;
      Procedure readNoninterlaced (inputstream : TPngInputStream ; image  : TBitmapImage) ;
      Procedure readInterlaced (inputstream : TPngInputStream ; image : TBitmapImage) ;
      Procedure filterRow (filter : TPngFilterType) ;
    public
      Constructor Create ;
      Destructor Destroy ; Override ;
      Procedure readImage (inputstream : TPngInputStream ; image : TBitmapImage) ; 
      Procedure readImageFile (filename : String ; image : TBitmapImage) ; Override ;

      Property Verbose : Boolean read verbose_flag write verbose_flag ;
    End ;

    EPngError = Class (EGraphicsException) ;
    EBadPngStream = Class (EGraphicsException) ;

implementation

Uses adler32, systemspecific, pnginputfilestream ;

Type
  InterlaceInfo = Record
    row_interval,
    col_interval,
    start_row,
    start_col : Cardinal ;
    End ;

Const
  CHAR_BIT = 8 ;
  InterlacePasses = 7 ;
  // This matrix defines the interlace pattern. Example: On the
  // 3rd pass, we place the first pixel from the pass in row 4 (zero based),
  // column 0. From there we place a news pixel every 4 columns and every
  // 8 rows.
  interlace_values : Array [1..InterlacePasses] Of InterlaceInfo =
   (
    (row_interval : 8 ; col_interval : 8 ; start_row : 0 ; start_col : 0),
    (row_interval : 8 ; col_interval : 8 ; start_row : 0 ; start_col : 4),
    (row_interval : 8 ; col_interval : 4 ; start_row : 4 ; start_col : 0),
    (row_interval : 4 ; col_interval : 4 ; start_row : 0 ; start_col : 2),
    (row_interval : 4 ; col_interval : 2 ; start_row : 2 ; start_col : 0),
    (row_interval : 2 ; col_interval : 2 ; start_row : 0 ; start_col : 1),
    (row_interval : 2 ; col_interval : 1 ; start_row : 1 ; start_col : 0)
   ) ;

//
// Class Constructor
//
// Nothing gets done here except initializing variables to a known state.
//
Constructor TPngDecoder.Create ;
  Begin
  Inherited Create ;
  verbose_flag := false ;
  inflate_decoder := TInflateDecoder.Create ;
  End ;


Destructor TPngDecoder.Destroy ;
  Begin
  inflate_decoder.Destroy ;
  Inherited Destroy ;
  End ;

//
//  Description:
//
//    This function reads a PNG image.
//
//  Parameters:
//
//    strm:  The input stream
//    image:  The output image
//
Procedure TPngDecoder.readImage (inputstream : TPngInputStream ; image : TBitmapImage) ;
  Begin
  reading_pixel_data := false ;
  data_read := false ;
  header_read := false ;
  end_read := false ;
  palette_read := false ;

  palette_size := 0 ;

  // At the start of the image we should find
  // 1. PNG Signature
  // 2. IHDR Chunk
  // After that almost anything goes.

  readSignature (inputstream) ;

  inputstream.getNextChunk ;
  if inputstream.ChunkType <> 'IHDR' Then
      Raise EBadPngStream.Create ('Missing IHDR Chunk') ;
  processChunk (inputstream, image) ;

  // Now chunks can come in almost any order.
  while (Not end_read) Do
    Begin
    inputstream.getNextChunk ;
    processChunk (inputstream, image) ;
    End ;

  if Not data_read Then
    Raise EBadPngStream.Create ('Image Contains no Pixel Data') ;
  if Not end_read Then
    Raise EBadPngStream.Create ('Image Contains no IDAT block') ;
  End ;

//
//  Description:
//
//    This function reads the PNG signature from the input stream. It
//    throws an exception if the stream does not have a valid signature.
//
//  Parameters:
//
//    inputstream : The stream to read the signature from.
//
Procedure TPngDecoder.readSignature (inputstream : TPngInputStream) ;
  Var
    sigbuf : String ;
  Begin
  inputstream.readRaw (PngSignatureSize, sigbuf) ;
  If sigbuf <> PngSignature Then
    Raise EBadPngStream.Create ('Not a PNG file') ;
  End ;

//
//  Description:
//
//    This function reads a complete chunk from the input stream. A chunk
//    consists of:
//
//      Chunk Length:  4 Bytes
//      Chunk Type:    4 Bytes
//      Chunk Data:    "Chunk Length" Bytes
//      Chunk CRC:     4 Bytes
//
//    The chunk CRC value is calculated from the type and data fields.
//
//  Parameters:
//
//    inputstream : The stream to read the image from
//    image : The output image object
//
Procedure TPngDecoder.processChunk (inputstream : TPngInputStream ; image : TBitmapImage) ;
  Var
    chunktype : String ;
  Begin
  if verbose_flag Then
    Begin
    WriteLn ('{') ;
    WriteLn (' Block Type: ', inputstream.ChunkType) ;
    WriteLn (' Length: ',  inputstream.ChunkDataLength + 4) ;
    End ;

  chunktype := inputstream.ChunkType ;
  if chunktype = 'IHDR' Then processHeader (inputstream, image)
  else if chunktype = 'IDAT' Then readPixelData (inputstream, image)
  else if chunktype = 'PLTE' Then processPalette (inputstream)
  else if chunktype = 'IEND' Then end_read := true
  else if chunktype = 'bKGD' Then processBackground (inputstream)
  else if chunktype = 'gAMA' Then processGamma (inputstream)
  else if chunktype = 'cHRM' Then processChromaticities (inputstream)
  else if chunktype = 'hIST' Then processHistogram (inputstream)
  else if chunktype = 'sBIT' Then processSignificantBits (inputstream)
  else if chunktype = 'tRNS' Then processTransparency (inputstream)
  else if chunktype = 'pHYs' Then processPhysicalPixelDimensions (inputstream)
  else if chunktype = 'tIME' Then processImageTime (inputstream)
  else if chunktype = 'tEXt' Then processTextualData (inputstream)
  else if chunktype = 'zEXt' Then processCompressedText (inputstream)
  else if (Ord (chunktype [1])  And (1 Shl 5)) = 0 Then
    Raise EBadPngStream.Create ('Unknown critical chunk') ;

  previous_chunk_type := chunktype ;

  if (verbose_flag) Then
    WriteLn ('}') ;
  End ;

//
// Description:
//
//   This function processes an IHDR chuck. This chunk contains the image
//   header which defines the dimensions and color type.
//
//   The main functions here are to allocate space in the image object and
//   to create the color table for grayscale images.
//
//  Parameters:
//
//    inputstream : The input stream to read the header from
//    image : The image to output
//
Procedure TPngDecoder.processHeader (inputstream : TPngInputStream ; image : TBitmapImage) ;
  Var
    header : ^TPngImageHeader ;
    ii : cardinal ;
    maxindex : Cardinal ;
  Begin
  If (header_read) Then
    Raise EBadPngStream.Create ('Duplicate IHDR Block') ;
  header_read := true ;

  if inputstream.ChunkDataLength <> sizeof (TPngImageHeader) Then
    Raise EBadPngStream.Create ('Invalid Header Length') ;

  // Save the data from the image header.
  header := inputstream.ChunkData ;
  image_height := BigEndianLongToSystem (header^.height) ;
  image_width := BigEndianLongToSystem (header^.width) ;
  image_depth := header^.bitdepth ;
  image_color_type := TPngColorType (header^.colortype) ;
  image_compression_method := header^.compressionmethod ;
  image_filter_method := header^.filtermethod ;
  image_interlace_method := header^.interlacemethod ;

  if (verbose_flag) Then
    Begin
    WriteLn (' Image Dimensions: ', image_height, ' x ', image_width) ;
    WriteLn (' Bit Depth: ', image_depth) ;
    Write (' Color Type: (', Ord (image_color_type), ') ') ;
    Case image_color_type Of
      Grayscale:       Writeln ('grayscale') ;
      RGB:             Writeln ('RGB') ;
      Palette:         Writeln ('palette') ;
      GrayscaleAlpha:  Writeln ('grayscale/alpha') ;
      RGBAlpha:        Writeln ('RGB/alpha') ;
      Else             Writeln ('invalid') ;
      End ;
    WriteLn (' Compression Method: ',image_compression_method) ;
    WriteLn (' Filter Method: ',image_filter_method) ;
    Write (' Interlace Method: (', image_interlace_method, ') ') ;
    Case image_interlace_method Of
      0:  WriteLn ('none') ;
      1:  WriteLn ('Adam7') ;
      else WriteLn ('unknown') ;
      End ;
    End ;

  // Ensure the that the values in the image header are valid.
  if (image_compression_method <> 0) Then
    Raise EBadPngStream.Create ('Invalid Compression Method') ;

  if (image_filter_method <> 0) Then
    Raise EBadPngStream.Create ('Invalid Filter Method') ;

  if (image_interlace_method <> 0) And (image_interlace_method <> 1) Then
    Raise EBadPngStream.Create ('Invalid Interlace Method') ;
  Case image_depth Of
    1, 2, 4, 8, 16: ;
    Else Raise EBadPngStream.Create ('Invalid Bit Depth') ;
    End ;
    
  // Ensure that the color type and bit depth values are consistent.
  Case image_color_type Of
    Grayscale: ; // All bit depths are legal for grayscale.
    RGB, RGBAlpha, GrayscaleAlpha:
      if (image_depth <> 8) And (image_depth <> 16) Then
        Raise EBadPngStream.Create ('Invalid Bit Depth for Color Type') ;
    Palette:
      if (image_depth = 16) Then
        Raise EBadPngStream.Create ('Invalid Bit Depth for Color Type') ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;

  // For grayscale images of less than 16 bpp we create a fixed palette.
  if (image_color_type = Grayscale)
      Or (image_color_type = GrayscaleAlpha) Then
    Begin
    If image_depth < 8 Then
      maxindex := (1 Shl image_depth) - 1
    Else
      maxindex := $FF ;
    For II := low (image_depth) To maxindex Do
      Begin
      color_palette [ii].red :=  ii * $FF Div maxindex ;
      color_palette [ii].green := ii * $FF Div maxindex  ;
      color_palette [ii].blue := ii * $FF Div maxindex  ;
      End ;
    End ;

  // Allocate space space in the image object.
  image.setSize (image_width, image_height) ;
  End ;

//
//  Description:
//
//    This function processes the data in a PLTE block. A PLTE block
//    defines the color palette for the image.
//
//  Parameters:
//
//    inputstream : The stream to read the palette from
//    image : The image to output
//
Procedure TPngDecoder.processPalette (inputstream : TPngInputStream) ;
  Var
    chunklength : Cardinal ;
    chunkdata : PChar ;
    ii : Cardinal ;
  Begin
  // There can only be one palette for the image.
  if palette_read Then
    Raise EPngError.Create ('Duplicate PLTE block') ;
  palette_read := true ;

  // Grayscale images are not allowed to have an palette.
  if (image_color_type = Grayscale) Or (image_color_type = GrayscaleAlpha) Then
    Raise EPngError.Create ('Grayscale image contains a PLTE block') ;

  // The palette size must be divisable by 3.
  chunklength := inputstream.ChunkDataLength ;
  if (chunklength  Mod 3) <> 0 Then
    Raise EBadPngStream.Create ('PLTE chunk length not divisible by 3') ;
  if chunklength > (3 * 256) Then
    Raise EBadPngStream.Create ('PLTE chunk length too large') ;

  palette_size := chunklength Div 3 ;

  // PLTE chunks are permitted with RGB images to suggest colors
  // for quantization.  Our implementation does not do anything
  // with this information.
  if (image_color_type = RGB) Or (image_color_type = RGBAlpha) Then
    Exit ;

  // Store the palette in the image.
  chunkdata := inputstream.ChunkData ;
  For II := 0 To palette_size - 1 Do
    Begin
    color_palette [ii].red := Ord (chunkdata [3 * ii]) ;
    color_palette [ii].green := Ord (chunkdata [3 * ii + 1]) ;
    color_palette [ii].blue := Ord (chunkdata [3 * ii + 2]) ;
    End ;
  End ;


//
//  Description:
//
//    This function processes an IDAT data stream. It decompresses the
//    pixel data and stores it in the image.
//
//    The Deflate compression system supports many options that are not
//    permitted in PNG. This is the reason for checking for specific
//    parameter values.
//
//  Parameters:
//
//    inputstream : The read to read the pixel data form
//    image : The output image
//
Procedure TPngDecoder.readPixelData (inputstream : TPngInputStream ; image : TBitmapImage) ;
  var
    rowbits : Cardinal ;
    tmp : Char ;
  Begin
  // Ensure that we only have one IDAT stream in an image stream.
  if data_read Then
    Begin
    If previous_chunk_type = 'IDAT' Then
      Exit ;
    Raise EBadPngStream.Create ('Image Contains Multiple Data Segments') ;
    End ;

  data_read := true ;

  // If the image requires a palette then it must have been read by
  // the time we get here.
  if (image_color_type = Palette) And Not palette_read Then
    Raise EBadPngStream.Create ('PLTE block must occur before IDAT') ;

  // This block determines the number of bytes needed to store each
  // data row of the image. Then allocate two buffers to hold row
  // data.  We need to row buffers to support filtering.
  Case image_color_type Of
    Grayscale, Palette:
      Begin
      rowbits := image_width * image_depth ;
      row_buffer_width := (rowbits + CHAR_BIT - 1) Div CHAR_BIT ;
      End ;
    GrayscaleAlpha:
      row_buffer_width := 2 * image_width * image_depth Div CHAR_BIT ;
    RGB:
      row_buffer_width := image_width * 3 * image_depth Div CHAR_BIT ;
    RGBAlpha:
      row_buffer_width := image_width * 4 * image_depth Div CHAR_BIT ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;
  SetLength (row_buffers [0], row_buffer_width) ;
  SetLength (row_buffers [1], row_buffer_width) ;

  inflate_decoder.Verbose := Verbose ;
  inflate_decoder.openStream (inputstream) ;

  // Read the image data.
  If (image_interlace_method = 0) Then
    readNoninterlaced (inputstream, image)
  Else
    Begin
    interlace_pass := 1 ;
    While interlace_pass <= InterlacePasses Do
      Begin
      readInterlaced (inputstream, image) ;
      Inc (interlace_pass) ;
      End ;
    End ;

  If (Not inflate_decoder.NoMoreData) And (inflate_decoder.decode (inputstream, sizeof (tmp), @tmp) <> 0) Then
    Raise EBadPngStream.Create ('Extra bytes in compressed data') ;

  reading_pixel_data := false ;
  End ;

//
//  Description:
//
//    This function filters a row of data.
//
//  Parameters:
//
//    filter:  The type of filter
//
Procedure TPngDecoder.filterRow (filter : TPngFilterType) ;
  Var
    lastrow : Cardinal ;
    offset : Cardinal ;                         // Pixel width

  Procedure DoFilterSub ;
    Var
      Col : Cardinal ;
    Begin
    // The value is the difference from the value to the left.
    for col := offset To row_buffer_width - 1 Do
      Begin
      row_buffers [current_row_buffer][col] :=
           (row_buffers [current_row_buffer][col] +
            row_buffers [current_row_buffer][col-offset]) And $FF ;
      End ;
    End ;
  Procedure DoFilterUp ;
    Var
      Col : Cardinal ;
    Begin
    // The value is the difference from the value in the previous row.
    for col := 0 To row_buffer_width - 1 Do
      Begin
      row_buffers [current_row_buffer][col] :=
           (row_buffers [current_row_buffer][col]
            + row_buffers [lastrow][col]) And $FF ;
      End
    End ;
  Procedure DoFilterAverage ;
    Var
      col : Cardinal ;
      left, above : Integer ;
    Begin
    for col := 0 To row_buffer_width - 1 Do
        Begin
        above := row_buffers [lastrow][col] ;
        if (col < offset) Then
          left := 0
        else
          left := row_buffers [current_row_buffer][col-offset] ;

        row_buffers [current_row_buffer][col] :=
                    (row_buffers [current_row_buffer][col]
             + (left + above) Div 2) And $FF ;
        End ;
    End ;
  Procedure DoFilterPaeth ;
    Var
      left, above, aboveleft : Byte ;
      vv, pp : Integer ;
      col : Cardinal ;
    Begin
    for col := 0 To row_buffer_width - 1 Do
      Begin
      above := row_buffers [lastrow][col] ;
      if (col < offset) Then
        Begin
        left := 0 ;
        aboveleft := 0 ;
        End
      else
        Begin
        left := row_buffers [current_row_buffer][col-offset] ;
        aboveleft := row_buffers [lastrow][col-offset] ;
        End ;
      vv := row_buffers [current_row_buffer][col] ;
      pp := PaethPredictor (left, above, aboveleft) ;
      row_buffers [current_row_buffer][col] := (pp + vv) And $FF ;
      End ;
    End ;
  Begin
  lastrow := (current_row_buffer + 1) Mod 2 ; // Index of the previous row

  // Filtering is done on corresponding items within a record. Determine
  // the number of bytes between corresponding items.
  Case image_color_type Of
    Grayscale:          If image_depth = 16 Then
                          Offset := 2
                        Else
                          Offset := 1 ;
    Palette:            offset := 1 ;
    RGB:                offset := 3 * image_depth Div CHAR_BIT ;
    GrayscaleAlpha:     offset := 2 * image_depth Div CHAR_BIT ;
    RGBAlpha:           offset := 4 * image_depth Div CHAR_BIT ;
    Else Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;

  // Filter the row based upon the filter type.
  Case filter Of
    FILTERNONE: ; // NoOp
    FILTERSUB:      DoFilterSub ;
    FILTERUP:       DoFilterUp ;
    FILTERAVERAGE:  DoFilterAverage ;
    FILTERPAETH:    DoFilterPaeth ;
    Else Raise EBadPngStream.Create ('Invalid Filter Method') ;
    End ;
  End ;
//
//  Description:
//
//    This function copies the data from a non-interlaced row to the image.
//
//  Parameters:
//
//    row :  The output row number
//    image : The image to output
//
Procedure TPngDecoder.copyNoninterlacedRowToImage (row : Cardinal ; image : TBitmapImage) ;
  Var
    ii, Col : Cardinal ;
    byteoffset, bitoffset : Cardinal ;
    rawvalue : Cardinal ;
    index : Cardinal ;
  Begin
  Case image_color_type Of
    Grayscale, Palette:
      Case image_depth Of
        1:
          // We convert 1 bits to 8 bits/pixel.
          for ii := 0 To image_width - 1 Do
            Begin
            byteoffset := ii Div CHAR_BIT ;
            bitoffset := ii Mod CHAR_BIT ;

            rawvalue := row_buffers [current_row_buffer][byteoffset] ;
            index := (rawvalue Shr (CHAR_BIT - 1 - bitoffset)) And $1 ; 
            image.Pixels [row * image_width + ii].red := color_palette [index].red ;
            image.Pixels [row * image_width + ii].green := color_palette [index].green ;
            image.Pixels [row * image_width + ii].blue := color_palette [index].blue ;
            image.Pixels [row * image_width + ii].alpha := $FF ;
            End ;
        2:
          // We convert 2 bits to 8 bits/pixel.
          for ii := 0 To image_width - 1 Do
            Begin
            byteoffset := ii Div (CHAR_BIT Div 2) ;
            bitoffset := 2 * (ii Mod (CHAR_BIT Div 2)) ;
            rawvalue := row_buffers [current_row_buffer][byteoffset] ;
            index := (rawvalue Shr (CHAR_BIT - bitoffset - 2)) And $3 ;
            image.Pixels [row * image_width + ii].red := color_palette [index].red ;
            image.Pixels [row * image_width + ii].green := color_palette [index].green ;
            image.Pixels [row * image_width + ii].blue := color_palette [index].blue ;
            image.Pixels [row * image_width + ii].alpha := $FF ;
            End ;
        4:
          // We convert 4 bits to 8 bits/pixel.
          for ii := 0 To image_width - 1 Do
            Begin
            byteoffset := ii Div 2 ;

            rawvalue := row_buffers [current_row_buffer][byteoffset] ;
            if ii Mod 2 = 0 Then
              index := (rawvalue Shr 4) And $F
            Else
              index := rawvalue And $F ;
            image.Pixels [row * image_width + ii].red := color_palette [index].red ;
            image.Pixels [row * image_width + ii].green := color_palette [index].green ;
            image.Pixels [row * image_width + ii].blue := color_palette [index].blue ;
            image.Pixels [row * image_width + ii].alpha := $FF ;
            End ;
        8:
          Begin
          Col := 0 ;
          for ii := 0 To image_width - 1 Do
            Begin
            index := row_buffers [current_row_buffer][col] ;
            image.Pixels [row * image_width + ii].red    := color_palette [index].red ;
            image.Pixels [row * image_width + ii].green  := color_palette [index].green ;
            image.Pixels [row * image_width + ii].blue   := color_palette [index].blue ;
            image.Pixels [row * image_width + ii].alpha  := $FF ;
            Inc (Col, image_depth Div CHAR_BIT) ;
            End ;
          End ;
        16:
          Begin
          Col := 0 ;
          for ii := 0 To image_width - 1 Do
            Begin
            index := row_buffers [current_row_buffer][col] ;
            image.Pixels [row * image_width + ii].red    := index ;
            image.Pixels [row * image_width + ii].green  := index ;
            image.Pixels [row * image_width + ii].blue   := index ;
            image.Pixels [row * image_width + ii].alpha  := $FF ;
            Inc (Col, image_depth Div CHAR_BIT) ;
            End ;
          End ;
        Else  Raise EBadPngStream.Create ('Invalid Bit Depth') ;
        End ;
    RGB:
      if (image_depth = 8) Or (image_depth = 16) Then
        Begin
        col := 0 ;
        for ii := 0 To image_width - 1 Do
          Begin
          image.Pixels [row * image_width + ii].red   := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].green := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].blue  := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].alpha := $FF ;
          End ;
        End
      else
        Raise EBadPngStream.Create ('Invalid Image Depth') ;
    GrayscaleAlpha:
      // For 8-bit samples each sample interval is 2 bytes. For 16-bit
      // samples it is four bytes.
      if (image_depth = 8) Or (image_depth = 16) Then
        Begin
        col := 0 ;
        for ii := 0 To image_width - 1 Do
          begin
          index := row_buffers [current_row_buffer][col] ;
          Inc (col, image_depth Div CHAR_BIT) ; // Advance to Alpha value
          image.Pixels [row * image_width + ii].red   := color_palette [index].red ;
          image.Pixels [row * image_width + ii].green := color_palette [index].green ;
          image.Pixels [row * image_width + ii].blue  := color_palette [index].blue ;
          image.Pixels [row * image_width + ii].alpha := row_buffers [current_row_buffer][col] ;
          Inc (col, image_depth Div CHAR_BIT) ;
          End ;
        End
      else
        Raise EBadPngStream.Create ('Invalid Image Depth for Grayscale/Alpha') ;
    RGBAlpha:
      if (image_depth = 8) Or (image_depth = 16) Then
        Begin
        Col := 0 ;
        for ii := 0 To image_width - 1 Do
          Begin
          image.Pixels [row * image_width + ii].red   := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].green := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].blue  := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          image.Pixels [row * image_width + ii].alpha := row_buffers [current_row_buffer][col] ; Inc (col, image_depth Div CHAR_BIT) ;
          End ;
        End
      else
        Raise EBadPngStream.Create ('Invalid Image Depth for RGB/Alpha') ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;
  End ;

//
//  Description:
//
//    This function reads the pixel data for a non-interlaced image.
//
//  Parameters:
//
//    inputstream : The input stream to read the image from.
//    image : The image to output
//
Procedure TPngDecoder.readNoninterlaced (inputstream : TPngInputStream ;
                                         image : TBitmapImage) ;
  Const
    OPERATION = 'PNG Noninterlaced' ;
  Var
    ii : Cardinal ;
    row : Cardinal ;
    filterbuffer : Char ;
    filter : TPngFilterType ;
  Begin
  // Initialize the input buffers.
  current_row_buffer := 0 ;

  for II := 0 To row_buffer_width - 1 Do
    Begin
    row_buffers [0][ii] := 0 ;
    row_buffers [1][ii] := 0 ;
    End ;

  callProgressFunction (OPERATION, 0) ;
  for row := 0 To image_height - 1 Do
    Begin
    inflate_decoder.decode (inputstream, sizeof (filterbuffer), @filterbuffer) ;
    filter := TPngFilterType (filterbuffer) ;
    inflate_decoder.decode (inputstream,
                            row_buffer_width,
                            @row_buffers [current_row_buffer][0]) ;
    filterRow (filter) ;
    copyNoninterlacedRowToImage (row, image) ;
    current_row_buffer := (current_row_buffer + 1) Mod 2 ; // Switch buffers
    callProgressFunction (OPERATION, 100 * row Div image_height) ;
    End ;
  callProgressFunction (OPERATION, 100) ;
  End ;

//
//  Description:
//
//    This function reads all the rows for an interlaced pass.
//
//  Parameters:
//
//    inputstream : The input stream to read the image from
//    image : The image object to return
//
Procedure TPngDecoder.readInterlaced (inputstream : TPngInputStream ;
                                      image : TBitmapImage) ;
  const
    OPERATION = 'PNG Interlaced' ;
  var
    II : Cardinal ;
    pixelsthisrow : Cardinal ;
    rowbytes  : Cardinal ;
    destrow : Cardinal ;
    info : ^InterlaceInfo ;
    filterbuffer : Byte;
    filter : TPngFilterType ;

  Begin
  info := @interlace_values [interlace_pass] ;

  // If the image is too small we may not have to do any work.
  if (info^.start_row >= image_height) Or (info^.start_col  >= image_width) Then
    Exit ;

  current_row_buffer := 0 ;
  for II := 0 To row_buffer_width - 1 Do
    Begin
    row_buffers [0][ii] := 0 ;
    row_buffers [1][ii] := 0 ;
    End ;


  pixelsthisrow := (image_width - info^.start_col
                                + info^.col_interval - 1)
                               Div info^.col_interval ;
  Case image_color_type Of
    Grayscale, Palette:
      rowbytes := (pixelsthisrow * image_depth + CHAR_BIT - 1) Div CHAR_BIT ;
    RGB:
      rowbytes := pixelsthisrow * 3 * image_depth Div CHAR_BIT ;
    RGBAlpha:
      rowbytes := pixelsthisrow * 4 * image_depth Div CHAR_BIT ;
    GrayscaleAlpha:
      rowbytes := pixelsthisrow * 2 * image_depth Div CHAR_BIT ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;

  destrow := info^.start_row ;
  While (destrow < image_height) Do
    Begin
    // The filter type precedes the row data.
    inflate_decoder.decode (inputstream, sizeof (filterbuffer), @filterbuffer) ;
    filter := TPngFilterType(filterbuffer) ;
    // Read the row data.
    inflate_decoder.decode (inputstream, rowbytes, @row_buffers [current_row_buffer][0]) ;
    // Filter the data
    filterRow (filter) ;

    copyInterlacedRowToImage (destrow, rowbytes, image) ;

    callProgressFunction (OPERATION, 100 * destrow Div image_height) ;
    current_row_buffer := (current_row_buffer + 1) Mod 2 ;  // Switch buffers

    Inc (destrow, info^.row_interval) ;
    End ;

  callProgressFunction (OPERATION, 100) ;
  End ;

//
//  Description:
//
//    This function copies an interlaced row to the output image.
//
//  Parameters:
//
//    row: The output row number
//    rowwidth: The number of bytes of data in the row
//    image : The image to output
//
Procedure TPngDecoder.copyInterlacedRowToImage (row, rowwidth : Cardinal ;
                                                image : TBitmapImage) ;
  var
    info : ^InterlaceInfo ;
    col, destcol : Cardinal ;
    ii : Integer ;
    value : Cardinal ;
  Begin
  info := @interlace_values [interlace_pass] ;

  case image_color_type Of
    Grayscale, Palette:
      Case image_depth Of
        1:
          Begin
          destcol := info^.start_col ;
          for col := 0 To rowwidth - 1 Do
            Begin
            ii := CHAR_BIT - 1 ;
            While (ii >= 0) And (destcol < image_width) Do
              Begin
              value := (row_buffers [current_row_buffer][col] Shr ii) And $1 ;
              image.Pixels [row * image_width + destcol].red   := color_palette [value].red ;
              image.Pixels [row * image_width + destcol].green := color_palette [value].green ;
              image.Pixels [row * image_width + destcol].blue  := color_palette [value].blue ;
              image.Pixels [row * image_width + destcol].alpha := $FF ;
              Dec (ii) ;
              Inc (destcol, info^.col_interval) ;
              End ;
            End ;
          End ;
        2:
          Begin
          // 2 Bits per pixel gets converted to 8 Bits.
          destcol := info^.start_col ;
          for col := 0 To rowwidth - 1 Do
            Begin
            ii := 3 * CHAR_BIT Div 4 ;
            While (ii >= 0) And (destcol < image_width) Do
              Begin
              value := (row_buffers [current_row_buffer][col] Shr ii) And $3 ;
              image.Pixels [row * image_width + destcol].red   := color_palette [value].red ;
              image.Pixels [row * image_width + destcol].green := color_palette [value].green ;
              image.Pixels [row * image_width + destcol].blue  := color_palette [value].blue ;
              image.Pixels [row * image_width + destcol].alpha := $FF ;
              Dec (ii, CHAR_BIT Div 4) ;
              Inc (destcol, info^.col_interval)
              End ;
            End ;
          End ;
        4:
          Begin
          destcol := info^.start_col ;
          for col := 0 To rowwidth - 1 Do
            Begin
            ii := CHAR_BIT Div 2 ;
            While (ii >= 0) And (destcol < image_width) Do
              Begin
              value := (row_buffers [current_row_buffer][col] Shr ii) And $F ;
              image.Pixels [row * image_width + destcol].red   := color_palette [value].red ;
              image.Pixels [row * image_width + destcol].green := color_palette [value].green ;
              image.Pixels [row * image_width + destcol].blue  := color_palette [value].blue ;
              image.Pixels [row * image_width + destcol].alpha := $FF ;
              Dec (ii, CHAR_BIT Div 2) ;
              Inc (destcol, info^.col_interval) ;
              End ;
            End ;
          End ;
        8:
          Begin
          destcol := info^.start_col ;
          for col := 0 To rowwidth - 1 Do
            Begin
            value := row_buffers [current_row_buffer][col] ;
            image.Pixels [row * image_width + destcol].red   := color_palette [value].red ;
            image.Pixels [row * image_width + destcol].green := color_palette [value].green ;
            image.Pixels [row * image_width + destcol].blue  := color_palette [value].blue ;
            image.Pixels [row * image_width + destcol].alpha := $FF ;
            Inc (destcol, info^.col_interval) ;
            End ;
          End ;
        16:
          Begin
          destcol := info^.start_col ;
          col := 0  ;
          While  col < rowwidth  Do
            Begin
            value := row_buffers [current_row_buffer][col] ;
            image.Pixels [row * image_width + destcol].red   := value ;
            image.Pixels [row * image_width + destcol].green := value ;
            image.Pixels [row * image_width + destcol].blue  := value ;
            image.Pixels [row * image_width + destcol].alpha := $FF ;
            Inc (col, 2) ;
            Inc (destcol, info^.col_interval) ;
            End ;
          End
        Else
          Raise EBadPngStream.Create ('Invalid Image Depth') ;
      End ;
    RGB:
      if image_depth = 8 Then
        Begin
        destcol := info^.start_col ;
        Col := 0 ;
        While col < rowwidth Do
          Begin
          image.Pixels [row * image_width + destcol].red   := row_buffers [current_row_buffer][col] ; Inc (col) ;
          image.Pixels [row * image_width + destcol].green := row_buffers [current_row_buffer][col] ; Inc (col) ;
          image.Pixels [row * image_width + destcol].blue  := row_buffers [current_row_buffer][col] ; Inc (col) ;
          image.Pixels [row * image_width + destcol].alpha := $FF ;
          Inc (destcol, info^.col_interval) ;
          End
        End
      else if image_depth = 16 Then
        Begin
        Col := 0 ;
        destcol := info^.start_col ;
        While col < rowwidth Do
          Begin
          image.Pixels [row * image_width + destcol].red   := row_buffers [current_row_buffer][col] ; Inc (col, 2) ;
          image.Pixels [row * image_width + destcol].green := row_buffers [current_row_buffer][col] ; Inc (col, 2) ;
          image.Pixels [row * image_width + destcol].blue  := row_buffers [current_row_buffer][col] ; Inc (col, 2) ;
          image.Pixels [row * image_width + destcol].alpha := $FF ;
          Inc (destcol, info^.col_interval) ;
          End
        End
      else
        Raise EBadPngStream.Create ('Invalid Image Depth') ;
    RGBAlpha:
      Begin
      destcol := info^.start_col ;
      Col := 0 ;
      While col < rowwidth Do
        Begin
        image.Pixels [row * image_width + destcol].red := row_buffers [current_row_buffer][col] ;
        Inc (col, image_depth Div CHAR_BIT) ;
        image.Pixels [row * image_width + destcol].green := row_buffers [current_row_buffer][col] ;
        Inc (col, image_depth Div CHAR_BIT) ;
        image.Pixels [row * image_width + destcol].blue := row_buffers [current_row_buffer][col] ;
        Inc (col, image_depth Div CHAR_BIT) ;
        image.Pixels [row * image_width + destcol].alpha := row_buffers [current_row_buffer][col] ;
        Inc (col, image_depth Div CHAR_BIT) ;
        Inc (destcol, info^.col_interval) ;
        End ;
      End ;
    GrayscaleAlpha:
      Begin
      col := 0 ;
      destcol := info^.start_col ;
      While col < rowwidth Do
        Begin
        image.Pixels [row * image_width + destcol].red := color_palette [row_buffers [current_row_buffer][col]].red ;
        image.Pixels [row * image_width + destcol].green := color_palette [row_buffers [current_row_buffer][col]].green ;
        image.Pixels [row * image_width + destcol].blue := color_palette [row_buffers [current_row_buffer][col]].blue ;
        Inc (col, image_depth Div CHAR_BIT) ;
        image.Pixels [row * image_width + destcol].alpha := row_buffers [current_row_buffer][col] ;
        Inc (col, image_depth Div CHAR_BIT) ;
        Inc (destcol, info^.col_interval) ;
        End ;
      End ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End
  End ;

//
//  Description:
//
//    This function processes a bKGD chuck. This chunk defines the background
//    color for the image. We only use this for Alpha channel processing.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processBackground (inputstream : TPngInputStream) ;
  Var
    chunkdata : PChar ;
  Begin
  chunkdata := inputstream.ChunkData ;
  Case image_color_type Of
    Grayscale, GrayscaleAlpha, Palette:
      Begin
      if inputstream.ChunkDataLength = 0 Then
        raise EBadPngStream.Create ('bKGD Chunk too small') ;
      background_gray := Ord (chunkdata [0]) ;
      if (background_gray >= (1 Shl image_depth)) Then
        Raise EBadPngStream.Create ('bKGD palette index too large') ;
      if (verbose_flag) Then
        WriteLn ('Background Grayscale: ', background_gray) ;
      End ;

    RGB, RGBAlpha:
      Begin
      if inputstream.ChunkDataLength < 6 Then
        Raise EBadPngStream.Create ('bKGD Chunk too small') ;
      background_red := Ord (chunkdata [0]) ;
      background_green := Ord (chunkdata [2]) ;
      background_blue := Ord (chunkdata [4]) ;
      if verbose_flag Then
        WriteLn ('Background RGB: ', background_red, ' ', background_green,
                 background_blue) ;
      End ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;
  End ;
//
//  Description:
//
//    This function processes a gAMA chunk. The game value is stored
//    as a 4-byte integer which is the Gamma value times 100,000.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processGamma (inputstream : TPngInputStream) ;
  Var
    value : Cardinal ;
    chunkdata :PChar ;
  Begin
  if palette_read Then
    Raise EBadPngStream.Create ('gAMA chunk may not occur before PLTE') ;
  if data_read Then
    Raise EBadPngStream.Create ('gAMA chunk may not occur before IDAT') ;

  if inputstream.ChunkDataLength < 4 Then
    Raise EBadPngStream.Create ('gAMA chunk too small') ;
  chunkdata := inputstream.ChunkData ;
  value := Ord (chunkdata [0]) Or (Ord (chunkdata [1]) Shl CHAR_BIT)
                        Or (Ord (chunkdata [2]) Shl (2*CHAR_BIT))
                        Or (Ord (chunkdata [3]) Shl (3*CHAR_BIT)) ;
  value := BigEndianLongToSystem (value) ;
  file_gamma := value / 100000.0 ;
  if verbose_flag Then
    WriteLn (' Gamma: ', file_gamma) ;
  End ;

//
//  Description:
//
//    This function processes a cHRM chunk. This chunk defines
//    precise color values for the image.
//
//    We do nothing with this chunk but dump its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processChromaticities (inputstream : TPngInputStream) ;
  Var
    Data : ^TPngChromaticitiesData ;
    whitepointx, whitepointy, redx, redy, greenx, greeny, bluex, bluey : Cardinal ;
  Begin
  if palette_read Then
    Raise EBadPngStream.Create ('cHRM chunk may not occur after PLTE') ;
  if data_read Then
    Raise EBadPngStream.Create ('cHRM chunk may not occur after IDAT') ;

  if inputstream.ChunkDataLength <> sizeof (TPngChromaticitiesData) Then
    Raise EBadPngStream.Create ('Invalid cHRM chunk length') ;

  data := inputstream.ChunkData ;

  whitepointx := BigEndianLongToSystem (data^.whitepointx) ;
  whitepointy := BigEndianLongToSystem (data^.whitepointy) ;
  redx := BigEndianLongToSystem (data^.redx) ;
  redy := BigEndianLongToSystem (data^.redy) ;
  greenx := BigEndianLongToSystem (data^.greenx) ;
  greeny := BigEndianLongToSystem (data^.greeny) ;
  bluex := BigEndianLongToSystem (data^.bluex) ;
  bluey := BigEndianLongToSystem (data^.bluey) ;

  if verbose_flag Then
    Begin
    WriteLn ('  White Point X: ', whitepointx / 100000.0) ;
    WriteLn ('  White Point y: ', whitepointy / 100000.0) ;
    WriteLn ('  Red X:         ', redx / 100000.0) ;
    WriteLn ('  Red Y:         ', redy / 100000.0) ;
    WriteLn ('  Green X:       ', greenx / 100000.0) ;
    WriteLn ('  Green Y:       ', greeny / 100000.0) ;
    WriteLn ('  Blue X:        ', bluex / 100000.0) ;
    WriteLn ('  Blue Y:        ', bluey / 100000.0) ;
    End ;
  End ;

//
//  Description:
//
//    Tais function processes an hIST chunk. This chunk defines the frequency
//    that each color in the palette is used within the imamge. This
//    information can be used to assist in quantization.
//
//    We do nothing with this chunk but dump its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processHistogram (inputstream : TPngInputStream) ;
  Var
    II : Cardinal ;
    values : PWideChar ;
  Begin
  if Not palette_read Then
    Raise EBadPngStream.Create ('hIST chunk may not appear before PLTE') ;
  if data_read Then
    Raise EBadPngStream.Create ('hIST chunk must appear before IDAT') ;

  if inputstream.ChunkDataLength <> (2 * palette_size) Then
    Raise EBadPngStream.Create ('Bad size for hIST chunk') ;

  values := inputstream.ChunkData ;

  if verbose_flag Then
    Begin
    for ii := 0 To palette_size - 1 Do
      WriteLn ('  ', ii, ') ', ord (values [ii])) ;
    End ;
  End ;

//
//  Description:
//
//    This function processes the pHYs chunk. This chunk defines the
//    dimensions for the image pixels.
//
//    We do nothing with this chunk except print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processPhysicalPixelDimensions (inputstream : TPngInputStream) ;
  Const
    unitstrings : Array [0..1] of String = ('unknown unit', 'meter') ;
  Var
    pd : ^TPngPixelDimensions ;
  Begin
  if data_read Then
    Raise EBadPngStream.Create ('pHYS chunk must come before IDAT chunks') ;

  if inputstream.ChunkDataLength <> sizeof (TPngPixelDimensions) Then
    Raise EBadPngStream.Create ('pHYs chunk size invalid') ;

  pd := inputstream.ChunkData ;
  if pd^.units > 1 Then
    Raise EBadPngStream.Create ('pHYs contains an invalid unit') ;

  if verbose_flag Then
    Begin
    WriteLn ('  ', pd^.pixelsx, ' per ', unitstrings [pd^.units]) ;
    WriteLn ('  ', pd^.pixelsy, ' per ', unitstrings [pd^.units]) ;
    WriteLn ('  Unit: ', pd^.units) ;
    End ;
  End ;

//
//  Description:
//
//    This function processes the sBIT chunk. This chunk can be used to
//    set the number of significant bits used in color values.
//
//    We do nothing with this chunk but print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processSignificantBits (inputstream : TPngInputStream) ;
  Var
    chunkdata : PChar ;
  Begin
  if data_read Then
    Raise EBadPngStream.Create ('sBIT chunk must occur before IDAT chunks') ;
  if palette_read Then
    Raise EBadPngStream.Create ('sBIT chunk must occur before PTLE chunk') ;

  chunkdata := inputstream.ChunkData ;

  Case image_color_type Of
    Grayscale:
      Begin
      if inputstream.ChunkDataLength < 1 Then
        Raise EBadPngStream.Create ('sBIT chunk length invalid') ;

      if verbose_flag Then
        WriteLn ('  Significant Bits: ', Ord (chunkdata [0])) ;
      End ;

    Palette, RGB:
      Begin
      if inputstream.ChunkDataLength < 3 Then
        Raise EBadPngStream.Create ('sBIT chunk length invalid') ;

      if verbose_flag Then
        Begin
        WriteLn (' Significant Red Bits: ', Ord (chunkdata [0])) ;
        WriteLn (' Significant Green Bits: ', Ord (chunkdata [1])) ;
        WriteLn (' Significant Blue Bits: ', Ord (chunkdata [2])) ;
        End ;
      End ;
    GrayscaleAlpha:
      Begin
      if inputstream.ChunkDataLength < 2 Then
        Raise EBadPngStream.Create ('sBIT chunk length invalid') ;

      if verbose_flag Then
        Begin
        WriteLn (' Significant Bits: ', Ord (chunkdata [0])) ;
        WriteLn (' Significant Alpha Bits: ', Ord (chunkdata [1])) ;
        End ;
      End ;
    RGBAlpha:
      Begin
      if inputstream.ChunkDataLength < 4 Then
        Raise EBadPngStream.Create ('sBIT chunk length invalid') ;

      if verbose_flag Then
        Begin
        WriteLn (' Significant Red Bits: ', Ord (chunkdata [0])) ;
        WriteLn (' Significant Green Bits: ', Ord (chunkdata [1])) ;
        WriteLn (' Significant Blue Bits: ', Ord (chunkdata [2])) ;
        WriteLn (' Significant Alpha Bits: ', Ord (chunkdata [3])) ;
        End ;
      End ;
    Else
      Raise EBadPngStream.Create ('Invalid Color Type') ;
    End ;
  End ;

//
//  Description:
//
//    This function processes the tEXt chunk. This chunk stores text
//    information in the image.
//
//    We do nothing with the chunk except print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processTextualData (inputstream : TPngInputStream) ;
  Var
   end_found : Boolean ;
   ii : Cardinal ;
   chunkdata : PChar ;
   offset : Cardinal ;
   datalength : Cardinal ;
  Begin
  end_found := false ;

  datalength := inputstream.ChunkDataLength ;
  chunkdata := inputstream.ChunkData ;

  offset := 0 ;
  While (offset < datalength) And (offset < 80) And (Not end_found) Do
    Begin
    if Ord (chunkdata [offset]) = 0 Then
      end_found := true
    Else
      Inc (offset) ;
    End ;

  if Not end_found Then
    Raise EBadPngStream.Create ('tEXt keyword not found') ;

  if verbose_flag Then
    Begin
    WriteLn (' Keyword: "', chunkdata,  '"') ;
    Write (' Value: ') ;
    for ii := offset + 1 To datalength - 1 Do
      Write (chunkdata [ii]) ;
    WriteLn ('"') ;
    End ;
  End ;
//
//  Description:
//
//    This function processes the tIME chunk. This chunk stores the last time
//    the image was modified.
//
//    We do nothing with the chunk except print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processImageTime (inputstream : TPngInputStream) ;
  var
    td : ^TPngTimeData ;
  Begin
  if inputstream.ChunkDataLength <> sizeof (TPngTimeData) Then
    Raise EBadPngStream.Create ('tIME chunk size invalid') ;

  if verbose_flag Then
    Begin
    td := inputstream.ChunkData ;

    WriteLn ('  Year: ', td^.year) ;
    WriteLn ('  Month: ', td^.month) ;
    WriteLn ('  Day: ', td^.day) ;
    WriteLn ('  Hour: ', td^.hour) ;
    WriteLn ('  Minute: ', td^.minute) ;
    WriteLn ('  Second: ', td^.second) ;
    End ;
  End ;

//
//  Description:
//
//    This function processes the tRNS chunk. This chunk allows transparency
//    to be define for color types without an alpha channel.
//
//    We do nothing with the chunk except print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processTransparency (inputstream : TPngInputStream) ;
  Var
    chunkdata : PChar ;
    wordvalues : PWideChar ;
    ii : Cardinal ;
  Begin
  if data_read Then
    Raise EBadPngStream.Create ('tRNS chunk cannot occur before IDAT chunks') ;

  chunkdata := inputstream.ChunkData ;
  wordvalues := inputstream.chunkdata ;

  if verbose_flag Then
    Begin
    Case image_color_type Of
      Palette:
        Begin
        if Not palette_read Then
          Raise EBadPngStream.Create ('tRNS chunk must occur after PLTE chunk') ;

        if palette_size < inputstream.ChunkDataLength Then
          Raise EBadPngStream.Create ('tRNS block length invalid') ;

        for ii := 0 To inputstream.ChunkDataLength - 1 Do
          WriteLn (ii, ') ', Ord (chunkdata [ii])) ;
        End ;
      Grayscale:
        Begin
        if inputstream.ChunkDataLength < 2 Then
          Raise EBadPngStream.Create ('tRNS chunk length invalid') ;
        WriteLn ('  Transparency: ', BigEndianWordToSystem (Ord (wordvalues [0]))) ;
        End ;
      RGB:
        Begin
        if inputstream.ChunkDataLength < 6 Then
          Raise EBadPngStream.Create ('tRNS chunk length invalid') ;
        WriteLn ('  Red Transparency: ', BigEndianWordToSystem (Ord (wordvalues [0]))) ;
        WriteLn ('  Green Transparency: ', BigEndianWordToSystem (Ord (wordvalues [1]))) ;
        WriteLn ('  Blue Transparency: ', BigEndianWordToSystem (Ord (wordvalues [2]))) ;
        End ;
      Else
        Raise EBadPngStream.Create ('Invalid Color Type of tRNS chunk') ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    This function processes the zTXt chunk. This chunk stores text
//     information.  This chunk is similar to a tEXt chunk except that the
//    data is compressed.
//
//    We do nothing with the chunk except print its contents.
//
//  Parameters:
//
//    inputstream : The stream to read the chunk data from
//    image : The output image
//
Procedure TPngDecoder.processCompressedText (inputstream : TPngInputStream) ;
  var
    end_found : boolean ;
    offset : Cardinal ;
    chunkdata : pchar ;
    datalength : Cardinal ;
  Begin
  end_found := false ;
  chunkdata := inputstream.ChunkData ;
  datalength := inputstream.ChunkDataLength ;

  offset := 0 ;
  While (offset < datalength) And (offset < 80) And Not end_found Do
    Begin
    if Ord (chunkdata [offset]) = 0 Then
      end_found := true 
    Else
      Inc (Offset) ;
    End ;
  if Not end_found Then
    Raise EBadPngStream.Create ('zEXt keyword not found') ;

  if verbose_flag Then
    Begin
    WriteLn ('Keyword: "', chunkdata, '"') ;
    WriteLn ('Compression Method: "', chunkdata [offset + 1]) ;
    End ;
  End ;

//
//  Description:
//
//    This function reads a PNG file.
//
//  Parameters:
//
//    filename : The name of the file to read.
//    image : The image object to read into.
//
Procedure TPngDecoder.readImageFile (filename : String ; image : TBitmapImage) ;
  Var
    input : TPngInputFileStream ;
//    input : TPngInputMapStream ;
  Begin
  input := TPngInputFileStream.Create ;
  Try
    input.open (filename) ;
    readImage (input, image) ;
  Finally
    input.Destroy ;
    End ;
  End ;

//
//  Description:
//
//    This function calls the progress function if it has been
//    defined.
//
//  Parameters:
//    percent:  The completion percentage (0..100)
//
Procedure TPngDecoder.callProgressFunction (description : string ; percent : Cardinal) ;
  Var
    cancel : Boolean ;
  Begin
  if Not Assigned (progress_function) Then
    Exit ;

  if (percent > 100) Then
    percent := 100 ;

  cancel := false ;
  if (image_interlace_method = 0) Then
    progress_function (Self, progress_data, 1, 1, description, 
                       percent, cancel)
  else
    if (interlace_pass = InterlacePasses) Then
      progress_function (self, progress_data, interlace_pass,
                         InterlacePasses, description,
                         percent, cancel)
    else
      progress_function (self, progress_data, interlace_pass + 1,
                         InterlacePasses,  description, 
                         percent, cancel)
  End ;


End.
