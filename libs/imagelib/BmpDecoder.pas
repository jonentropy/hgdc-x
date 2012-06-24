unit bmpdecoder ;
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
//  BMP Decoder Library.
//
//  Title:   BmpDecoder Class Implementation
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class decodes Windows BMP file.
//
//

interface

uses bitmapimage, classes ;

type
  RGBQUAD = Packed Record
    rgbBlue, rgbGreen, rgbRed, spare : BYTE ;
    End ;

  TBmpDecoder = class (TBitmapImageDecoder)
    private
      color_table : Array [0..255] Of RGBQUAD  ;
      color_table_size : Cardinal ;

    public
      Constructor Create ;
      Destructor Destroy ; Override ;

      Procedure readImageFile (filename : String ; image : TBitmapImage) ; Override ;
      Procedure readImage (strm : TStream ; image : TBitmapImage) ;
    End ;

    EBmpError = class (EGraphicsException)
      End ;

Implementation

Uses SysUtils, bmppvt ;

Procedure FindMask (mask : Cardinal ; var offset,  size : Cardinal) ;
  Begin
  offset := 0 ;
  While ((mask AND (1 Shl offset)) = 0) And (offset < 32) Do
    inc (offset) ;
  size := 0 ;
  While (offset + size < 32) And ((mask And (1 Shl (offset + size))) <> 0) Do
    Inc (size) ;
  End ;


//
//  Description:
//
//    Class default constructor
//

//
//  Description:
//
//    Default Constructor
//
Constructor TBmpDecoder.Create ;
  Begin
  Inherited Create ;
  End ;

//
//  Description:
//
//    Class Destructor
//
Destructor TBmpDecoder.Destroy ;
  Begin
  Inherited Destroy ;
  End ;

  //
//  Description:
//
//    This function reads an image from a Windows BMP stream.
//
//  Parameters:
//    strm: The input stream
//    image: The image to be read
//
Procedure TBmpDecoder.readImage (strm : TStream ; image : TBitmapImage) ;
  Var
    bytesread : Cardinal ;
    fileheader : BITMAPFILEHEADER ;
    count : Integer ;
    headerbuffer : Array [0..512] of DWord ;
    width : Cardinal ;
    height : Integer ;
    imagesize : Cardinal ;
    bitcount : Cardinal ;
    compression : Cardinal;
    redmask, greenmask, bluemask, alphamask : Cardinal ;
  Const
    Signature = Ord ('B') Or (Ord ('M') Shl BITSPERBYTE) ;

  Function ReadOs2ColorTable (colorcount : Cardinal) : Cardinal ;
    Var
      ii : Integer ;
      buffer : Array [0..255] of RGBTRIPLE ;
      count : Integer ;
    Begin
    count := strm.read (buffer  [0], colorcount * sizeof (RGBTRIPLE)) ;
    If count <> colorcount * sizeof (RGBTRIPLE) Then
      raise EBmpError.Create ('Premature end of file in color table') ;
    For ii := 0 To colorcount - 1 Do
      Begin
      color_table [ii].rgbRed   := buffer [ii].rgbtRed ;
      color_table [ii].rgbBlue  := buffer [ii].rgbtBlue ;
      color_table [ii].rgbGreen := buffer [ii].rgbtGreen ;
      End ;
    Result := count ;
    End ;

  Function ReadColorTable (colorcount : Cardinal) : Cardinal ;
    var
      count : Cardinal ;
    Begin
    count := strm.read (color_table [0], sizeof (RGBQUAD) * colorcount) ;
    If (count <> sizeof (RGBQUAD) * colorcount) Then
      raise EBmpError.Create ('Premature end of file in color table') ;
    Result := count ;
    End ;

  Procedure GetOs2Header ;
    Var
      header : ^BITMAPCOREHEADER ;
    Begin
    header := @headerbuffer ;
    width := header^.bcWidth ;
    height := header^.bcHeight ;
    bitcount := header^.bcBitCount ;

    color_table_size := 1 Shl bitcount ;
    Inc (bytesread,  readOs2ColorTable (color_table_size)) ;
    compression := BI_RGB ;
    End ;

  Procedure GetWindowsHeader ;
    Var
      Header : ^BITMAPINFOHEADER ;
    Begin
    header := @headerbuffer ;
    compression := header^.biCompression ;
    width := header^.biWidth ;
    height := header^.biHeight ;
    bitcount := header^.biBitCount ;

    color_table_size := header^.biClrUsed ;
    if (color_table_size = 0) And (bitcount  < 16) Then
      Begin
      // If the colors used field is non-zero, it gives the size of the
      // color table. Otherwise, the number of bits per pixel gives the size.
      color_table_size := 1 Shl bitcount ;
      End ;

    // Validate Compression
    Case bitcount Of
      1:
        If (compression <> BI_RGB) Then
          Raise EBmpError.Create ('Unsupported Compression') ;
      4:
        If (compression <> BI_RGB) And (compression <> BI_RLE4) Then
          Raise EBmpError.Create ('Unsupported Compression') ;
      8:
        If (compression <> BI_RGB) And  (compression <> BI_RLE8) Then
          Raise EBmpError.Create ('Unsupported Compression') ;
      24:
        If (compression <> BI_RGB) Then
          Raise EBmpError.Create ('Unsupported Compression') ;
      16, 32:
        If (compression <> BI_RGB) And (compression <> BI_BITFIELDS) Then
          Raise EBmpError.Create ('Unsupported Compression') ;
    Else
      Raise EBmpError.Create ('Unsupported bit count') ;
      End ;
    If (color_table_size <> 0) Then
      Inc (bytesread, readColorTable (color_table_size)) ;
    End ;

    Procedure Get16BitMasks ;
      Var
        Header : ^BITMAPV4HEADER ;
      Begin
      header := @headerbuffer ;
      redmask := header^.bV4RedMask ;
      If (redmask = 0) Then
        redmask := $7C00 ;
      greenmask := header^.bV4GreenMask ;
      If (greenmask = 0) Then
        greenmask := $3E0 ;
      bluemask := header^.bV4BlueMask ;
      If (bluemask = 0) Then
        bluemask := $1F ;
      alphamask := 0 ;
      If (headerbuffer [0] >= sizeof (BITMAPV4HEADER)) Then
        alphamask := header^.bV4AlphaMask ;
      End ;

  Procedure Get32BitMasks ;
    Var
      header : ^BITMAPV4HEADER ;
    Begin
    header := @headerbuffer ;
    redmask := header^.bV4RedMask ;
    If (redmask = 0) Then
      redmask := $FF0000 ;
    greenmask := header^.bV4GreenMask ;
    If (greenmask = 0) Then
      greenmask := $FF00 ;
    bluemask := header^.bV4BlueMask ;
    If (bluemask = 0) Then
      bluemask := $FF ;
    alphamask := 0 ;
    If (headerbuffer [0] >= sizeof (BITMAPV4HEADER)) Then
      alphamask := header^.bV4AlphaMask ;
    End ;

  Procedure SkipBytes (count : integer) ;
    var
      Buffer : Array [1..512] of Byte ;
    Begin
    While count > 0 Do
      Begin
      if Count > Sizeof (Buffer) Then
        strm.read (Buffer [1], sizeof (Buffer))
      else
        strm.read (Buffer [1], count) ;
      Dec (count, sizeof (buffer)) ;
      End
    End ;

  Procedure callProgressFunction (offset : Cardinal) ;
    const
      OPERATION = 'BMP Decode' ;
    Var
      cancel : Boolean ;
    Begin
    if Assigned (progress_function) Then
     Begin
     cancel := false ;
     progress_function (Self, progress_data, 1, 1, OPERATION, 100 * offset Div Imagesize, cancel) ;
     if (cancel) Then
       Raise EGraphicsAbort.Create ('')  ;
     End ;
  End ;

  Procedure readFractionalByteData ;
    Var
      bitwidth, rowwidth, physicalrowsize : Cardinal ;
      buffer : Array of Byte ;
      count : Integer ;
      ii : Integer ;
      color : Cardinal ;
    Procedure ReadRow (rowindex : Cardinal) ;
      const
        Masks : Array [1..4] of Cardinal = ($1, $3, 0, $F ) ;
      Var
        ii, jj : Cardinal ;
        offset, position : Cardinal ;
      Begin
      callProgressFunction (rowindex) ;
      count := strm.read (buffer [0], physicalrowsize) ;
      if (count <> physicalrowsize) Then
         Raise EBmpError.Create ('Premature End of Stream') ;

      // The pixel rows are stored in reverse order.
      jj := 0 ;
      For ii := 1 To width Do
        Begin
        position := jj div (BITSPERBYTE div bitcount) ;
        offset := BITSPERBYTE - ((jj mod (BITSPERBYTE div bitcount)) + 1) * bitcount ;
        color := (buffer [position] Shr offset) AND masks [bitcount] ;
        image.Pixels [rowindex * width + jj].red := color_table [color].rgbred ;
        image.Pixels [rowindex * width + jj].green := color_table [color].rgbGreen ;
        image.Pixels [rowindex * width + jj].blue := color_table [color].rgbBlue ;
        image.Pixels [rowindex * width + jj].alpha := 0 ;
        Inc (jj) ;
        End ;
      End ;

    Begin
    // Number of bits required for each pixel row.
    bitwidth := bitcount * width ;
    // Number of bytes need to store each pixel row.
    rowwidth := (bitwidth + BITSPERBYTE - 1) Div BITSPERBYTE ;
    // Number of bytes used to store each row in the BMP file.
    // This is is rowwidth rounded up to the nearest 4 bytes.
    physicalrowsize := (rowwidth + $3) And Not $3 ;

    SetLength (buffer, physicalrowsize) ;

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    If height > 0 Then
      For II := 0 To Height - 1 Do
        ReadRow (height - ii - 1)
    Else
      For II := 0 To 1 - Height Do
        ReadRow (height) ;
    End ;

  Procedure Read8Bitdata ;
    Var
      Buffer : Array of Byte ;
      physicalrowsize : Cardinal ;
      ii : Integer ;
    Procedure ReadRow (rowindex : Cardinal) ;
      var
        Count : Integer ;
        ii, jj : Integer ;
        color : Cardinal ;
      Begin
      callProgressFunction (rowindex) ;
      count := strm.read (buffer [0], physicalrowsize) ;
      if (count <> physicalrowsize) Then
        Raise EBmpError.Create ('Premature End of Stream') ;

      jj := 0 ;
      For ii := RowIndex * Width To (RowIndex + 1) * width - 1 Do
        Begin
        color := buffer [jj] ;
        image.Pixels [ii].red := color_table [color].rgbRed ;
        image.Pixels [ii].green := color_table [color].rgbGreen ;
        image.Pixels [ii].blue := color_table [color].rgbBlue ;
        image.Pixels [ii].alpha := 0 ;
        jj := jj + 1 ;
        End ;
      End ;
    Begin
    physicalrowsize := (width + $3) And Not $3 ;
    SetLength (buffer, physicalrowsize) ;

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    If (height > 0) Then
      For ii := 0 To Height - 1 Do
        ReadRow (height - ii - 1) 
    Else
      For ii := 0 To 1 - Height Do
        ReadRow (ii) ;
    End ;

  Procedure Read16BitData ;
    Var
      allbits : Cardinal ;
      redsize, redoffset,
      greensize, greenoffset,
      bluesize, blueoffset,
      alphasize, alphaoffset : Cardinal ;
      physicalrowsize : Cardinal ;
      buffer : Array of Word ;
      ii : Integer ;
    Procedure ReadRow (rowindex : Cardinal) ;
      Var
        ii, jj : Integer ;
      Begin
      callProgressFunction (rowindex) ;
      count := strm.read (buffer [0], physicalrowsize) ;
      if (count <> physicalrowsize) Then
        Raise EBmpError.Create ('Premature End of Stream') ;

      jj := 0 ;
      For ii := rowindex To rowindex * width - 1 Do
        Begin
        image.Pixels [ii].red   := ((buffer [jj] And redmask) Shr (redoffset + redsize - 1)) And $FF ;
        image.Pixels [ii].green := ((buffer [jj] And greenmask) Shr (greenoffset + greensize - 1)) And $FF ;
        image.Pixels [ii].blue  := ((buffer [jj] And bluemask) Shr (blueoffset + bluesize - 1)) And $FF ;
        image.Pixels [ii].alpha := ((buffer [jj] And alphamask) Shr (alphaoffset + alphasize - 1)) And $FF ;
        Inc (jj) ;
        End ;
      End ;
    Begin
    allbits := redmask Or greenmask Or bluemask Or alphamask ;
    If ((allbits And redmask) <> redmask) Or ((allbits And greenmask) <> greenmask)
         Or ((allbits And bluemask) <> bluemask) Or ((allbits And alphamask) <> alphamask) Then
      Raise EBmpError.Create ('Overlapping component mask') ;

    FindMask (redmask, redoffset, redsize) ;
    FindMask (greenmask, greenoffset, greensize) ;
    FindMask (bluemask, blueoffset, bluesize) ;
    FindMask (alphamask, alphaoffset, alphasize) ;

    physicalrowsize := (Sizeof(Word) * width + $3) And Not $3 ;
    SetLength (buffer, physicalrowsize Div sizeof (Word)) ;

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    If (height > 0) Then
      For ii := 0 To height - 1 Do
        ReadRow ((height - ii - 1) * width)
    Else
      For ii := 0 To 1 - height Do
        callProgressFunction (ii * 100 Div  -height) ;
    End ;

  Procedure Read24BitData ;
    Var
      physicalrowsize : Cardinal ;
      buffer : Array of Byte ;
      ii : Integer ;
      count : Integer ;
    Procedure ReadRow (rowindex : Cardinal) ;
      Var
        ii, jj : Integer ;
      Begin
      callProgressFunction (rowindex) ;
      jj := 0 ;
      count := strm.read (buffer [0], physicalrowsize) ;
      if count <> physicalrowsize Then
        raise EBmpError.Create ('Premature EOF in image data') ;
      For ii := rowindex To rowindex + width - 1 Do
        Begin
        image.Pixels [ii].blue := buffer [jj] ;
        image.Pixels [ii].green := buffer [jj + 1] ;
        image.Pixels [ii].red := buffer [jj + 2] ;
        image.Pixels [ii].alpha := 0 ;
        Inc (jj, 3) ;
        End ;
      End ;
    Begin
    physicalrowsize := (3 * width + $3) And Not $3 ;
    SetLength (buffer, physicalrowsize) ;

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    if (height > 0) Then
      For ii := 0 To height - 1 Do
        ReadRow ((height - ii - 1) * width)
    Else
      For ii := 0 To 1 - Height Do
        ReadRow (- height * width) ;
    End ;

  Procedure Read32BitData ;
    Var
      allbits : Cardinal ;
      redsize, redoffset,
      greensize, greenoffset,
      bluesize, blueoffset,
      alphasize, alphaoffset : Cardinal ;
      buffer : Array of Cardinal ;
      physicalrowsize : Cardinal ;
      ii : Integer ;

    Procedure ReadRow (rowindex : Cardinal) ;
      Var
        count : Integer ;
        ii, jj : Integer ;
      Begin
      callProgressFunction (rowindex) ;
      count := strm.read (buffer [0], physicalrowsize) ;
      if (count <> physicalrowsize) Then
        raise EBmpError.Create ('Premature End of Stream') ;

      jj := 0 ;
      For ii := rowindex To rowindex + width - 1 Do
        Begin
        image.Pixels [ii].red   := ((buffer [jj] And redmask) Shr (redoffset + redsize - BITSPERBYTE)) And $FF ;
        image.Pixels [ii].green := ((buffer [jj] And greenmask) Shr (greenoffset + greensize - BITSPERBYTE)) And $FF ;
        image.Pixels [ii].blue  := ((buffer [jj] And bluemask) Shr (blueoffset + bluesize - BITSPERBYTE)) And $FF ;
        image.Pixels [ii].alpha := ((buffer [jj] And alphamask) Shr (alphaoffset + alphasize - BITSPERBYTE)) And $FF ;
        Inc (jj) ;
        End ;
      End ;

    Begin
    allbits := redmask Or greenmask Or bluemask Or alphamask ;
    If ((allbits And redmask) <> redmask) Or ((allbits And greenmask) <> greenmask)
         Or ((allbits And bluemask) <> bluemask) Or ((allbits And alphamask) <> alphamask) then
      Raise EBmpError.Create ('Overlapping component mask') ;

    FindMask (redmask, redoffset, redsize) ;
    FindMask (greenmask, greenoffset, greensize) ;
    FindMask (bluemask, blueoffset, bluesize) ;
    FindMask (alphamask, alphaoffset, alphasize) ;

    physicalrowsize := sizeof (Cardinal) * width ;
    SetLength (buffer, width) ;

    // Images with positive heights are stored bottom up. Images with
    // negative height are stored top down.
    If (height > 0) Then
      For ii := 0 To height - 1 Do
        ReadRow ((height - ii - 1) * width)
    Else
      For ii := 0 To height -1 Do
        ReadRow (- height * width) ;
    End ;

  Procedure ReadRle4 ;
    var
      row, col : Cardinal ;
      done : boolean ;
      opcode : RLEOPCODE ;
      count : Integer ;
      dx, dy : Byte ;
      data, low, hi : Byte ;
      ii : Integer ;
    Begin
    if (height < 0) Then
      Raise EBmpError.Create ('Negative height not allowed in an RLE image') ;

    // The mechanism here is the same as for BI_RLE8 with two
    // exceptions. Here we are dealing with 4-bit nibbles rather
    // than whole bytes. This results in some extra work. In
    // addition, the coding of runs includes two color values.

    row :=  height - 1 ;
    col := 0 ;
    done := false ;
    While Not Done Do
      Begin
      callProgressFunction (row * width + col) ;
      count := strm.read (opcode, sizeof (opcode)) ;
      if (count <> sizeof (opcode)) Then
        raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;

      If (opcode.count = 0) Then
        Begin
        Case (opcode.command) Of
          0:    // Advance to next pixel row
            Begin
            If row > 0 Then
              dec (row) ;
            col := 0 ;
            End ;
          1:    // Image complete
            done := true ;
          2:   // Move to relative location the image.
            Begin
            count := strm.read (dx, sizeof (dx)) ;
            if count <> sizeof (dx) Then
              raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
            count := strm.read (dy, sizeof (dy)) ;
            if count <> sizeof (dy) Then
              raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
            Inc (col, dx) ;
            Dec (row, dy) ;
            End ;
          Else
            Begin
            If (row >= height) Or (col + opcode.command > width) Then
              raise EBmpError.Create ('Corrupt RLE-compressed Data') ;
            For ii := 0 To opcode.command - 1 Do
              Begin
              If ((ii And 1) = 0) Then
                Begin
                strm.read (data, sizeof (data)) ;
                low := (data And $F) ;
                hi := (data And $F0) Shr 4 ;
                image.Pixels [row * width + col].red := color_table [hi].rgbRed ;
                image.Pixels [row * width + col].green := color_table [hi].rgbGreen ;
                image.Pixels [row * width + col].blue := color_table [hi].rgbBlue ;
                End
              Else
                Begin
                image.Pixels [row * width + col].red := color_table [low].rgbRed ;
                image.Pixels [row * width + col].green := color_table [low].rgbGreen ;
                image.Pixels [row * width + col].blue := color_table [low].rgbBlue ;
                End  ;
              inc (col) ;
              End ;
            // If the number of bytes used in this instruction
            // is odd then there is a padding byte. Note that if
            // command Mod 4 is 3, that means that the data is word aligned,
            // but we only used 4-bit of the last byte.
            If (opcode.command Mod 4) In [1..2] Then
              begin
              count := strm.read (data, sizeof (data)) ;
              If count <> sizeof (data) Then
                raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
              End ;
            End ;
          End
        End
      Else
        Begin
        // Process a run of the same color value pairs.
        hi := opcode.command Shr 4 ;
        low := opcode.command And $F ;
        if (row >= height) Or (col + opcode.count > width) Then
          Raise EBmpError.Create ('Corrupt RLE-compressed Data') ;

        For ii := 0 To opcode.count - 1 Do
          Begin
          if (ii And 1) = 0 Then
            begin
            image.Pixels [row * width + col].red := color_table [hi].rgbRed ;
            image.Pixels [row * width + col].green := color_table [hi].rgbGreen ;
            image.Pixels [row * width + col].blue := color_table [hi].rgbBlue ;
            end
          Else
            Begin
            image.Pixels [row * width + col].red := color_table [low].rgbRed ;
            image.Pixels [row * width + col].green := color_table [low].rgbGreen ;
            image.Pixels [row * width + col].blue := color_table [low].rgbBlue ;
            End ;
          Inc (col) ;
          End ;
        End
      End ;
    End ;

  Procedure ReadRle8 ;
    var
      row, col : Cardinal ;
      done : boolean ;
      opcode : RLEOPCODE ;
      count : integer ;
      dx, dy : Byte ;
      data : Byte ;
      ii : integer ;
    Begin
    if (height < 0) Then
      Raise EBmpError.Create ('Negative height not allowed in an RLE image') ;
    row := height - 1 ;  // Current row
    col := 0 ;           // Current column
    done := false ;
    While Not Done Do
      Begin
      callProgressFunction (row * width + col) ;
      count := strm.read (opcode, sizeof (opcode)) ;
      if count <> sizeof (opcode) Then
        raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
      if (opcode.count = 0) Then
        Begin
        // A byte count of zero means that this is a special
        // instruction.
        Case (opcode.command) Of
          0: // 0 => Move to next row
            Begin
            if row > 0 then
              dec (row) ;
            col := 0 ;
            End ;
          1: // 1 => Image is finished
            done := true ;
          2: // 2 => Move to a new relative position.
            Begin
            // Read the relative position.
            count := strm.read (dx, sizeof (dx)) ;
            if count <> sizeof (dx) Then
              raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
            count := strm.read (dy, sizeof (dy)) ;
            if count <> sizeof (dy) Then
              raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
            inc (col, dx) ;
            dec (row, dy) ;
            end
          Else
            Begin
            // Store absolute data. The command code is the
            // number of absolute bytes to store.
            if (row >= height) Or (col + opcode.command > width) Then
              Raise EBmpError.Create ('Corrupt RLE-compressed Data') ;
            For ii := 0 To opcode.command - 1  Do
              Begin
              count := strm.read (data, sizeof (data)) ;
              if count <> sizeof (data) Then
                raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;

              image.Pixels [row * width + col].red := color_table [data].rgbRed ;
              image.Pixels [row * width + col].green := color_table [data].rgbGreen ;
              image.Pixels [row * width + col].blue := color_table [data].rgbBlue ;
              Inc (col) ;
              End ;
            // An odd number of bytes is followed by a pad byte.
            If ((opcode.command Mod 2) = 1) Then
              Begin
              count := strm.read (data, sizeof (data)) ;
              if count <> sizeof (data) Then
                raise EBmpError.Create ('Premature EOF in RLE-compressed data') ;
              End ;
            End ;
          End ;
        End
      Else
        Begin
        // Store a run of the same color value.
        if (row >= height) Or (col + opcode.count > width) Then
          raise EBmpError.Create ('Corrupt RLE-compressed Data') ;
        For ii := 0 To opcode.count - 1 Do
          Begin
          image.Pixels [row * width + col].red := color_table [opcode.command].rgbRed ;
          image.Pixels [row * width + col].green := color_table [opcode.command].rgbGreen ;
          image.Pixels [row * width + col].blue := color_table [opcode.command].rgbBlue ;
          Inc (col) ;
          End ;
        End ;
      End ;
    End ;

  Begin

  bytesread := 0 ;

  count := strm.read (fileheader, sizeof (fileheader)) ;
  if count <> sizeof(fileheader) Then
    Raise EBmpError.Create ('Premature End of Stream') ;
  Inc (bytesread, count) ;

  if fileheader.bfType <> signature Then
    Raise EBmpError.Create ('Not a BMP Image') ;

  // The header can come in one of two flavors.  They both
  // begin with a 4-byte headersize.

  count := strm.read (headerbuffer [0], Sizeof (DWORD)) ;
  If count <> sizeof(DWORD) Then
    raise EBmpError.Create ('Premature End of Stream') ;

  count := strm.read (headerbuffer [1], headerbuffer [0] - sizeof (DWORD)) ;
  if count <> headerbuffer [0] - sizeof (DWORD) Then
    raise EBmpError.Create ('Premature End of Stream') ;

  Inc (bytesread, headerbuffer [0]) ;

  color_table_size := 0 ;
  If (headerbuffer [0] = sizeof (BITMAPCOREHEADER)) Then
    GetOs2Header
  Else If (headerbuffer [0] >= sizeof (BITMAPINFOHEADER)) Then
    GetWindowsHeader
  Else
   Raise EBmpError.Create ('Invalid header size') ;

  // Allocate storage for the image.
  If (height >= 0) Then
    Begin
    image.setSize (width, height) ;
    imagesize := width * height ;
    End
  Else
    Begin
    image.setSize (width, -height) ;
    imagesize := width * -height ;
    End ;

  // It is poss ible to have a file where the image data does not
  // immediately follow the color map (or headers). If there is
  // padding we skip over it.

  If (bytesread > fileheader.bfOffBits) Then
    Raise EBmpError.Create ('Corrupt File') ;

  SkipBytes (fileheader.bfOffBits - bytesread) ;

  Case bitcount Of
    1, 2, 4:
      if compression = BI_RGB Then
        readFractionalByteData
      else
        readRle4 ;
    8:
      if compression = BI_RGB Then
        read8BitData
      else
        readRle8 ;
    16:
      Begin
      Get16BitMasks ;
      read16BitData ;
      End ;
    24:
      read24BitData ;
    32:
      Begin
      Get32BitMasks ;
      read32BitData ;
      End ;
    End ;

  callProgressFunction (imagesize) ;
  End ;

Procedure TBmpDecoder.readImageFile (filename : String ;image : TBitmapImage) ;
  Var
    Stream : TFileStream ;
  Begin
  stream := TFileStream.Create (filename, fmOpenRead Or fmShareDenyWrite) ;
  try
    readImage (stream, image) ;
  finally
    Stream.Destroy ;
    End ;
  End ;

End .





