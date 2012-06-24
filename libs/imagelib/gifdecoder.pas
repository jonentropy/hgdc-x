unit gifdecoder;
//
// Copyright (c) 2005 Colosseum Builders, Inc.
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
// Description:
//
//   TGifDecoder is a class for decoding GIF images into a TBitmapImage
//   object.
//
//   This decoder supports the GIF format as defined in
//   "GRAPHICS INTERCHANGE FORMAT", CompuServe, July 31, 1990.
//   Markers of the form #N reference section N of this document.
//
//  Author: John M. Miano - miano@colosseumbuilders.com
//  Date:  June 10, 2005
//
interface

Uses
  gifinputstream, bitmapimage, gif, systemspecific ;

Type
  GifColorTable = Array [0..255] of GifColorTableEntry ;

  DictionaryTree = Record
     data : UBYTE1 ;
     Parent : Cardinal ;
     End ;

  TGifDecoder = class (TBitmapImageDecoder)
    Private
      global_color_table : GifColorTable ;
      global_color_count : Cardinal ;
      screen_width : Cardinal ;
      screen_height : Cardinal ;
      more_images : Boolean ;
      verbose_flag : Boolean ;

      multi_image_mode : Boolean ;

      // Values to indicate the presence of a transparent color.
      transparent_flag : Boolean ;
      transparent_color : Cardinal ;

      // Position of last image on the logical screen.
      image_position_top, image_position_left : Cardinal ;


      // We have to save a reference to the input stream
      // because GIF allows multiple images per file.
      input_stream : TGifInputStream ;
      Procedure Reset ;
      Procedure ReadToNextImage ;
      Procedure callProgressFunction (pass, passcount : Cardinal ; progress : Cardinal) ;
    Protected
      Procedure SetMultiImageMode (state : Boolean) ;
    Public
      Constructor Create ;
      Destructor Destroy ; Override ;
      Procedure readImage (inputstream : TGifInputStream ; image : TBitmapImage) ;
      Procedure readNextImage (image: TBitmapImage) ;
      Procedure readImageFile (filename : String ; image : TBitmapImage) ; Override ;
      Property MoreImages : Boolean read more_images ;
      Property Verbose : Boolean read verbose_flag write verbose_flag ;
      Property MultiImageMode : Boolean read multi_image_mode write SetMultiImageMode ;
      Function ActiveInputStream : Boolean ;
      Procedure CloseInputSTream ;
      Property ImagePositionTop : Cardinal read image_position_top ;
      Property ImagePositionLeft : Cardinal read image_position_left ;
    End ;

    EGifError = Class (EGraphicsException) ;

implementation

Uses
  gifInputfilestream ;

Constructor TGifDecoder.Create ;
  Begin
  Inherited Create ;
  Reset ;
  End ;

Destructor TGifDecoder.Destroy ;
  Begin

  Inherited Destroy ; // Final Step
  End ;

Procedure TGifDecoder.Reset ;
  Begin
  more_images := false ;
  transparent_flag := false ;
  input_stream := Nil ;
  End ;

//
//  Function to read the first image from the input stream.
//
//  Parameters:
//    inputstream : the stream to read the image from.
///   image : the image to read to.
//
Procedure TGifDecoder.readImage (inputstream : TGifInputStream ; image : TBitmapImage) ;

  // This nested function reads the fixed area at the start of the input stream.
  Procedure ReadStreamHeader ;
    Var
      header : GifHeader ;
      screen : GifScreenDescriptor ;
      count : Cardinal ;
      II : Integer ;
    Begin
    count := inputstream.read (header, sizeof (header)) ;
    if count <> sizeof (header) then
      Raise EGifError.Create ('GIF: Cannot read GIF header') ;

    if verbose_flag then
      Print (header) ;

    if (header.file_signature [0] <> 'G')
       OR (header.file_signature [1] <> 'I')
       OR (header.file_signature [2] <> 'F')
       OR (header.file_version [0] <> '8')
       OR ((header.file_version [1] <> '7') AND (header.file_version [1] <> '9'))
       OR (header.file_version [2] <> 'a') Then
      Raise EGifError.Create ('Not a GIF stream') ;

    count := inputstream.read (screen, sizeof (screen)) ;
    if count <> sizeof (screen) then
      Raise EGifError.Create ('Cannot read GIF screen descriptor.') ;

    screen_width := screen.logical_screen_width ;
    screen_height := screen.logical_screen_height ;
    if verbose_flag then
      Print (screen) ;

    if GlobalColorTableFlag (screen) Then
      Begin
      global_color_count := GlobalColorTableSize (screen) ;
      For ii := 0 to global_color_count - 1 Do
        Begin
        Count := inputstream.read (global_color_table [ii], sizeof (global_color_table [ii])) ;
        if count <> sizeof (global_color_table [ii]) then
          Raise EGifError.Create ('Error reading global color table in GIF file.') ;
        End ;
      End
    Else
      Begin
      global_color_count := 0 ;
      End ;
    End ; // ReadImageHeader


  Begin // Read Image

  if Assigned (input_stream) Then
    Raise EGifError.Create ('GIF: Already have open input stream.') ;

  Reset ;
  ReadStreamHeader ;

  // We read the stream header through the parameter. We only save
  // the stream once we know it is good for something.
  input_stream := inputstream ;
  ReadToNextimage ;
  if Not more_images then
    Raise EGifError.Create ('GIF: No image in input stream.') ;

  ReadNextImage (image) ;
  End ;

//
//  This function reads the first image from a GIF file.
//
//  Parameters:
//    filename : the input file name
//    image : the image to write to.
//
Procedure TGifDecoder.readImageFile (filename : String ; image : TBitmapImage) ;
  Var
    input : TGifInputFileStream ;
  Begin
  if Assigned (input_stream) then
    raise EGifError.Create ('GIF: Already have open input stream') ;

  input := TGifInputFileStream.Create (0) ;
  Try
    input.open (filename) ;
    readImage (input, image) ;
  Finally
    if Not MultiImageMode and Assigned (input_stream) Then
      Begin
      if input_stream <> input then
        Raise EGifError.Create ('GIF: Internal Error - Corrupt input stream.') ;
      CloseInputStream ;
      End ;
    End ;
  End ;

//
//  This procedure reads the next image in a GIF stream.
//
//  This procedure expects readToNextImage to have been called immediately
//  before this function.
//
Procedure TGifDecoder.ReadNextImage (image : TBitmapImage) ;
  Var
    // Descriptor for the image being decoded.
    id : GifImageDescriptor ;
    // Color table for the image being decode. Either the local color table
    // or a copy of the global color table.
    isinterlaced : boolean ;

    ct : GifColorTable ;
    colorcount : cardinal ; // Size of the color table.
    count : Cardinal ;
    ii : Integer ;

    pass : Cardinal ; // Interlace lace

    firstcharacter : UBYTE1 ; // The first character in the last code.

    rowptr, colptr : Cardinal ; // Input output pointers.

    // mincodesize (read from the input stream) is the maximum number
    // of bit needed to represent literals in the input stream.
    mincodesize : Cardinal ;
    codesize : Cardinal ;  // Current code size
    clearcode : Cardinal ; // The code to reset the decompressor.
    endcode : Cardinal ;  // The code to mark the end of the compressed stream.
    nextcode : Cardinal ;  // The next available dynamic code.

    // The LZW dictionary.
    Dictionary : Array [0..(1 SHL GifMaxBitsPerCode) - 1] of DictionaryTree ;


  // This nexted function writes a data value to the output image.
  Procedure OutputToImage (data : UBYTE1) ;
    // All of these constants are used to support interlaced files.
    const
      maxpasses = 4 ;
      // We have one extra entry here for debugging. If
      passstart : Array [0..maxpasses] of Cardinal = (0, 4, 2, 1, High (Cardinal)) ;
      passcount : Array [0..maxpasses - 1] of Cardinal = (8, 8, 4, 2) ;
      passrows : Array [0..maxpasses - 1] of Cardinal = (8, 4, 2, 1) ;
    var
      offset : cardinal ;
      limit : cardinal ;
      ii : cardinal ;
    Begin
    if rowptr >= image.height then
      Raise EGifError.Create ('GIF: Corrupt input stream. Compressed data past image.') ;

    if isinterlaced Then
      Begin
      limit := image.width * image.height ; // Mark the end of the image.

      offset := rowptr * image.width + colptr ; // Locate the current pixel.
      ii := 0 ;
      while (ii < passrows [pass]) And (offset < limit) do
        Begin // Here we fillin the pixel and all the pixels between the
              // interlaced rows.
        image.pixels [offset].red := ct [data].red ;
        image.pixels [offset].green := ct [data].green ;
        image.pixels [offset].blue := ct [data].blue ;
        if transparent_flag and (data = transparent_color) then
          image.pixels [offset].alpha := $00
        else
          image.pixels [offset].alpha := $FF ;
        inc (ii) ;
        inc (offset, image.width) ; // Advance to the next row.
        End ;
      inc (colptr) ;
      if colptr = image.width then
        Begin
        colptr := 0 ;
        inc (rowptr, passcount [pass]) ;
        callProgressFunction (pass + 1, maxpasses, (100 * rowptr) div image.height) ;
        if (rowptr >= image.height) then
          begin
          inc (pass) ;

          // Handle something stupid like a one-pixel high interlaced image.
          while (passstart [pass] >= image.height) And (pass < maxpasses) do
            inc (pass) ;
          rowptr := passstart [pass] ;
          End ;
        End ;
      end
    Else
      Begin // Noninterlaced file.
      offset := rowptr * image.width + colptr ;
      image.pixels [offset].red := ct [data].red ;
      image.pixels [offset].green := ct [data].green ;
      image.pixels [offset].blue := ct [data].blue ;
      if transparent_flag and (data = transparent_color) then
        image.pixels [offset].alpha := $00
      else
        image.pixels [offset].alpha := $FF ;

      inc (colptr) ;
      if colptr = image.width then
        begin
        inc (rowptr) ;
        callProgressFunction (1, 1, (100 * rowptr) div image.height) ;
        colptr := 0 ;
        end ;
      end ;
    End ;

  // This function outputs the data for a single code in the
  // the compressed stream.
  Procedure Outputcode (code : Cardinal) ;
    Var
      SP : Cardinal ;
      Stack : Array [1..(1 SHL GifMaxBitsPerCode)] of UBYTE1 ;
      lastcode : Cardinal ;
    Begin

    // Push the values on the stack in reverse order.
    sp := 0 ;
    repeat
      Begin
      inc (sp) ;
      Stack [sp] := dictionary [code].data ;
      lastcode := code ;
      code := dictionary [code].parent ;
      End
    until lastcode < clearcode ;

    firstcharacter := stack [sp] ;

    // Pop the values form the stack in the correct order.
    While sp > 0 Do
      Begin
      OutputToImage (stack [sp]) ;
      dec (sp) ;
      End ;
    End ;

  // This nested function initializes the decompressor. It gets
  // called at the start of decompression and whenever the input
  // stream contains a CLEAR CODE.
  Procedure Initialize ;
    Var
      ii : Integer ;
      limit : integer ;
    Begin
    // Initialize the LZW Dictionary
    Limit := (1 SHL mincodesize) - 1 ;
    For ii := 0 to Limit do
      Begin
      Dictionary [ii].data := ii ;
      Dictionary [ii].parent := 0 ;
      End ;
    for II := Limit + 1 To High (Dictionary) Do
      Begin
      // Load with junk to aid debugging.
      Dictionary [ii].data := $FF ;
      Dictionary [ii].parent := $FFFF ;
      End ;

    // Generate values based upon the code size
    codesize := mincodesize + 1 ;  // Initial code size
    clearcode := 1 Shl mincodesize ;  // Reset decompressor code
    endcode := clearcode + 1 ; // End of compressed stream code
    nextcode := endcode + 1 ;  // Next free code
    End ;

  // This nested function decompresses the compressed GIF image data.
  Procedure Decompress ;
    Var
      Code, lastcode : Cardinal ;
    Begin
    // Get the first code in the stream. A clear code can come anywhere and
    // some compressors like to put them at the start of the stream.
    code := input_stream.nextCode (codesize) ;
    While code = clearcode do
      code := input_stream.nextCode (codesize) ;
    OutputCode (code) ;

    While True Do
      Begin
      lastcode := code ;
      // See if we need to increase the code size.
      if (nextcode >= (1 SHL codesize)) And (codesize < GifMaxBitsPerCode) then
        inc (codesize) ;

      code := input_stream.nextCode (codesize) ;
      If code = endcode Then
        Begin
        Exit ;  // Endcode => all done.
        End
      Else if Code = clearcode then
        Begin
        // Clearcode => Reset the compressor and start over.
        Initialize ;
        code := input_stream.nextCode (codesize) ;
        While code = clearcode Do
          code := input_stream.nextCode (codesize) ;
        If code = endcode then
          Exit ;
        OutputCode (code) ;
        End
      Else if Code < nextcode then
        Begin
        // A code that has already been defined.
        OutputCode (code) ;
        dictionary [nextcode].parent := lastcode ;
        dictionary [nextcode].data := firstcharacter ;
        inc (nextcode) ;
        End
      Else
        Begin
        // Special Case of undefined code.
        dictionary [nextcode].parent := lastcode ;
        dictionary [nextcode].data := firstcharacter ;
        inc (nextcode) ;
        OutputCode (code) ;
        End ;
      End ;
    End ;

  Procedure UseGlobalColorTable ;
    var
      ii : Cardinal ;
    Begin
    for II := Low (global_color_table) To global_color_count - 1 Do
      ct [ii] := global_color_table [ii] ;
    End ;
  Begin // ReadNextImage

  if Not Assigned (input_stream) Then
     Raise EGifError.Create ('GIF: No input stream') ;

  if NOT more_images then
    Raise EGifError.Create ('GIF: No more images in input stream.') ;

  if input_stream.read (id, sizeof (id)) <> sizeof (id) Then
    Raise EGifError.Create ('Error reading GIF image descriptor') ;
  If verbose_flag Then
    Print (id) ;

  // Save the image position so the user can retrieve it later to position
  // the image on the logical screen.
  image_position_top := id.top_position ;
  image_position_left := id.left_position ;

  // Interlaced handling.
  isinterlaced := InterLaceFlag (id) ;
  pass := 0 ;

  // Determine if this image uses the local color table.
  if LocalColorTableFlag (id) And (ColorTableSize (id) <> 0) Then
    Begin
    // Use a local color table.

    colorcount := 1 Shl (ColorTableSize (Id) + 1) ;
    input_stream.read (ct [0], sizeof (ct [0]) * colorcount) ;

    For ii := 0 to colorcount - 1 Do
        Begin
        Count := input_stream.read (ct [ii], sizeof (ct [ii])) ;
        if count <> sizeof (ct [ii]) then
          Raise EGifError.Create ('Error reading local color table in GIF file.') ;
        End ;
    End
  Else
    Begin
    // No local color table.

    // If there is no global color table as well then we're in trouble.
    if global_color_count = 0 then
      Raise EGifError.Create ('GIF image contains no color table') ;
    UseGlobalColorTable ;
    End ;

  // Allocate the image buffer.
  image.setSize (id.image_width, id.image_height) ;

  // Initialize the output position in the image.
  rowptr := 0 ;
  colptr := 0 ;

  // The minimum code size comes before the compressed data.
  mincodesize := input_stream.getByte ;

  // Read the comrpessed data.
  Initialize ;
  input_stream.EnterBitMode ;
  Decompress ;
  input_stream.ExitBitMode ;

  // Get in a position where the user can read the next image in the stream.
  if MultiImageMode Then
    ReadToNextImage ;

  End ; // ReadNextImage ;

Procedure TGifDecoder.ReadToNextImage ;
  Var
    blocktype : Cardinal ;
    controllabel : Cardinal ;
    size : Cardinal ;
    graphiccontrolbblock : GifGraphicControlBlock ;
    applicationblock : GifApplicationBlock ;
    textblock : GifPlainTextBlock ;
    buffer : Array [0..255] of char ;
  Begin

  While input_stream.MoreData Do
    Begin
    blocktype := input_stream.getByte ;
    Case blocktype Of
      GifTrailer:
        Begin
        more_images := false ;
        Exit ;
        End ;
      GifImageSeparator:
         Begin
         more_images := true ;
         Exit ;
         End ;
      GifExtension:
        Begin
        controllabel := input_stream.GetByte ;
        Case controllabel Of
          GifPlainTextExtension:
            Begin
            size := input_stream.read (textblock, sizeof (textblock)) ;
            if size <> sizeof (textblock) then
              raise EGifError.Create ('GIF: Cannot read Plain Text Extension Block') ;
            if textblock.block_size <> (sizeof (textblock) - sizeof (textblock.block_size)) then
              Raise EGifError.Create ('Corrupt GIF file - wrong size in GIF extension') ;
            if verbose_flag then
              Print (textblock) ;
            size := input_stream.getByte ;
            while (size <> 0) And input_stream.MoreData Do
              Begin
              input_stream.read (buffer [0], size) ;
              size := input_stream.getByte ;
              End ;
            if size <> 0 then
              Raise EGifError.Create ('GIF: Cannot read Plaint Text data') ;
            End ;
          GifGraphicControlExtension:
            Begin
            input_stream.read (graphiccontrolbblock, sizeof (graphiccontrolbblock)) ;
            if graphiccontrolbblock.block_size <> (sizeof (graphiccontrolbblock) - sizeof (graphiccontrolbblock.block_size)) then
              Raise EGifError.Create ('Corrupt GIF file - wrong size in Graphic Control Block') ;
            if verbose_flag then
              Begin
              Print (graphiccontrolbblock) ;
              End ;
            blocktype := input_stream.GetByte ;
            if blocktype <> 0 then
              raise EGifError.Create ('Corrupt GIF file - missing terminator') ;

            transparent_flag := TransparentColorFlag (graphiccontrolbblock) ;
            transparent_color := graphiccontrolbblock.transparent_color ;
            End ;
          GifCommentExtension:
            Begin
            Size := input_stream.getByte ;
            input_stream.read (buffer, size) ;
            if verbose_flag then
              Begin
              WriteLn ('{ Comment Extension') ;
              WriteLn (buffer) ;
              WriteLn ('}') ;
              End
            End ;
          GifApplicationExtension:
            Begin
            input_stream.read (applicationblock, sizeof (applicationblock)) ;
            if applicationblock.block_size <> 11 then
              raise EGifError.Create ('Corrupt GIF file - invalid Application Extension Length') ;
            if verbose_flag then
              Begin
              WriteLn ('{ Application Extension') ;
              WriteLn ('    Application Name: ', applicationblock.application_name) ;
              WriteLn ('    Application ID: ', applicationblock.authentication_code [0], ' ',
                        applicationblock.authentication_code [1], ' ',
                        applicationblock.authentication_code [2]) ;
              WriteLn ('}') ;
              End ;
            size := input_stream.getByte ;
            while (size <> 0) And input_stream.MoreData Do
              Begin
              input_stream.read (buffer [0], size) ;
              size := input_stream.getByte ;
              End ;
            if size <> 0 then
              Raise EGifError.Create ('GIF: Cannot read Application Extension data') ;
            End ;
          Else
            Raise EGifError.Create ('GIF: Invalid GIF Extension') ;
          End ;
        End ;
      Else
        Raise EGifError.Create ('Invalid GIF block') ;
      End ;
    End ;

  // Reached the end of stream without the GifTrailer marker.
  Raise EGifError.Create ('Missing GIF Trailer marker') ;
  End ; // ReadToNextImage


Procedure TGifDecoder.SetMultiImageMode (state : Boolean) ;
  Begin
  if Assigned (Input_stream) then
    Raise EGifError.Create ('GIF: Cannot change multi-image mode with active input stream.') ;

  multi_image_mode := state ;
  End ;

Function TGifDecoder.ActiveInputStream : Boolean ;
  Begin
  Result := Assigned (input_stream) ;
  End ;

Procedure TGifDecoder.CloseInputSTream ;
  Begin
  if Not Assigned (input_stream) then
    Exit ;

  try
    input_stream.destroy ;
  finally
    input_stream := Nil ;
    end ;
  End ;

Procedure TGifDecoder.callProgressFunction (pass, passcount : Cardinal ; progress : Cardinal) ;
  Var
    abort : Boolean ;
    percent : Cardinal ;
  Begin
  if (Not Assigned (progress_function)) Then
    Exit ;

  abort := false ;
  if (progress > 100) Then
    percent := 100
  else
    percent := progress ;

  progress_function (Self,
                     progress_data,
                     pass,
                     passcount,
                     'GIF Decode',
                     percent,
                     abort) ;
  if (abort) Then
    Raise EGraphicsAbort.Create ('');
  End ;


End.