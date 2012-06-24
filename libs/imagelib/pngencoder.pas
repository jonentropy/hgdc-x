unit pngencoder;
//
// Copyright (c) 1997,1998 Colosseum Builders, Inc.
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
//  Title: PNG Encoder Class Definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class implements PNG encoding.
//

interface

Uses pngpvt, bitmapimage, deflateencoder, pngoutputstream ;

Const
  FILTERBUFFERCOUNT = 5 ;

Type

  TPngEncoder = Class (TBitmapImageEncoder)
    protected
      row_width : Cardinal ;
      title_string : String ;
      author_string : String ;
      description_string : String ;
      copyright_string : String ;
      software_string : String ;
      disclaimer_string : String ;
      warning_string : String ;
      source_string : String ;
      comment_string : String ;

      filter_buffers : Array [0..FILTERBUFFERCOUNT - 1] of Array of Byte ;
      current_filter : TPngFilterType ;
      filter_mask  : Cardinal ;
      filter_width  : Cardinal ;
      row_buffer : Array of Byte ;
      deflate_encoder : TDeflateEncoder ;

      use_alpha_channel : Boolean ;

      processing_image : Boolean ;


      Procedure filterRow ;
      Procedure doWrite (outputstream : TPngOutputStream ; image : TBitmapImage) ;
      Procedure writeImageData (outputstream : TPngOutputStream ; image : TBitmapImage) ;

      Procedure writeText (outputstream : TPngOutputStream ; keyword, value : String) ;
      Procedure writeTextBlocks (outputstream : TPngOutputStream) ;

      Procedure callProgressFunction (percent : Cardinal) ;

      Procedure setCompressionLevel (value : TCompressionLevel) ;
      Function getCompressionLevel : TCompressionLevel ;

      Procedure setUseFilters (value : boolean) ;
      Function getUseFilters : Boolean ;

      Function getBlockSize : Cardinal ;
      Procedure setBlockSize (value : Cardinal) ;

      Procedure setUseAlphaChannel (value : Boolean) ;

    public
      Constructor Create ;
      Destructor Destroy ; Override ;

      procedure writeImageFile (filename : String ; image : TBitmapImage) ; Override ;
      Procedure writeImage (strm : TPngOutputStream ; image : TBitmapImage) ;

      // Property functions for predefined tEXt strings.
      Property Title : String read title_string Write title_string ;
      Property Author : String read author_string Write author_string ;
      Property Description : String read description_string Write description_string ;
      Property Copyright : String read copyright_string write copyright_string ;
      Property Software : String read software_string Write software_string ;
      Property Disclaimer : String read disclaimer_string Write disclaimer_string ;
      Property Warning : String read warning_string write warning_string ;
      Property Source : String read source_string Write source_string ;
      Property Comment : String Read comment_string Write comment_string ;
      Property CompressionLevel : TCompressionLevel read getCompressionLevel Write setCompressionLevel ;
      Property UseFilters : Boolean Read getUseFilters Write setUseFilters ;
      Property BlockSize : Cardinal Read getBlockSize Write setBlockSize ;
      Property UseAlphaChannel : Boolean read use_alpha_channel Write setUseAlphaChannel ;

  End ;

  EPngError = Class (EGraphicsException) ;

implementation

Uses
  systemspecific, pngoutputfilestream ;

//
//  Description:
//
//    Class Default Constructor
//
Constructor TPngEncoder.Create ;
  Begin
  Inherited Create ;
  software_string := 'Colosseum Builders Image Library' ;
  filter_mask := 1 Shl Ord (FILTERNONE) ;
  use_alpha_channel := false ;
  processing_image := false ;
  deflate_encoder := TDeflateEncoder.Create ;
  End ;

Destructor TPngEncoder.Destroy ;
  Begin
  deflate_encoder.Destroy ; deflate_encoder := Nil ;
  End ;

//
//  Description:
//
//    This function writes an image to a PNG stream.
//
//  Parameters:
//    strm:  The output stream
//    image:  The image to output
//
Procedure TPngEncoder.writeImage (strm : TPngOutputStream ; image : TBitmapImage) ;
  Begin
  filter_mask := filter_mask OR (1 Shl Ord (FILTERNONE)) ; // Require the none filter.
  processing_image := true ;
  try
    doWrite (strm, image) ;
  Except
    On E : EDeflateError Do
      begin
      processing_image := false ;
      Raise EPngError.Create (e.Message) ;
      End ;
    Else
      Begin
      processing_image := false ;
      Raise ;
      End ;
    End ;
  processing_image := false ;
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
Procedure TPngEncoder.callProgressFunction (percent : Cardinal) ;
  Var
    cancel : Boolean ;
  Begin
  if Not Assigned (progress_function) Then
    Exit ;

  if percent > 100 Then
    percent := 100 ;

  cancel := false ;
  progress_function (Self, progress_data, 1, 1,
                     'PNG Encode', percent, cancel) ;
  End ;

//
//  Description:
//
//    This function creates a tEXt chunk.
//
//  Parameters:
//    keyword:  The chunk keyword
//    value:  The keyword value
//
Procedure TPngEncoder.writeText (outputstream : TPngOutputStream ; keyword, value : String) ;
  Begin
  if Length (keyword) > 79 Then
    Raise EPngError.Create ('tEXt Keyword Too Long') ;

  outputstream.startChunk ('tEXt') ;
  outputstream.write (@keyword [1], Length (keyword)) ;
  outputstream.writeByte (0) ;
  if Length (value) > outputstream.remainingBufferSpace Then
    Raise EPngError.Create ('tEXt Value Too Long') ;

  outputstream.write (@value [1], Length (value)) ;
  outputstream.endChunk ;
  End ;

//
//  Description:
//
//    This function outputs tEXt blocks for any keywords
//    that have values assigned to them.
//
//  Parameters:
//
//    outputstream : The stream to write to
//
Procedure TPngEncoder.writeTextBlocks (outputstream : TPngOutputStream) ;
  Begin
  if title_string <> '' Then
    writeText (outputstream, 'Title', title_string) ;
  if author_string <> '' Then
    writeText (outputstream, 'Author', author_string) ;
  if description_string <> '' Then
    writeText (outputstream, 'Description', description_string) ;
  if copyright_string <> '' Then
    writeText (outputstream, 'Copyright', copyright_string) ;
  if software_string <> '' Then
    writeText (outputstream, 'Software', software_string) ;
  if disclaimer_string <> '' Then
    writeText (outputstream, 'Disclaimer', disclaimer_string) ;
  if warning_string <> '' Then
    writeText (outputstream, 'Warning', warning_string) ;
  if source_string <> '' Then
    writeText (outputstream, 'Source', source_string) ;
  if comment_string <> '' Then
    writeText (outputstream, 'Comment', comment_string) ;
  End ;

//
//  Description:
//
//    This function sets the compression level used.  The compression level
//    determines the depth to which hash chains are searched.
//
//  Parameters:
//
//    value:  The new compression level
//
Procedure TPngEncoder.setCompressionLevel (value : TCompressionLevel) ;
  Begin
  deflate_encoder.CompressionLevel := value ;
  End ;

//
//  Description:
//
//    The function returns the current compression level.
//
//  Return Value:
//
//    The compresion level.
//
Function TPngEncoder.getCompressionLevel : TCompressionLevel ;
  Begin
  result :=  deflate_encoder.CompressionLevel ;
  End ;

//
//  Description:
//
//    This function performs the actual output of the image.
//
//  Parameters:
//
//    outputstream : The stream to write the image to
//    image : The image to output
//
Procedure TPngEncoder.doWrite (outputstream : TPngOutputStream ; image : TBitmapImage) ;
  var
    ii : Cardinal ;
    header : TPngImageHeader ;
  Begin
  if use_alpha_channel Then
    Begin
    row_width := 4 * image.Width ;
    filter_width := 4 ;
    End
  else
    Begin
    row_width := 3 * image.Width ;
    filter_width := 3 ;
    End ;
  SetLength (row_buffer, row_width) ;

  for ii := 0 To FILTERBUFFERCOUNT - 1 Do
    Begin
    SetLength (filter_buffers [ii], row_width) ;
    End ;

  for ii := 0 to row_width - 1 Do
    filter_buffers [Ord (FILTERNONE)][ii] := 0 ;

  // Fill in the image header.
  header.width := SystemToBigEndianLong (image.Width) ;
  header.height := SystemToBigEndianLong (image.Height) ;
  header.bitdepth := 8 ;
  if use_alpha_channel Then
    header.colortype := Ord (RGBAlpha)
  else
    header.colortype := Ord (RGB) ;

  header.compressionmethod := 0 ;
  header.filtermethod := 0 ;
  header.interlacemethod := 0 ;

  // Output the PNG Signature and header.
  outputstream.writeRaw (PngSignature, PngSignatureSize) ;
  outputstream.startChunk ('IHDR') ;
  outputstream.write (@header, sizeof (header)) ;
  outputstream.endChunk ;

  // Output any text blocks with keywords defined.
  writeTextBlocks (outputstream) ;

  writeImageData (outputstream, image) ;

  // Write the IEND marker.
  outputstream.startChunk ('IEND') ;
  outputstream.endChunk ;
  End ;

//
//  Description:
//
//    This function filters an image row.
//
//  Implicit Inputs
//
//      row_buffer contains the row to filter
//      filter_buffers [FILTERNONE] contains the last row
//
Procedure TPngEncoder.filterRow ;
  Var
    ii, jj : Cardinal ;
    mask : Cardinal ;
    last, above, lastabove : Byte ;
    run, longestrun : Cardinal ;
  Begin
  mask := (1 Shl Ord (FILTERNONE)) ;

  // Filter for each type in the filter_mask.
  if ((filter_mask And (1 Shl Ord (FILTERSUB))) <> 0) Then
    Begin
    mask := mask Or (1 Shl Ord (FILTERSUB)) ;

    for ii := 0  To row_width - 1 Do
      Begin
      if ii >= filter_width Then
        last := row_buffer [ii - filter_width]
      else
        last := 0 ;

      filter_buffers [Ord (FILTERSUB)][ii] := row_buffer [ii] - last ;
      End ;
    End ;

  if (filter_mask And (1 Shl Ord (FILTERUP))) <> 0 Then
    Begin
    mask := mask Or (1 Shl Ord (FILTERUP)) ;

    for ii := 0 To row_width - 1 Do
      Begin
      filter_buffers [Ord (FILTERUP)][ii] := row_buffer [ii] - filter_buffers [Ord (FILTERNONE)][ii] ;
      End ;
    End ;

  if (filter_mask And (1 Shl Ord (FILTERAVERAGE))) <> 0 Then
    Begin
    mask := mask Or (1 Shl Ord (FILTERAVERAGE)) ;

    for ii := 0  To row_width - 1 Do
      Begin
      if ii >= filter_width Then
        last := row_buffer [ii - filter_width]
      else
        last := 0 ;
      above := filter_buffers [Ord (FILTERNONE)][ii] ;

      filter_buffers [ord (FILTERAVERAGE)][ii] := row_buffer [ii] - (above + last) Div 2 ;
      End ;
    End ;

  if filter_mask And (1 Shl Ord (FILTERPAETH)) <> 0 Then
    Begin
    mask := mask Or (1 Shl Ord (FILTERPAETH)) ;

    for ii := 0 To row_width - 1 Do
      Begin
      if ii >= filter_width Then
        Begin
        last := row_buffer [ii-filter_width] ;
        lastabove := filter_buffers [Ord (FILTERNONE)][ii - filter_width] ;
        End
      else
        Begin
        last := 0 ;
        lastabove := 0 ;
        End ;
      above := filter_buffers [Ord (FILTERNONE)][ii] ;
      filter_buffers [Ord (FILTERPAETH)][ii] := row_buffer [ii] - PaethPredictor (last, above, lastabove) ;
      End ;
    End ;


  // Filter None
  // THIS MUST BE THE LAST FILTER!!!!!!!!!! We save the value
  // here to be used in the next call with the filters that require data from the
  // previous row.

  for ii := 0 To row_width - 1 Do
    filter_buffers [Ord (FILTERNONE)][ii] := row_buffer [ii] ;

  // If we only performed FilterNone then we do not need to proceed
  // any further.
  current_filter := FILTERNONE ;
  if mask = (1 Shl Ord (FILTERNONE)) Then
    Exit ;

  // Find the best filter. We do a simple test for the
  // longest runs of the same value.
  longestrun := 0 ;
  for ii := 0 To FILTERBUFFERCOUNT - 1 Do
    Begin
    if (mask And (1 Shl ii)) <> 0 Then
      Begin
      run := 0 ;
      for jj := 4 To row_width - 1 Do
        Begin
        if (filter_buffers [ii][jj] = filter_buffers [ii][jj-1])
            And (filter_buffers [ii][jj] = filter_buffers [ii][jj-2])
            And (filter_buffers [ii][jj] = filter_buffers [ii][jj-3])
            And (filter_buffers [ii][jj] = filter_buffers [ii][jj-4]) Then
          Begin
          Inc (run) ;
          End ;
        End ;

      if run > longestrun Then
        Begin
        current_filter := TPngFilterType (ii) ;
        longestrun := run ;
        End ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    This function specifies whether or not filters are used.
//
//  Parameters:
//
//    value: true=>Use Filters, false=>Don't use filters
//
Procedure TPngEncoder.setUseFilters (value : Boolean) ;
  Begin
  if value Then
    filter_mask := $FFFFFFFF
  else
    filter_mask := 1 Shl Ord (FILTERNONE) ;
  End ;

//
//  Description:
//
//   This function tells if filters are being used.
//
//  Return Value:
//    true=>Filters are being used
//    false=>Filters are not being used
//
Function TPngEncoder.getUseFilters : Boolean ;
  Begin
  if filter_mask = $1 Then
    Result := false
  else
    Result := true ;
  End ;

//
//  Description:
//
//    This function writes the image data to the output stream.
//
//  Parameters:
//
//    outputstream : The stream to write to
//    image : The image to output
//
Procedure TPngEncoder.writeImageData (outputstream : TPngOutputStream ;
                                      image : TBitmapImage) ;
  Var
    ii, jj, pixel, col : Cardinal ;
    filter : BYTE ;
  Begin
  outputstream.startChunk ('IDAT') ;
  deflate_encoder.startCompressedStream (outputstream) ;
  callProgressFunction (0) ;
  Pixel := 0 ;
  for ii := 0 To image.Height - 1 Do
    Begin
    Col := 0 ;
    for jj := 0 To image.Width - 1 Do
      Begin
      row_buffer [col] := image.pixels [pixel].red ; Inc (col) ;
      row_buffer [col] := image.pixels [pixel].green ; Inc (col) ;
      row_buffer [col] := image.pixels [pixel].blue ; Inc (col) ;
      if use_alpha_channel Then
        Begin
        row_buffer [col] := image.pixels [pixel].alpha ;
        Inc (col) ;
        End ;
      Inc (pixel) ;
      End ;
    filterRow ;
    filter := Ord (current_filter) ;
    deflate_encoder.compressData (outputstream,
                                  @filter,
                                  sizeof (filter)) ;
    deflate_encoder.compressData (outputstream,
                                  @filter_buffers [Ord (current_filter)][0],
                                  row_width) ;
    callProgressFunction (100 * ii Div image.Height) ;
    End ;
  deflate_encoder.endCompressedStream (outputstream) ;
  outputstream.endChunk ;
  callProgressFunction (100) ;
  End ;

//
//  Description:
//
//    This function writes a image to a PNG file.
//
//  Parameters:
//
//    filename : The name of the output file
//    image : The image to compress
//
Procedure TPngEncoder.writeImageFile (filename : String ; image : TBitmapImage) ;
  Var
    outputstream : TPngOutputFileStream ;
  Begin
  outputstream := TPngOutputFileStream.Create ;
  Try
    outputstream.open (filename) ;
  writeImage (outputstream, image) ;
  Finally
    outputstream.Destroy ;
    End ;
  End ;
//
//  Description:
//
//    This function defines the block size used by the Deflate encoder.
//
//  Parameters:
//
//    value : The new block size
//
Procedure TPngEncoder.setBlockSize (value : Cardinal) ;
  Begin
  deflate_encoder.BlockSize := value ;
  End ;

Function TPngEncoder.getBlockSize : Cardinal ;
  Begin
  Result := deflate_encoder.BlockSize ;
  End ;

Procedure TPngEncoder.setUseAlphaChannel (value : Boolean) ;
  Begin
  if processing_image Then
    Raise EPngError.Create ('Attempt to change parameters during compression') ;

  use_alpha_channel := value ;
  End ;

end.
