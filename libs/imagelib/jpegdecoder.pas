unit jpegdecoder;
//
// Copyright (c) 1999, 2001 Colosseum Builders, Inc.
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
// Descripton:
//
//   TJpegDecoder class. This class decodes JPEG (JFIF) files using
//   Baseline, Sequential and Progressive formats.
//
interface

Uses sysutils, bitmapimage, jpeginputstream,
     jpegpvt, jpeghuffmandecoder, jpegdecoderdataunit ;

Type
  EJpegBadStream = Class (Exception) ;
  EJpegError = Class (Exception) ;

  TJpegDecoderComponent = class ;

  TJpegDecoder = Class (TBitmapImageDecoder)
    private
      // Frame (image) dimensions
      frame_height, frame_width : Cardinal ;
      // The image dimensions in MCUs
      mcu_rows, mcu_cols : Cardinal ;
      // Set to true when processing a progressive frame.
      is_progressive : Boolean ;

      // Flags set determine if markers have been found.
      eoi_found, sof_found : Boolean ;

      // The current restart interval.
      restart_interval : Cardinal ;

      // Maximum sampling frequencies among all components.
      max_horizontal_frequency, max_vertical_frequency : JPEGSAMPLINGFREQUENCY ;

      progressive_frame : Boolean ;
      use_filters : Boolean ;

      component_count : Cardinal ;
      components : Array [1..JPEGMAXCOMPONENTSPERFRAME] of TJpegDecoderComponent ;
      component_indices : Array [1..JPEGMAXCOMPONENTSPERFRAME] of Cardinal ;

      // Quantization tables defined.
      quantization_tables : Array [JPEGQUANTIZATIONTABLEID] of TJpegDecoderQuantizationTable ;

      // Hufman tables defined.
      ac_tables : Array [JPEGHUFFMANTABLEID] of TJpegHuffmanDecoder ;
      dc_tables : Array [JPEGHUFFMANTABLEID] of TJpegHuffmanDecoder ;


      // Address of the image that is currently being processed.
      current_image : TBitmapImage ;

      // Progress Counters
      current_pass, scan_count, pass_count : Cardinal ;
      in_progress : Boolean ;

      // MCU dimensions in pixels.
      mcu_height, mcu_width : Cardinal ;

      // Number components in the current scan and the components in the scan.
      scan_component_count : Cardinal ;
      scan_components : Array [1..JPEGMAXCOMPONENTSPERFRAME] of ^TJpegDecoderComponent ;

      // The next expected restart marker. Used to ensure restarts are in order.
      expected_restart : Cardinal ;

      // Flag to determine if strict JFIF is to be enforced.
      strict_jfif : Boolean ;

      // This flag is available to setXYZ functions to prevent the changing
      // of parameters during decompression that could screw things up.
      processing_image : Boolean ;

      verbose_flag : boolean ;

      Procedure callProgressFunction (description : String ; progress : Cardinal) ;
      Procedure resetDcDifferences ;
      Procedure processRestartMarker (inputstream : TJpegInputStream) ;
      Function scanIsInterleaved : Boolean ;
      Procedure freeAllocatedResources ;

    public

      Constructor Create ;
      Destructor Destroy ; Override ;
      property frameHeight : Cardinal read frame_height ;
      property frameWidth : Cardinal read frame_height ;
      property mcuRows : Cardinal read mcu_rows ;
      property mcuCols : Cardinal read mcu_cols ;
      property isProgressive : Boolean read is_progressive ;

      Procedure readImageFile (filename : String ; image : TBitmapImage) ; Override ;
      Procedure readImage (inputstream : TJpegInputStream ; image : TBitmapImage) ; Virtual ;

      Procedure setVerbose (value : boolean) ;
      Function getVerbose  : Boolean ;

      Property UseFilters : Boolean read use_filters write use_filters ;

      Procedure updateImage ; Override ;
    End ;

  TJpegDecoderComponent = Class
    Private
      // Jfif/Frame component ID
      component_id : Cardinal ; // Could Use type here

      // Sampling Frequencies
      horizontal_frequency : JPEGSAMPLINGFREQUENCY ;
      vertical_frequency : JPEGSAMPLINGFREQUENCY;

      // These values are the numnber of samples to take for each data
      // point. They come from the sampling frequencies and the maximum
      // sampling frequencies of all the components in the image.
      // sampling frequencies of all the components in the image.
      v_sampling : Cardinal ;
      h_sampling : Cardinal ;

      // Last encoded DC value.
      last_dc_value : Integer ;

      // Entropy tables used by the component.
      ac_table, dc_table : TJpegHuffmanDecoder ;

      // Quantization table used by the component
      quantization_table : TJpegDecoderQuantizationTable ;

      // End of band Run - Progressive Specific
      eob_run : Cardinal ;

      // Non-interleaved dimensions.
      noninterleaved_rows, noninterleaved_cols : Cardinal ;

      du_rows, du_cols : Cardinal ;
      data_units : Array of DecoderDataUnit ;
      upsample_data : Array of JPEGSAMPLE ;
      coefficient_blocks : Array of JpegCoefficientBlock ;
      image_height, image_width : Cardinal ;

      Procedure upSample1To1 (decoder : TJpegDecoder) ;
      Procedure blockFilterImage (decoder : TJpegDecoder) ;
      Procedure triangleFilterImage (decoder : TJpegDecoder) ;

    Public
      Constructor Create ;

      // We have made the color conversions static because RGB
      // conversion requires the data from three components.
      // Grayscale conversion is static strictly for consistency
      // with RGB.
      Class Procedure convertRgb (decoder : TJpegDecoder ;
                                  c1, c2, c3 : TJpegDecoderComponent ;
                                  image : TBitmapImage) ;
      Class Procedure convertCymk (decoder : TJpegDecoder ;
                                  c1, c2, c3, c4 : TJpegDecoderComponent ;
                                  image : TBitmapImage) ;
      Class Procedure convertGrayscale (decoder : TJpegDecoder ;
                                        cc : TJpegDecoderComponent ;
                                        image : TBitmapImage) ;

      Property horizontalFrequency : JPEGSAMPLINGFREQUENCY
                                   read horizontal_frequency
                                   write horizontal_frequency ;
      Property verticalFrequency : JPEGSAMPLINGFREQUENCY
                                   read vertical_frequency
                                   write vertical_frequency ;

      Procedure setQuantizationTable (table : TJpegDecoderQuantizationTable) ;
      Procedure allocateComponentBuffers (width, height : Cardinal ;
                                          maxhoriz, maxvert : JPEGSAMPLINGFREQUENCY ;
                                          isprogressive : Boolean) ;
      Procedure freeComponentBuffers () ;
      Procedure setHuffmanTables (dc, ac : TJpegHuffmanDecoder) ;
      Procedure upsampleImage (decoder : TJpegDecoder ; usefilter : Boolean) ;

      Procedure checkAcTable ;
      Procedure checkDcTable ;
      Procedure checkQuantizationTable ;

      Procedure decodeSequential (inputstream : TJpegInputStream ; mcurow, mcucol : Cardinal) ;
      Property noninterleavedRows : Cardinal Read noninterleaved_rows ;
      Property noninterleavedCols : Cardinal Read noninterleaved_cols ;
      Procedure resetDcDifference ;

      Procedure decodeDcFirst (inputstream : TJpegInputStream ;
                                row, col, ssa : Cardinal) ;
      Procedure decodeDcRefine (inputstream : TJpegInputStream ;
                                row, col, ssa : Cardinal) ;

      Procedure decodeAcFirst (inputstream : TJpegInputStream ;
                                row, col, sss, sse, ssa : Cardinal) ;

      Procedure decodeAcRefine (inputstream : TJpegInputStream ;
                                row, col, sss, sse, ssa : Cardinal) ;

      Procedure progressiveInverseDct  (decoder : TJpegDecoder) ;
  End ;

implementation

uses
  Jfif, jpeginputfile, inputbytestream, systemspecific ;

Const
  CHAR_BIT = 8 ;
  BUFFERSIZE = 2048 ;


//
//  Description:
//
//    Class Default Constructor
//
Constructor TJpegDecoder.Create ;
  Begin
  verbose_flag := false ;
  strict_jfif := false ;
  processing_image := false ;
  use_filters := false ;
  End ;



Destructor TJpegDecoder.Destroy ;
  Begin
  freeAllocatedResources ;
  Inherited Destroy ;
  End ;






//
//  Dimensions:
//
//    This function calls the progress function if it has
//    been supplied by the user.
//
//  Parameters:
//    progress: The progress percentage.
//
Procedure TJpegDecoder.callProgressFunction (description : String ; progress : Cardinal) ;
  Var
    abort : Boolean ;
    percent : Cardinal ;
    saved_pass : Cardinal ;
  Begin
  if (Not Assigned (progress_function)) Or in_progress Then
    Exit ;
  in_progress := true ;
  saved_pass := current_pass ; // Ensure nothing the user does changes this.
  abort := false ;
  if (progress > 100) Then
    percent := 100
  else
    percent := progress ;

  try
    progress_function (Self,
                       progress_data,
                       current_pass,
                       scan_count,
                       description,
                       percent,
                       abort) ;
  finally 
    in_progress := false ;
    current_pass := saved_pass ;
    end ;

  if (abort) Then
    Raise EGraphicsAbort.Create ('');
  End ;

      
        
        
        
        



//
//  Description:
//
//    This function resets the DC difference values for all components
//    for the current scan.
//
//    This function gets called before each scan is processed and
//    whenever a restart marker is read.
//

Procedure TJpegDecoder.resetDcDifferences ;
  Var
    ii : Integer ;
  Begin
  for ii := 1 To scan_component_count Do
    scan_components [ii]^.resetDcDifference ;
  End ;

//
//  Description:
//
//    This function reads a restart marker from the input stream.
//    It gets called byte functions that read scan data whenever
//    a restart marker is expected. An exception is raise if the
//    correct restart marker is not next in the input stream.
//
Procedure TJpegDecoder.processRestartMarker (inputstream : TJpegInputStream) ;
  var
    data : byte ;
  Begin
  inputstream.exitBitMode ;
  data := inputstream.getByte ;
  if (data <> $FF) Then
    raise EJpegBadStream.Create ('Missing Restart Marker') ;
  // According to E.1.2 0xFF can be used as a fill character
  // before the marker.
  while (data = $FF) Do
    data := inputstream.getByte () ;
  if (data < RST0) Or (data > RST7) Then
    raise EJpegBadStream.Create ('Missing Restart Marker') ;

  // Restart markers RST0..RST7 should come in sequence.
  if (($0F And data) <> expected_restart) Then
    raise EJpegBadStream.Create ('Incorrect Restart Marker') ;

  // Move the counter to the next restart marker
  Inc (expected_restart) ;
  expected_restart := expected_restart Mod 8 ;

  // Reset the DC coefficent differences to zero.
  resetDcDifferences () ;
  inputstream.enterBitMode (CHAR_BIT) ;
  End ;


Function TJpegDecoder.scanIsInterleaved : Boolean ;
  Begin
  If scan_component_count = 1 Then
    Result := false
  Else
    Result := true ;
  End ;

//
//  Description:
//
//    This function reads an image from a JPEG stream. The
//    stream needs to have been opened in binary mode.
//
//  Parameters:
//    istrm: Input stream
//    image: The output image
//
Procedure TJpegDecoder.readImage (inputstream : TJpegInputStream ; image : TBitmapImage) ;
  Const
    CR = 13 ;
    LF = 10 ;
  Var
    data : byte ;
  //
  //  Description:
  //
  //    This function reads the Start of Image Marker and the JFIF APP0
  //    marker that begin a JPEG file.
  //
  //    The JFIF standard states "The JPEG FIF APP0 marker is mandatory
  //     right after the SOI marker."
  //
  //    I have come across some JPEG files that have a COM marker between
  //    the SOI marker and APP0. This code will reject these non-conforming
  //    files.
  //

  Procedure readStreamHeader (inputstream : TJpegInputStream) ;
    Var
      header : JfifHeader ;
      ii : Cardinal ;
      count : Cardinal ;

    Procedure PrintJfifHeader ;
      Begin
      WriteLn ('{ Start Of Image }') ;
      WriteLn ('{ JFIF APP0 Marker') ;
      WriteLn ('  Length: ', BigEndianWordToSystem (header.length)) ;
      WriteLn ('  Version: ', header.version [1], '.', header.version [2]) ;
      // density unit = 0 => Only the aspect ratio is specified.
      // density unit = 1 => Density in pixels per inch.
      // density unit = 2 => Density in pixels per centimeter.
      Case header.units of
        0: WriteLn ('  Density Unit:  (aspect ratio)') ;
        1: WriteLn ('  Density Unit:  (pixels per inch)') ;
        2: WriteLn ('  Density Unit:  (pixels/cm)') ;
        else
          WriteLn ('   Density Unit: Unknown (', header.units, ')') ;
        End ;
      WriteLn ('  X Density: ', BigEndianWordToSystem (header.xdensity)); ;
      WriteLn ('  Y Density: ', BigEndianWordToSystem (header.xdensity)) ;
      WriteLn ('  Thumbnail Width: ', header.xthumbnail) ;
      WriteLn ('  Thumbnail Height: ', header.xthumbnail) ;
      WriteLn ('}') ;
      End ;

    begin
    if (inputstream.getByte <> SOB) Then
      raise EJpegBadStream.Create ('Missing SOI Marker') ;
    if (inputstream.getByte () <> SOI) Then
      raise EJpegBadStream.Create ('Missing SOI Marker') ;
    if (inputstream.getByte () <> SOB) Then
      raise EJpegBadStream.Create ('Missing JFIF APP0 Marker') ;
    case inputstream.getByte () Of
      APP0:
        Begin
        count := inputstream.read (header, sizeof (Header)) ;
        if count <> Sizeof (Header) Then
          raise EJpegBadStream.Create ('Premature end of file in JFIF header') ;

        if (header.identifier [1] <> 'J')
          Or (header.identifier [2] <> 'F')
          Or (header.identifier [3] <> 'I')
          Or (header.identifier [4] <> 'F') Then
          raise EJpegBadStream.Create ('Not a JFIF file') ;

        if verbose_flag Then
          PrintJfifHeader ;

        // Skip over any thumbnail data.
        for ii := sizeof (header) + 1 To BigEndianWordToSystem (header.length) Do
          inputstream.getByte ;
        End ;
      APP1:
        Begin
        count := inputstream.getBigEndianWord ;
        if (inputstream.getByte <> Ord ('E'))
          Or (inputstream.getByte <> Ord ('x'))
          Or (inputstream.getByte <> Ord ('i'))
          Or (inputstream.getByte <> Ord ('f')) Then
          raise EJpegBadStream.Create ('Not a Exif file') ;

        // Read the marker. 6 bytes have already been read.  
        for ii := 7 To count Do
          inputstream.getByte
        End ;
      End ;

    End ;

  //
  //  Description:
  //
  //    This function reads the next marker in the input
  //    stream. If the marker is followed by a data block
  //    this function dispatches a routine to read the
  //    data.
  //
  Procedure readMarker (inputstream : TJpegInputStream) ;
    var
      marker : byte ;
    //
    //  Description:
    //
    //    This method reads an application or comment marker
    //    from the input stream.
    //
    //  Parameters:
    //    type:  The marker type
    //
    Procedure readApplication (marker : Byte ; inputstream : TJpegInputStream) ;
      Const
        hex: Array [0..15] of Char = ('0', '1', '2', '3', '4', '5', '6', '7',
                                      '8', '9', 'A', 'B', 'C', 'D', 'E', 'F') ;

      var
        length : cardinal ;
        id : String ;
      Begin
      length := inputstream.getBigEndianWord () ;
      SetLength (id, length) ;
      inputstream.read (id [1], length- Sizeof (WORD)) ;

      if verbose_flag then
        begin
        if (marker = COM) then
          Begin
          WriteLn ('{ Comment Marker ') ;
          WriteLn ('  ', id) ;
          WriteLn ('}') ;
          End
        else
          Begin
          WriteLn ('{ APP', hex [marker And $0F], ' Marker') ;
          WriteLn ('Length: ', length) ;
          WriteLn ('ID: ', id) ;
          WriteLn ('}') ;
          End ;
        End ;
      End ;

    //
    //  Description:
    //
    //    The function reads a Define Huffman Table marker from the input
    //    stream.
    //
    Procedure readHuffmanTable (inputstream : TJpegInputStream) ;
      var
        length : Cardinal ;
        remaining : Cardinal ;
        data : Byte;
        tableclass, id : Cardinal ;
        table : TJpegHuffmanDecoder ;
      Begin
      // Section B.2.4.2

      if verbose_flag Then
        WriteLn ('{ Define Huffman Table') ;

      length := inputstream.getBigEndianWord ;
      if verbose_flag Then
        WriteLn ('  Length: ', length) ;
      remaining := length - sizeof (WORD) ;
      while (remaining > 0) Do
        Begin
        data := inputstream.getByte ;
        Dec (remaining) ;

        // Tc in standard 0=>DC, 1=>AC
        tableclass := data Shr 4 ;
        id := data And $F ; // Th in standard
        if (id > 3) Then
          raise EJpegBadStream.Create ('Huffman Table Index outside range [0..3]') ;
        if verbose_flag Then
          Begin
          WriteLn ('   Table Index ', id) ; ;
          if (tableclass = 0) Then
            WriteLn ('   Table Class: DC')
          else
            WriteLn ('   Table Class: AC') ;
          End ;

        if (tableclass <> 0) Then
          Begin
          if Not Assigned (ac_tables [id]) Then
            ac_tables [id] := TJpegHuffmanDecoder.Create ;
          table := ac_tables [id] ;
          End
        else
          Begin
          if Not Assigned (dc_tables [id]) Then
            dc_tables [id] := TJpegHuffmanDecoder.Create ;
          table := dc_tables [id] ;
          End ;

        // Read the table data into the table object
        Dec (remaining, table.readTable (inputstream)) ;

       if verbose_flag Then
          table.print ;
        End ;
      if verbose_flag Then
        Writeln ('}') ;
      End ;

    //
    //  Description:
    //
    //    This function reads a DQT marker from the input stream.
    //
    Procedure readQuantization (inputstream : TJpegInputStream) ;
      Var
        length : Word ;
        data : Byte ;
        remaining : Integer ;
        precision : cardinal ;
        index : Cardinal ;
      Begin
      // Defined in Section B.2.4.1

      length := inputstream.getBigEndianWord ;

      // Maintain a counter for the number of bytes remaining to be read in
      // the quantization table.
      remaining := length - sizeof (length) ;

      if verbose_flag Then
        Begin
        WriteLn ('{ Define Quantization Table') ;
        WriteLn ('  Length: ', length) ;
        End ;
      while (remaining > 0) Do
        Begin
        data := inputstream.getByte ;
        Dec (remaining) ;
        precision := data Shr 4 ;    // Pq in standard
        index := data And $F ;      // Tq in standard

        if (index >= JPEGMAXQUANTIZATIONTABLES) Then
          raise EJpegBadStream.Create ('Quantization Table Index Too Large') ;

        if verbose_flag Then
          Begin
          WriteLn ('  Table Index: ', index) ;
          WriteLn ('  Table Precision: ', precision) ;
          End ;

        Case precision Of
          1:
            Dec (remaining, sizeof(Word) * JPEGSAMPLESIZE) ;
          0:
            Dec (remaining, sizeof (Byte) * JPEGSAMPLESIZE) ;
          Else
            Raise EJpegBadStream.Create ('Invalid TableSize') ;
          End ;

        // Read the table data into the table object
        if Not Assigned (quantization_tables [index]) Then
          quantization_tables [index] := TJpegDecoderQuantizationTable.Create ;
        quantization_tables [index].readTable (inputstream, precision) ;

        if verbose_flag Then
          Begin
          WriteLn ('  Table Values: ') ;
          quantization_tables [index].print ;
          WriteLn ('}') ;
          End ;
        End ;
      End ;

    //
    //  Description:
    //
    //    This function reads a define restart interval marker
    //    from the input stream.
    //
    Procedure readRestartInterval (inputstream : TJpegInputStream) ;
      Var
        length : Cardinal ;
      Begin
      // Section B.2.4.4

      length := inputstream.getBigEndianWord ;
      if (length <> 4) Then
        raise EJpegBadStream.Create ('Invalid length for restart marker') ;

      restart_interval := inputstream.getBigEndianWord ;
      if verbose_flag Then
        Begin
        WriteLn ('{ Define Restart Interval') ;
        WriteLn ('  Length:  ', length) ; // Should be 4
        WriteLn ('  Interval: ', restart_interval) ;
        WriteLn ('}') ;
        End ;
      End ;

    //
    //  Description:
    //
    //    The function reads a start of frame marker from the input stream.
    //
    //  Parameters:
    //    type:  The marker type for the frame
    //
    Procedure readStartOfFrame (inputstream : TJpegInputStream ; marker : Cardinal) ;
      Var
        length : Cardinal ;
        dataprecision : Cardinal ;
        ii : cardinal ;
        id, qtable : Cardinal ;
        data : Byte ;
      //
      //  Description:
      //
      //    This function determines for non-interlaced scans:
      //
      //     o The dimensions in pixels for an MCU
      //     o The number of MCU rows and columns needed to encode the scan.
      //
      Procedure calculateMcuDimensions ;
        Begin
        mcu_height := max_vertical_frequency * JPEGSAMPLEWIDTH ;
        mcu_width := max_horizontal_frequency * JPEGSAMPLEWIDTH ;
        mcu_rows := (frame_height + mcu_height - 1) Div mcu_height ;
        mcu_cols := (frame_width + mcu_width - 1) Div mcu_width ;
        End ;

      Begin
      if (marker = SOF2) Then
        progressive_frame := true
      else
        progressive_frame := false ;

      // Section B.2.2
      // Read in the image dimensions
      length := inputstream.getBigEndianWord ;
      dataprecision := inputstream.getByte ;  // P in standard
      if (dataprecision <> 8) Then
        raise EJpegBadStream.Create ('Only 8-bit data supported') ;

      frame_height := inputstream.getBigEndianWord ;            // Y in standard
      frame_width := inputstream.getBigEndianWord ;             // X in standard
      component_count := inputstream.getByte ;   // Nf in standard

      if (component_count <> 1) And (component_count <> 3)  And (component_count <> 4) Then
        raise EJpegBadStream.Create ('JFIF only supports 1 and 3 component streams') ;

      if verbose_flag Then
        Begin
        WriteLn ('{ Start Of Frame ') ;
        Case marker of
          SOF0:  WriteLn ('  Baseline') ;
          SOF1:  WriteLn ('  Sequential') ;
          SOF2:  WriteLn ('  Progressive') ;
          Else   WriteLn ('  Unknown Frame Type') ;
          End ;


        WriteLn ('  Length: ', length) ;
        WriteLn ('  Precision: ', dataprecision) ;
        WriteLn ('  Height: ', frame_height) ;
        WriteLn ('  Width: ', frame_width) ;
        WriteLn ('  Component Count: ', component_count) ;
        End ;

      if (length <> (component_count * 3 + 8)) Then
        raise EJpegBadStream.Create ('Invalid Frame Size') ;

      // Rread the component descriptions
      max_horizontal_frequency := 1 ;
      max_vertical_frequency := 1 ;
      for ii := 1 To component_count Do
        Begin
        ID := inputstream.getByte ;  // Ci in standard

        // While JPEG does not put these restrictions on component IDs
        // the JFIF standard does.
        if (strict_jfif) Then
          Begin
          if (component_count = 1) And (ID <> 1) Then
            raise EJpegBadStream.Create ('Component ID not 1')
          else if (ID <> ii) Then
            raise EJpegBadStream.Create ('Invalid Component ID or ID out of order') ;
          End ;

        component_indices [ii] := ID ;

        data := inputstream.getByte ;
        if Not Assigned (components [ID]) Then
          components [ID] := TJpegDecoderComponent.Create ;
        components [ID].horizontalFrequency := data Shr 4 ; // Hi in standard
        components [ID].verticalFrequency := data And $F ;  // Vi in standard
        qtable := inputstream.getByte ;  // Tqi in standard
        if (qtable >= JPEGMAXQUANTIZATIONTABLES) Then
          raise EJpegBadStream.Create ('Bad Quantization Table Index') ;
        if Not Assigned (quantization_tables [qtable]) Then
          quantization_tables [qtable] := TJpegDecoderQuantizationTable.Create ;
        components [ID].setQuantizationTable (quantization_tables [qtable]) ;

        // Keep track of the largest values for horizontal and vertical
        // frequency.
        if (components [ID].horizontalFrequency > max_horizontal_frequency) Then
          max_horizontal_frequency := components [ID].horizontalFrequency ;

        if (components [ID].verticalFrequency > max_vertical_frequency) Then
          max_vertical_frequency := components [ID].verticalFrequency ;

        if verbose_flag Then
          Begin
          WriteLn ('   Component ', ID) ;
          WriteLn ('   Horizontal Frequency: ', components [ID].horizontalFrequency) ;
          WriteLn ('   Vertical Frequency: ', components [ID].verticalFrequency) ;
          WriteLn ('   Quantization Table: ', qtable) ;
          End ;
        End ;

      calculateMcuDimensions ;

      // Allocate storage for the image.
      current_image.setSize (frame_width, frame_height) ;

      if verbose_flag Then
        WriteLn ('}') ;

      sof_found := true ;
      if scan_count <> 0 then
        Begin
        pass_count := scan_count ;
        if progressive_frame Then
          Inc (pass_count, component_count + 1)
        else
          Inc (pass_count, component_count + 1) ;
        End ;
      End ;

    //
    //  Description:
    //
    //    This function reads a start of scan marker and the scan data
    //    following the marker.
    //
    Procedure readStartOfScan (inputstream : TJpegInputStream) ;
      Var
        ii : Cardinal ;
        length : Cardinal ;
        componentID : Cardinal ;
        rb : Cardinal ;
        ssa : Cardinal ;
        actable, dctable : Cardinal ;
        spectralselectionstart, spectralselectionend : COEFFICIENTINDEX ;
        successiveapproximationhigh, successiveapproximationlow : SUCCESSIVEAPPROXIMATION ;
      //
      //  Description:
      //
      //    This function reads the scan data for progressive scans.
      //
      //    All we do here is determine if we are processing a DC
      //    scan (sss==sse==0) or AC scan and if we are processing
      //    the first scan for the spectral selection (sah==0) or
      //    subsequent scan.
      //
      //  Parameters:
      //    sss: Spectral Selection Start (0..63)
      //    sse: Spectral Selection End (sse..63)
      //    sah: Successive Approximation High
      //    sal: Successive Approximation Low
      //
      Procedure readProgressiveScanData (inputstream : TJpegInputStream ;
                                         sss, sse : COEFFICIENTINDEX ;
                                         sah, sal : SUCCESSIVEAPPROXIMATION) ;
        //
        //  Description:
        //
        //    This funtion reads the scan data for the first DC scan for
        //    one or more components.
        //
        //  Parameters:
        //
        //    ssa:  Successive Approximation
        //
        Procedure readDcFirst (inputstream : TJpegInputStream ;
                               ssa : SUCCESSIVEAPPROXIMATION) ;
          Const
            OPERATION = 'Interleaved Progressive DC Scan' ;
          Var
            restartcount : Cardinal ;
            mcurow, mcucol : Cardinal ;
            cc, cx, cy : Cardinal ;
            durow, ducol : Cardinal ;
            row, col : Cardinal ;
          Begin
          resetDcDifferences ;
          restartcount := 0 ;

          If (scanIsInterleaved) Then
            Begin
            For mcurow := 0 To mcu_rows - 1 Do
              Begin
              callProgressFunction (OPERATION, mcurow * 100 Div mcu_rows) ;
              For mcucol := 0 To mcu_cols - 1 Do
                Begin
                If (restart_interval <> 0) And (restart_interval = restartcount) Then
                  Begin
                  resetDcDifferences  ;
                  processRestartMarker (inputstream) ;
                  restartcount := 0 ;
                  End ;
                For cc := 1 To scan_component_count Do
                  Begin
                  For cy := 0 To scan_components [cc]^.verticalFrequency - 1 Do
                    Begin
                    durow := Cardinal (scan_components [cc]^.verticalFrequency) * mcurow + cy ;
                    for cx := 0 To scan_components [cc]^.horizontalFrequency - 1 Do
                      Begin
                      ducol := Cardinal (scan_components [cc]^.horizontalFrequency) * mcucol + cx ;
                      scan_components [cc]^.decodeDcFirst (inputstream, durow, ducol, ssa) ;
                      End ;
                    End ;
                  End ;
                Inc (restartcount) ;
                End ;
              End ;
              callProgressFunction (OPERATION, 100) ;
            End
          else
            Begin
            for row := 0 To scan_components [1]^.noninterleavedRows - 1 Do
              Begin
              callProgressFunction (OPERATION, row * 100 Div scan_components [1]^.noninterleavedRows) ;
              for col := 0 To scan_components [1]^.noninterleavedCols - 1 Do
                Begin
                if (restart_interval <> 0) And (restart_interval = restartcount) Then
                  Begin
                  resetDcDifferences ;
                  processRestartMarker (inputstream) ;
                  restartcount := 0 ;
                  End ;
                scan_components [1]^.decodeDcFirst (inputstream, row, col, ssa) ;
                Inc (restartcount) ;
                End ;
              callProgressFunction (OPERATION, 100) ;
              End ;
            End ;
          End ; // End Procedure

        //
        //  Description:
        //
        //    This function reads the scan data for a refining DC scan.
        //
        //  Parameters:
        //    ssa:  The successive approximation value for this scan.
        //
        Procedure readDcRefine (inputstream : TJpegInputStream ;
                                ssa : SUCCESSIVEAPPROXIMATION) ;
          Const
            OPERATION = 'Progressive DC Scan' ;
          Var
            restartcount : Cardinal ;
            mcurow, mcucol : Cardinal ;
            cc, cx, cy : Cardinal ;
            durow, ducol : Cardinal ;
            row, col : Cardinal ;
          Begin
          resetDcDifferences ;
          restartcount := 0 ;

          if (scanIsInterleaved) Then
            Begin
            for mcurow := 0 To mcu_rows - 1 Do
              Begin
              callProgressFunction (OPERATION, mcurow * 100 Div mcu_rows) ;
              for mcucol := 0 To mcu_cols - 1 Do
                Begin
                if (restart_interval <> 0) And (restart_interval = restartcount) Then
                  Begin
                  resetDcDifferences ;
                  processRestartMarker (inputstream) ;
                  restartcount := 0 ;
                  End ;
                for cc := 1 To scan_component_count Do
                  Begin
                  for cy := 0 To scan_components [cc]^.verticalFrequency - 1 Do
                    Begin
                    durow := Cardinal (scan_components [cc]^.verticalFrequency) * mcurow + cy ;
                    for cx := 0 To scan_components [cc]^.horizontalFrequency - 1 Do
                      Begin
                      ducol := Cardinal (scan_components [cc]^.horizontalFrequency) * mcucol + cx ;

                      scan_components [cc]^.decodeDcRefine (inputstream, durow, ducol, ssa) ;
                      End ;
                    End ;
                  End ;
                Inc (restartcount)
                End ;
              End ;
            callProgressFunction (OPERATION, 100) ;
            End
          else
            Begin
            for row := 0 To scan_components [1]^.noninterleavedRows - 1 Do
              Begin
              callProgressFunction (OPERATION, row * 100 Div scan_components [1]^.noninterleavedRows) ;
              for col := 0 To scan_components [1]^.noninterleavedCols - 1 Do
                Begin
                if (restart_interval <> 0) And (restart_interval = restartcount) Then
                  Begin
                  resetDcDifferences ;
                  processRestartMarker (inputstream) ;
                  restartcount := 0 ;
                  End ;
                scan_components [1]^.decodeDcRefine (inputstream, row, col, ssa) ;
                Inc (restartcount)
                End ;
              End ;
            callProgressFunction (OPERATION, 100) ;
            End ;
          End ; // End Procedure

        //
        //  Description:
        //
        //    This function reads the scan data for the first AC scan for a
        //    component. Progressive scans that read AC data cannot be
        //    interleaved.
        //
        //  Parameters:
        //    sss:  Spectral Selection Start
        //    sse:  Spectral Selection End
        //    ssa:  Spectral Selection
        //

        Procedure readAcFirst (inputstream : TJpegInputStream ;
                               sss, sse : COEFFICIENTINDEX ;
                               ssa : SUCCESSIVEAPPROXIMATION) ;
          Const
            OPERATION = 'Progressive AC Scan' ;
          Var
            restartcount : Cardinal ;
            row, col : Cardinal ;
          Begin
          resetDcDifferences ;

          restartcount := 0 ;
          for row := 0 To scan_components [1]^.noninterleavedRows - 1 Do
            Begin
            callProgressFunction (OPERATION, row * 100 Div scan_components [1]^.noninterleavedRows) ;
            for col := 0 To scan_components [1]^.noninterleavedCols - 1 Do
              Begin
              if (restart_interval <> 0) And (restart_interval = restartcount) Then
                Begin
                resetDcDifferences ;
                processRestartMarker (inputstream) ;
                restartcount := 0 ;
                End ;
              scan_components [1]^.decodeAcFirst (inputstream,
                                                  row, col,
                                                  sss, sse,
                                                  ssa) ;
              Inc (restartcount) ;
              End ;
            End ;
          callProgressFunction (OPERATION, 100) ;
          End ;

        //
        //  Description:
        //
        //    This function reads the scan data for a refining AC scan for a
        //    component. Progressive scans that read AC data cannot be
        //    interleaved.
        //
        //  Parameters:
        //    sss:  Spectral Selection Start
        //    sse:  Spectral Selection End
        //    ssa:  Spectral Selection
        //

        Procedure readAcRefine (inputstream : TJpegInputStream ;
                                sss, sse : COEFFICIENTINDEX ;
                                ssa : SUCCESSIVEAPPROXIMATION) ;
          Const
            OPERATION = 'Progressive AC Scan' ;
          Var
            restartcount : Cardinal ;
            row, col : Cardinal ;
          Begin
          resetDcDifferences ;

          restartcount := 0 ;
          for row := 0 To scan_components [1]^.noninterleavedRows - 1 Do
            Begin
            callProgressFunction (OPERATION, row * 100 Div scan_components [1]^.noninterleavedRows) ;
            for col := 0 To scan_components [1]^.noninterleavedCols - 1 Do
              Begin
              if (restart_interval <> 0) And (restart_interval = restartcount) Then
                Begin
                resetDcDifferences ;
                processRestartMarker (inputstream) ;
                restartcount := 0 ;
                End ;
              scan_components [1]^.decodeAcRefine (inputstream,
                                                   row, col,
                                                   sss, sse,
                                                   ssa) ;
              Inc (restartcount) ;
              End ;
            End ;
          callProgressFunction (OPERATION, 100) ;
          End ; // End Procedure

        Begin

        if sss = 0 Then
          Begin
          if sse <> 0 Then
            raise EJpegBadStream.Create ('Progressive scan contains DC and AC data') ;

          if sah = 0 Then
            readDcFirst (inputstream, sal)
          else
            readDcRefine (inputstream, sal) ;
          End
        else
          Begin
          if sah = 0 Then
            readAcFirst (inputstream, sss, sse, sal)
          else
            readAcRefine (inputstream, sss, sse, sal) ;
          End ;
        End ;

      //
      //  Parameters:
      //
      //    The function reads the scan data for a sequential scan. All
      //    we do here is determine whether or not we have an interleaved
      //    or non-interleaved scan then call a function that handles
      //    the scan type.

      Procedure readSequentialScanData (inputstream : TJpegInputStream) ;
        //
        //  Description:
        //
        //    This function reads the scan data for an interleaved scan.
        //

        Procedure readSequentialInterleavedScan (inputstream : TJpegInputStream) ;
          Const
            OPERATION = 'Sequential Interleaved Scan' ;
          var
            restartcount : Cardinal ;
            mcurow, mcucol : Cardinal ;
            cc, cx, cy : Cardinal ;
            durow, ducol : Cardinal ;
          Begin
          resetDcDifferences ;

          restartcount := 0 ;
          for mcurow := 0 To mcu_rows - 1 Do
            Begin
            callProgressFunction (OPERATION, mcurow * 100 Div mcu_rows) ;
            for mcucol := 0 To mcu_cols - 1 Do
              Begin
              if (restart_interval <> 0) And (restart_interval = restartcount) Then
                Begin
                processRestartMarker (inputstream) ;
                restartcount := 0 ;
                End ;
              for cc := 1 To scan_component_count Do
                Begin
                for cy := 0 To scan_components [cc]^.verticalFrequency - 1 Do
                  Begin
                  durow := Cardinal (scan_components [cc]^.verticalFrequency) * mcurow + cy ;
                  for cx := 0 To scan_components [cc]^.horizontalFrequency - 1 Do
                    Begin
                    ducol := Cardinal (scan_components [cc]^.horizontalFrequency) * mcucol + cx ;
                    scan_components [cc]^.decodeSequential (
                                               inputstream,
                                               durow,
                                               ducol) ;
                    End ;
                  End ;
                End ;
              Inc (restartcount)
              End ;
            End ;
          callProgressFunction (OPERATION, 100) ;
          End ;

        //
        //  Description:
        //
        //    This function reads the scan data for a non-interleaved scan.
        //

        Procedure readSequentialNonInterleavedScan (inputstream : TJpegInputStream ) ;
          Const
            OPERATION = 'Sequential Noninterleaved Scan' ;
          Var
            restartcount : Cardinal ;
            row, col : Cardinal ;
          Begin
          restartcount := 0 ;
          resetDcDifferences ;
          for row := 0 To scan_components [1]^.noninterleavedRows - 1 Do
            Begin
            callProgressFunction (OPERATION, row * 100 Div scan_components [1]^.noninterleavedRows) ;
            for col := 0 To scan_components [1]^.noninterleavedCols - 1 Do
              Begin
              if (restart_interval <> 0) And (restart_interval = restartcount) Then
                begin
                processRestartMarker (inputstream) ;
                restartcount := 0 ;
                End ;
              scan_components [1]^.decodeSequential (inputstream, row, col) ;
              Inc (restartcount) ;
              End ;
            End ;
          callProgressFunction (OPERATION, 100) ;
          End ;

        begin
        expected_restart := 0 ;
        If (scanIsInterleaved) Then
          readSequentialInterleavedScan (inputstream)
        Else
          readSequentialNonInterleavedScan (inputstream) ;
        End ;

      Begin
      if (Not sof_found) Then
        raise EJpegBadStream.Create ('Scan found before frame defined') ;

      // Section B.2.3

      length := inputstream.getBigEndianWord ;
      if verbose_flag Then
        Begin
        WriteLn ('{ Start Of Scan ') ;
        WriteLn ('  Length:  ', length) ;
        End ;

      scan_component_count := inputstream.getByte ;  // Ns in standard
      if (scan_component_count > 4) Or (scan_component_count < 1) Then
        raise EJpegBadStream.Create ('Invalid component count in scan') ;

      for ii := 1 To scan_component_count Do
        Begin
        componentID := inputstream.getByte ;  // Csi in standard

        scan_components [ii] := @components [componentID] ;
        // If the horizontal frequency is zero then the component was not
        // defined in the SOFx marker.
        if (scan_components [ii]^.horizontalFrequency = 0) Then
          raise EJpegBadStream.Create ('Component Not Defined') ;

        rb := inputstream.getByte () ;
        actable := rb And $0F ;
        dctable := rb Shr 4 ;

        scan_components [ii]^.setHuffmanTables (
                                  dc_tables [dctable],
                                  ac_tables [actable]) ;
        if verbose_flag Then
          Begin
          WriteLn ('  Component ID: ', componentID) ;
          WriteLn ('  DC Entropy Table: ', dctable)  ;
          WriteLn ('  AC Entropy Table: ', actable) ;
          End ;
        End ;

      spectralselectionstart := inputstream.getByte () ; // Ss in standard
      spectralselectionend := inputstream.getByte ()  ;  // Se in standard

      ssa := inputstream.getByte ;
      successiveapproximationhigh := ssa Shr 4 ;  // Ah in standard
      successiveapproximationlow := ssa And $0F ; // Al in standard

      if verbose_flag Then
        Begin
        WriteLn (' Spectral Selection Start: ', spectralselectionstart) ;
        WriteLn (' Spectral Selection End: ', spectralselectionend) ;
        WriteLn (' Successive Approximation High: ', successiveapproximationhigh) ;
        WriteLn (' Successive Approximation Low: ', successiveapproximationlow)  ;
        WriteLn ('}') ;
        End ;


      for ii := 1 To scan_component_count Do
        Begin
        if (progressive_frame) Then
          Begin
          scan_components [ii]^.checkQuantizationTable ;
          if (spectralselectionstart = 0) Then
            scan_components [ii]^.checkDcTable
          else
            scan_components [ii]^.checkAcTable ;
          End
        else
          Begin
          scan_components [ii]^.checkQuantizationTable ;
          scan_components [ii]^.checkDcTable ;
          scan_components [ii]^.checkAcTable ;
          End ;

        scan_components [ii]^.allocateComponentBuffers (frame_width,
                                                        frame_height,
                                                        max_horizontal_frequency,
                                                        max_vertical_frequency,
                                                        progressive_frame) ;
        End ;

      Inc (current_pass) ;
      inputstream.enterBitMode (CHAR_BIT) ;
      if (progressive_frame) Then
        Begin
        readProgressiveScanData (inputstream,
                                 spectralselectionstart,
                                 spectralselectionend,
                                 successiveapproximationhigh,
                                 successiveapproximationlow) ;
        End
      else
        Begin
        readSequentialScanData (inputstream) ;
        End ;
      inputstream.exitBitMode ;
      End ;

    Begin
    while (inputstream.moreData) Do
      Begin
      marker := inputstream.getByte () ;
      case marker Of
        SOB:
        // According to E.1.2, 0xFF is allowed as fill when a
        // marker is expected.
        ;
        SOI:

          if verbose_flag then
            Begin
            Writeln ('{ Start Of Image }') ;
            exit ; // SOI has no data.
            End ;
        DQT:
          Begin
          readQuantization (inputstream) ;
          Exit ;
          End ;
        DHP:
          raise EJpegBadStream.Create ('DHP marker not supported') ;

        // The only difference between a Sequential DCT Frame
        // (SOF0) and an extended Sequential DCT Frame (SOF1)
        // is that a baseline frame may only have 2 DC and 2 AC
        // Huffman tables per scan (F.1.2) and and extended
        // Sequential Frame may have up to 4 DC and 4 AC Huffman
        // tables per scan (F.1.3). Both are decoded identically
        // for 8-bit precision. Extended Sequential frames may
        // use 12-bit precision (F, Table B.2) which we do not
        // support.
        SOF0, SOF1, SOF2:
          Begin
          readStartOfFrame (inputstream, marker) ;
          Exit ;
          End ;
        SOF3:
          raise EJpegBadStream.Create ('Lossless Huffman Coding Not Supported') ;
        SOF5:
          raise EJpegBadStream.Create ('Differential Sequential Huffman Coding Not Supported') ;
        SOF6:
          raise EJpegBadStream.Create ('Differential Progressive Huffman Coding Not Supported') ;
        SOF7:
          raise EJpegBadStream.Create ('Differential Lossless Huffman Coding Not Supported') ;

          // These are markers for frames using arithmetic coding.
          // Arithmetic coding is covered by patents so we ignore
          // this type.
        SOF9, SOFA, SOFB, SOFD, SOFE, SOFF:
          raise EJpegBadStream.Create ('Cannot read image - Arithmetic Coding covered by patents') ;
        DHT:
          Begin
          readHuffmanTable (inputstream) ;
          Exit ;
          End ;
        SOS:
          Begin
          readStartOfScan (inputstream) ;
          Exit ;
          End ;
        DRI:
          Begin
          readRestartInterval (inputstream) ;
          Exit ;
          End ;
        EOI:
          Begin
          eoi_found := true ;
          if verbose_flag Then
            WriteLn ('{ End Of Image }') ;
          Exit ;
          End ;
        APP0, APP1, APP2, APP3,
        APP4, APP5, APP6, APP7,
        APP8, APP9, APPA, APPB,
        APPC, APPD, APPE, APPF, COM:
          Begin
          readApplication (marker, inputstream) ;
          Exit;
          End ;
        Else
          Begin
          // We call ReadByte to make sure the problem
          // is not a premature EOF.
          inputstream.getByte ;
          raise EJpegBadStream.Create ('Unknown, unsupported, or reserved marker encountered') ;
          End ;
        End
      End ;
    raise EJpegBadStream.Create ('Premature end of file') ;
    End ;

  //
  //  Description:
  //
  //    This function scans a stream and counts the number of scans.  This
  //    allows an exact count for a progress function.
  //
  Procedure getScanCount (inputstream : TJpegInputStream) ;
    Var
      startpos : Integer ;
      data : Byte ;
      endfound : boolean ;
    Begin
    // Save the stream position so we can go back
    // when we are finished.
    startpos := inputstream.tellg ;

    // Count the number of SOS markers.
    scan_count := 0 ;
    endfound := false ;
    while inputstream.moreData And Not endfound Do
      Begin
      data := inputstream.getByte ;
      if (data = SOB) Then
        Begin
        while data = SOB Do
          data := inputstream.getByte ;
        if (data = SOS) Then
          Inc (scan_count)
        else if (data = EOI) Then
          endfound := True ;
        End ;
      End ;
    // Go back to where we were in the stream.
    inputstream.seekg (startpos) ;
    End ;

  Begin
  current_pass := 0 ;
  scan_count := 0 ;
  pass_count := 0 ;
  current_image := image ;
  in_progress := false ;

  if Assigned (progress_function) Then
    getScanCount (inputstream) ;

  restart_interval := 0 ;  // Clear the restart interval ;
  try
    Begin
    processing_image := true ;
    current_image.clearImage ;
    eoi_found := false ;
    sof_found := false ;

    // Read the required SOI and APP0 markers at the start of the image.
    readStreamHeader (inputstream) ;

    data := inputstream.getByte ;
    while (inputstream.moreData And Not eoi_found) Do
      Begin
      if (data = SOB) Then
        Begin
        readMarker (inputstream) ;
        if Not eoi_found Then
          Begin
          data := inputstream.getByte ;
          if (Not inputstream.moreData ) Then
            raise EJpegBadStream.Create ('Premature end of file') ;
          End ;
        End
      Else
         Begin
         data := inputstream.getByte ;
         End ;
      End ;
    End
  Except
    On EGraphicsAbort Do
      Begin
      freeAllocatedResources ;
      current_image := Nil ;
      End
    Else
      Begin
      updateImage ;
      freeAllocatedResources ;
      current_image := Nil ;
      processing_image := false ;
      Raise ;
      End ;
    End ;

  updateImage ;
  processing_image := false ;

  // Some people say we should not have this check here. If it bothers you
  // remove it.
  if (Not eoi_found) Then
    raise EJpegBadStream.Create('End of Image Marker Not Found') ;

  // We do no want an exception so do not call ReadByte ()
  // Sometimes there can be trailing end of record markers.
  inputstream.read (data, sizeof (data)) ;
  while ((data = CR) Or (data = LF)) And inputstream.moreData Do
    data := inputstream.getByte ;

  if (inputstream.moreData) Then
    raise EJpegBadStream.Create ('Extra Data After End of Image Marker') ;

  freeAllocatedResources ;
  current_image := Nil ;
  End ;


//
//  Description:
//

//    This function writes the image data that has been read so
//    far to the image. This function gets called after reading
//    the entire image stream.  The user can also call this function
//    from a progress function to display progressive images,
//    multi-scan sequential images, or just to display the image
//    as it is being read (slow).
//
Procedure TJpegDecoder.updateImage ;
  Var
    ii : Cardinal ;
  Begin
  if Not Assigned (current_image) Then
    Raise EJpegError.Create ('Not reading an image') ;
  // Make sure we have read at least one scan.
  if (current_pass > 0) Then
    Begin
    if (progressive_frame) Then
      Begin
      for ii := 1 To component_count Do
        Begin
        Inc (current_pass) ;
        components [component_indices [ii]].progressiveInverseDct (self) ;
        Inc (current_pass) ;
        components [component_indices [ii]].upsampleImage (self, use_filters) ;
        End ;
      End
    else
      Begin
      for ii := 1 To component_count Do
        Begin
        Inc (current_pass) ;
        components [component_indices [ii]].upsampleImage (self, use_filters) ;
        End ;
      End ;

    Inc (current_pass) ;
    Case (component_count) Of
      4:
        TJpegDecoderComponent.convertCymk (self,
                                          components [component_indices [1]],
                                          components [component_indices [2]],
                                          components [component_indices [3]],
                                          components [component_indices [4]],
                                          current_image) ;
      3:
        TJpegDecoderComponent.convertRgb (self,
                                          components [component_indices [1]],
                                          components [component_indices [2]],
                                          components [component_indices [3]],
                                          current_image) ;
      1:
        TJpegDecoderComponent.convertGrayscale (
                               self,
                               components [component_indices [1]],
                               current_image) ;
      End ;
    End;
  End ;

//
//  Description:
//
//    This function frees all the memory dynamically allocated
//    during the image decoding process.
//
Procedure TJpegDecoder.freeAllocatedResources ;
  Var
    ii : Integer ;
  Begin
  If (current_pass > 0) Then
    Begin
    for ii := 1 To component_count Do
      Begin
      if components [component_indices [ii]] <> Nil Then
        components [component_indices [ii]].Destroy ;
      components [component_indices [ii]] := Nil ;
      End ;
    End ;

  For ii := Low (JPEGHUFFMANTABLEID) To High (JPEGHUFFMANTABLEID) Do
    Begin
    If Assigned (ac_tables [ii]) Then
      Begin
      ac_tables [ii].Destroy ; ac_tables [ii] := Nil ;
      End ;
    If Assigned (dc_tables [ii]) Then
      Begin
      dc_tables [ii].Destroy ; dc_tables [ii] := Nil ;
      End ;
    End ;
  For ii := Low (JPEGQUANTIZATIONTABLEID) To High (JPEGQUANTIZATIONTABLEID) Do
    Begin
    If Assigned (quantization_tables [ii]) Then
      Begin
      quantization_tables [ii].Destroy ;
      quantization_tables [ii] := Nil ;
      End ;
    End ;
  End ;



Procedure TJpegDecoder.readImageFile (filename : String ;
                                      image :  TBitmapImage) ;
  Var
    inputstream : TJpegInputFileStream ;
//    JpegInputMapStream inputstream ;
  Begin
  inputstream := TJpegInputFileStream.Create (BUFFERSIZE) ;
  try
    try
      inputstream.open (filename) ;
      readImage (inputstream, image) ;
    Except On error : EStreamError Do
      // Convert input stream errors to JPEG errors.
      Raise EJpegError.Create (error.Message) ;
      End ;
  Finally
    inputstream.Destroy ;
    End ;
  End ;

Procedure TJpegDecoder.setVerbose (value : Boolean) ;
  Begin
  verbose_flag := value ;
  End ;

Function TJpegDecoder.getVerbose  : Boolean ;
  Begin
  result := verbose_flag ;
  End ;

Procedure RefineAcCoefficient (inputstream : TJpegInputStream ;
                              ssa : Cardinal ;
                              var value : JpegCoefficient) ;
  Begin
  // Section G.1.2.3
  if (value > 0) Then
    Begin
    if (inputstream.nextBit <> 0) Then
      Inc (value, (1 Shl ssa)) ;
    End
  else if (value < 0) Then
    Begin
    if (inputstream.nextBit <> 0) Then
      inc (value, (-1 Shl ssa)) ;
    End ;
  End ;


//
//  Description:
//
//    SequentialOnly
//
//    This function extends the sign bit of a decoded value.
//
//  Parameters:
//    vv: The bit value
//    tt: The length of the bit value
//

Function Extend (vv, tt : Integer) : Integer ;
  Var
    vt : Integer ;
  Begin
  // Extend function defined in Section F.2.2.1 Figure F.12
  // The tt'th bit of vv is the sign bit. One is for
  // positive values and zero is for negative values.
  vt := 1 Shl (tt - 1) ;
  if (vv < vt) Then
    Begin
    vt := (-1 Shl tt) + 1 ;
    Result := vv + vt ;
    End
  else
    Begin
    Result := vv ;
    End ;
  End ;

//
//  Description:
//
//    This function calculates the value for a pixel using a triangle filter.
//
//    The filtering is performed as follows:
//
//    o We make the origin at the upper left corner and have the positive
//      axis go down and to the left.
//    o We place the component values [x,y] at (0,0), (hperiod, 0),
//      (0, vperiod), (hperiod, vperiod).
//    o We assume the physical pixels are located at
//
//      (1/2, 1/2), (3/2, 1/2) ... (hperiod - 1/2, 1/2)
//      (1/2, 3/2), (3/2, 3/2, ... (hperiod - 1/2, 3/2)
//        ...
//      (1/2, vperiod - 1/2), (3/2, vperiod - 1/2), ... (hperiod - 1/2, vperiod - 1/2)
//
//     o We calculate the filter value for each pixel using a linear weighting
//       of the four component values based upon the distance from each.
//
//  Parameters:
//
//  xpos, ypos: The position of the pixel relative to the upper left
//              corner. Since the positions are always fractions (N + 1/2) the
//              input valus are multiplied by 2.
//  ul, ur, ll, lr: The component values at the upper left, upper right, lower
//                   left, and lower right.
//  hperiod, vperiod: The sampling for the component. Each component value
//                    represents the value of hperiod x vperiod pixels.
//
//
Function TriangleFilter (xpos, ypos : Cardinal ;
                        ul, ur, ll, lr : JPEGSAMPLE ;
                        hperiod, vperiod : Cardinal) : JPEGSAMPLE ;
  Begin
  result :=  ((ul * (2 * hperiod - xpos) + ur * xpos) * (2 * vperiod - ypos)
                + (ll * (2 * hperiod - xpos) + lr * xpos) * ypos
                + 4 * hperiod * vperiod - 1)  Div (4 * hperiod * vperiod) ;
  End ;




//
//  Description:
//
//    Class default constructor
//
Constructor TJpegDecoderComponent.Create ;
  Begin
  Inherited Create ;
  component_id := 0 ;
  horizontal_frequency := 1 ;
  vertical_frequency := 1 ;
  v_sampling := 0 ;
  h_sampling := 0 ;
  last_dc_value := 0 ;
  ac_table := Nil ;
  dc_table := Nil ;
  quantization_table := Nil ;
  eob_run := 0 ;
  noninterleaved_rows := 0 ;
  noninterleaved_cols := 0 ;
  End ;


//
//  Description:
//
//    This function associates a quantization table with the component.
//
//  Parameters:
//    table:  The quantization table
//
Procedure TJpegDecoderComponent.setQuantizationTable (table : TJpegDecoderQuantizationTable) ;
  Begin
  quantization_table := table ;
  End ;

//
//  Description:
//
//    This function determines the dimensions for the component and allocates
//    the storage to hold the component's data.
//
//  Parameters:
//
//    width : Image width in pixels
//    height : Image height in pixels
//    maxhoriz : Maximum horizontal sampling frequency for all components.
//    maxvert : Maximum vertical sampling frequency for all components.
//
Procedure TJpegDecoderComponent.allocateComponentBuffers (width, height : Cardinal ;
                                                          maxhoriz, maxvert : JPEGSAMPLINGFREQUENCY ;
                                                          isprogressive : Boolean) ;
  Begin
  if (Length (data_units) = 0) Then
    Begin
    image_height := height ;
    image_width := width ;

    // Determine sampling for the component. This is the amount of
    // stretching needed for the component.
    v_sampling := maxvert Div vertical_frequency ;
    h_sampling := maxhoriz Div horizontal_frequency ;

    // Determine the component's dimensions in a non-interleaved scan.
    noninterleaved_rows := (image_height
                           + v_sampling * JPEGSAMPLEWIDTH - 1)
                          Div (v_sampling * JPEGSAMPLEWIDTH) ;
    noninterleaved_cols := (image_width
                           + h_sampling * JPEGSAMPLEWIDTH - 1)
                          Div (h_sampling * JPEGSAMPLEWIDTH) ;

    du_rows := McusRequired (image_height, maxvert) * Cardinal (vertical_frequency) ;
    du_cols := McusRequired (image_width , maxhoriz) * Cardinal (horizontal_frequency) ;

    SetLength (data_units, du_rows * du_cols) ;
    End ;

  if isprogressive And (Length (coefficient_blocks) = 0) Then
    Begin
    SetLength (coefficient_blocks, du_rows * du_cols) ;
    End ;
  End ;

//
//  Description:
//
//    This function frees the memory allocated by the component
//    during the decompression process.
//
Procedure TJpegDecoderComponent.freeComponentBuffers ;
  Begin
  data_units := Nil ;
  coefficient_blocks := Nil ;
  upsample_data := Nil ;
  End ;

//
//  Description:
//
//    This function asigned Huffman tables to the component.
//
//  Parameters:
//    dc:  The DC Huffman table
//    ac:  The AC Huffman table
//
Procedure TJpegDecoderComponent.setHuffmanTables (dc, ac : TJpegHuffmanDecoder) ;
  Begin
  dc_table := dc ;
  ac_table := ac ;
  End ;

//
//  Description:
//
//    This function ensures that this component has a defined
//    AC table assigned to it. If not, it throws an exception.
//
Procedure TJpegDecoderComponent.checkAcTable ;
  Begin
  // If this occurs then we have a programming error.
  if (Not ac_table.tableDefined) Then
    Raise EJpegBadStream.Create ('AC Table Not Defined') ;
  End ;

//
//  Sequential and Progressive
//
//  This function is called before processing a scan. It ensures that the
//  DC Huffman table used by the component has actually been defined.
//
Procedure TJpegDecoderComponent.checkDcTable ;
  Begin
  // This condition could be caused by a corrupt JPEG stream.
  if (Not dc_table.tableDefined) Then
    Raise EJpegBadStream.Create ('DC Table Not Defined') ;
  End ;

//
//  Description:
//
//    Sequential and Progressive
//
//    This function is called before processing a scan. It ensures that the
//    Quantization table used by the component has actually been defined.
//
Procedure TJpegDecoderComponent.checkQuantizationTable ;
  Begin
  if (quantization_table = Nil) Then
    Raise EJpegError.Create ('INTERNAL ERROR - Quantization Table Not Assigned') ;
  if (Not quantization_table.tableDefined) Then
    Raise EJpegBadStream.Create ('Quantization Table Not Defined') ;
  End ;

//
//  Description:
//
//    This function decodes a data unit in a sequential scan.
//
//  Parameters:
//    decoder: The decoder that owns this component
//    mcurow, mcucol:  The row and column for this data unit.
//
Procedure TJpegDecoderComponent.decodeSequential (inputstream : TJpegInputStream ;
                                                  mcurow, mcucol : Cardinal) ;
  var
    data : JpegCoefficientBlock ;
    ii, kk : Cardinal ;
    count : Cardinal ; // called T in F.2.2.1
    bits, value, diff : Integer ;
    dc : Integer ;
    rs, ssss, rrrr : Word ;
  Begin
  for ii := low (data) to high (data) Do
    data [ii] := 0 ;

  // Decode the DC differce value.
  // Section F.2.2.1
  count := dc_table.decode (inputstream) ;
  bits := inputstream.getBits (count) ;
  diff := Extend (bits, count) ;
  // Create the DC value from the difference and the previous DC value.
  dc := diff + last_dc_value ;
  last_dc_value := dc ;
  data [Low (COEFFICIENTINDEX)] := dc ;
  // Decode the AC coefficients.
  // Section F.2.2.2 Figure F.13
  kk := Low (COEFFICIENTINDEX) + 1 ;
  while kk <= High (COEFFICIENTINDEX) Do
    Begin
    rs := ac_table.decode (inputstream) ;
    ssss := (rs And $F) ;
    rrrr := (rs Shr $4) ;
    if (ssss = 0) Then
      Begin
      // ssss is zero then rrrr should either be 15 or zero according to
      // Figure F.1. 0 means that the rest of the coefficients are zero
      // while 15 means the next 16 coefficients are zero. We are not checking
      // for other values because Figure F.13 shows values other than 15
      // as being treated as zero.
      if (rrrr  <> 15) Then
        Inc (kk, JPEGSAMPLESIZE) ;
      Inc (kk, 15) ; // Actually 16 since one more gets added by the loop.
      End
    else
      Begin
      // If ssss is non-zero then rrrr gives the number of zero coefficients
      // to skip.

      Inc (kk, rrrr) ;

      // Receive and extend the additional bits.
      // Section F2.2.2 Figure F.14
      bits := inputstream.getBits (ssss) ;
      value := Extend (bits, ssss) ;
      data [JpegZigZagInputOrder [kk]] := value ;
      End ;
    Inc (kk) ;
    End ;
  inverseDCT (data, quantization_table, data_units [mcurow * du_cols + mcucol]) ;
  End ;

//
//  Description:
//
//    This function upsamples the data for the component. Here we take
//    the values from the data_units array and copy it to the
//    upsample_data. If the horizontal or vertical sampling frequencies
//    are less than the maximum for the image then we need to
//    stretch the data during the copy.
//
//  Parameters:
//
//    usefilter: true => Upsample using triangle filter if appropriate.
//
Procedure TJpegDecoderComponent.upsampleImage (decoder : TJpegDecoder ; usefilter : Boolean) ;
  var
    rowwidth, imagesize : Cardinal ;
  Begin
  rowwidth := du_cols * h_sampling * JPEGSAMPLEWIDTH ;
  imagesize := rowwidth * du_rows * v_sampling * JPEGSAMPLEWIDTH ;

  if (imagesize = 0) Then
    Exit ;  // No data for this component yet.

  if Length (upsample_data) = 0 Then
    SetLength (upsample_data, imagesize) ;

  // Simple case where component does not need to be upsampled.
  if (v_sampling = 1) And (h_sampling = 1) Then
    upSample1To1 (decoder)
  Else
    Begin
    // The triangle filter only workd for 2:1 and 4:1 sampling.
    if usefilter
        And ((v_sampling Mod 2 = 0) Or (v_sampling = 1))
        And ((h_sampling Mod 2 = 0) Or (h_sampling = 1)) Then
      triangleFilterImage (decoder)
    else
      blockFilterImage (decoder) ;
    End ;
  End ;

//
//  Description:
//
//    This static member function grayscale converts component
//    image data in the upsample_data array and writes it to the
//    the output image.  Actually for a grayscale conversion all
//    we do is copy.
//
//  Parameters:
//    cc:  The component
//    image:  The output image
//
Class Procedure TJpegDecoderComponent.convertGrayscale (
                                                        decoder : TJpegDecoder ;
                                                        cc : TJpegDecoderComponent ;
                                                        image : TBitmapImage) ;
  Var
    ii, jj, pixel : Integer ;
    rowstart, offset : Cardinal ;
  Begin
  rowstart := 0 ;
  pixel := 0 ;
  For ii := 1 To image.Height Do
    Begin
    offset := rowstart ;
    for jj := 1 To image.Width Do
      Begin
      Image.Pixels [pixel].red   := cc.upsample_data [offset] ;
      Image.Pixels [pixel].green := cc.upsample_data [offset] ;
      Image.Pixels [pixel].blue  := cc.upsample_data [offset] ;
      Image.Pixels [pixel].alpha := $FF ;
      Inc (offset) ;
      Inc (pixel) ;
      End ;
    Inc (rowstart, cc.du_cols * cc.h_sampling * JPEGSAMPLEWIDTH) ;
    End ;
  End ;

//
//  Description:
//
//    This static member function converts the upsample_data in three
//    components from YCbCr to RGB and writes it to an image.
//
//  Parameters:
//    c1: The component containing the Y data
//    c2: Ditto for Cb
//    c3: Ditto for Cr
//    image: The output image
//
Class Procedure TJpegDecoderComponent.convertRgb (
                                                 decoder : TJpegDecoder ;
                                                 c1, c2, c3 : TJpegDecoderComponent ;
                                                 image : TBitmapImage) ;
  Var
    yvalue, cbvalue, crvalue : JPEGSAMPLE ;
    rowstart, pixel, offset : Cardinal ;
    ii, jj : Cardinal ;
  Begin

  if (Length (c1.upsample_data) = 0)
      Or (Length (c2.upsample_data) = 0)
      Or (Length (c3.upsample_data) = 0) Then
    Exit ; // If we get here then do not yet have data for all components.

  rowstart := 0 ;
  pixel := 0 ;
  for ii := 1 To image.Height Do
    Begin
    offset := rowstart ;
    for jj := 1 To image.Width Do
      Begin
      yvalue := c1.upsample_data [offset] ;
      cbvalue := c2.upsample_data [offset] ;
      crvalue := c3.upsample_data [offset] ;
      image.Pixels [pixel].red := YCbCrToR (yvalue, cbvalue, crvalue) ;
      image.Pixels [pixel].green := YCbCrToG (yvalue, cbvalue, crvalue) ;
      image.Pixels [pixel].blue := YCbCrToB (yvalue, cbvalue, crvalue) ;
      image.Pixels [pixel].alpha := $FF ;
      Inc (offset) ;
      Inc (pixel) ;
      End ;
    Inc (rowstart, c1.du_cols * c1.h_sampling * JPEGSAMPLEWIDTH) ;
    End ;
  End ;


Class Procedure TJpegDecoderComponent.convertCymk (decoder : TJpegDecoder ;
                                  c1, c2, c3, c4 : TJpegDecoderComponent ;
                                  image : TBitmapImage) ;
  Var
    cvalue, mvalue, yvalue, kvalue : JPEGSAMPLE ;
    rowstart, pixel, offset : Cardinal ;
    ii, jj : Cardinal ;

  function Clamp (input : LongInt) : JPEGSAMPLE ;
    Begin
    if input > JPEGMAXSAMPLEVALUE then
       Result := JPEGMAXSAMPLEVALUE
    else if input < 0 then
      Result := 0
    else
      Result := input ;
    end ;
  Begin
  if (Length (c1.upsample_data) = 0)
      Or (Length (c2.upsample_data) = 0)
      Or (Length (c3.upsample_data) = 0)
      Or (Length (c4.upsample_data) = 0) Then
    Exit ; // If we get here then do not yet have data for all components.

  rowstart := 0 ;
  pixel := 0 ;
  for ii := 1 To image.Height Do
    Begin
    offset := rowstart ;
    for jj := 1 To image.Width Do
      Begin
      cvalue := c1.upsample_data [offset] ;
      mvalue := c2.upsample_data [offset] ;
      yvalue := c3.upsample_data [offset] ;
      kvalue := c4.upsample_data [offset] ;

      image.Pixels [pixel].red   := Clamp (cvalue - (JPEGMAXSAMPLEVALUE - kvalue)) ;
      image.Pixels [pixel].green := Clamp (mvalue - (JPEGMAXSAMPLEVALUE - kvalue)) ;
      image.Pixels [pixel].blue  := Clamp (yvalue - (JPEGMAXSAMPLEVALUE - kvalue)) ;
      image.Pixels [pixel].alpha := $FF ;
      Inc (offset) ;
      Inc (pixel) ;
      End ;
    Inc (rowstart, c1.du_cols * c1.h_sampling * JPEGSAMPLEWIDTH) ;
    End ;
  End ;
//
//  Description:
//
//    Progressive Only
//
//    This function decodes the DC coefficient for a data unit in the first
//    DC scan for the component.
//
//    According to G.2 "In order to avoid repetition, detail flow diagrams
//    of progressive decoder operation are not included. Decoder operation is
//    defined by reversing the function of each stop described in the encoder
//    flow charts, and performing the steps in reverse order."
//
//  Parameters:
//    decoder:  The JPEG decoder
//    row:  The data unit row
//    col:  The data unit column
//    ssa:  Successive Approximation
//
Procedure TJpegDecoderComponent.decodeDcFirst (inputstream : TJpegInputStream ;
                                              row, col, ssa : Cardinal) ;
  Var
    count : Cardinal ; // called T in F.2.2.1
    bits, diff, value : Integer ;
  Begin
  // We decode the first DC coeffient that same way as in a sequential
  // scan except for the point transform according to G.1.2.1

  // Section F.2.2.1
  count := dc_table.decode (inputstream) ;
  bits := inputstream.getBits (count) ;
  diff := Extend (bits, count) ;
  value := diff + last_dc_value ;
  last_dc_value := value ;
  coefficient_blocks [row * du_cols + col][Low (COEFFICIENTINDEX)] := (value Shl ssa) ;
  End ;

//
//  Description:
//
//    Progressive Only
//
//    This function decodes the DC coefficient for a data unit in refining
//    DC scans for the component.
//
//    According to G.2 "In order to avoid repetition, detail flow diagrams
//    of progressive decoder operation are not included. Decoder operation is
//    defined by reversing the function of each stop described in the encoder
//    flow charts, and performing the steps in reverse order."
//
//  Parameters:
//    decoder:  The JPEG decoder
//    row:  The data unit row
//    col:  The data unit column
//    ssa:  Successive Approximation
//
Procedure TJpegDecoderComponent.decodeDcRefine (inputstream : TJpegInputStream ;
                                                row, col, ssa : Cardinal) ;
  Begin
  // Reversing G.1.2.1
  If (inputstream.nextBit <> 0) Then
    Begin
    coefficient_blocks [row * du_cols + col][Low (COEFFICIENTINDEX)] :=
      coefficient_blocks [row * du_cols + col][Low (COEFFICIENTINDEX)] Or (1 Shl ssa) ;
    End ;
  End ;

//
//  Description:
//
//    Progressive Only
//
//    This function decodes the AC coefficients for a data unit in the first
//    AC scans for a spectral range within the component.
//
//    According to G.2 "In order to avoid repetition, detail flow diagrams
//    of progressive decoder operation are not included. Decoder operation is
//    defined by reversing the function of each stop described in the encoder
//    flow charts, and performing the steps in reverse order."
//
//    This function comes from reversing the steps in Figures G.3-G.5.
//
//  Parameters:
//    decoder:  The JPEG decoder
//    row:  The data unit row
//    col:  The data unit column
//    sss:  Spectral Selection Start
//    sse:  Spectral Selection End
//    ssa:  Successive Approximation
//
Procedure TJpegDecoderComponent.decodeAcFirst (inputstream : TJpegInputStream ;
                                               row, col, sss, sse, ssa : Cardinal) ;
  Var
    kk : Cardinal ;
    rs : Word ;
    ssss, rrrr : Byte ;
    bits, value : Integer ;
  Begin
  If eob_run > 0 Then
    Begin
    // If a previous call created a nonzero EOB run then we decrement the
    // counter and return.
    Dec (eob_run) ;
    End
  Else
    Begin
    kk := sss ;
    While kk <= sse Do
      Begin
      // Decode the next value in the input stream.
      rs := ac_table.decode (inputstream) ;
      ssss := (rs And $F) ;
      rrrr := (rs Shr $4) ;
      if ssss = 0 Then
        Begin
        If rrrr = 15 Then
          Begin
          // A zero value ssss with rrrr = 15 means to skip
          // 16 zero coefficients.
          Inc (kk, 16) ;
          End
        Else
          Begin
          // A zero value ssss with rrrr != 15 means to create
          // End of Band run.

          // The EOB run includes the current block. This is why we
          // do no processing for rrrr = 0 and substract one when
          // rrrrr != 0.
          If rrrr <> 0 Then
            Begin
            bits := inputstream.getBits (rrrr) ;
            eob_run := (1 Shl rrrr) + bits - 1 ;
            End ;
          Exit ;
          End ;
        End
      Else
        Begin
        // When ssss != 0, rrrr gives the number of zero elements to skip
        // before the next non-zero coefficient.
        Inc (kk, rrrr) ;
        // Extend the value and store.
        bits := inputstream.getBits (ssss) ;
        value := Extend (bits, ssss) ;
        coefficient_blocks [row * du_cols + col][JpegZigZagInputOrder [kk]] := (value Shl ssa) ;
        Inc (kk) ;
        End ;
      End ;
    End ;
  End ;
//
//  Description:
//
//    Progressive Only
//
//    This function decodes the AC coefficients for a data unit in the
//    refining AC scans for a spectral range within the component.
//
//    According to G.2 "In order to avoid repetition, detail flow diagrams
//    of progressive decoder operation are not included. Decoder operation is
//    defined by reversing the function of each stop described in the encoder
//    flow charts, and performing the steps in reverse order."
//
//    Section G.1.2.3 defines how to encode refining scans for AC
//    coefficients. Unfortunately this section is vague and
//    undecipherable. Reversing an undecipherable process results
//    in something unimaginable. This is a "best-guess" interpretation
//    that seems to work.
//
//    The basic process at work is that zero counts do not include nonzero
//    values. Whenever we skip a value due to zero count or End of Band runs
//    we have to read one bit to refine each non-zero value we skip. The
//    process is ugly and it means that data is encoding out of order.
//
//  Parameters:
//    decoder:  The JPEG decoder
//    row:  The data unit row
//    col:  The data unit column
//    sss:  Spectral Selection Start
//    sse:  Spectral Selection End
//    ssa:  Successive Approximation
//
Procedure TJpegDecoderComponent.decodeAcRefine (inputstream : TJpegInputStream ;
                                               row, col, sss, sse, ssa : Cardinal) ;
  var
    rs : Word ;
    ssss, rrrr : Byte ;
    ii, kk : Cardinal ;
    offset : Cardinal ;
    bits : Integer ;
    newvalue : Integer ;
    zerocount : Cardinal ;
  Begin
  offset := row * du_cols + col ;
  // kk is incremented within the loop.
  kk := sss ;
  while kk <= sse Do
    Begin
    if (eob_run <> 0) Then
      Begin
      // An EOB run has caused us to skip entire data units. We need
      // to refine any previously non-zero coefficients.
      // Notice that we do not initialize kk here. We could be using
      // an EOB run to skip all the remaining coefficients in the current
      // one.

      While kk <= sse Do
        Begin
        if (coefficient_blocks [offset][JpegZigZagInputOrder [kk]] <> 0) Then
          Begin
          RefineAcCoefficient (inputstream,
                               ssa,
                               coefficient_blocks [offset][JpegZigZagInputOrder [kk]]) ;
          End ;
        inc (kk) ;
        End ;
      Dec (eob_run) ;
      End
    else
      Begin
      rs := ac_table.decode (inputstream) ;
      ssss := (rs And $F) ;
      rrrr := (rs Shr $4) ;
      if (ssss = 0) Then
        Begin
        // ssss == 0 means that we either have an EOB run or we need to
        // 16 non-zero coefficients.

        if (rrrr = 15) Then
          Begin
          // ssss == 0 and rrrr == 15 => Skip over 16 zero coefficients
          ii := 0 ;
          while (kk <= sse) And (ii < 16) Do
            Begin
            if coefficient_blocks [offset][JpegZigZagInputOrder [kk]] <> 0 Then
              Begin
              RefineAcCoefficient (inputstream,
                                   ssa,
                                   coefficient_blocks [offset][JpegZigZagInputOrder [kk]]) ;
              End
            else
              Inc (ii) ;
            inc (kk) ;
            End ;
          End
        else
          Begin
          // We are reading an EOB run.
          if (rrrr = 0) Then
            eob_run := 1
          else
            Begin
            bits := inputstream.getBits (rrrr) ;
            eob_run := (1 Shl rrrr) + bits ;
            End
          End
        End
      else if ssss = 1 Then
        Begin
        // ssss == 1 means that we are creating a new non-zero
        // coefficient. rrrr gives the number of zero coefficients to
        // skip before we reach this one.
        // Save the value for the new coefficient. Unfortunately the data
        // is stored out of order.
        newvalue := inputstream.nextBit ;

        // Skip the zero coefficients.
        zerocount := 0 ;
        while (kk < JPEGSAMPLESIZE)
               And ((zerocount < rrrr)
                    Or (coefficient_blocks [offset][JpegZigZagInputOrder [kk]] <> 0)) Do
          Begin
          if (kk >  sse) Then
            Raise EJpegBadStream.Create ('Error in progressive scan') ;

          if (coefficient_blocks [row * du_cols + col][JpegZigZagInputOrder [kk]] <> 0) Then
            Begin
            RefineAcCoefficient (inputstream,
                                 ssa,
                                 coefficient_blocks [offset][JpegZigZagInputOrder [kk]]) ;
            End
          Else
            Inc (zerocount) ;
          Inc (kk) ;
          End ;

        If (kk >  sse) Then
          Raise EJpegBadStream.Create ('Error in progressive scan') ;
        If newvalue <> 0 Then
          coefficient_blocks [offset][JpegZigZagInputOrder [kk]] := (1 Shl ssa)
        else
          coefficient_blocks [offset][JpegZigZagInputOrder [kk]] := (-1 Shl ssa) ;
        Inc (kk) ;
        End
      else
        Begin
        // The value of SSSS must be zero or one. Since we add data one
        // bit at at a time data values larger than 1 make no sense.
        Raise EJpegBadStream.Create ('Invalid value in input stream') ;
        End ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    Progressive Only
//
//    This function performs the IDCT on all the data in
//    coefficient_blocks and stores the result in data_units.
//
//    This function gets called whenever the image data is written
//    to the image.  For a sequential image the IDCT only needs to
//    performed once no matter how often the data gets updated in the
//    image but to continuously update the progressive image data
//    an update after each scan gives new results.
//
Procedure TJpegDecoderComponent.progressiveInverseDct (decoder : TJpegDecoder) ;
  var
    ii : Integer ;
  Begin
  // If UpdateImage gets called before the image is completely
  // decoded the these values may be 0.
  if (Length (data_units) = 0) And (Length (coefficient_blocks) = 0) Then
    Exit ;

  for ii := 0 To du_cols * du_rows - 1 Do
    inverseDCT (coefficient_blocks [ii], quantization_table, data_units [ii]) ;
  End ;

//  This function performs pseudo"upsampling" for components that don't need to
//  be upsampled. For such components we just make a straight copy of the
//  upsample data.
Procedure TJpegDecoderComponent.upSample1To1 (decoder : TJpegDecoder) ;
  Var
    output : Cardinal ;
    startdu, du : Cardinal ;
    durow, ducol : Cardinal ;
    ii, jj : Integer ;
  Begin
  output := 0 ;
  startdu := 0 ;
  for durow := 1 to du_rows Do
    Begin
    for ii := Low (DATAUNITINDEX) To High (DATAUNITINDEX) Do
      Begin
      du := startdu ;
	    for ducol := 1 To du_cols Do
        begin
        for jj := Low (DATAUNITINDEX) to High (DATAUNITINDEX) do
          Begin
          upsample_data [output] := data_units [du][ii, jj] ;
          Inc (output) ;
          End ;
        Inc (du) ;
    	  End ;
      End ;
    Inc (startdu, du_cols) ;
    End ;
  End ;

//  This function upsamples a component that has a sampling frequency less
//  than the maximum for the image. In such a component, each compressed value
//  represents multiple pixels.
//
//  The function upsamples using a "block filter" which simply means the
//  compressed values are stretched across multiple pixels by duplicating them.
//
Procedure TJpegDecoderComponent.blockFilterImage (decoder : TJpegDecoder) ;
  Var
    output : Cardinal ;
    startdu : Cardinal ;
    durow : Cardinal ;
    row, vv : Integer ;

  Procedure FilterRow (startdu : Cardinal ; row : DATAUNITINDEX) ;
    Var
      ii, col, hh : Cardinal ;
    Begin
    for ii := 0 to du_cols - 1 do
      Begin
      for col := Low (DATAUNITINDEX) To High (DATAUNITINDEX) Do
        Begin
        for hh := 1 to h_sampling Do
          Begin
          upsample_data [output] := data_units [startdu + ii][row, col] ;
          Inc (output) ;
          End ;
        end ;
      End ;
    End ;

  Begin
  output := 0 ;
  startdu := 0 ;
  for durow := 1 To du_rows Do
    Begin
    for row := Low (DATAUNITINDEX) To High (DATAUNITINDEX) Do
      Begin
      for vv := 1 To v_sampling Do
        Begin
        FilterRow (startdu, row) ;
        End ;
      End ;
    Inc (startdu, du_cols) ;
    End ;
  End ;

Procedure TJpegDecoderComponent.triangleFilterImage (decoder : TJpegDecoder) ;
  Var
    // These are dummy data units that are used to fill in round the edges.
    upperleft, uppercenter, upperright,
    left, right,
    lowerleft, lowercenter, lowerright : DecoderDataUnit ;
    rowwidth : Cardinal ;

  //    If the width of the image is such that the image does not span a complete
  //    data unit, this function copies the rightmost column to the next column
  //    in the last data units of each data unit row.
  Procedure extendRightColumn ;
    Var
      row, col, durow : Cardinal ;
    Begin
    if (image_width Mod (h_sampling * JPEGSAMPLEWIDTH) <> 0) Then
      Begin
      col := (image_width Mod (h_sampling * JPEGSAMPLEWIDTH) + h_sampling - 1) Div h_sampling + 1 ;

      for durow := 0 To du_rows - 1 Do
        Begin
        for row := Low (DATAUNITINDEX) To High (DATAUNITINDEX) Do
          Begin
          data_units [du_cols * durow][row, col] :=
              data_units [du_cols * durow][row, col - 1] ;
          End ;
        End ;
      // Handle the lower right corner.
      if (image_height Mod (v_sampling * JPEGSAMPLEWIDTH) <> 0) Then
        Begin
        row := image_height Mod JPEGSAMPLEWIDTH ;
        data_units [du_cols * du_rows - 1][row, col] :=
          data_units [du_cols * du_rows - 1][row - 1, col - 1] ;
        End ;
      End ;
    End ;
  //    If the image height is such that it does not span an integral number
  //    of data unit rows, this function copies the last row of the image to
  //    the next row.
  Procedure extendBottomRow ;
    Var
      row : Cardinal ;
      du, Col : Cardinal ;
    Begin
    // If the image height does not fill the data unit height, extend the last row.
    if (image_height Mod (v_sampling * JPEGSAMPLEWIDTH) <> 0) Then
      Begin
      row := (image_height Mod (v_sampling * JPEGSAMPLEWIDTH) + v_sampling - 1)
                          Div v_sampling + 1 ;

      for du := du_cols * (du_rows - 1) To du_cols * du_rows - 1 Do
        Begin
        for col := 1 To JPEGSAMPLEWIDTH - 1 Do
          data_units [du][row, col] := data_units [du][row - 1, col] ;
        End ;
      End ;
    End ;

  //  Description:
  //
  //    This function triangle filters a data unit.
  //
  //  Parameters:
  //
  //    start: The index of the location in upsample_data to output the upper left pixel.
  //    rowwidth: The number of pixels per row in the output buffer.
  //    hperiod, vperiod: The component sampling.
  //    upperleft, uppercenter, upperright, left: Adjacent data units (used
  //                                              to filter pixels at the edge.
  //    center:  The data unit to filter.
  //    right, lowerleft, lowercenter, lowerright: More adjacent data units.
  //
  Procedure FilterDataUnit (
                  start : Cardinal ;
                  rowwidth, hperiod, vperiod : Cardinal ;
                  upperleft, uppercenter, upperright,
                  left, center, right,
                  lowerleft, lowercenter, lowerright : DecoderDataUnit) ;
    var
      offset : Cardinal ;
      mcurow, mcucol : Cardinal ;
    //
    //  Description:
    //
    //    This function triangle filters a block of pixels at the corner of a
    //    data unit. At the corner there are (hperiod/2) * (vperiod/2) pixels.
    //
    //  Parameters:
    //
    //    startx, starty: The position of the upper left pixel in the block relative
    //                    to the positions of the component values. These values
    //                    are scaled by 2 so they will not be fractions. For an
    //                    upper left corner these values will be
    //                    (hperiod + 1,  vperiod + 1), upper right (1, vperiod + 1),
    //                    lower left (hperiod + 1, 1), lower right (1, 1).
    //    startposition: The location to output the upper left pixel.
    //    rowwidth: The number of pixels per row in the output buffer.
    //    up, up, ll, lr: The component values as the four corners.
    //    hperiod, vperiod: The component sampling.
    //
    Procedure FilterCorner (startx, starty : Cardinal ;
                            start : Cardinal ;
                            rowwidth : Cardinal ;
                            ul, ur, ll, lr : Integer ;
                            hperiod, vperiod : Cardinal) ;
      Var
        xx, yy : Cardinal ;
        rowoffset, offset : cardinal ;
        ii, jj : Integer ;
      Begin
      yy := starty ;
      rowoffset := 0 ;
      for ii := 1 To vperiod Div 2 Do
        Begin
        xx := startx ;
        offset := rowoffset ;
        for jj := 1 To hperiod Div 2 Do
          Begin
          upsample_data [start + offset] := TriangleFilter (xx, yy,
                                                   ul, ur,
                                                   ll, lr,
                                                   hperiod, vperiod) ;
          Inc (offset) ;
          Inc (xx, 2) ;
          End ;
        Inc (rowoffset, rowwidth) ;
        Inc (yy, 2) ;
        End ;
      End ;
    //
    //  Description:
    //
    //    This function triangle filters a block of pixels at the top or bottom of
    //    data unit (but not at the corner). Each block contains
    //    hperiod * (vperiod / 2) pixels.
    //
    //  Parameters:
    //
    //    starty: The position of the frst row of pixels pixels in the block relative
    //            to the positions of the component values. These values are scaled
    //            by 2 to eliminate fractions. For an upper row, this value is vperiod + 1,
    //            for a bottom row it is 1.
    //    startposition: The location to output the upper left pixel.
    //    rowwidth: The number of pixels per row in the output buffer.
    //    up, up, ll, lr: The component values as the four corners.
    //    hperiod, vperiod: The component sampling.
    //
    Procedure FilterTopOrBottom (starty : Cardinal ;
                                 start: Cardinal ;
                                 rowwidth : Cardinal ;
                                 ul, ur, ll, lr : Integer ;
                                 hperiod, vperiod : Cardinal) ;
      var
        offset, rowoffset : Cardinal ;
        xx, yy : Cardinal ;
        ii, jj : Cardinal ;
      Begin
      rowoffset := 0 ;
      yy := starty ;
      for ii := 1 To vperiod Div 2 Do
        Begin
        xx := 1 ;
        offset := rowoffset ;
        for jj := 1 To hperiod Do
          Begin
          upsample_data [start + offset] := TriangleFilter (xx, yy,
                                                   ul, ur,
                                                   ll, lr,
                                                   hperiod, vperiod) ;
          Inc (offset) ;
          Inc (xx, 2) ;
          End ;
        Inc (rowoffset, rowwidth) ;
        Inc (yy, 2) ;
        End ;
      End ;
    //
    //  Description:
    //
    //    This function triangle filters a block of pixels at either side of a data
    //    unit (but not at the corner). Each block contains
    //    (hperiod / 2) * vperiod pixels.
    //
    //  Parameters:
    //
    //    startx: The position of the first column of pixels pixels in the block relative
    //            to the positions of the component values. These values are scaled
    //            by 2 to eliminate fractions. For pixels at the right side of a data
    //            unit, this value is hperiod + 1. At the left size it is 1.
    //    startposition: The location to output the upper left pixel.
    //    rowwidth: The number of pixels per row in the output buffer.
    //    up, up, ll, lr: The component values as the four corners.
    //    hperiod, vperiod: The component sampling.
    //
    Procedure FilterSide (startx : Cardinal ;
                          start : Cardinal ;
                          rowwidth : Cardinal ;
                          ul, ur, ll, lr : Integer ;
                          hperiod, vperiod : Cardinal) ;
      Var
        offset, rowoffset : Cardinal ;
        xx, yy : Cardinal ;
        ii, jj : Cardinal ;
      Begin
      rowoffset := 0 ;
      yy := 1 ;
      for ii := 1  To vperiod Do
        Begin
        offset := rowoffset ;
        xx := startx ;
        for jj := 1 To hperiod Div 2 Do
          Begin
          upsample_data [start + offset] := TriangleFilter (xx, yy,
                                                   ul, ur,
                                                   ll, lr,
                                                   hperiod, vperiod) ;
          Inc (offset) ;
          Inc (xx, 2) ;
          End ;
        Inc (rowoffset, rowwidth) ;
        Inc (yy, 2) ;
        End ;
      End ;

    //
    //  Description:
    //
    //    This function triangle filters a vperiod x hperiod block of pixels that
    ///   are not at the edge of the data unit.
    //
    //  Parameters:
    //
    //    startposition: The location to output the upper left pixel.
    //    rowwidth: The number of pixels per row in the output buffer.
    //    up, up, ll, lr: The component values as the four corners.
    //    hperiod, vperiod: The component sampling.
    //
    Procedure FilterMiddle (start : Cardinal ;
                            rowwidth : Cardinal ;
                            ul, ur, ll, lr : Integer ;
                            hperiod, vperiod : Cardinal) ;
      var
        ii, jj : Cardinal ;
        xx, yy : Cardinal ;
        offset, rowoffset : Cardinal ;
      Begin
      rowoffset := 0 ;
      yy := 1 ;
      for ii := 1 To vperiod Do
        Begin
        offset := rowoffset ;
        xx := 1 ;
        for jj := 1 To hperiod Do
          Begin
          upsample_data [start + offset] := TriangleFilter (xx, yy,
                                                   ul, ur,
                                                   ll, lr,
                                                   hperiod, vperiod) ;
          Inc (offset) ;
          Inc (xx, 2) ;
          End ;
        Inc (rowoffset, rowwidth) ;
        Inc (yy, 2) ;
        End ;
      End ;

    Begin
    offset := 0 ;
    // Upper Right Corner
    FilterCorner (hperiod + 1, vperiod + 1,
                  start + offset, rowwidth,
                  upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)], uppercenter [High (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  left [Low (DATAUNITINDEX), High (DATAUNITINDEX)], center [Low (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  hperiod, vperiod) ;
    Inc (offset, hperiod Div 2) ;
    // Top Edges
    for mcucol := Low (DATAUNITINDEX) To High (DATAUNITINDEX) - 1 Do
      Begin
      FilterTopOrBottom (vperiod + 1,
                         start + offset, rowwidth,

                         uppercenter [High (DATAUNITINDEX), mcucol],
                         uppercenter [High (DATAUNITINDEX), mcucol + 1],
                         center [Low (DATAUNITINDEX), mcucol], center [Low (DATAUNITINDEX), mcucol + 1],
                         hperiod, vperiod) ;
      Inc (offset, hperiod) ;
      End ;
    // Upper Left Corner
    FilterCorner (1, vperiod + 1,
                  start + offset, rowwidth,
                  uppercenter [High (DATAUNITINDEX), High (DATAUNITINDEX)], upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  center [Low (DATAUNITINDEX), High (DATAUNITINDEX)], right [Low (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  hperiod, vperiod) ;
    Inc (offset, (hperiod + vperiod * rowwidth) Div 2 - JPEGSAMPLEWIDTH * hperiod) ;

    for mcurow := Low (DATAUNITINDEX) To High (DATAUNITINDEX) - 1 Do
      Begin
      // Left Edge
      FilterSide (hperiod + 1,
                  start + offset, rowwidth,
                  left [mcurow, High (DATAUNITINDEX)], center [mcurow, Low (DATAUNITINDEX)],
                  left [mcurow + 1, High (DATAUNITINDEX)], center [mcurow + 1, Low (DATAUNITINDEX)],
                  hperiod, vperiod) ;
      Inc (offset, hperiod Div 2) ;

      // Middle
      for mcucol := Low (DATAUNITINDEX) To  High (DATAUNITINDEX) - 1 Do
        Begin
        FilterMiddle (start + offset, rowwidth,
                      center [mcurow, mcucol], center [mcurow, mcucol + 1],
                      center [mcurow + 1, mcucol], center [mcurow + 1, mcucol + 1],
                      hperiod, vperiod) ;
        Inc (offset, hperiod) ;
        End ;
      // Right Edge
      FilterSide (1,
                  start + offset, rowwidth,
                  center [mcurow][High (DATAUNITINDEX)], right [mcurow][1],
                  center [mcurow + 1][High (DATAUNITINDEX)], right [mcurow + 1][1],
                  hperiod, vperiod) ;

      Inc (offset, (hperiod Div 2 + vperiod * rowwidth) - JPEGSAMPLEWIDTH * hperiod) ;
      End ;

    // Bottom Left Corner
    FilterCorner (hperiod + 1, 1,
                  start + offset, rowwidth,
                  left [High (DATAUNITINDEX), High (DATAUNITINDEX)], center [High (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)], lowercenter [Low (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  hperiod, vperiod) ;
    Inc (offset, hperiod Div 2) ;

    // Bottom Edge
    for mcurow := Low (DATAUNITINDEX) To High (DATAUNITINDEX) - 1 Do
      Begin
      FilterTopOrBottom (1,
                         start + offset, rowwidth,
                         center [High (DATAUNITINDEX), mcurow], center [High (DATAUNITINDEX), mcurow + 1],
                         lowercenter [Low (DATAUNITINDEX), mcurow], lowercenter [Low (DATAUNITINDEX), mcurow + 1],
                         hperiod, vperiod) ;
      Inc (offset, hperiod) ;
      End ;
    // Bottom Right Corner
    FilterCorner (1, 1,
                  start + offset, rowwidth,
                  center [High (DATAUNITINDEX), High (DATAUNITINDEX)], right [High (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  lowercenter [Low (DATAUNITINDEX), High (DATAUNITINDEX)], lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)],
                  hperiod, vperiod) ;
    End ;


  //
  //  Description:
  //
  //    This function copies the component values at the edge of a data
  //    unit to the 8 surrounding data units so that each value at the edge
  //    of the center is the same as the adjacent value in the ajoining
  //    data unit. This is used to create dummy data units at the edge of an
  //    image.
  //
  //  Parameters:
  //    center: The data unit to copy
  //    upperleft, uppercenter, upperight,
  //    left, right,
  //    lowerleft, lowercenter, lowerright: The dummy data units to copy to.
  //
  Procedure FillEdges (center : DecoderDataUnit ;
                       var upperleft, uppercenter, upperright,
                       left, right,
                       lowerleft, lowercenter, lowerright : DecoderDataUnit) ;
    Var
      ii : Integer ;
    Begin
    upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] := center [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] := center [High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] := center [Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := center [High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    for ii := Low (DATAUNITINDEX) To High (DATAUNITINDEX) Do
      Begin
      left [ii, High (DATAUNITINDEX)] := center [ii, Low (DATAUNITINDEX)] ;
      uppercenter [High (DATAUNITINDEX), ii] := center [Low (DATAUNITINDEX), ii] ;
      right [ii, Low (DATAUNITINDEX)] := center [ii, High (DATAUNITINDEX)] ;
      lowercenter [Low (DATAUNITINDEX), ii] := center [High (DATAUNITINDEX), ii] ;
      End ;
    End ;

  Procedure FilterSingleDataUnitImage ;
    Begin
    FillEdges (data_units [0],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    FilterDataUnit (upsample_data [0], rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    left, data_units [0], right,
                    lowerleft, lowercenter, lowerright) ;
    End ;

  Procedure FilterSingleDataUnitRowImage ;
    var
      offset : Cardinal ;
      du : Cardinal ;
    Begin
    // Special case where the component has just one row of data units.
    offset := 0 ;
    FillEdges (data_units [0],
           upperleft, uppercenter, upperright,
           left, right,
           lowerleft, lowercenter, lowerright) ;
    // Except in the case of a component with one data unit,
    // FillEdges makes a mistake with at least two edge positions.
    upperright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [1][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;

    FilterDataUnit (upsample_data [offset], rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    left, data_units [0], data_units [1],
                    lowerleft, lowercenter, lowerright) ;
    Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;

    for du := 1 To du_cols - 2 Do
      Begin
      FillEdges (data_units [du],
             upperleft, uppercenter, upperright,
             left, right,
             lowerleft, lowercenter, lowerright) ;
      upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du - 1][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du - 1][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [du + 1][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;

      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      upperleft, uppercenter, upperright,
                      data_units [du - 1], data_units [du], data_units [du + 1],
                      lowerleft, lowercenter, lowerright) ;
      Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
      End ;

    FillEdges (data_units [du_cols - 1],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du_cols - 2][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du_cols - 2][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    FilterDataUnit (upsample_data [offset], rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    data_units [du_cols - 1], data_units [du_cols], right,
                    lowerleft, lowercenter, lowerright) ;
    End ;

  Procedure FilterSingledataUnitColumnImage ;
    var
      du : Cardinal ;
      offset : Cardinal ;
    Begin
    offset := 0 ;
    FillEdges (data_units [0],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    lowerleft  [Low (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [1][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    left, data_units [0], right,
                    lowerleft, data_units [1], lowerright) ;
    Inc (offset, v_sampling * h_sampling * JPEGSAMPLESIZE) ;

    for du := 1 To du_rows - 2 Do
      Begin
      FillEdges (data_units [du],
                 upperleft, uppercenter, upperright,
                 left, right,
                 lowerleft, lowercenter, lowerright) ;
      upperleft  [High (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du - 1][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [du - 1][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      lowerleft  [Low (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [du + 1][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      upperleft, data_units [du - 1], upperright,
                      left, data_units [du], right,
                      lowerleft, data_units [du + 1], lowerright) ;
      Inc (offset, v_sampling * h_sampling * JPEGSAMPLESIZE) ;
      End ;

    FillEdges (data_units [du_cols - 1],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] := data_units [du_cols - 2][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] := data_units [du_cols - 2][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    upperleft, data_units [du_cols - 1], upperright,
                    left, data_units [du_cols], right,
                    lowerleft, lowercenter, lowerright) ;
    End ;

  Procedure FilterNormalImage ;
    Var
      // Source offset.
      du : Cardinal ;
      ii : Cardinal ;
      durows, ducols : Cardinal ;
      // The position to start writing the pixel data to. (Destination offset)
      offset : Cardinal ;
    Begin
    du := Low (data_units) ;
    offset := 0 ;
    // Upper Left Corner
    FillEdges (data_units [du],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
        data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] :=
        data_units [du + du_cols][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;

    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    left, data_units [du], data_units [du + 1],
                    lowerleft, data_units [du + du_cols], data_units [du + du_cols + 1]) ;
    Inc (du) ;
    Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;

    // Top Row
    for ii := 1 To du_cols - 2 Do
      Begin
      FillEdges (data_units [du],
                 upperleft, uppercenter, upperright,
                 left, right,
                 lowerleft, lowercenter, lowerright) ;
      upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] :=
          data_units [du - 1][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
          data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;

      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      upperleft, uppercenter, upperright,
                      data_units [du - 1], data_units [du], data_units [du + 1],
                      data_units [du + du_cols  - 1], data_units [du + du_cols], data_units [du + du_cols + 1]) ;
      Inc (du) ;
      Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
      End ;

    // Upper Right
    FillEdges (data_units [du],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] :=
        data_units [du - 1][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
        data_units [du + du_cols][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    upperleft, uppercenter, upperright,
                    data_units [du - 1], data_units [du], right,
                    data_units [du + du_cols  - 1], data_units [du + du_cols], lowerright) ;
    Inc (du) ;
    Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
    Inc (offset, rowwidth * (JPEGSAMPLEWIDTH * v_sampling - 1)) ;

    for durows := 1 To du_rows - 2 Do
      Begin
      // Left Edge
      FillEdges (data_units [du],
                 upperleft, uppercenter, upperright,
                 left, right,
                 lowerleft, lowercenter, lowerright) ;
      upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] :=
          data_units [du - du_cols][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] :=
          data_units [du + du_cols][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      upperleft, data_units [du - du_cols], data_units [du - du_cols + 1],
                      left, data_units [du], data_units [du + 1],
                      lowerleft, data_units [du + du_cols], data_units [du + du_cols + 1]) ;
      Inc (du) ;
      Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
      for ducols := 1 To du_cols - 2 Do
        Begin
        FilterDataUnit (offset, rowwidth,
                        h_sampling, v_sampling,
                        data_units [du - du_cols - 1], data_units [du - du_cols], data_units [du - du_cols + 1],
                        data_units [du - 1], data_units [du], data_units [du + 1],
                        data_units [du + du_cols - 1], data_units [du + du_cols], data_units [du + du_cols + 1]) ;
        Inc (du) ;
        Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
        End ;
      // Right Edge
      FillEdges (data_units [du],
                 upperleft, uppercenter, upperright,
                 left, right,
                 lowerleft, lowercenter, lowerright) ;
      upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
          data_units [du - du_cols][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
          data_units [du + du_cols][Low (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      data_units [du - du_cols - 1], data_units [du - du_cols], upperright,
                      data_units [du - 1], data_units [du], right,
                      data_units [du + du_cols - 1], data_units [du + du_cols], lowerright) ;
      Inc (du) ;
      Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
      Inc (offset, rowwidth * (JPEGSAMPLEWIDTH * v_sampling - 1)) ;
      End ;

    // Bottom Left Corner
    FillEdges (data_units [du],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    upperleft [High (DATAUNITINDEX), High (DATAUNITINDEX)] :=
        data_units [du - du_cols][High (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
        data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    upperleft, data_units [du - du_cols], data_units [du - du_cols + 1],
                    left, data_units [du], data_units [du + 1],
                    lowerleft, lowercenter, lowerright) ;
    Inc (du) ;
    Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;

    for ii := 1 To du_cols - 2 Do
      Begin
      FillEdges (data_units [du],
                 upperleft, uppercenter, upperright,
                 left, right,
                 lowerleft, lowercenter, lowerright) ;
      lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] :=
          data_units [du - 1][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
      lowerright [Low (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
          data_units [du + 1][Low (DATAUNITINDEX), Low (DATAUNITINDEX)] ;
      FilterDataUnit (offset, rowwidth,
                      h_sampling, v_sampling,
                      data_units [du - du_cols - 1], data_units [du - du_cols], data_units [du - du_cols + 1],
                      data_units [du - 1], data_units [du], data_units [du + 1],
                      lowerleft, lowercenter, lowerright) ;
      Inc (du) ;
      Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
      End ;

    FillEdges (data_units [du],
               upperleft, uppercenter, upperright,
               left, right,
               lowerleft, lowercenter, lowerright) ;
    lowerleft [Low (DATAUNITINDEX), High (DATAUNITINDEX)] :=
        data_units [du - 1][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    upperright [High (DATAUNITINDEX), Low (DATAUNITINDEX)] :=
        data_units [du - du_cols][High (DATAUNITINDEX), High (DATAUNITINDEX)] ;
    FilterDataUnit (offset, rowwidth,
                    h_sampling, v_sampling,
                    data_units [du - du_cols - 1], data_units [du - du_cols], upperright,
                    data_units [du - 1], data_units [du], right,
                    lowerleft, lowercenter, lowerright) ;
    // The remaining increments are just for debugging.
    Inc (du) ;
    Inc (offset, h_sampling * JPEGSAMPLEWIDTH) ;
    Inc (offset, rowwidth * (JPEGSAMPLEWIDTH * v_sampling - 1)) ;

    End ;

  begin

  rowwidth := du_cols * h_sampling * JPEGSAMPLEWIDTH ;

  extendRightColumn ;
  extendBottomRow ;

  if (du_rows = 1) And (du_cols = 1) Then
    // Special case where the component has just one data unit.
    FilterSingleDataUnitImage
  else if (du_rows = 1) Then
    // Special case of a component with a single row of data units.
    FilterSingledataUnitRowImage
  else if (du_cols = 1) Then
    // Special case of a component with a single column of data units.
    FilterSingledataUnitColumnImage
  else
    // General case. The component has at least 2 rows and two columns of data
    // units.
    FilterNormalImage ;

  End ;

Procedure TJpegDecoderComponent.resetDcDifference ;
  begin
  last_dc_value := 0 ;
  End ;

End.
