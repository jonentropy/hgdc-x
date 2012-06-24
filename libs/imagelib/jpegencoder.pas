unit jpegencoder;
//
// Copyright (c) 2001 Colosseum Builders, Inc.
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
//  JPEG Encoder Library.
//
//  Title: JpegEncoder class definition.           
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class is an encoder for JPEG image. The process for using this
//    class is to call the property functions to set the attributes for the
//    stream then call the writeImage function to create a JPEG stream with
//    those attributes.
//

interface

Uses
  bitmapimage, jpegpvt, jpegoutputstream, jpegencoderdataunit, jpeghuffmanencoder,
  systemspecific ;

Const
  MAXCOMPONENTS = 4 ;
  MAXQUALITY = 9 ;

Type
  QUALITYVALUE = 0..MAXQUALITY ;

  // Values assigned according to the JFIF standard
  COMPONENT = (YCOMPONENT = 1, CBCOMPONENT = 2, CRCOMPONENT = 3) ;
  COMPONENTSET = Set Of COMPONENT ;

  TJpegEncoderComponent = class ;

  COMPONENTID = 1..MAXCOMPONENTS ;
  SCANID = 1..256 ;

  TScan = Record
    // Bitmap of components in the scan.
    component_mask  : COMPONENTSET ;
    // Spectral Selection for the scan.
    spectral_selection_start : COEFFICIENTINDEX ;
    spectral_selection_end : COEFFICIENTINDEX ;
    // Successive Approximation for the first iteration of the scan
    successive_approximation : JPEGSUCCESSIVAPPROXIMATIONVALUE ;
    // Successive Approximation Progress. Updated as scans are output.
    successive_approximation_high : Integer ;
    successive_approximation_low : Integer ;
    End ;

  DCOUTPUTFUNCTION = Procedure (value, refine : Integer) Of Object ;
  ACOUTPUTFUNCTION = Procedure (huffman, value, size : Integer) Of Object ;
  COMPONENTPASSFUNCTION  = Procedure (row, col : Cardinal ;
                                      dcfunction : DCOUTPUTFUNCTION ;
                                      acfunction : ACOUTPUTFUNCTION ;
                                      sss, sse : COEFFICIENTINDEX ;
                                      ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) Of Object ;

  TJpegEncoder = Class (TBitmapImageEncoder)
    Private
      // Quantization Tables
      quantization_tables : Array [0..JPEGMAXQUANTIZATIONTABLES-1] of TJpegEncoderQuantizationTable ;

      // Huffman Tables
      ac_tables : Array [0..1] Of TJpegHuffmanEncoder ;
      dc_tables : Array [0..1] Of TJpegHuffmanEncoder ;

      // Properties
      gray_scale : Boolean ;
      rows_per_restart : Cardinal ;
      restart_interval : Cardinal ;
      image_quality : QUALITYVALUE ;
      comment_string : String ;
      progressive_mode : Boolean ;

      total_passes : Cardinal ;
      current_pass : Cardinal ;

      in_progress : Boolean ;

      verbose_flag : Boolean ;

      // Image Size
      frame_width : Cardinal ;
      frame_height : Cardinal ;
      // Maximum Frequencies in all components
      max_horizontal_frequency : Cardinal ;
      max_vertical_frequency : Cardinal ;
      // MCU Dimensions
      mcu_rows : Cardinal ;
      mcu_cols : Cardinal ;

      scan_count : Cardinal ;

      // Scan Descriptors
      image_scans : Array [SCANID] of TScan ;

      // Components
      image_components : Array [1..MAXCOMPONENTS] of TJpegEncoderComponent ;
      // Index of the quantization table assigned to each component.
      quantization_table_assignments : Array [1..MAXCOMPONENTS] of UBYTE1 ;

      // Components used in the scan being processed.
      scan_component_count : Cardinal ;
      scan_components : Array [1..JPEGMAXCOMPONENTSPERSCAN] Of TJpegEncoderComponent ;

      Procedure initialize ;

      // This function determines if the output scan parameters are valid. It
      // raises the EJpegError exception an inconsistency is found.
      Procedure validateParameters ;

      Procedure outputMarker (strm : TJpegOutputStream ;
                              marker : Byte) ;

      // Block Output Methods
      Procedure printQuantizationTables (strm : TJpegOutputStream) ;
      Procedure printSequentialFrame (strm : TJpegOutputStream ; image : TBitmapImage) ;
      Procedure printProgressiveFrame (strm : TJpegOutputStream ; image : TBitmapImage) ;
      Procedure printComment (outputstream : TJpegOutputStream ; str : String) ;
      Procedure outputJfifHeader (strm : TJpegOutputStream) ;

      Procedure outputRestartInterval (strm : TJpegOutputStream ;
                                       restartinterval : UBYTE2) ;

      Procedure printHuffmanTables (strm : TJpegOutputStream ;
                                    scan : TScan ;
                                    usedc, useac : Boolean) ;

      // Sequential Scan Output
      Procedure printSequentialScan (strm : TJpegOutputStream ; scan : TScan) ;

      // Progressive Output Functions
      Procedure printProgressiveScan (strm : TJpegOutputStream ; scan : TScan) ;
      Procedure printDcFirst (strm : TJpegOutputStream ; scan : TScan) ;
      Procedure printDcRefine (strm : TJpegOutputStream ; scan : TScan) ;
      Procedure printAcFirst (strm : TJpegOutputStream ; scan : TScan) ;
      Procedure printAcRefine (strm : TJpegOutputStream ; scan : TScan) ;
      Procedure firstAcData (strm : TJpegOutputStream ;
                             scan : TScan ;
                             outputrestarts : Boolean ;
                             acfunction : ACOUTPUTFUNCTION) ;
      Procedure refineAcData (strm : TJpegOutputStream ;
                              scan : TScan ;
                              outputrestarts : Boolean ;
                              acfunction : ACOUTPUTFUNCTION) ;


      Procedure interleavedPass (
               strm : TJpegOutputStream ;
               writedata : boolean ;
               passfunction : Array Of COMPONENTPASSFUNCTION ;
               dcfunction   : Array Of DCOUTPUTFUNCTION ;
               acfunction   : Array Of ACOUTPUTFUNCTION ;
               sss, sse : COEFFICIENTINDEX ;
               ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure noninterleavedPass (
               strm : TJpegOutputStream ;
               writedata : Boolean ;
               passfunction : COMPONENTPASSFUNCTION ;
               dcfunction : DCOUTPUTFUNCTION ;
               acfunction : ACOUTPUTFUNCTION ;
               sss, sse : COEFFICIENTINDEX ;
               ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure resetDcValues ;
      Procedure calculateMcuDimensions ;
      Procedure countPassesForProgressReporting ;

      Procedure findComponentsInScan (scan : TScan) ;

      Procedure outputRestartMarker (strm : TJpegOutputStream ; marker : Cardinal) ;

      Procedure setQuality (value : QUALITYVALUE) ;
      Procedure setProgressive (value : Boolean) ;

    Protected
      Procedure callProgressFunction (description : String ; progress : Cardinal) ;

    Public
      Constructor Create  ;
      Destructor Destroy ; Override ;

      Procedure writeImage (strm : TJpegOutputStream ;
                            image : TBitmapImage) ;
      Procedure writeImageFile (filename : String ; image : TBitmapImage) ; Override ;


      // Image Quality
      Property Quality : QUALITYVALUE read image_quality Write setQuality ;

      // Grayscale Mode (True=Gray Scale, False=Color)
      Property Grayscale : Boolean read gray_scale write gray_scale ;

      // Progressive Mode (True=Progressive, False=Sequential)
      Property Progressive : Boolean Read progressive_mode Write SetProgressive ;

      // Number of rows between restart markers (0=> No restart markers)
      Property RowsPerRestart : Cardinal Read rows_per_restart Write rows_per_restart ;

      // Comment String
      Property Comment : String Read comment_string Write comment_string ;

      // Component Sampling Frequencies (1-4)
      Procedure setSamplingFrequency (component : COMPONENTID ; hf, vf : JPEGSAMPLINGFREQUENCY) ;
      Procedure getSamplingFrequency (component : COMPONENTID ; var hf, vf : JPEGSAMPLINGFREQUENCY) ;

      // Scan Attributes {scan number, component bit mask,
      // spectral selection end (0-63), successive approximation (0-13) }
      Procedure setScanAttributes (scan : SCANID ;
                                   components : COMPONENTSET ;
                                   sse : COEFFICIENTINDEX ;
                                   ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure getScanAttributes (scan : SCANID ;
                                   Var components : COMPONENTSET ;
                                   Var sse : COEFFICIENTINDEX ;
                                   Var ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

    Procedure setVerbose (state : boolean) ;
    End ;

  RGBTOYCBCRFUNCTION = Function (red, green, blue : JPEGSAMPLE) : JPEGSAMPLE ;

  TJpegEncoderComponent = class
    private
      // DCT Coefficients and dimensions
      du_rows, du_cols : Cardinal ;
      dct_coefficients : Array of TJpegCoefficientBlock ;

      // EOB-run in context
      eob_run : Cardinal ;
      eob_start_du_row, eob_start_du_col : Cardinal ;
      eob_start_position : Cardinal ;

      // Sampling Frequencies
      v_frequency, h_frequency : JPEGSAMPLINGFREQUENCY ;

      // Huffman and Quantization tables
      ac_table, dc_table : TJpegHuffmanEncoder ;
      quantization_table : TJpegEncoderQuantizationTable ;
      output_stream : TJpegOutputStream ;

      // Last DC value used to calculate DC differences
      last_dc_value : Integer ;

      component_buffer : Array of JPEGSAMPLE ;
      component_buffer_rows, component_buffer_columns : Cardinal ;

      noninterleaved_du_rows, noninterleaved_du_colummns : Cardinal ;

      Procedure sample1to1Component (encoder : TJpegEncoder) ;
      Procedure sampleNtoNComponent (encoder : TJpegEncoder ; maxhf, maxvf : JPEGSAMPLINGFREQUENCY ) ;

    Public
      Constructor Create ;

      Procedure printAcData (huffvalue, value, size : Integer) ;
      Procedure printDcData (huffvalue, bits : Integer) ;
      Procedure gatherAcData (huffvalue, value, size : Integer) ;
      Procedure gatherDcData (huffvalue, bits : Integer) ;

      Procedure freeDynamicStorage ;

      Procedure encodeSequential (row, col : Cardinal ;
                                  dcfunction : DCOUTPUTFUNCTION ;
                                  acfunction : ACOUTPUTFUNCTION ;
                                  sss, sse : COEFFICIENTINDEX ;
                                  ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure progressiveDcFirst (row, col : Cardinal ;
                                    dcfunction : DCOUTPUTFUNCTION ;
                                    acfunction : ACOUTPUTFUNCTION ;
                                    sss, sse : COEFFICIENTINDEX ;
                                    ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure progressiveDcRefine (row, col : Cardinal ;
                                     dcfunction : DCOUTPUTFUNCTION ;
                                     acfunction : ACOUTPUTFUNCTION ;
                                     sss, sse : COEFFICIENTINDEX ;
                                     ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure progressiveAcFirst (row, col : Cardinal ;
                                    acfunction : ACOUTPUTFUNCTION ;
                                    sss, sse : COEFFICIENTINDEX ;
                                    ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure progressiveAcRefine (row, col : Cardinal ;
                                     acfunction : ACOUTPUTFUNCTION ;
                                     sss, sse : COEFFICIENTINDEX ;
                                     ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;

      Procedure resetEobRun ;
      Procedure printEobRun (acfunction : ACOUTPUTFUNCTION) ;
      Procedure printRefineEobRun (acfunction : ACOUTPUTFUNCTION ;
                                   sss, sse, ssa : Cardinal) ;

      Procedure resetDcDifference ;

      Procedure setHuffmanTables (dc, ac : TJpegHuffmanEncoder) ;

      class Procedure rgbConvert (encoder : TJpegEncoder ;
                            image : TBitmapImage ;
                            maxhf, maxvf : JPEGSAMPLINGFREQUENCY ;
                            ycomponent, cbcomponent, crcomponent : TJpegEncoderComponent) ;
      class Procedure grayScaleConvert (encoder : TJpegEncoder ;
                                  image : TBitmapImage ;
                                  ycomponent : TJpegEncoderComponent) ;
      Procedure sampleComponent (encoder : TJpegEncoder ;
                                 maxhf, maxvf : JPEGSAMPLINGFREQUENCY) ;

      Procedure allocateComponentBuffer (imagewidth, imageheight : Cardinal ;
                                maxhf, maxvf : JPEGSAMPLINGFREQUENCY) ;

      Property HorizontalFrequency : JPEGSAMPLINGFREQUENCY read h_frequency write h_frequency ;
      Property VerticalFrequency : JPEGSAMPLINGFREQUENCY read v_frequency write v_frequency ;

      Property QuantizationTable : TJpegEncoderQuantizationTable read quantization_table write quantization_table ;
      Property OutputStream : TJpegOutputStream read output_stream write output_stream ;
      Property noninterleavedDuColumns : Cardinal read noninterleaved_du_colummns write noninterleaved_du_colummns ;
      Property noninterleavedDuRows : Cardinal read noninterleaved_du_rows write noninterleaved_du_rows ;
      Property dataUnitRows : Cardinal read du_rows ;
      Property dataUnitCols : Cardinal read du_cols ;

    End ;

  EJpegError = class (EGraphicsException) ;

implementation

Uses
  jfif, jpegoutputfilestream ;

Type
  QuantizationValues = Array [0..JPEGSAMPLESIZE-1] of Cardinal ;

const
  CHAR_BIT = 8 ;

  QTABLEWORST : QuantizationValues = (
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255) ;

  QTABLENIMAQ1 : QuantizationValues = (
 13,  13,  15,  25,  40,  68, 114, 201,
 13,  16,  22,  32,  49,  79, 131, 230,
 16,  22,  38,  54,  78, 117, 188, 255,
 26,  33,  56,  93, 142, 213, 255, 255,
 45,  54,  84, 149, 255, 255, 255, 255,
 79,  92, 134, 237, 255, 255, 255, 255,
144, 165, 232, 255, 255, 255, 255, 255,
255, 255, 255, 255, 255, 255, 255, 255) ;

  QTABLEDODQ1 : QuantizationValues = (
  8,  72,  72,  72,  78,  89, 106, 129,
 72,  72,  72,  74,  81,  93, 111, 135,
 72,  72,  76,  84,  94, 108, 128, 155,
 72,  74,  84,  99, 116, 136, 160, 193,
 78,  81,  94, 116, 145, 177, 213, 255,
 89,  93, 108, 136, 177, 228, 255, 255,
106, 111, 128, 160, 213, 255, 255, 255,
129, 135, 155, 193, 255, 255, 255, 255) ;

// K.1 Table K-1
  QTABLEJPEGL1 : QuantizationValues = (
 16,  11,  10,  16,  24,  40,  51,  61,
 12,  12,  14,  19,  26,  58,  60,  55,
 14,  13,  16,  24,  40,  57,  69,  56,
 14,  17,  22,  29,  51,  87,  80,  62,
 18,  22,  37,  56,  68, 109, 103,  77,
 24,  35,  55,  64,  81, 104, 113,  92,
 49,  64,  78,  87, 103, 121, 120, 101,
 72,  92,  95,  98, 112, 100, 103,  99) ;

  QTABLEDODQ2 : QuantizationValues = (
  8,  36,  36,  36,  39,  45,  53,  65,
 36,  36,  36,  37,  41,  47,  56,  68,
 36,  36,  38,  42,  47,  54,  64,  78,
 36,  37,  42,  50,  59,  69,  81,  98,
 39,  41,  47,  59,  73,  89, 108, 130,
 45,  47,  54,  69,  89, 115, 144, 178,
 53,  56,  64,  81, 108, 144, 190, 243,
 65,  68,  78,  98, 130, 178, 243, 255) ;

  QTABLENIMAQ2 : QuantizationValues = (
  8,   8,   8,  12,  19,  32,  50,  83,
  8,   8,  11,  15,  23,  35,  57,  93,
  9,  11,  18,  25,  35,  50,  77, 124,
 13,  16,  26,  41,  59,  84, 126, 196,
 21,  25,  38,  62,  99, 151, 230, 255,
 36,  41,  58,  94, 161, 255, 255, 255,
 63,  71,  95, 150, 255, 255, 255, 255,
115, 127, 166, 255, 255, 255, 255, 255) ;

  QTABLENIMAQ3 : QuantizationValues = (
 10,  10,  10,  10,  10,  11,  15,  20,
 10,  10,  10,  10,  10,  12,  16,  21,
 10,  10,  10,  10,  11,  14,  18,  25,
 10,  10,  10,  12,  14,  18,  23,  32,
 11,  11,  12,  15,  19,  25,  32,  44,
 13,  14,  16,  20,  26,  35,  47,  63,
 18,  20,  22,  28,  37,  50,  68,  94,
 28,  29,  33,  41,  54,  74, 102, 143) ;

  QTABLEDODQ3 : QuantizationValues = (
  8,  10,  10,  10,  11,  13,  15,  18,
 10,  10,  10,  10,  11,  13,  16,  19,
 10,  10,  11,  12,  13,  15,  18,  22,
 10,  10,  12,  14,  16,  19,  23,  27,
 11,  11,  13,  16,  21,  25,  30,  36,
 13,  13,  15,  19,  25,  32,  40,  50,
 15,  16,  18,  23,  30,  40,  53,  68,
 18,  19,  22,  27,  36,  50,  68,  91) ;

  QTABLEDODQ4 : QuantizationValues = (
  8,   7,   7,   7,   8,   9,  11,  13,
  7,   7,   7,   7,   8,   9,  11,  14,
  7,   7,   8,   8,   9,  11,  13,  16,
  7,   7,   8,  10,  12,  14,  16,  20,
  8,   8,   9,  12,  15,  18,  22,  26,
  9,   9,  11,  14,  18,  23,  29,  36,
 11,  11,  13,  16,  22,  29,  38,  49,
 13,  14,  16,  20,  26,  36,  49,  65) ;

// K.1 Table K-1 (Each Element Divided By 2)
  QTABLEJPEGL2 : QuantizationValues = (
  8,   5,   5,   8,  12,  20,  25,  30,
  6,   6,   7,   9,  13,  28,  30,  27,
  7,   6,   8,  12,  20,  26,  34,  28,
  7,   8,  11,  14,  25,  43,  40,  31,
  9,  11,  18,  28,  34,  54,  51,  38,
 12,  17,  27,  32,  40,  52,  56,  46,
 24,  32,  39,  43,  51,  60,  60,  50,
 36,  46,  47,  49,  56,  50,  51,  49) ;

// NIMA Q4 and DoD Q5
  QTABLENIMAQ4 : QuantizationValues = (
  4,   4,   4,   4,   4,   5,   6,   7,
  4,   4,   4,   4,   5,   5,   6,   8,
  4,   4,   4,   5,   5,   6,   7,   9,
  4,   4,   5,   6,   6,   8,   9,  11,
  4,   5,   5,   6,   8,  10,  12,  14,
  5,   5,   6,   8,  10,  13,  16,  20,
  6,   6,   7,   9,  12,  16,  21,  27,
  7,   8,   9,  11,  14,  20,  27,  36) ;

// NIMA Q5
  QTABLEBEST : QuantizationValues = (
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1,
  1,   1,   1,   1,   1,   1,   1,   1) ;

  QTABLENIMADOWNSAMPLE : QuantizationValues = (
 36,  36,  37,  39,  42,  45,  50,  54,
 36,  37,  39,  42,  45,  50,  54,  60,
 37,  39,  42,  45,  50,  54,  60,  66,
 39,  42,  45,  50,  54,  60,  66,  74,
 42,  45,  50,  54,  60,  66,  74,  81,
 45,  50,  54,  60,  66,  74,  81,  90,
 50,  54,  60,  66,  74,  81,  90,  99,
 54,  60,  66,  74,  81,  90,  88, 110) ;

// K.1 Table K-2
  QTABLEJPEGC1 : QuantizationValues = (
 17,  18,  24,  47,  99,  99,  99,  99,
 18,  21,  26,  66,  99,  99,  99,  99,
 24,  26,  56,  99,  99,  99,  99,  99,
 47,  66,  99,  99,  99,  99,  99,  99,
 99,  99,  99,  99,  99,  99,  99,  99,
 99,  99,  99,  99,  99,  99,  99,  99,
 99,  99,  99,  99,  99,  99,  99,  99,
 99,  99,  99,  99,  99,  99,  99,  99) ;

//K.1 Table K-2 (Each Element Divided By 2)
  QTABLEJPEGC2 : QuantizationValues = (
  8,   9,  12,  23,  49,  49,  49,  49,
  9,  10,  13,  33,  49,  49,  49,  49,
 12,  13,  28,  49,  49,  49,  49,  49,
 23,  33,  49,  49,  49,  49,  49,  49,
 49,  49,  49,  49,  49,  49,  49,  49,
 49,  49,  49,  49,  49,  49,  49,  49,
 49,  49,  49,  49,  49,  49,  49,  49,
 49,  49,  49,  49,  49,  49,  49,  49) ;

luminance_quantization_tables : Array [QUALITYVALUE ] Of ^QuantizationValues =
(
@QTABLEDODQ1,
@QTABLEDODQ2,
@QTABLENIMAQ1,
@QTABLEJPEGL1,
@QTABLENIMAQ2,
@QTABLEDODQ3,
@QTABLENIMAQ3,
@QTABLEDODQ4,
@QTABLENIMAQ4,
@QTABLEBEST
) ;

chrominance_quantization_tables : Array [QUALITYVALUE ] Of ^QuantizationValues =
(
@QTABLEDODQ1,
@QTABLEDODQ2,
@QTABLENIMAQ1,
@QTABLEJPEGC1,
@QTABLENIMAQ2,
@QTABLEDODQ3,
@QTABLENIMAQ3,
@QTABLEDODQ4,
@QTABLENIMAQ4,
@QTABLEBEST
) ;

//
//  Description:
//
//    Class Default Constructor
//
Constructor TJpegEncoder.Create ;
  Begin
  Inherited Create ;
  initialize ;
  End ;

//
//  Description:
//
//    Class Destructor
//
Destructor TJpegEncoder.Destroy ;
  Var
    ii : Cardinal ;
  Begin
  for ii := Low (quantization_tables) To High (quantization_tables) Do
    quantization_tables [ii].Destroy ;

  For ii := Low (image_components) To High (image_components) Do
    image_components [ii].Destroy ;

  For ii := Low (ac_tables) To High (ac_tables) Do
    ac_tables [ii].Destroy ;

  For ii := Low (dc_tables) To High (dc_tables) Do
    dc_tables [ii].Destroy ;

  End ;

//
//  Description:
//
//    This function writes an image to the output stream.
//
//  Parameters:
//    strm:   The output stream to write the image to. This most be opened in
//            binary mode
//    image:  The image to output.
//
Procedure TJpegEncoder.writeImage (strm : TJpegOutputStream ; image : TBitmapImage) ;
  Var
    ii : Integer ;
  Begin
  image_components [Ord (YCOMPONENT)].OutputStream  := strm ;
  image_components [Ord (CBCOMPONENT)].OutputStream := strm ;
  image_components [Ord (CRCOMPONENT)].OutputStream := strm ;

  for ii := Low (quantization_tables) To High (quantization_tables) Do
    quantization_tables [ii].isUsed := false ;

  if gray_scale Then
    Begin
    quantization_tables [quantization_table_assignments [Ord (YCOMPONENT)]].isUsed := true ;
    End
  else
    Begin
    quantization_tables [quantization_table_assignments [Ord (YCOMPONENT)]].isUsed  := true ;
    quantization_tables [quantization_table_assignments [Ord (CBCOMPONENT)]].isUsed := true ;
    quantization_tables [quantization_table_assignments [Ord (CRCOMPONENT)]].isUsed := true ;
    End ;

  for ii := Low (quantization_tables) To High (quantization_tables) Do
    Begin
    if quantization_tables [ii].isUsed Then
      quantization_tables [ii].buildScaledTables ;
    End ;

  frame_height := image.Height ;
  frame_width := image.Width ;

  // Find the MCU size and maximum sampling frequencies.
  calculateMcuDimensions ;

  // Validate Parameters. If there is an error this function will raise
  // an exception.
  validateParameters ;

  countPassesForProgressReporting ;

  // Write the image header.
  outputMarker (strm, SOI) ;
  outputMarker (strm, APP0) ;
  outputJfifHeader (strm) ;

  if comment_string <> '' Then
    printComment (strm, comment_string) ;

  printComment (strm, 'Created using the Colosseum Builders JPEG library') ;

  if progressive_mode Then
    printProgressiveFrame (strm, image)
  else
    printSequentialFrame (strm, image) ;

  outputMarker (strm, EOI) ;

  // Make sure that we are not holding on to any memory we do not
  // need any more.
  image_components [Ord (YCOMPONENT)].freeDynamicStorage ;
  image_components [Ord (CBCOMPONENT)].freeDynamicStorage ;
  image_components [Ord (CRCOMPONENT)].freeDynamicStorage ;
  End ;

//
//  Description:
//
//    Class Initialization function. This function is intended to be
//    called from constructors.
//
Procedure TJpegEncoder.initialize ;
  Var
    ii : Cardinal ;
  Begin
  in_progress := false ;

  for ii := Low (SCANID) to High (SCANID) Do
    Begin
    image_scans [ii].component_mask := [] ;
    image_scans [ii].spectral_selection_start := 0 ;
    image_scans [ii].spectral_selection_end := 0 ;
    image_scans [ii].successive_approximation := 0 ;
    image_scans [ii].successive_approximation_high := 0 ;
    image_scans [ii].successive_approximation_low := 0 ;
    End ;

  For ii := Low (image_components) To High (image_components) Do
    image_components [ii] := TJpegEncoderComponent.Create ;


  For ii := Low (ac_tables) To High (ac_tables) Do
    ac_tables [ii] := TJpegHuffmanEncoder.Create ;

  For ii := Low (dc_tables) To High (dc_tables) Do
    dc_tables [ii] := TJpegHuffmanEncoder.Create ;

  image_scans [1].component_mask := [YCOMPONENT, CBCOMPONENT, CRCOMPONENT] ;

  for ii := Low (image_components) To High (image_components) Do
    Begin
    image_components [ii] := TJpegEncoderComponent.Create ;
    image_components [ii].HorizontalFrequency := 1 ;
    image_components [ii].VerticalFrequency   := 1 ;
    End ;

  for ii := Low (quantization_tables) To High (quantization_tables) Do
    Begin
    quantization_tables [ii] := TJpegEncoderQuantizationTable.Create ;
    End ;

  quantization_table_assignments [Ord (YCOMPONENT)]  := 0 ;
  quantization_table_assignments [Ord (CBCOMPONENT)] := 1 ;
  quantization_table_assignments [Ord (CRCOMPONENT)] := 1 ;

  image_components [Ord (YCOMPONENT)].setHuffmanTables (dc_tables [0],
                                                  ac_tables [0]) ;
  image_components [Ord (CBCOMPONENT)].setHuffmanTables (dc_tables [1],
                                                   ac_tables [1]) ;
  image_components [Ord (CRCOMPONENT)].setHuffmanTables (dc_tables [1],
                                                   ac_tables [1]) ;

  image_components [Ord (YCOMPONENT)].QuantizationTable :=
      quantization_tables [quantization_table_assignments [Ord (YCOMPONENT)]] ; ;
  image_components [Ord (CBCOMPONENT)].QuantizationTable :=
      quantization_tables [quantization_table_assignments [Ord (CBCOMPONENT)]] ; ;
  image_components [Ord (CRCOMPONENT)].QuantizationTable :=
      quantization_tables [quantization_table_assignments [Ord (CRCOMPONENT)]] ; ;

  progressive_mode := false ;
  setQuality (3) ;
  progress_function := Nil ;
  progress_data := Nil ;
  restart_interval := 0 ;
  rows_per_restart := 0 ;
  gray_scale := false ;

  End ;

//
//  This function writes a marker to the output stream.
//
//  Parameters:
//    marker: The marker to be written to the output stream
//
Procedure TJpegEncoder.outputMarker (strm : TJpegOutputStream ;
                                     marker : Byte) ;
  Begin
  strm.writeByte (SOB) ;
  strm.writeByte (marker) ;
  End ;

//
//  Description:
//
//    This function validates the user-set output parameters. If an error is
//    detected an EJpegBadOutputParameter exception is raised.
//

Procedure TJpegEncoder.validateParameters ;
  Procedure CheckForFractionalSampling ;
    Begin
  // Ensure we do not have fractional sampling of pixels.
    if (image_components [Ord (YCOMPONENT)].HorizontalFrequency = 3)
       Or (image_components [Ord (CBCOMPONENT)].HorizontalFrequency = 3)
       Or (image_components [Ord (CRCOMPONENT)].HorizontalFrequency = 3) Then
      Begin
      if (image_components [Ord (YCOMPONENT)].HorizontalFrequency = 2)
         Or (image_components [Ord (YCOMPONENT)].HorizontalFrequency = 4)
         Or (image_components [Ord (CBCOMPONENT)].HorizontalFrequency = 2)
         Or (image_components [Ord (CBCOMPONENT)].HorizontalFrequency = 4)
         Or (image_components [Ord (CRCOMPONENT)].HorizontalFrequency = 2)
         Or (image_components [Ord (CRCOMPONENT)].HorizontalFrequency = 4) Then
        Begin
        Raise EJpegError.Create ('Fractional Horizontal Sampling') ;
        End ;
      End ;

    if (image_components [Ord (YCOMPONENT)].VerticalFrequency = 3)
       Or (image_components [Ord (CBCOMPONENT)].VerticalFrequency = 3)
       Or (image_components [Ord (CRCOMPONENT)].VerticalFrequency = 3) Then
      Begin
      if (image_components [Ord (YCOMPONENT)].VerticalFrequency = 2)
         Or (image_components [Ord (YCOMPONENT)].VerticalFrequency = 4)
         Or (image_components [Ord (CBCOMPONENT)].VerticalFrequency = 2)
         Or (image_components [Ord (CBCOMPONENT)].VerticalFrequency = 4)
         Or (image_components [Ord (CRCOMPONENT)].VerticalFrequency = 2)
         Or (image_components [Ord (CRCOMPONENT)].VerticalFrequency = 4) Then
        Begin
        Raise EJpegError.Create ('Fractional Vertical Sampling') ;
        End ;
      End ;
    End ;

  Procedure CheckProgressiveScans ;
    Var
      lasty  : Integer ;
      lastcb : Integer ;
      lastcr : Integer ;
    Begin
    // For a progressive scan the following rules apply
    //
    // o The spectral selection start can be zero if and only if
    // the spectral selection end is zero.
    //
    // o For each component the zero spectral selection start must occur
    // before any other.
    //
    // o If the spectral selection start is not zero there may only be
    // one component in a scan.
    //
    // o The entire spectral range must be specified for each component.
    //
    // o There can be no overlapping spectral ranges in scans.
    //
    lasty  := -1 ;
    lastcb := -1 ;
    lastcr := -1 ;

    if gray_scale Then
      Begin
      // For a grayscale image the Cb and Cr components are not used.
      lastcb := High (COEFFICIENTINDEX) ;
      lastcr := High (COEFFICIENTINDEX) ;
      End ;

    scan_count := 0 ;
    while (scan_count <= High (SCANID))
          And Not ((lasty = High (COEFFICIENTINDEX))
          And (lastcb = High (COEFFICIENTINDEX))
          And (lastcr = High (COEFFICIENTINDEX))) DO
      Begin
      Inc (scan_count) ;
      if image_scans [scan_count].component_mask = [] Then
        Raise EJpegError.Create ('Scan contains no components') ;

      if image_scans [scan_count].successive_approximation > High (JPEGSUCCESSIVAPPROXIMATIONVALUE) Then
        Raise EJpegError.Create ('Successive Approximation too large') ;

      image_scans [scan_count].successive_approximation_low := image_scans [scan_count].successive_approximation ;
      image_scans [scan_count].successive_approximation_high := 0 ;

      // Y Component Validation
      if YCOMPONENT In image_scans [scan_count].component_mask Then
        Begin
        if image_scans [scan_count].spectral_selection_end = Low (COEFFICIENTINDEX) Then
          Begin
          if lasty <> -1 Then
            Raise EJpegError.Create ('Duplicate Y Component Scan') ;

          image_scans [scan_count].spectral_selection_start := 0 ;
          lasty := 0 ;
          End
        Else
          Begin
          If image_scans [scan_count].component_mask <> [YCOMPONENT] Then
            Raise EJpegError.Create ('Multiple Components in AC Scan') ;

          if (image_scans [scan_count].spectral_selection_end <> Low (COEFFICIENTINDEX))
              And (lasty = -1) Then
            Raise EJpegError.Create ('AC Scan specified before DC scan for Y Component') ;

          if image_scans [scan_count].spectral_selection_end <= lasty Then
            Raise EJpegError.Create ('Duplicate or overlapping spectral selection for Y Component') ;

          image_scans [scan_count].spectral_selection_start := lasty + 1 ;
          lasty := image_scans [scan_count].spectral_selection_end ;
          End ;
        End ;

      if Not gray_scale Then
        Begin
        // Cb Component Validation
        if CBCOMPONENT In image_scans [scan_count].component_mask Then
          Begin
          if image_scans [scan_count].spectral_selection_end = Low (COEFFICIENTINDEX) Then
            Begin
            if lastcb <> -1 Then
              Raise EJpegError.Create ('Duplicate Cb Component Scan') ;

            image_scans [scan_count].spectral_selection_start := Low (COEFFICIENTINDEX) ;
            lastcb := Low (COEFFICIENTINDEX) ;
            End
          else
            Begin
            if image_scans [scan_count].component_mask <> [CBCOMPONENT] Then
              Raise EJpegError.Create ('Multiple Components in AC Scan') ;

            if (image_scans [scan_count].spectral_selection_end <> Low (COEFFICIENTINDEX)) And (lastcb = -1) Then
              Raise EJpegError.Create ('AC Scan specified before DC scan for cb Component') ;

            if image_scans [scan_count].spectral_selection_end <= lastcb Then
              Raise EJpegError.Create ('Duplicate or overlapping spectral selection for Cb Component') ;

            image_scans [scan_count].spectral_selection_start := lastcb + 1 ;
            lastcb := image_scans [scan_count].spectral_selection_end ;
            End ;
          End ;

        // Cr Component Validation
        if CRCOMPONENT In image_scans [scan_count].component_mask Then
          Begin
          if image_scans [scan_count].spectral_selection_end = Low (COEFFICIENTINDEX) Then
            Begin
            if lastcr <> -1 Then
              Raise EJpegError.Create ('Duplicate Cr Component Scan') ;

            image_scans [scan_count].spectral_selection_start := Low (COEFFICIENTINDEX) ;
            lastcr := Low (COEFFICIENTINDEX) ;
            End
          else
            Begin
            if image_scans [scan_count].component_mask <> [CRCOMPONENT] Then
              Raise EJpegError.Create ('Multiple Components in AC Scan') ;

            if (image_scans [scan_count].spectral_selection_end <> Low (COEFFICIENTINDEX)) And (lastcr = -1) Then
              Raise EJpegError.Create ('AC Scan specified before DC scan for Cr Component') ;

            if image_scans [scan_count].spectral_selection_end <= lastcr Then
              Raise EJpegError.Create ('Duplicate or overlapping spectral selection for Cr Component') ;

            image_scans [scan_count].spectral_selection_start := lastcr + 1 ;
            lastcr := image_scans [scan_count].spectral_selection_end ;
            End ;
          End ;
        End ;
      End ;

    if lasty <> High (COEFFICIENTINDEX) Then
      Raise EJpegError.Create ('Y Component not completely defined by scans') ;
    if Not gray_scale Then
      Begin
      if lastcb <> High (COEFFICIENTINDEX) Then
        Raise EJpegError.Create ('Cb Component not completely defined by scans') ;
      if lastcr <> High (COEFFICIENTINDEX) Then
        Raise EJpegError.Create ('Cr Component not completely defined by scans') ;
      End ;
    End ;

  Procedure CheckSequentialScans ;
//    Const
 //      ALL = [YCOMPONENT, CBCOMPONENT, CRCOMPONENT] ;
    Var
      ALL : COMPONENTSET ;
    Begin
    ALL := [YCOMPONENT, CBCOMPONENT, CRCOMPONENT]  ;
    if Not gray_scale Then
      Begin
      if (image_scans [1].component_mask
          + image_scans [2].component_mask
          + image_scans [3].component_mask)
          <> ALL Then
        Raise EJpegError.Create ('Not all components specified in scans') ;

      if (image_scans [1].component_mask
          * image_scans [2].component_mask
          * image_scans [3].component_mask)
          <> [] Then
        Raise EJpegError.Create ('Component in more than one scan') ;

      if image_scans [3].component_mask <> [] Then
        scan_count := 3
      else if image_scans [2].component_mask <> [] Then
        scan_count := 2
      else
        scan_count := 1 ;
      End
    else
      Begin
      scan_count := 1 ;
      End ;
    End ;

  Procedure EnforceMcuLimits ;
    Var
      ii : Cardinal ;
      dataunits, componentcount : Cardinal ;
    Begin
    // Enforce the MCU size limitation in Section B.2.3
    //
    // I am not certain why this limitation of 10 data units per MCU even
    // exists. It seems to be an silly arbitrary limit. Maybe its to set
    // a maximum upper size for an MCU decoding buffer. In any event we
    // must abide by it.
    for ii := 1 To scan_count Do
      Begin
      dataunits := 0 ;
      componentcount := 0 ;
      if YCOMPONENT In image_scans [ii].component_mask Then
        Begin
        Inc (dataunits, image_components [Ord (YCOMPONENT)].HorizontalFrequency
                     * image_components [Ord (YCOMPONENT)].VerticalFrequency) ;
        Inc (componentcount) ;
        End ;
      if CBCOMPONENT In image_scans [ii].component_mask Then
        Begin
        Inc (dataunits, image_components [Ord (CBCOMPONENT)].HorizontalFrequency
                  * image_components [Ord (CBCOMPONENT)].VerticalFrequency) ;
        Inc (componentcount) ;
        End ;
      if CRCOMPONENT In image_scans [ii].component_mask Then
        Begin
        Inc (dataunits, image_components [Ord (CRCOMPONENT)].HorizontalFrequency
                  * image_components [Ord (CRCOMPONENT)].VerticalFrequency) ;
        Inc (componentcount) ;
        End ;
      if (dataunits > JPEGMAXDATAUNITSPERMCU) And (componentcount > 1) Then
        Raise EJpegError.Create ('Data units in MCU exceeds 10') ;
      End ;
    End ;

  Begin
  if Not gray_scale Then
    CheckForFractionalSampling ;

  if progressive_mode Then
    CheckProgressiveScans 
  else
    CheckSequentialScans ;

  if Not gray_scale Then
    EnforceMcuLimits ;
  End ;

//
//  Description:
//
//    This function writes a comment marker to the output stream.
//
//  Parameters:
//    str:  The comment string
//
Procedure TJpegEncoder.printComment (outputstream : TJpegOutputStream ; str : String) ;
  Var
    length : Cardinal ;
  Begin
  length := sizeof (Word) + System.Length (str) + 1 ;
  if length > $FFFF then
    EJpegError.Create ('Comment text too long for COM marker') ;
  outputMarker (outputstream, COM) ;
  outputstream.writeBigEndianWord (Word (length)) ;
  outputstream.write (@str [1], System.Length (str)) ;
  outputstream.writeByte (0) ; // Terminator
  End ;


//
//  Description:
//
//    This function writes the quantization tables to the output stream.
//
Procedure TJpegEncoder.printQuantizationTables (strm : TJpegOutputStream) ;
  const
    precision = 0 ; // Byte Precision
  var
   pqtq : Byte ;
   tablecount : Cardinal ;
   ii, jj : Cardinal ;
  Begin

  // Count the number of quantization tables needed by the frame and
  // create the scaled version of the table used in the DCT.
  tablecount := 0 ;
  for ii := Low (quantization_tables) To High (quantization_tables) Do
    Begin
    if quantization_tables [ii].isUsed Then
      Inc (tablecount) ;
    End ;

  // Section B.2.4.1
  outputMarker (strm, DQT) ;
  strm.writeBigEndianWord (sizeof(Word) + tablecount * (sizeof (pqtq) + JPEGSAMPLESIZE)) ;

  for ii := Low (quantization_tables) To High (quantization_tables) Do
    Begin
    if quantization_tables [ii].isUsed Then
      Begin
      pqtq := (precision Shl 4) Or ii ;
      strm.writeByte (pqtq) ;
      for jj := Low (COEFFICIENTINDEX) To High (COEFFICIENTINDEX) Do
        strm.writeByte (quantization_tables [ii][JpegZigZagInputOrder [jj]]);
      End
    End ;
  End ;

//
//  Description:
//
//    This function is called by a scan to write a restart interval to the
//    output stream. We do not output a Define Restart Interval marker if
//    the restart interval is not changing.
//
//  Parameters:
//    restartinterval: The new restart interval
//
Procedure TJpegEncoder.outputRestartInterval (strm : TJpegOutputStream ;
                                              restartinterval : UBYTE2) ;
  Begin
  if restartinterval <> restart_interval Then
    Begin
    restart_interval :=  restartinterval ;

    // B.2.4.4
    outputMarker (strm, DRI) ;
    strm.writeBigEndianWord (4) ;
    strm.writeBigEndianWord (restartinterval) ;
    End ;
  End ;

//
//  Description:
//
//    This function writes a sequential frame to the output stream. We create
//    frames with either one (black & white) or three (color) components.
//
//  Parameters:
//    image: The image to be written in the frame
//
Procedure TJpegEncoder.printSequentialFrame (strm : TJpegOutputStream ;
                                             image : TBitmapImage) ;
  Var
    ii : Cardinal ;
  Begin
  // Sample the components
  if gray_scale Then
    Begin
    Inc (current_pass) ;
    TJpegEncoderComponent.grayScaleConvert (Self, image, image_components [Ord (YCOMPONENT)]) ;
    End
  else
    Begin
    Inc (current_pass) ;
    TJpegEncoderComponent.rgbConvert (Self,
                image,
                max_horizontal_frequency,
                max_vertical_frequency,
                image_components [Ord (YCOMPONENT)],
                image_components [Ord (CBCOMPONENT)],
                image_components [Ord (CRCOMPONENT)]) ;
    Inc (current_pass) ;
    image_components [Ord (YCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;
    Inc (current_pass) ;
    image_components [Ord (CBCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;
    Inc (current_pass) ;
    image_components [Ord (CRCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;
    End ;

  // Write the Frame Header
  // Section B.2.2

  outputMarker (strm, SOF0) ;
  if gray_scale Then  // Length
    strm.writeBigEndianWord (8 + 3)
  else
    strm.writeBigEndianWord (8 + 3 * 3) ;

  strm.writeByte (8) ; // Precision
  strm.writeBigEndianWord (image.Height) ;
  strm.writeBigEndianWord (image.Width) ;

  if gray_scale Then
    Begin
    strm.writeByte (1) ;  // Component Count
    strm.writeByte(Ord (YCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (YCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (YCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (YCOMPONENT)]) ;


    printQuantizationTables (strm) ;
    scan_component_count := 1 ;
    scan_components [1] := image_components [Ord (YCOMPONENT)] ;
    Inc (current_pass) ;
    printSequentialScan (strm, image_scans [1]) ;
    End
  else
    Begin
    strm.writeByte (3) ;  // Component Count
    strm.writeByte(Ord (YCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (YCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (YCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (YCOMPONENT)]) ;

    strm.writeByte(Ord (CBCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (CBCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (CBCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (CBCOMPONENT)]) ;

    strm.writeByte(Ord (CRCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (CRCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (CRCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (CRCOMPONENT)]) ;

    printQuantizationTables (strm) ;

    // Print the scans that make up the frame.
    for ii := 1 To scan_count Do
      Begin
      Inc (current_pass) ;
      if image_scans [ii].component_mask <> [] Then
        Begin
        findComponentsInScan (image_scans [ii]) ;
        printSequentialScan (strm, image_scans [ii]) ;
        End ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    This function writes a progressive frame to the output stream.
//
//  Parameters:
//    image: The image to be written in the frame
//
Procedure TJpegEncoder.printProgressiveFrame (strm : TJpegOutputStream ;
                                              image : TBitmapImage) ;
  Var
    doingwork : Boolean ;
    ii : Cardinal ;
  Begin
  if gray_scale Then
    Begin
    Inc (current_pass) ;
    TJpegEncoderComponent.grayScaleConvert (Self,
                                            image,
                                            image_components [Ord (YCOMPONENT)]) ;
    // Output JPEG SOF Marker
    // Section B.2.2
    outputMarker (strm, SOF2) ;
    strm.writeBigEndianWord (8 + 3) ;

    strm.writeByte (8) ; // Precision
    strm.writeBigEndianWord (image.Height)  ;
    strm.writeBigEndianWord (image.Width) ;

    strm.writeByte (1) ;  // Component Count
    strm.writeByte(Ord (YCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (YCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (YCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (YCOMPONENT)]) ;

    printQuantizationTables (strm) ;

    scan_component_count := 1 ;
    scan_components [1] := image_components [Ord (YCOMPONENT)] ;

    Repeat
      doingwork := false ;
      for ii := 1 To scan_count Do
        Begin
        if (YCOMPONENT In image_scans [ii].component_mask)
             And (image_scans [ii].successive_approximation_low <> 0) Then
          Begin
          Inc (current_pass) ;
          printProgressiveScan (strm, image_scans [ii]) ;
          doingwork := true ;
          image_scans [ii].successive_approximation_high
            := image_scans [ii].successive_approximation_low ;
          Dec (image_scans [ii].successive_approximation_low) ;
          End ;
        End
      Until Not doingwork ;
    End
  else
    Begin
    Inc (current_pass) ;
    TJpegEncoderComponent.rgbConvert (Self,
                                      image,
                                      max_horizontal_frequency,
                                      max_vertical_frequency,
                                      image_components [Ord (YCOMPONENT)],
                                      image_components [Ord (CBCOMPONENT)],
                                      image_components [Ord (CRCOMPONENT)]) ;
    Inc (current_pass) ;
    image_components [Ord (YCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;
    Inc (current_pass) ;
    image_components [Ord (CBCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;
    Inc (current_pass) ;
    image_components [Ord (CRCOMPONENT)].sampleComponent (Self,
                                                    max_horizontal_frequency,
                                                    max_vertical_frequency) ;

    // Output JPEG SOS Marker
    // Section B.2.2
    outputMarker (strm, SOF2) ;
    strm.writeBigEndianWord (8 + 3 * 3) ;

    strm.writeByte (8) ; // Precision
    strm.writeBigEndianWord (image.Height) ;
    strm.writeBigEndianWord (image.Width) ;

    strm.writeByte (3) ;  // Component Count
    strm.writeByte(Ord (YCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (YCOMPONENT)].HorizontalFrequency Shl 4)
        Or image_components [Ord (YCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (YCOMPONENT)]) ;

    strm.writeByte(Ord (CBCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (CBCOMPONENT)].HorizontalFrequency Shl 4)
         Or image_components [Ord (CBCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (CBCOMPONENT)]) ;  // Quantization Table

    strm.writeByte(Ord (CRCOMPONENT)) ;
    strm.writeByte (
        (image_components [Ord (CRCOMPONENT)].HorizontalFrequency Shl 4)
         Or image_components [Ord (CRCOMPONENT)].VerticalFrequency) ;
    strm.writeByte (quantization_table_assignments [Ord (CRCOMPONENT)]) ;  // Quantization Table

    printQuantizationTables (strm) ;

    // Because of successive approximation we may have to go through the
    // scan list several times. This flag gets set each time a scan
    // is processed. The first time we search the scan list and do
    // nothing this flag will be false and we are all done.
    Repeat
      doingwork := false ;
      for ii := 1 To scan_count Do
        Begin
        findComponentsInScan (image_scans [ii]) ;

        if (scan_component_count <> 0)
            And (image_scans [ii].successive_approximation_low >= 0) Then
          Begin
          Inc (current_pass) ;
          printProgressiveScan (strm, image_scans [ii]) ;
          doingwork := true ;
          image_scans [ii].successive_approximation_high
            := image_scans [ii].successive_approximation_low ;
          Dec (image_scans [ii].successive_approximation_low) ;
          End ;
        End ;
      Until Not doingwork ;
    End ;
  End ;


//
//  Description:
//
//    This function enables or disables progressive output. When the mode is
//    changed the image_scans is initialized with a default set of values for
//    the new mode.
//
//  Parameters:
//    value: (true=>Progressive Mode, false=>Sequential Mode)
//
Procedure TJpegEncoder.setProgressive (value : Boolean) ;
  Var
    ii : Cardinal ;
  Begin
  if value = progressive_mode Then
    Exit ;

  progressive_mode := value ;
  For II := Low (image_scans) To High (image_scans) Do
    Begin
    image_scans [ii].component_mask := [] ;
    image_scans [ii].spectral_selection_end := High (COEFFICIENTINDEX) ;
    End ;

  if progressive_mode Then
    Begin
    // For Progressive Scans our default is four scans.  The first
    // contains the DC coefficients for all three components. The next
    // three scans contain the AC coefficients for each component.

    image_scans [1].component_mask := [YCOMPONENT, CBCOMPONENT, CRCOMPONENT] ;
    image_scans [1].spectral_selection_end := Low (COEFFICIENTINDEX) ;
    image_scans [2].component_mask := [YCOMPONENT] ;
    image_scans [2].spectral_selection_end := High (COEFFICIENTINDEX) ;
    image_scans [3].component_mask := [CBCOMPONENT] ;
    image_scans [3].spectral_selection_end := High (COEFFICIENTINDEX) ;
    image_scans [4].component_mask := [CRCOMPONENT] ;
    image_scans [4].spectral_selection_end := High (COEFFICIENTINDEX) ;
    End
  else
    Begin
    // For sequential mode the default is to create one scan with
    // all components.
    image_scans [1].component_mask := [YCOMPONENT, CBCOMPONENT, CRCOMPONENT] ;
    End ;
  End ;

//
//  Description:
//
//    This function writes a sequential scan to the output stream.
//
//  Parameters:
//    scan:   The scan descriptor
//
Procedure TJpegEncoder.printSequentialScan (strm : TJpegOutputStream ;
                                            scan : TScan) ;
  Var
    passfunction : Array [0..3] Of COMPONENTPASSFUNCTION ;
    dcfunction   : Array [0..3] Of DCOUTPUTFUNCTION ;
    acfunction   : Array [0..3] Of ACOUTPUTFUNCTION ;
    ii : Cardinal ;
  Begin

  // Output the restart interval and the Huffman tables. We must set the
  // restart interval *BEFORE* gathering statistics.
  if scan_component_count <> 1 Then
    outputRestartInterval (strm, rows_per_restart * mcu_cols)
  else
    outputRestartInterval (strm,
                           rows_per_restart * scan_components [1].dataUnitCols) ;

  // Gather Huffman Statistics
  if YCOMPONENT In scan.component_mask Then
    Begin
    dc_tables [0].reset ;
    ac_tables [0].reset ;
    End ;

  if (Not (YCOMPONENT In scan.component_mask)) Or (scan_component_count <> 1) Then
    Begin
    ac_tables [1].reset ;
    dc_tables [1].reset ;
    End ;

  For ii := 1 To scan_component_count Do
    Begin
{$IFDEF FPC}
    passfunction [ii] := @scan_components [ii].encodeSequential ;
    dcfunction   [ii] := @scan_components [ii].gatherDcData ;
    acfunction   [ii] := @scan_components [ii].gatherAcData ;
{$ELSE}
    passfunction [ii] := scan_components [ii].encodeSequential ;
    dcfunction   [ii] := scan_components [ii].gatherDcData ;
    acfunction   [ii] := scan_components [ii].gatherAcData ;
{$ENDIF}
    End ;

  if scan_component_count <> 1 Then
    Begin
    interleavedPass (
          strm,
          false,
          passfunction,
          dcfunction,
          acfunction,
          0, High (COEFFICIENTINDEX), 0) ;
    End
  else
    Begin
{$IFDEF FPC}
    noninterleavedPass (
          strm,
          false,
          @scan_components [1].encodeSequential,
          @scan_components [1].gatherDcData,
          @scan_components [1].gatherAcData,
          0, High (COEFFICIENTINDEX), 0) ;
{$ELSE}
    noninterleavedPass (
          strm,
          false,
          scan_components [1].encodeSequential,
          scan_components [1].gatherDcData,
          scan_components [1].gatherAcData,
          0, High (COEFFICIENTINDEX), 0) ;
{$ENDIF}
    End ;

  // Create the Huffman Tables
  if YCOMPONENT In scan.component_mask Then
    Begin
    dc_tables [0].buildTable ;
    ac_tables [0].buildTable ;
    End ;

  if (Not (YCOMPONENT In scan.component_mask)) Or (scan_component_count <> 1) Then
    Begin
    ac_tables [1].buildTable ;
    dc_tables [1].buildTable ;
    End ;

  // Output the Huffman tables
  printHuffmanTables (strm, scan, true, true) ; // Output DC and AC tables.

  // Create the scan marker.
  // Section B.2.3
  outputMarker (strm, SOS) ;
  strm.writeBigEndianWord (6 + 2 * scan_component_count) ;  // Length
  strm.writeByte (scan_component_count) ;  // Component Count

  if YCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (YCOMPONENT)) ;
    strm.writeByte ($00) ; // Entropy Tables
    End ;

  if Not gray_scale Then
    Begin
    if CBCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CBCOMPONENT)) ;
      strm.writeByte ($11) ; // Entropy Tables
      End ;

    if CRCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CRCOMPONENT)) ;
      strm.writeByte ($11) ; // Entropy Tables
      End ;
    End ;

  strm.writeByte (0) ; // Spectral Selection Start
  strm.writeByte (JPEGSAMPLESIZE - 1) ; // Spectral Selection End
  strm.writeByte (0) ; // Successive Approximation

  For ii := 1 To scan_component_count Do
    Begin
{$IFDEF FPC}
    passfunction [ii] := @scan_components [ii].encodeSequential ;
    dcfunction   [ii] := @scan_components [ii].printDcData ;
    acfunction   [ii] := @scan_components [ii].printAcData ;
{$ELSE}
    passfunction [ii] := scan_components [ii].encodeSequential ;
    dcfunction   [ii] := scan_components [ii].printDcData ;
    acfunction   [ii] := scan_components [ii].printAcData ;
{$ENDIF}
    End ;


  // Output the Scan data.
  strm.enterBitMode (CHAR_BIT) ;
  if scan_component_count <> 1 Then
    Begin
    interleavedPass (
          strm,
          true,
          passfunction,
          dcfunction,
          acfunction,
          0, High (COEFFICIENTINDEX), 0) ;
    End
  else
    Begin
{$IFDEF FPC}
    noninterleavedPass (
          strm,
          true,
          @scan_components [1].encodeSequential,
          @scan_components [1].printDcData,
          @scan_components [1].printAcData,
          0, High (COEFFICIENTINDEX), 0) ;
{$ELSE}
    noninterleavedPass (
          strm,
          true,
          scan_components [1].encodeSequential,
          scan_components [1].printDcData,
          scan_components [1].printAcData,
          0, High (COEFFICIENTINDEX), 0) ;
{$ENDIF}
    End ;
  strm.exitBitMode ;
  End ;

//
//  Description:
//
//    This function makes a pass through the data units for a non-interleaved
//    Scan.
//
//    The reason for having a generic function that uses pointers to member
//    functions to do processing is that there are several places where the
//    control processing has to be identical in multiple passes where the
//    low level actions are different. For example, the processing here when
//    gathering Huffman statistics has to be identical to the processing when
//    writing values to the output file. Using separate functions turned out
//    to be error prone due to the difficulty of keeping multiple functions
//    exactly in synchronization.
//
//  Parameters:
//    writerestarts: false => This is a statistics gathering pass
//                   true => This pass is writing data to the output file.
//    passfunctions: Pointer to EncoderComponent member function used
//                   to process this pass.
//    dcfunction:    Pointer to the EncoderComponent member function used
//                   to process DC coefficient values.
//    acfunction:    Pointer to the EncoderComponent member function used
//                   to process AC coefficient values.
//    sss:           Spectral Selection Start (0-63)
//    sse:           Spectral Selection End (0-63)
//    ssa:           Successive Approximation (0-13)
//
Procedure TJpegEncoder.noninterleavedPass (
               strm : TJpegOutputStream ;
               writedata : Boolean ;
               passfunction : COMPONENTPASSFUNCTION ;
               dcfunction : DCOUTPUTFUNCTION ;
               acfunction : ACOUTPUTFUNCTION ;
               sss, sse : COEFFICIENTINDEX ;
               ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  const
    progressscale = 8 ;
    OPERATION = 'Noninterleaved scan' ;
  var
    progress, progressincrement : Cardinal ;
    row, col : Cardinal ;
    intervalcount : Cardinal ;
    restartmarker : Cardinal ;
  Begin
  // We use a scale integer to keep trake of progress to lessen the
  // change that the progress increment will be zero.
  if writedata Then
    progress := 50 Shl progressscale
  else
    progress := 0 ;
  progressincrement := (100 Shl progressscale)
                    Div (2 * scan_components [1].dataUnitRows) ;

  resetDcValues ;

  intervalcount := 0 ;
  restartmarker := 0 ;
  for row := 0 To scan_components [1].noninterleavedDuRows - 1 Do
    Begin
    for col := 0 To scan_components [1].noninterleavedDuColumns - 1 Do
      Begin
      if restart_interval <> 0 Then
        Begin
        if (intervalcount = restart_interval) And (restart_interval <> 0) Then
          Begin
          // If there are any outstanding EOB runs we flush them before the
          // restart marker. This is not explicitly stated in the JPEG
          // standard. The definition of "Restart Interval" in section 4.1
          // states that restart markers separate independent sequences,
          // something we would not have if we did not output the EOB run
          // here.
          scan_components [1].printEobRun (acfunction) ;
          // E.1.4
          if writedata Then
            outputRestartMarker (strm, restartmarker) ;
          resetDcValues ;
          Inc (restartmarker) ;
          restartmarker := restartmarker Mod 8 ;
          intervalcount := 0 ;
          End ;
        End ;

      passfunction (row,
                    col,
                    dcfunction,
                    acfunction,
                    sss, sse,
                    ssa) ;
      Inc (intervalcount) ;
      End ;

    Inc (progress, progressincrement) ;
    callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;

  if writedata Then
    callProgressFunction (OPERATION, 100)
  else
    callProgressFunction (OPERATION, 50) ;
  End ;

//
//  Description:
//
//    This function makes a pass through the data units for an interleaved
//    Scan.
//
//    The reason for having a generic function that uses pointers to member
//    functions to do processing is that there are several places where the
//    control processing has to be identical in multiple passes where the
//    low level actions are different. For example, the processing here when
//    gathering Huffman statistics has to be identical to the processing when
//    writing values to the output file. Using separate functions turned out
//    to be error prone due to the difficulty of keeping multiple functions
//    exactly in synchronization.
//
//  Parameters:
//     writerestarts: false => This is a statistics gathering pass
//                    true => This pass is writing data to the output file.
//     passfunctions: Pointer to EncoderComponent member function used
//                    to process this pass.
//     dcfunction:    Pointer to the EncoderComponent member function used
//                    to process DC coefficient values.
//     acfunction:    Pointer to the EncoderComponent member function used
//                    to process AC coefficient values.
//     sss:           Spectral Selection Start (0-63)
//     sse:           Spectral Selection End (0-63)
//     ssa:           Successive Approximation (0-13)
//
Procedure TJpegEncoder.interleavedPass (
                    strm : TJpegOutputStream ;
                    writedata : Boolean ;
                    passfunction : Array Of COMPONENTPASSFUNCTION ;
                    dcfunction   : Array Of DCOUTPUTFUNCTION ;
                    acfunction   : Array Of ACOUTPUTFUNCTION ;
                    sss, sse : COEFFICIENTINDEX ;
                    ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  const
    progressscale = 8 ;
    OPERATION = 'Interleaved Scan' ;
  Var
    mcurow, mcucol : Cardinal ;
    intervalcount : Cardinal ;
    restartmarker : Cardinal ;
    cc, cx, cy : Cardinal ;
    durow, ducol : Cardinal ;
    progress, progressincrement : Cardinal ;
  Begin
  if writedata Then
    progress := 50 Shl progressscale
  else
    progress := 0 ;
  progressincrement := (100 Shl progressscale) Div (2 * mcu_rows) ;

  resetDcValues ;

  intervalcount := 0 ;
  restartmarker := 0 ;
  for mcurow := 0 To mcu_rows - 1 Do
    Begin
    for mcucol := 0 To mcu_cols - 1 Do
      Begin
      // Handle Restart Markers
      if restart_interval <> 0 Then
        Begin
        if (intervalcount = restart_interval) And (restart_interval <> 0) Then
          begin
          if writedata Then
            outputRestartMarker (strm, restartmarker) ;
          resetDcValues ;
          Inc (restartmarker) ;
          restartmarker := restartmarker Mod 8 ;
          intervalcount := 0 ;
          End ;
        Inc (intervalcount) ;
        End ;

      for cc := 1 To scan_component_count Do
        Begin
        for cy := 0 To scan_components [cc].VerticalFrequency  - 1 Do
          Begin
          durow := scan_components [cc].VerticalFrequency * mcurow + cy ;
          for cx := 0 To scan_components [cc].HorizontalFrequency - 1 Do
            Begin
            ducol := scan_components [cc].HorizontalFrequency
                  * mcucol + cx ;
            passfunction [cc] (durow, ducol,
                               dcfunction [cc], acfunction [cc],
                               sss, sse, ssa) ;
            End ;
          End ;
        End ;
      End ;
    Inc (progress, progressincrement) ;
    callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;

  if writedata Then
    callProgressFunction (OPERATION, 100)
  else
    callProgressFunction (OPERATION, 50) ;
  End ;

//
//  Description:
//
//    This function writes the Huffman tables used by a scan to the output
//    stream.
//
//    We only use two DC and two AC tables to be compatible with baseline
//    JPEG.  In progressive JPEG there is really no reason for more than
//    two tables for RGB images. If we ever go to four colors then things
//    may need to change.
//
//    The Y component always uses table ID 0. The Cb and Cr components always
//    use table ID 1.
//
//  Parameters:
//    scan:     Here the scan structure is used to determine which components
//              are part of the scan (Y and/or Cb/Cr)
//    usedc:    Set to true if the scan includes DC components. (Sequential
//              Scan or Progressive Scan with the spectral selection 0)
//    useac:    Set to true if the scan includes AC components. (Sequential
//              Scan or Progressive Scan with the spectral selection start
//              not zero)
//
Procedure TJpegEncoder.printHuffmanTables (strm : TJpegOutputStream ;
                                           scan : TScan ;
                                           usedc, useac : Boolean) ;
  Var
    size : Cardinal ;
  Begin
  // Section B.2.4.2

  if YCOMPONENT In scan.component_mask Then
    Begin
    // See if this is a color image.
    if scan_component_count <> 1 Then
      Begin
      // We have at least two components and the first is the Y component.
      // This means we need the both of the huffman tables.
      // B.2.4.2
      outputMarker (strm, DHT) ;
      size := sizeof (Word) ;
      if usedc Then
        Inc (size, dc_tables [0].outputSize + dc_tables [1].outputSize) ;

      if useac Then
        Inc (size, ac_tables [0].outputSize + ac_tables [1].outputSize) ;

      strm.writeBigEndianWord (size) ;
      if usedc Then
        Begin
        strm.writeByte ($00) ;
        dc_tables [0].printTable (strm) ;
        End ;

      if useac Then
        Begin
        strm.writeByte ($10) ;
        ac_tables [0].printTable (strm) ;
        End ;

      if usedc Then
        Begin
        strm.writeByte ($01) ;
        dc_tables [1].printTable (strm) ;
        End ;

      if useac Then
        Begin
        strm.writeByte ($11) ;
        ac_tables [1].printTable (strm) ;
        End ;
      End
    else
      Begin
      // The only component is Y
      size := sizeof (Word) ;
      if usedc Then
        Inc (size, dc_tables [0].outputSize) ;

      if useac Then
        Inc (size, ac_tables [0].outputSize) ;

      outputMarker (strm, DHT) ;
      strm.writeBigEndianWord (size) ;
      if usedc Then
        Begin
        strm.writeByte ($00) ;
        dc_tables [0].printTable (strm) ;
        End ;

      if useac Then
        Begin
        strm.writeByte ($10) ;
        ac_tables [0].printTable (strm) ;
        End ;
      End ;
    End
  else
    Begin
    // The Y component is not present. Output is the same for
    // Cb, Cr, or Cb & Cr.
    size := sizeof (Word) ;
    if usedc Then
      Inc (size, dc_tables [1].outputSize) ;

    if useac Then
      Inc (size, ac_tables [1].outputSize) ;

    outputMarker (strm, DHT) ;
    strm.writeBigEndianWord (size) ;
    if usedc Then
      Begin
      strm.writeByte ($01) ;
      dc_tables [1].printTable (strm) ;
      End ;

    if useac Then
      Begin
      strm.writeByte ($11) ;
      ac_tables [1].printTable (strm) ;
      End ;
    End ;
  End ;

//
//  Description:
//
//   This function resets the DC difference values for all
//   all the components in the scan.  We do this at the start of
//   each scan and whenever we output a restart marker.
//
Procedure TJpegEncoder.resetDcValues ;
  Var
    ii : Cardinal ;
  Begin
  for ii := 1 To scan_component_count Do
    scan_components [ii].resetDcDifference ;
  End ;

//
//  Description:
//
//    This function determines the dimensions of an MCU using the maximum
//    sampling frequencies of the components.
//
Procedure TJpegEncoder.calculateMcuDimensions ;
  Var
    ii : Cardinal ;
    mcuheight, mcuwidth : Cardinal ;
  Begin
  max_horizontal_frequency := 1 ;
  max_vertical_frequency := 1 ;

  if Not gray_scale Then
    Begin
    for ii := Ord (YCOMPONENT) To Ord (CRCOMPONENT) Do
      Begin
      if image_components [ii].HorizontalFrequency > max_horizontal_frequency Then
        max_horizontal_frequency := image_components [ii].HorizontalFrequency ;

      if image_components [ii].VerticalFrequency > max_vertical_frequency Then
        max_vertical_frequency := image_components [ii].VerticalFrequency ;
      End ;
    End
  else
    Begin
    max_horizontal_frequency := image_components [Ord (YCOMPONENT)].HorizontalFrequency ;
    max_vertical_frequency := image_components [Ord (YCOMPONENT)].VerticalFrequency ;
    End ;

  mcuheight := max_vertical_frequency * JPEGSAMPLEWIDTH ;
  mcuwidth := max_horizontal_frequency * JPEGSAMPLEWIDTH ;
  mcu_rows := (frame_height + mcuheight - 1) Div mcuheight ;
  mcu_cols := (frame_width + mcuwidth - 1) Div mcuwidth ;
  End ;

//
//  Description:
//
//    This function writes a progressive scan to the output stream.
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
Procedure TJpegEncoder.printProgressiveScan (strm : TJpegOutputStream ;
                                             scan : TScan) ;
  Begin
  if scan.spectral_selection_start = 0 Then
    Begin
    if scan.successive_approximation_high = 0 Then
      printDcFirst (strm, scan)
    else
      printDcRefine (strm, scan) ;
    End
  else
    Begin
    if scan.successive_approximation_high = 0 Then
      printAcFirst (strm, scan)
    else
      printAcRefine (strm, scan) ;
    End ;
  End ;

//
//  Description:
//
//    This function handles scans containing the first pass for DC
//    coefficients.  If successive approximation is not used then
//    this would be the only DC coefficient pass. DC progressive
//    scans may be interleaved, unlike AC scans.
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
Procedure TJpegEncoder.printDcFirst (strm : TJpegOutputStream ; scan : TScan) ;
  Var
    value : Integer ;
    passfunction : Array [0..3] Of COMPONENTPASSFUNCTION ;
    dcfunction   : Array [0..3] Of DCOUTPUTFUNCTION ;
    acfunction   : Array [0..3] Of ACOUTPUTFUNCTION ;
    ii : Cardinal ;
  Begin
  // Reset the Huffman statistics counters.
  if YCOMPONENT In scan.component_mask Then
    dc_tables [0].reset  ;

  if (Not (YCOMPONENT In scan.component_mask)) Or (scan_component_count > 1) Then
    dc_tables [1].reset ;

  outputRestartInterval (strm, restart_interval) ;

  For ii := 1 To scan_component_count Do
    Begin
{$IFDEF FPC}
    passfunction [ii] := @scan_components [ii].progressiveDcFirst ;
    dcfunction   [ii] := @scan_components [ii].gatherDcData ;
{$ELSE}
    passfunction [ii] := scan_components [ii].progressiveDcFirst ;
    dcfunction   [ii] := scan_components [ii].gatherDcData ;
{$ENDIF}
    acfunction   [ii] := Nil ;
    End ;

  // Gather the Huffman statistics
  if scan_component_count <> 1 Then
    Begin
    interleavedPass (
          strm,
          false,
          passfunction,
          dcfunction,
          acfunction,
          0, 0,
          scan.successive_approximation_low) ;
    End
  else
    Begin
    noninterleavedPass (
          strm,
          false,
{$IFDEF FPC}
          @scan_components [1].progressiveDcFirst,
          @scan_components [1].gatherDcData,
{$ELSE}
          scan_components [1].progressiveDcFirst,
          scan_components [1].gatherDcData,
{$ENDIF}
          Nil,
          0, 0,
          scan.successive_approximation_low) ;
    End ;

  // Create the Huffman tables from the statistics
  if YCOMPONENT In scan.component_mask Then
    dc_tables [0].buildTable ;

  if (Not (YCOMPONENT In scan.component_mask)) Or (scan_component_count > 1) Then
    dc_tables [1].buildTable ;

  printHuffmanTables (strm, scan, true, false) ;

  // Output the scan header.
  // Section B.2.3
  outputMarker (strm, SOS) ;
  strm.writeBigEndianWord (6 + 2 * scan_component_count) ;  // Length
  strm.writeByte (scan_component_count) ;  // Component Count

  if YCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (YCOMPONENT)) ;
    strm.writeByte ($00) ; // Entropy Tables
    End ;

  if Not gray_scale Then
    Begin
    if CBCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CBCOMPONENT)) ;
      strm.writeByte ($11) ; // Entropy Tables
      End ;

    if CRCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CRCOMPONENT)) ;
      strm.writeByte ($11) ; // Entropy Tables
      End ;
    End ;

  strm.writeByte (0) ; // Spectral Selection Start
  strm.writeByte (0) ; // Spectral Selection End
  value := (scan.successive_approximation_high Shl 4)
            Or scan.successive_approximation_low ;
  strm.writeByte (value) ; // Successive Approximation

  For ii := 1 To scan_component_count Do
    Begin
{$IFDEF FPC}
    passfunction [ii] := @scan_components [ii].progressiveDcFirst ;
    dcfunction   [ii] := @scan_components [ii].printDcData ;
{$ELSE}
    passfunction [ii] := scan_components [ii].progressiveDcFirst ;
    dcfunction   [ii] := scan_components [ii].printDcData ;
{$ENDIF}
    acfunction   [ii] := Nil ;
    End ;

  // Output the scan data.
  strm.enterBitMode (CHAR_BIT) ;
  if scan_component_count <> 1 Then
    Begin
    interleavedPass (
          strm,
          false,
          passfunction,
          dcfunction,
          acfunction,
          0, 0,
          scan.successive_approximation_low) ;
    End
  else
    Begin
    noninterleavedPass (
          strm,
          false,
{$IFDEF FPC}
          @scan_components [1].progressiveDcFirst,
          @scan_components [1].printDcData,
{$ELSE}
          scan_components [1].progressiveDcFirst,
          scan_components [1].printDcData,
{$ENDIF}
          Nil,
          0, 0,
          scan.successive_approximation_low) ;
    End ;
  strm.exitBitMode ;
  End ;

//
//  Description:
//
//    This function outputs the data for a refining DC scan. This type of
//    scan is unique in that it does use Huffman encoding.
//
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
Procedure TJpegEncoder.printDcRefine (strm : TJpegOutputStream ; scan  : TScan) ;
  var
    value : Integer ;
    passfunction : Array [0..3] Of COMPONENTPASSFUNCTION ;
    dcfunction   : Array [0..3] Of DCOUTPUTFUNCTION ;
    acfunction   : Array [0..3] Of ACOUTPUTFUNCTION ;
    ii : Cardinal ;
  Begin
  // Output the scan header.
  // Section B.2.3
  outputMarker (strm, SOS) ;
  strm.writeBigEndianWord (6 + 2 * scan_component_count) ;  // Length
  strm.writeByte (scan_component_count) ;


  if YCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (YCOMPONENT)) ;
    strm.writeByte (0) ; // No Huffman table is used.
    End ;
  if Not gray_scale Then
    Begin
    if CBCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CBCOMPONENT)) ;
      strm.writeByte (0) ; // No Huffman table is used.
      End ;

    if CRCOMPONENT In scan.component_mask Then
      Begin
      strm.writeByte (Ord (CRCOMPONENT)) ;
      strm.writeByte (0) ; // No Huffman table is used.
      End ;
    End ;

  strm.writeByte (0) ; // Spectral Selection Start
  strm.writeByte (0) ; // Spectral Selection End

  value := (scan.successive_approximation_high Shl 4)
            Or scan.successive_approximation_low ;
  strm.writeByte (value) ;

  For ii := 0 To scan_component_count Do
    Begin
{$IFDEF FPC}
    passfunction [ii] := @scan_components [ii].progressiveDcRefine ;
{$ELSE}
    passfunction [ii] := scan_components [ii].progressiveDcRefine ;
{$ENDIF}
    dcfunction   [ii] := Nil ;
    acfunction   [ii] := Nil ;
    End ;

  // Output the scan data.
  strm.enterBitMode (CHAR_BIT) ;
  if scan_component_count <> 1 Then
    Begin
    interleavedPass (
          strm,
          true,
          passfunction,
          dcfunction,
          acfunction,
          0, 0,
          scan.successive_approximation_low) ;
    End
  else
    Begin
    noninterleavedPass (
          strm,
          true,
{$IFDEF FPC}
          @scan_components [1].progressiveDcRefine,
{$ELSE}
          scan_components [1].progressiveDcRefine,
{$ENDIF}
          Nil, Nil,
          0, 0,
          scan.successive_approximation_low) ;
    End ;
  strm.exitBitMode ;
  End ;

//
//  Description:
//
//    This function outputs a scan that is the first pass for a set of AC
//    coefficients.
//
//    Even though AC progressive scans cannot be interleaved, we follow the
//    convention of using Huffman Table #0 for the Y component and #1 for
//    the Cb and Cr components.
//
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
Procedure TJpegEncoder.printAcFirst (strm : TJpegOutputStream ; scan : TScan) ;
  Var
    value : Integer ;
  Begin
  // Reset the Huffman statistics counters.
  scan_components [1].ac_table.reset ;

  outputRestartInterval (strm, restart_interval) ;

  // Gather the Huffman statistics
{$IFDEF FPC}
  firstAcData (strm, scan, false, @scan_components [1].gatherAcData) ;
{$ELSE}
  firstAcData (strm, scan, false, scan_components [1].gatherAcData) ;
{$ENDIF}
  // Generate the Huffman statistics
  scan_components [1].ac_table.buildTable ;

  printHuffmanTables (strm, scan, false, true) ;

  // Section B.2.3
  outputMarker (strm, SOS) ;
  strm.writeBigEndianWord (6 + 2 * scan_component_count) ;  // Length
  strm.writeByte (scan_component_count) ;  // Component Count

  if YCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (YCOMPONENT)) ;
    strm.writeByte ($00) ; // Entropy Table
    End
  else if CBCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (CBCOMPONENT)) ;
    strm.writeByte ($11) ; // Entropy Tables
    End
  else if CRCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (CRCOMPONENT)) ;
    strm.writeByte ($11) ; // Entropy Tables
    End ;

  strm.writeByte (scan.spectral_selection_start) ; // Spectral Selection Start
  strm.writeByte (scan.spectral_selection_end) ; // Spectral Selection End
  value := (scan.successive_approximation_high Shl 4)
            Or scan.successive_approximation_low ;
  strm.writeByte (value) ; // Successive Approximation

  strm.enterBitMode (CHAR_BIT) ;
{$IFDEF FPC}
  firstAcData (strm, scan, true, @scan_components [1].printAcData) ;
{$ELSE}
  firstAcData (strm, scan, true, scan_components [1].printAcData) ;
{$ENDIF}
  strm.exitBitMode ;
  End ;

//
//  Description:
//
//    This function outputs the data for a scan that refines AC coefficients
//    through successive approximation.
//
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
Procedure TJpegEncoder.printAcRefine (strm : TJpegOutputStream ; scan : TScan) ;
  Var
    value : Integer ;
  Begin
  // Reset the Huffman statistics counters.
  if YCOMPONENT In scan.component_mask Then
    ac_tables [0].reset
  else
    ac_tables [1].reset ;

  outputRestartInterval (strm, restart_interval) ;
  // Gather the Huffman statistics.
{$IFDEF FPC}
  refineAcData (strm, scan, false, @scan_components [1].gatherAcData) ;
{$ELSE}
  refineAcData (strm, scan, false, scan_components [1].gatherAcData) ;
{$ENDIF}

  // Create the Huffman Table.
  if YCOMPONENT In scan.component_mask Then
    Begin
    ac_tables [0].buildTable ;
    End
  else
    Begin
    ac_tables [1].buildTable ;
    End ;

  printHuffmanTables (strm, scan, false, true) ;  // Only output the AC table.

  // Create the scan header.
  // Section B.2.3
  outputMarker (strm, SOS) ;
  strm.writeBigEndianWord (6 + 2 * scan_component_count) ;  // Length
  strm.writeByte (scan_component_count) ;  // Component Count

  if YCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (YCOMPONENT)) ;
    strm.writeByte ($00) ; // Entropy Tables
    End ;
  if CBCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (CBCOMPONENT)) ;
    strm.writeByte ($11) ; // Entropy Tables
    End ;
  if CRCOMPONENT In scan.component_mask Then
    Begin
    strm.writeByte (Ord (CRCOMPONENT)) ;
    strm.writeByte ($11) ; // Entropy Tables
    End ;

  strm.writeByte (scan.spectral_selection_start) ; // Spectral Selection Start
  strm.writeByte (scan.spectral_selection_end) ; // Spectral Selection End
  value := (scan.successive_approximation_high Shl 4)
            Or scan.successive_approximation_low ;
  strm.writeByte (value) ; // Successive Approximation

  // Output the scan data.
  strm.enterBitMode (CHAR_BIT) ;
{$IFDEF FPC}
  refineAcData (strm, scan, true, @scan_components [1].printAcData) ;
{$ELSE}
  refineAcData (strm, scan, true, scan_components [1].printAcData) ;
{$ENDIF}
  strm.exitBitMode ;
  End ;

//
//  Description:
//
//    This function loops through the data in an initial AC scan. For
//    all scans other than AC progressive ones we use the same function for
//    this purpose. Due to the excessive complexity of AC progressive scans
//    we use a specialized function.
//
//    This function gets called twice for each scan. The first pass is used
//    to gather Huffman statistics. The second pass is to output the scan.
//    Having a common function ensures that Huffman statistics are gathered
//    in the exact same manner as the scan data is output.
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//    outputrestarts: flag to indicate if restart markers are to be output
//    acfunction: The member function to process a data unit
//
Procedure TJpegEncoder.firstAcData (
                        strm : TJpegOutputStream ;
                        scan : TScan ;
                        outputrestarts : Boolean ;
                        acfunction : ACOUTPUTFUNCTION) ;
  const
    progressscale = 8 ;
    OPERATION = 'AC Progressive Scan' ;
  Var
    progress, progressincrement : Cardinal ;
    intervalcount : Cardinal ;  // Count between restarts
    restartmarker : Cardinal ;  // Value 0..7
    row, col : Cardinal ;
  Begin
  // We use a scale integer to keep trake of progress to lessen the
  // change that the progress increment will be zero.
  if outputrestarts Then
    progress := 50 SHl progressscale
  else
    progress := 0 ;
  progressincrement := (100 Shl progressscale)
                    Div (2 * scan_components [1].dataUnitRows) ;

  scan_components [1].resetEobRun ;

  intervalcount := 0 ;  // Count between restarts
  restartmarker := 0 ;  // Value 0..7
  for row := 0 To scan_components [1].dataUnitRows - 1 Do
    Begin
    for col := 0  To scan_components [1].dataUnitCols - 1 Do
      Begin
      // See if we are using restart markers.
      if restart_interval <> 0 Then
        Begin
        // Is a restart marker needed.
        if intervalcount = restart_interval Then
          Begin
          // If there are any outstanding EOB runs we flush them before the
          // restart marker. This is not explicitly stated in the JPEG
          // standard. The definition of "Restart Interval" in section 4.1
          // states that restart markers separate independent sequences,
          // something we would not have if we did not output the EOB run
          // here.
          scan_components [1].printEobRun (acfunction) ;
          // Here we rely on the relationship RST0|n = RSTn [n = 0..7]
          // Section E.1.4
          if outputrestarts Then
            outputRestartMarker (strm, restartmarker) ;
          Inc (restartmarker) ;
          restartmarker := restartmarker Mod 8 ;
          intervalcount := 0 ;
          End ;
        End ;

      // Process the data unit
      scan_components [1].progressiveAcFirst (
                                row,
                                col,
                                acfunction,
                                scan.spectral_selection_start,
                                scan.spectral_selection_end,
                                scan.successive_approximation_low) ;
      Inc (intervalcount) ;
      End ;
    Inc (progress, progressincrement) ;
    callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;
  // If there is a final end of band run then write it out.
  scan_components [1].printEobRun (acfunction) ;
  if outputrestarts Then
    callProgressFunction (OPERATION, 100)
  else
    callProgressFunction (OPERATION, 50) ;
  End ;

//
//  Description
//
//    This function loops through the DC using in a refining AC scan. For
//    all scans other than AC progressive ones we use the same function for
//    this purpose. Due to the excessive complexity of AC progressive scans
//    we use a specialized function.
//
//    This function gets called twice for each scan. The first pass is used
//    to gather Huffman statistics. The second pass is to output the scan.
//    Having a common function ensures that Huffman statistics are gathered
//    in the exact same manner as the scan data is output.
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//    outputrestarts: flag to indicate if restart markers are to be output
//    acfunction: The member function to process a data unit
//
Procedure TJpegEncoder.refineAcData (
                      strm : TJpegOutputStream ;
                      scan : TScan ;
                      outputrestarts : Boolean ;
                      acfunction : ACOUTPUTFUNCTION) ;
  Const
    progressscale = 8 ;
    OPERATION = 'AC Progressive Scan' ;
  Var
    // We use a scale integer to keep trake of progress to lessen the
    // chance that the progress increment will be zero.
    progress, progressincrement : Cardinal ;
    intervalcount : Cardinal ;  // Count between restart markers
    restartmarker : Cardinal ;  // 0..7 => RST0..RST7
    row, col : Cardinal ;
  Begin
  if outputrestarts Then
    progress := 50 Shl progressscale
  else
    progress := 0 ;
  progressincrement := (100 Shl progressscale)
                    Div (2 * scan_components [1].dataUnitRows) ;

  scan_components [1].resetEobRun ;

  intervalcount := 0 ;  // Count between restart markers
  restartmarker := 0 ;  // 0..7 => RST0..RST7
  for row := 0 To scan_components [1].dataUnitRows - 1 Do
    Begin
    for col := 0 To scan_components [1].dataUnitCols - 1 Do ;
      Begin
      // Are we using restart markers?
      if restart_interval <> 0 Then
        Begin
        // Do we need to output a restart marker?
        if intervalcount = restart_interval Then
          Begin
          // If there are any outstanding EOB runs we flush them before the
          // restart marker. This is not explicitly stated in the JPEG
          // standard. The definition of "Restart Interval" in section 4.1
          // states that restart markers separate independent sequences,
          // something we would not have if we did not output the EOB run
          // here.
          scan_components [1].printRefineEobRun (
                                    acfunction,
                                    scan.spectral_selection_start,
                                    scan.spectral_selection_end,
                                    scan.successive_approximation_low) ;
          // Section E.1.4
          if outputrestarts Then
            outputRestartMarker (strm, restartmarker) ;
          Inc (restartmarker) ;
          restartmarker := restartmarker Mod 8 ;
          intervalcount := 0 ;
          End ;
        End ;
      scan_components [1].progressiveAcRefine (
                                row,
                                col,
                                acfunction,
                                scan.spectral_selection_start,
                                scan.spectral_selection_end,
                                scan.successive_approximation_low) ;
      Inc (intervalcount) ;
      End ;
    Inc (progress, progressincrement) ;
    callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;
  // If there is a final end of band run then write it out.
  scan_components [1].printRefineEobRun (acfunction,
                                     scan.spectral_selection_start,
                                     scan.spectral_selection_end,
                                     scan.successive_approximation_low) ;

  if outputrestarts Then
    callProgressFunction (OPERATION, 100)
  else
    callProgressFunction (OPERATION, 50) ;
  End ;

//
//  Description:
//
//    This function determines which components is in a given scan.
//
//  Parameters:
//    scan:  The structure that contains the scan parameters.
//
//  Implicit Outputs:
//    scan_component_count: The number of components in the scan.
//    scan_components:      The list of components in the scan.
//

Procedure TJpegEncoder.findComponentsInScan (scan : TScan) ;
  begin
  scan_component_count := 0 ;
  if YCOMPONENT In scan.component_mask Then
    Begin
    Inc (scan_component_count) ;
    scan_components [scan_component_count] := image_components [Ord (YCOMPONENT)] ;
    End ;

  if Not gray_scale Then
    begin
    if CBCOMPONENT In scan.component_mask Then
      Begin
      Inc (scan_component_count) ;
      scan_components [scan_component_count] := image_components [Ord (CBCOMPONENT)] ;
      End ;
    if CRCOMPONENT In scan.component_mask Then
      Begin
      Inc (scan_component_count) ;
      scan_components [scan_component_count] := image_components [Ord (CRCOMPONENT)] ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    This function sets the sampling frequencies for a component
//
//  Parameters:
//    component:  The component ID
//    hf:         Horizontal Frequency
//    vf:         Vertical Frequency
//
Procedure TJpegEncoder.setSamplingFrequency (component : COMPONENTID ; hf, vf : JPEGSAMPLINGFREQUENCY) ;
  Begin
  if component >= MAXCOMPONENTS Then
    Raise EJpegError.Create ('Invalid Component ID') ;

  // These checks may appear to be redundant. Their only reason for existance
  // is in case range checking is disabled.
  if (hf > High (JPEGSAMPLINGFREQUENCY)) Or (hf < Low (JPEGSAMPLINGFREQUENCY)) Then
    Raise EJpegError.Create ('Invalid Horizontal Sampling Frequency') ;

  if (vf > High (JPEGSAMPLINGFREQUENCY)) Or (vf < Low (JPEGSAMPLINGFREQUENCY)) Then
    Raise EJpegError.Create ('Invalid Vertical Sampling Frequency') ;

  image_components [component].HorizontalFrequency := hf ;
  image_components [component].VerticalFrequency := vf ;
  End ;

//
//  Description:
//
//    This function gets the sampling frequencies for a component
//
//  Parameters:
//    component:  The component ID
//    hf:         Horizontal Frequency
//    vf:         Vertical Frequency
//
Procedure TJpegEncoder.getSamplingFrequency (component : COMPONENTID ; var hf, vf : JPEGSAMPLINGFREQUENCY) ;
  Begin
  if component >= MAXCOMPONENTS Then
    Raise EJpegError.Create ('Invalid Component ID') ;

  hf := image_components [component].HorizontalFrequency ;
  vf := image_components [component].VerticalFrequency ;
  End ;

//
//  Description:
//
//    This function writes the JFIF header for the image.
//
Procedure TJpegEncoder.outputJfifHeader (strm : TJpegOutputStream) ;
  Var
    jfif : JfifHeader ;
  Begin
  jfif.length := SystemToBigEndianWord (Sizeof (jfif)) ;
  jfif.identifier [1] := 'J' ;
  jfif.identifier [2] := 'F' ;
  jfif.identifier [3] := 'I' ;
  jfif.identifier [4] := 'F' ;
  jfif.identifier [5] := Chr (0) ;
  jfif.version [1] := 1 ;
  jfif.version [2] := 1 ;
  jfif.units := 0 ;
  jfif.xdensity := SystemToBigEndianWord (1) ;
  jfif.ydensity := SystemToBigEndianWord (1) ;
  jfif.xthumbnail := 0 ;
  jfif.ythumbnail := 0 ;
  strm.write (@jfif, sizeof (jfif)) ;
  End ;

//
//  Description:
//
//    This function sets the attributes for a scan.
//
//  Parameters:
//    scan:  Scan number (1..MaxScan)
//    components: Bit mask of components to include inthe scan
//    sse:  The spectral selection end for the scan.  The
//          spectral selection start is the sse+1 of the previous
//          scan
//    ssa:  Initial successive approximation value for the scan.
//
Procedure TJpegEncoder.setScanAttributes (scan : SCANID ;
                                  components : COMPONENTSET ;
                                  sse : COEFFICIENTINDEX ;
                                  ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Begin
  if (scan > High (SCANID)) Or (scan < Low (SCANID)) Then
    Raise EJpegError.Create ('Scan Index Out of Range') ;

  image_scans [scan].component_mask := components ;
  image_scans [scan].spectral_selection_end := sse ;
  image_scans [scan].successive_approximation := ssa ;
  End ;

//
//  Description:
//
//    This fnction returns the attributes that have been defined
//    for a scan (revsere of SetScanAttributes).
//
//  Parameters:
//    scan: (in) The scan to retrieve information about
//    components: (out) Bitmask of component IDs in the scan
//    sse: (out) Spectral selection end
//    ssa: (out) Successive Approximation
//
Procedure TJpegEncoder.getScanAttributes (scan : SCANID ;
                                  var components : COMPONENTSET ;
                                  var sse : COEFFICIENTINDEX ;
                                  var ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Begin
  if (scan > High (SCANID)) Or (scan < Low (SCANID)) Then
    Raise EJpegError.Create ('Scan Index Out of Range') ;

  components := image_scans [scan].component_mask ;
  sse := image_scans [scan].spectral_selection_end ;
  ssa := image_scans [scan].successive_approximation ;
  End ;

//
//  Description:
//
//    This function counts the number of passes through the data required
//    to write the image. This value is passed t the progress function.
//
Procedure TJpegEncoder.countPassesForProgressReporting ;
  Var
    ii : Cardinal ;
  Begin
  current_pass := 0 ;
  if progressive_mode Then
    Begin
    if gray_scale Then
      Begin
      total_passes := 1 ;
      for ii := 1 To scan_count Do
        Begin
        if YCOMPONENT In image_scans [ii].component_mask Then
          Inc (total_passes, image_scans [ii].successive_approximation + 1) ;
        End
      End
    else
      Begin
      total_passes := 4 ; // 1 Color Conversion, 1 (x3) for each component
      for ii := 1 To scan_count Do
        Inc (total_passes, image_scans [ii].successive_approximation + 1) ;
      End ;
    End
  else
    Begin
    if gray_scale Then
      total_passes := 2
    else
      total_passes := 4 + scan_count ; // 1 Color Conversion, 1 (x3) for each component
    End
  End ;


//
//  Description:
//
//    This function sets the image quality value.
//
//  Parameters:
//    value: The quality value (1..100)
//
Procedure TJpegEncoder.setQuality (value : QUALITYVALUE) ;
  Var
    ii : Cardinal ;
  Begin

  for ii := 0 To JPEGSAMPLESIZE - 1 Do
    Begin
    quantization_tables [0][ii] := luminance_quantization_tables [value]^[ii] ;
    quantization_tables [2][ii] := luminance_quantization_tables [value]^[ii] ;
    End ;

  for ii := 0 To JPEGSAMPLESIZE - 1 Do
    Begin
    quantization_tables [1][ii] := chrominance_quantization_tables [value]^[ii] ;
    quantization_tables [3][ii] := chrominance_quantization_tables [value]^[ii] ;
    End ;

  image_quality := value ;
  End ;

//
//  Description:
//
//    This function writes a restart marker to the output stream.
//
//  Parameters:
//
//    outputstream : The output stream to write to
//    marker : The restart marker to output - an integer in the range 0 .. 7.
//

Procedure TJpegEncoder.outputRestartMarker (strm : TJpegOutputStream ; marker : Cardinal) ;
  Begin
  strm.exitBitMode ;
  outputMarker (strm, RST0 Or marker) ;
  strm.enterBitMode (CHAR_BIT) ;
  End ;


Procedure TJpegEncoder.writeImageFile (filename : String ; image : TBitmapImage) ;
  Var
    outputstream : TJpegOutputFileStream ;
  Begin
  outputstream := TJpegOutputFileStream.Create ;
  outputstream.open (filename) ;
  try
    writeImage (outputstream, image) ;
  finally
    outputstream.Destroy ;
    End ;
  End ;



//
//  This function calls the progress function if one has been specified.
//
//  Parameters:
//      progress: Percent completed (0-100)
//
Procedure TJpegEncoder.callProgressFunction (description : String ; progress : Cardinal) ;
  Var
    abort : Boolean ;
    percent : Cardinal ;
    savedpass : Cardinal ;
  Begin
  if Not Assigned (progress_function) Or in_progress Then
    Exit ;
  in_progress := true ;
  savedpass := current_pass ;

  abort := false ;
  percent := progress ;
  if percent > 100 Then
    percent := 100 ;

  try
    progress_function (Self,
                       progress_data,
                       current_pass,
                       total_passes,
                       description,
                       percent,
                       abort) ;
  finally
    in_progress := false ;
    current_pass := savedpass ;
    end ;

  // See if the user wants to stop.
  if abort Then
    Raise EGraphicsAbort.Create ('') ;
  End ;

Procedure TJpegEncoder.setVerbose (state : boolean) ;
  Begin
  verbose_flag := state ;
  End ;
//**************************************************************************************
Procedure TJpegEncoderComponent.resetDcDifference ;
  Begin
  last_dc_value := 0 ;
  End ;

Procedure TJpegEncoderComponent.setHuffmanTables (dc, ac : TJpegHuffmanEncoder) ;
  Begin
  dc_table := dc ;
  ac_table := ac ;
  End ;

//
//  Description:
//
//    This function returns the number of pixel rows encoded for a component
//    in an interleaved scan. This number is at least as large as the image
//    height. However, since it takes into account sampling frequencies,
//    it can be larger if the height is not evenly divisible by the number
//    sample rows in an MCU.
//
//  Parameters:
//
//    imageheight : The image width
//    maxvf: The maximum horizontal sampling frequency among all components.
//
//  Return Value:
//
//    The number of pixel rows to be encoded.
//
Function NoninterleavedRows (imageheight, maxvf : Cardinal) : Cardinal ;
  Var
    heightmask : Cardinal ;
  Begin
  heightmask := JPEGSAMPLEWIDTH * maxvf - 1 ;
  Result := (imageheight + heightmask) And Not heightmask ;
  End ;
//
//  Description:
//
//    This function returns the number of pixel columns encoded for a component
//    in an interleaved scan. This number is at least as large as the image
//    width. However, since it takes into account sampling frequencies,
//    it can be larger if the width is not evenly divisible by the number
//    sample columns in an MCU.
//
//  Parameters:
//
//    imagewidth : The image height
//    maxhf: The maximum vertical sampling frequency among all components.
//
//  Return Value:
//
//    The number of pixel columns to be encoded.
//
Function NoninterleavedColumns (imagewidth : Cardinal ; maxhf : JPEGSAMPLINGFREQUENCY) : Cardinal ;
  Var
    widthmask : Cardinal ;
  Begin
  widthmask := JPEGSAMPLEWIDTH * maxhf - 1 ;
  Result := (imagewidth + widthmask) And Not widthmask ;
  End ;

Constructor TJpegEncoderComponent.Create ;
  Begin
  Inherited Create ;
  output_stream := Nil ;
  du_rows := 0 ;
  du_cols := 0 ;
  eob_run := 0 ;
  eob_start_du_row := 0 ;
  eob_start_du_col := 0 ;
  eob_start_position := 0 ;
  v_frequency :=1 ;
  h_frequency := 1 ;
  ac_table := Nil ;
  dc_table := Nil ;
  End ;

//
//  Description:
//
//    This function is use to gather Huffman statistics for DC coefficients.
//    The calling sequences is idential to PrintDcData (). Only the huffvalue
//    parameter is used here.
//
//  Parameters:
//    huffvalue:  The Huffman value (not code) to process.
//
Procedure TJpegEncoderComponent.gatherDcData (huffvalue, bits : Integer) ;
  Begin
  dc_table.incrementFrequency (huffvalue) ;
  End ;

//
//  Description:
//
//    This function is use to gather Huffman statistics for AC coefficients.
//    The calling sequences is idential to PrintAcData (). Only the huffvalue
//    parameter is used here.
//
//    If huffval==-1 then an unencoded bit string would be output by
//    PrintAcData (). In that situation this function does nothing.
//
//  Parameters:
//    huffvalue:  The Huffman value (not code) to process
//
Procedure TJpegEncoderComponent.gatherAcData (huffvalue, value, size : Integer) ;
  Begin
  if huffvalue >= 0 Then
    ac_table.incrementFrequency (huffvalue) ;
  End ;

//
//  Description:
//
//    This function is use to output Huffman-encoded data for a DC value.
//
//  Parameters:
//    huffvalue:  The Huffman value
//    bits: The additional data bits
//
//    8-bit DC values are in the range 0..11. The value specifies the number
//    of additional bits of refining data (bits).
//

Procedure TJpegEncoderComponent.printDcData (huffvalue, bits : Integer) ;
  var
    huffcode : Word ;
    huffsize : Byte ;
  Begin
  // Section F.1.2.1
  dc_table.encode (huffvalue, huffcode, huffsize) ;
  output_stream.outputBits (huffcode, huffsize) ;
  if huffvalue <> 0 Then
    output_stream.outputBits (bits, huffvalue) ;
  End ;

//
//  Description:
//
//    This function is use to output Huffman-encoded data for an AC value.
//
//  Parameters:
//    huffvalue:  The Huffman value
//    value: The additional data bits
//    size: The number of additional data bits.
//
//    When huffvalue=-1 this function is also used to output unencoded bit
//    strings.
//
Procedure TJpegEncoderComponent.printAcData (huffvalue, value, size : Integer) ;
  Var
    huffcode : Word ;
    huffsize : Byte ;
  Begin
  // Section F.1.2.2.1
  if huffvalue >= 0 Then
    Begin
    ac_table.encode (huffvalue, huffcode, huffsize) ;
    output_stream.outputBits (huffcode, huffsize) ;
    End ;
  if size <> 0 Then
    output_stream.outputBits (value, size) ;
  End ;
//
//  Description:
//
//    This function is used for two purposes in a sequential scan:
//
//      o To gather statistics for generating Huffman Tables
//      o To encode and output a data unit.
//
//    The dcfunction and acfunction arguments are determine which of these
//    actions are performed. If these arguments are PrintDcData () and
//    PrintAcData () the data unit is encoded and written to the output
//    stream.  If the arguments are GatherDcData () and GatherAcData ()
//    usage statistics are gathered.
//
//    While creating a separate function for each purpose may have been clearer,
//    it would create maintenance problems because they would have to be kept
//    in strict synchronization with each other.
//
//  Parameters:
//    row,col: Data unit position
//    dcfunction: Function for outputting DC coefficient values.
//    acfunction: Function for outputting AC coefficient values.
//    sss, sse, ssa : Not used
//
//    This function is of the type COMPONENTPASSFUNCTION.
//
Procedure TJpegEncoderComponent.encodeSequential (row, col : Cardinal ;
                                  dcfunction : DCOUTPUTFUNCTION ;
                                  acfunction : ACOUTPUTFUNCTION ;
                                  sss, sse : COEFFICIENTINDEX ;
                                  ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Var
    diff : Integer ;
    bits : Integer ;
    du : ^TJpegCoefficientBlock ;
    ssss : Integer ;
    zerorun : Integer ;
    index : Integer ;
    rrrrssss : Integer ;
    value : Integer ;
  Begin
  du := @dct_coefficients [row * du_cols + col] ;

  // DC value calculation
  // Section F.1.2.1.3
  diff := du^ [0] - last_dc_value ;
  last_dc_value := du^ [0] ;

  // Break the DC coefficient into a category (Table F.12) and
  // additional bits.
  if diff >= 0 Then
    Begin
    bits := diff ;
    End
  else
    Begin
    diff := -diff ;
    bits := Not diff ;
    End ;
  ssss := 0 ;  // Category
  while diff <> 0 Do
    Begin
    Inc (ssss) ;
    diff := diff Shr 1 ;
    End ;
  dcfunction (ssss, bits) ;

  // AC coefficient coding
  // F.1.2.2.3 Figure F.2
  zerorun := 0 ; // Called rrrr in the specification.
  for index := 1 To JPEGSAMPLESIZE -1 Do
    Begin
    if du^ [JpegZigZagInputOrder [index]] <> 0 Then
      Begin
      // 16 is the longest run of zeros that can be encoded except for the
      // final EOB code.
      while zerorun >= 16 Do
        Begin
        // 0xF0 is the code to skip 16 zeros (Figure F.1)
        acfunction ($F0, 0, 0) ;
        Dec (zerorun, 16) ;
        End ;

      // Non-zero AC coefficients are encoded with
      // 8-bit Huffman-encoded values of the form rrrrssss followed by
      // 1..10 additional bits of data. rrrr is the number of zero values
      // to skip (0..15). ssss is the category (1..10) which specifies the
      // number of additional raw bits to follow. (Figure F.1)
      value := du^ [JpegZigZagInputOrder [index]] ;
      if value >= 0 Then
        Begin
        bits := value ;
        End
      else
        Begin
        value := -value ;
        bits := Not value ;
        End ;
      ssss := 0 ;
      while value <> 0 Do
        Begin
        value := value shr 1 ;
        Inc (ssss) ;
        End ;

      rrrrssss := (zerorun Shl 4) Or ssss ;
      acfunction (rrrrssss, bits, ssss) ;
      zerorun := 0 ;
      End
    else
      Begin
      Inc (zerorun) ;
      End ;
    End ;
  // The code $00 indicates all remaining AC coefficients are zero.
  if zerorun > 0 Then
    Begin
    acfunction (0, 0, 0) ;
    End ;
  End ;

//
//  Description:
//
//    This function outputs an End of Band run in an initial AC coefficient
//    scan of a progressive frame.
//
//  Parameters:
//    acfunction: Function for outputting AC coefficient values
//
Procedure TJpegEncoderComponent.printEobRun (acfunction : ACOUTPUTFUNCTION) ;
  var
    bits : Cardinal ;
    value : Cardinal ;
    ssss : Cardinal ;
  Begin
  // Figure G.4
  if eob_run <> 0 Then
    Begin
    bits := eob_run ;
    value := bits Shr 1 ;
    ssss := 0 ; // Category (Table G.1)
    while value <> 0 Do
      Begin
      value := value Shr 1 ;
      Inc (ssss) ;
      End ;
    acfunction (ssss Shl 4, bits, ssss) ;
    eob_run := 0 ;
    End ;
  End ;

//
//  Description:
//
//    This function is use to output a data unit for the first pass
//    DC progressive scan. The DC coefficients are encoded in the same manner
//    as in a sequential scan except for the point transform.
//
//    This function gets called twice for each data unit in the scan. The
//    first pass is used to gather Huffman statistics and the second is
//    used to Huffman-encode the data and write it to the output stream.
//    We use pointers to the statistics/output functions to ensure that
//    both passes are performed in the exact same manner.
//
//  Parameters:
//    row,col: Data unit position
//    dcfunction: Function for outputting DC coefficient values.
//    ssa: Successive Approximation
//
//    Other parameters are not used.
//
//    This function is of the type COMPONENTPASSFUNCTION.
//
Procedure TJpegEncoderComponent.progressiveDcFirst (row, col : Cardinal ;
                                    dcfunction : DCOUTPUTFUNCTION ;
                                    acfunction : ACOUTPUTFUNCTION ;
                                    sss, sse : COEFFICIENTINDEX ;
                                    ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Var
    value : Integer ;
    diff : Integer ;
    bits : Integer ;
    ssss : Integer ;
  Begin
  // G.1.2.1

  // DC value calculation
  // A.4
  value := dct_coefficients [row * du_cols + col][0] Shr ssa ;

  // Section F.1.2
  diff := value - last_dc_value ;
  last_dc_value := value ;

  // Break the difference into a category for Huffman coding and additional
  // raw bits for refinement.
  if diff >= 0 Then
    Begin
    bits := diff ;
    End
  else
    Begin
    diff := -diff ;
    bits := Not diff ;
    End ;
  ssss := 0 ;  // Category
  while diff <> 0 Do
    Begin
    Inc (ssss) ;
    diff := diff Shr 1 ;
    End ;
  dcfunction (ssss, bits) ;
  End ;

//
//  Description:
//
//    This function outputs DC coefficient data for refining scans in a
//    progressive frame. This is the only thing simple about progressive
//    JPEG. In this scan we simply encode an additional bit for each
//    DC coefficient.
//
//    Since there is no Huffman coding for this refining DC scans this function
//    only gets called once per data unit in a scan. Therefore we do not
//    use the output function pararmeters.
//
//  Parameters:
//    row,col: Data unit position
//    ssa:  Successive Approximation
//
//    This function is of the type COMPONENTPASSFUNCTION.
//
Procedure TJpegEncoderComponent.progressiveDcRefine (row, col : Cardinal ;
                                     dcfunction : DCOUTPUTFUNCTION ;
                                     acfunction : ACOUTPUTFUNCTION ;
                                     sss, sse : COEFFICIENTINDEX ;
                                     ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Var
    value : Integer ;
  Begin
  // Section G.1.2.1
  value := (dct_coefficients [row * du_cols + col][0] Shr ssa) And $01 ;
  output_stream.outputBits (value, 1) ;
  End ;

//
//  Description:
//
//    This function encodes a data unit for the first AC coefficient scan
//    for a spectral range in a progressive frame. The procedure here
//    is taken literally from the JPEG specification.
//
//    The AC encoding complexity is significantly increased over that of
//    sequential scans because End of Bands can span data units.
//
//  Parameters:
//    row,col: Data unit position
//    acfunction: Function for outputting AC coefficient values
//    sss:  Spectral Selection Start
//    sse:  Spectral Selection End
//    ssa:  Successive Approximation
//
Procedure TJpegEncoderComponent.progressiveAcFirst (row, col : Cardinal ;
                                    acfunction : ACOUTPUTFUNCTION ;
                                    sss, sse : COEFFICIENTINDEX ;
                                    ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Var
    du : ^TJpegCoefficientBlock ;
    zerorun : Cardinal ;
    ii : Cardinal ;
    bits : Integer ;
    ssss : Cardinal ;
    rrrrssss : Cardinal ;
    value : Integer ;
  Begin
  If (row * du_cols + col) >= Length (dct_coefficients) Then
    Raise EJpegError.Create ('INTERNAL ERROR - Bad Array Bound') ;
  du := @dct_coefficients [row * du_cols + col] ;

  // G.1.2.2 Figure G.3
  zerorun := 0 ;
  for ii := sss To sse Do
    Begin
    value := du^ [JpegZigZagInputOrder [ii]]  ;
    // Point Transform
    value := value Div (1 Shr ssa) ;

    if value = 0 Then
      Begin
      Inc (zerorun) ;
      End
    else
      Begin
      printEobRun (acfunction) ;
      // Figure G.5
      while zerorun >= 16 Do
        Begin
        acfunction($F0, 0, 0) ;
        Dec (zerorun, 16) ;
        End ;

      if value >= 0 Then
        Begin
        bits := value ;
        End
      else
        Begin
        value := -value ;
        bits := Not value ;
        End ;
      ssss := 0 ;
      while value <> 0 Do
        Begin
        Inc (ssss) ;
        value := value Shr 1 ;
        End ;
      rrrrssss := (zerorun Shl 4) Or ssss ;
      acfunction (rrrrssss, bits, ssss) ;
      zerorun := 0 ;

      if ii >= sse Then
        Exit ;
      End ;
    End ;
  Inc (eob_run) ;

  // Do not allow the EOB run to exceed 0x7FFF.
  // G.1.2.2
  if eob_run = $7FFF Then
    printEobRun (acfunction) ;
  End ;

//
//  Description:
//
//    This function encodes the AC coefficients for a refining scan in a
//    progressive frame.
//
//    The JPEG standard is nebulous here (Section G.1.2.3). It is
//    unfortunate that for such a brain-damaged encoding method as
//    this that they should be unclear. In addition to the complexity
//    of having EOB runs span data units, data does not get written
//    out in the order it occurs.
//
//    This is why there are no section references in the code other than
//    the one above. I am simply guessing here, sorry. I created this code by
//    guessing and running the output through various applications that handle
//    progressive JPEG until I could find no complaints.
//
//    If you thing this is bad wait until you get to the part about decoding
//    progressive scans (G.2)!
//
//  Parameters:
//    row,col: Data unit position
//    acfunction: Function for outputting AC coefficient values
//    sss:  Spectral Selection Start
//    sse:  Spectral Selection End
//    ssa:  Successive Approximation
//
Procedure TJpegEncoderComponent.progressiveAcRefine (row, col : Cardinal ;
                                     acfunction : ACOUTPUTFUNCTION ;
                                     sss, sse : COEFFICIENTINDEX ;
                                     ssa : JPEGSUCCESSIVAPPROXIMATIONVALUE) ;
  Var
    du : ^TJpegCoefficientBlock ;
    zerorun : Cardinal ;
    zerostart : Cardinal ;
    zerocount : Cardinal ;
    ii, jj : Cardinal ;
    value : Integer ;
    oldvalue : Integer ;
    output : Integer ;
  Begin
  du := @dct_coefficients [row * du_cols + col] ;
  // Number of zero coefficients - Note that existing non-zero coefficients
  // are not included in this count.
  zerorun := 0 ;
  // The start of the zero run.
  zerostart := sss ;

  for ii := sss To sse Do
    Begin
    // Point Transform
    value := du^ [JpegZigZagInputOrder [ii]] Div (1 Shl ssa) ;

    // We have three types of values:
    //  o A Zero
    //  o A coefficient that was zero in all previous scan that we are
    //    going to make non-zero in this scan (value = +/-1)
    //  o An coefficient that was made non-zero in a previous scan
    //      (value > 1 OR value < -1)
    if value = 0 Then
      Begin
      Inc (zerorun) ;
      End
    else if (value = 1) Or (value = -1) Then
      Begin
      // If way have an EOB run then print it out.
      printRefineEobRun (acfunction, sss, sse, ssa) ;

      // The longest zero run we can have is 16.
      while zerorun >= 16 Do
        Begin
        acfunction ($F0, 0, 0) ;
        Dec (zerorun, 16) ;

        // Refine all the existing coefficients skipped by the zero run.
        zerocount := 0 ;
        While zerocount < 16 Do
          Begin

          oldvalue := du^ [JpegZigZagInputOrder [zerostart]] Div (1 Shl ssa) ;
          if oldvalue < 0 Then
            oldvalue := - oldvalue ;
          if oldvalue > 1 Then
            Begin
            acfunction (-1, (oldvalue And $01), 1) ;
            End
          else if oldvalue = 0 Then
            Begin
            // Because we need to count only zero values our loop counter
            // gets incremented here.
            Inc (zerocount) ;
            End
          else
            Begin
            // If the value is +/- 1 we should have already processed it.
            Raise EJpegError.Create ('INTERNAL ERROR - Bad Value') ;
            End ;
          zerostart := 0 ;
          End ;
        End ;

      // This is the first time this value has been nonzero.
      output := (zerorun Shl $04) Or 1 ;
      if value > 0 Then
        acfunction (output, 1, 1)
      else
        acfunction(output, 0, 1) ;
      zerorun := 0 ;

      // No go back and refine all the previously non-zero coefficients
      // skipped by this zero run.
      for jj := zerostart To ii - 1 Do
        Begin
        oldvalue := du^ [JpegZigZagInputOrder [jj]] Div (1 Shl ssa) ;
        if oldvalue < 0 Then
          oldvalue := - oldvalue ;
        if oldvalue > 1 Then
          Begin
          acfunction (-1, (oldvalue And $01), 1) ;
          End ;
        End ;
      zerostart := ii + 1 ;
      if ii = sse Then
        Exit ;  // All finished with this data unit.
      End ;
    End ;
  // If we get here then the data unit ends with a string of zero coefficients
  // or previously non-zero coefficients that we are skipping.
  if eob_run = 0 Then
    Begin
    // We are beginning and End of Band run. Mark the starting position
    // including the spectral position.
    eob_start_du_row := row ;
    eob_start_du_col := col ;
    eob_start_position := zerostart ;
    End ;
  Inc (eob_run) ;
  // G.1.2.2
  if eob_run = $7FFF Then
    printRefineEobRun (acfunction, sss, sse, ssa) ;
  End ;

//
//  Description:
//
//    This function Resets the End of Band run counters. It should be called
//    before beginning to output a scan.
//
Procedure TJpegEncoderComponent.resetEobRun ;
  Begin
  eob_run := 0 ;
  // We use values here that should make an error easier to detect.
  eob_start_du_row := du_cols * du_rows ;
  eob_start_du_col := du_cols * du_rows ;
  eob_start_position := JPEGSAMPLESIZE ;
  End ;

//
//  Description:
//
//    This function outputs an End of Band run in a refining AC ceofficient
//    scan of a progressive frame. As for the rest of the refining AC scan
//    data I am mostly guessing here. I have taken the process from
//    Figure G.4 and the vague description in Section G.1.2.3. It seems to
//    work.
//
//  Parameters:
//    acfunction: Function for outputting AC coefficient values
//    sss:  Spectral Selection Start
//    sse:  Spectral Selection End
//    ssa:  Successive Approximation
//
Procedure TJpegEncoderComponent.printRefineEobRun (acfunction : ACOUTPUTFUNCTION ;
                                                   sss, sse, ssa : Cardinal) ;
  Var
    bits : Cardinal ;
    value : Integer ;
    ssss : Cardinal ;
    eobcounter : Cardinal ;
    row, col : Cardinal ;
    du : ^TJpegCoefficientBlock ;
    kk : Integer ;
  Begin
  if eob_run <> 0 Then
    Begin
    bits := eob_run ;
    value := bits Shr 1 ;
    ssss := 0 ; // Category (Table G.1)
    while value <> 0 Do
      Begin
      value := value shr 1 ;
      Inc (ssss) ;
      End ;
    acfunction (ssss Shl 4, bits, ssss) ;

    // Now that we have output the EOB run we need to refine each coefficient
    // that we skipped that had previously been non-zero.
    eobcounter := 0 ;
    row := eob_start_du_row ;
    col := eob_start_du_col ;
    while eobcounter < eob_run Do
      Begin
      du := @dct_coefficients [row * du_cols + col] ;
      for kk := eob_start_position  To sse Do
        Begin
        value := du^ [JpegZigZagInputOrder [kk]] Div (1 Shl ssa) ;
        if value < 0 Then
          value := - value ;
        if value > 1 Then
          Begin
          acfunction (-1, (value And $01), 1) ;
          End ;
        End ;
      // Except for the first data unit we go to the first spectral value
      // in the scan.
      eob_start_position := sss ;

      Inc (eobcounter) ;
      Inc (col) ;
      if col = du_cols Then
        Begin
        col := 0 ;
        Inc (row) ;
        End ;
      End ;

    // Some values that are certain to cause errors if improperly used.
    eob_start_position := JPEGSAMPLESIZE ;
    eob_start_du_row := du_cols * du_rows ;
    eob_start_du_col := du_cols * du_rows ;

    eob_run := 0 ; // Reset the counter.
    End ;
  End ;

class Procedure TJpegEncoderComponent.rgbConvert (encoder : TJpegEncoder ;
                                            image : TBitmapImage ;
                                       maxhf, maxvf : JPEGSAMPLINGFREQUENCY ;
                                       ycomponent, cbcomponent, crcomponent : TJpegEncoderComponent) ;
  const
    progressscale = 8 ;
    OPERATION = 'RGB Convert' ;
  Var
    columns, rows : Cardinal ;
    ii, jj : Cardinal ;
    yvalue, cbvalue, crvalue : JPEGSAMPLE ;
    ypointer, cbpointer, crpointer : Cardinal ;
    progress, progressincrement : Cardinal ;
    pixel : Cardinal ;
  Begin

  progress := 0 ;
  progressincrement := (100 Shl progressscale) Div image.Height ;

  yvalue  := 0 ;
  cbvalue := 0 ;
  crvalue := 0 ;

  // Image dimensions rounded up to a multiple of JpegSampleWidth x Max Sampling Frequency

  columns := NoninterleavedColumns (image.Width, maxhf) ;
  rows := NoninterleavedRows (image.Height, maxvf) ;
  ycomponent.allocateComponentBuffer (image.Width, image.Height, maxhf, maxvf) ;
  cbcomponent.allocateComponentBuffer (image.Width, image.Height, maxhf, maxvf) ;
  crcomponent.allocateComponentBuffer (image.Width, image.Height, maxhf, maxvf) ;

  ypointer := 0 ;
  cbpointer := 0 ;
  crpointer := 0 ;
  pixel := 0 ;

  for ii := 0 To image.Height - 1 Do
    Begin
    for jj := 0 To image.Width - 1 Do
      Begin
      yvalue  := RgbToY (image.Pixels [pixel].red, image.Pixels [pixel].green, image.Pixels [pixel].blue) ;
      cbvalue := RgbToCb (image.Pixels [pixel].red, image.Pixels [pixel].green, image.Pixels [pixel].blue) ;
      crvalue := RgbToCr (image.Pixels [pixel].red, image.Pixels [pixel].green, image.Pixels [pixel].blue) ;
      ycomponent.component_buffer [ypointer] := yvalue ;    Inc (ypointer) ;
      cbcomponent.component_buffer [cbpointer] := cbvalue ; Inc (cbpointer) ;
      crcomponent.component_buffer [crpointer] := crvalue ; Inc (crpointer) ;
      Inc (pixel) ;
      End ;
    // Extend the last row
    for jj := image.Width To columns - 1 Do
      Begin
      ycomponent.component_buffer [ypointer]   := yvalue ;  Inc (ypointer) ;
      cbcomponent.component_buffer [cbpointer] := cbvalue ; Inc (cbpointer) ;
      crcomponent.component_buffer [crpointer] := crvalue ; Inc (crpointer) ;
      End ;
    Inc (progress, progressincrement) ;
    encoder.callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;

  for ii := image.Height To rows - 1 Do
    Begin
    // Copy from the previous row
    ycomponent.component_buffer [ypointer]   := ycomponent.component_buffer [ypointer - columns]   ; Inc (ypointer) ;
    cbcomponent.component_buffer [cbpointer] := cbcomponent.component_buffer [cbpointer - columns] ; Inc (cbpointer) ;
    crcomponent.component_buffer [crpointer] := crcomponent.component_buffer [crpointer - columns] ; Inc (crpointer) ;
    End ;

  encoder.callProgressFunction (OPERATION, 100) ;
  End ;

class Procedure TJpegEncoderComponent.grayScaleConvert (
                                  encoder : TJpegEncoder ;
                                  image : TBitmapImage ;
                                  ycomponent : TJpegEncoderComponent) ;
  const
    progressscale = 8 ;
    OPERATION = 'Grayscale Convert' ;
  Var
    progress : Cardinal ;
    progressincrement : Cardinal ;
    ii, jj, kk, ll : Cardinal ;
    row, column : Cardinal ;
    index : Cardinal ;
    data : TJpegEncoderDataUnit ;
    pixel : Cardinal ;
    yvalue : JPEGSAMPLE ;
  Begin
  ycomponent.allocateComponentBuffer (image.Width, image.Height,
                                      ycomponent.HorizontalFrequency,
                                      ycomponent.VerticalFrequency) ;

  ycomponent.du_rows := (image.Height + JPEGSAMPLEWIDTH - 1) Div JPEGSAMPLEWIDTH ;
  ycomponent.du_cols := (image.Width + JPEGSAMPLEWIDTH - 1) Div JPEGSAMPLEWIDTH ;

  SetLength (ycomponent.dct_coefficients, ycomponent.du_cols * ycomponent.du_rows) ;

  progress := 0 ;
  progressincrement := (100 Shr progressscale) Div (ycomponent.du_rows) ;

  index := 0 ;

  for ii := 0 To ycomponent.du_rows -  1 Do
    Begin
    for jj := 0 To ycomponent.du_cols - 1 Do
      Begin
      for kk := 0 To JPEGSAMPLEWIDTH - 1 Do
        Begin
        for ll := 0 To JPEGSAMPLEWIDTH - 1 Do
          Begin
          row := ii * JPEGSAMPLEWIDTH + kk ;
          column := jj * JPEGSAMPLEWIDTH + ll ;
          if (row < image.Height) And (column < image.Width) Then
            Begin
            pixel := row * image.Width + column ;
            yvalue := RgbToY (Image.Pixels [pixel].red, Image.Pixels [pixel].green, Image.Pixels [pixel].blue) ;
            data [kk, ll] := yvalue ;
            End
          else
            Begin
            data [kk, ll] := JPEGMIDPOINTSAMPLEVALUE ;
            End ;
          End ;
        End ;
      ForwardDct (data, ycomponent.quantization_table, ycomponent.dct_coefficients [index]) ;
      Inc (index) ;
      End ;
    Inc (progress, progressincrement) ;
    encoder.callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;

  encoder.callProgressFunction (OPERATION, 100) ;
  End ;


Procedure TJpegEncoderComponent.sample1to1Component (encoder : TJpegEncoder) ; 
  const
    progressscale = 8 ;
    OPERATION = 'Sampling' ;
  Type
    ROWBUFFER = Array [0..$7FFFFF] of JPEGSAMPLE ;
  Var
    progress, progressincrement : Cardinal ;
    row0, row1, row2, row3, row4, row5, row6, row7 : Cardinal ;
    index : Cardinal ;
    data : TJpegEncoderDataUnit ;
    ii, jj, kk : Cardinal ;
    offset : Cardinal ;
  Begin
  progress := 0 ;
  progressincrement := (100 Shl progressscale) Div (du_rows) ;

  index := 0 ;
  row0 := 0 ;
  row1 := component_buffer_columns ;
  row2 := component_buffer_columns * 2 ;
  row3 := component_buffer_columns * 3 ;
  row4 := component_buffer_columns * 4 ;
  row5 := component_buffer_columns * 5 ;
  row6 := component_buffer_columns * 6 ;
  row7 := component_buffer_columns * 7 ;

  offset := 0 ;
  for ii := 0 To du_rows - 1 Do
    Begin
    for jj := 0 To du_cols - 1 Do
      Begin

      for kk := 0 To JPEGSAMPLEWIDTH - 1 Do
        Begin
        data [0][kk] := component_buffer [row0 + offset] ;
        data [1][kk] := component_buffer [row1 + offset] ;
        data [2][kk] := component_buffer [row2 + offset] ;
        data [3][kk] := component_buffer [row3 + offset] ;
        data [4][kk] := component_buffer [row4 + offset] ;
        data [5][kk] := component_buffer [row5 + offset] ;
        data [6][kk] := component_buffer [row6 + offset] ;
        data [7][kk] := component_buffer [row7 + offset] ;
        Inc (offset) ;
        End ;

      ForwardDct (data, quantization_table, dct_coefficients [index]) ;
      Inc (index) ;
      End ;
    row0 := row7 + offset ;
    row1 := row0 + component_buffer_columns ;
    row2 := row1 + component_buffer_columns ;
    row3 := row2 + component_buffer_columns ;
    row4 := row3 + component_buffer_columns ;
    row5 := row4 + component_buffer_columns ;
    row6 := row5 + component_buffer_columns ;
    row7 := row6 + component_buffer_columns ;
    offset := 0 ;
    Inc (progress, progressincrement) ;
    encoder.callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;
  encoder.callProgressFunction (OPERATION, 100) ;
  End ;


Procedure TJpegEncoderComponent.sampleNtoNComponent (encoder : TJpegEncoder ;
                                                maxhf, maxvf : JPEGSAMPLINGFREQUENCY) ;
  const
    progressscale = 8 ;
    OPERATION = 'Sampling' ;
  type
    ROWTYPE = Array [0..$7FFFFF] of JPEGSAMPLE ;
  var
    rowbuffer : ^ROWTYPE ;
    progress, progressincrement : Cardinal ;
    durow, ducol : Cardinal ;
    mcurow, mcucol : Cardinal ;
    ii, jj : Cardinal ;
    data : TJpegEncoderDataUnit ;
    index : Cardinal ;
    vperiod, hperiod : JPEGSAMPLINGFREQUENCY ;
    mcuoffset : Cardinal ;
    sums : Array [0..JPEGSAMPLEWIDTH-1, 0..JPEGSAMPLEWIDTH-1] of Cardinal ;
    offset : Cardinal ;
  Begin
  vperiod := maxvf Div v_frequency ;
  hperiod := maxhf Div h_frequency ;

  index := 0 ;

  progress := 0 ;
  progressincrement := (100 Shl progressscale) Div du_rows ;

  for durow := 0 To du_rows - 1 Do
    Begin
    // Offset of the upper left corner of the MCU from the start of the image.
    mcuoffset := durow * component_buffer_columns * vperiod * JPEGSAMPLEWIDTH ;
    for ducol := 0 To du_cols -1 Do
      Begin

      For ii := 0 To JPEGSAMPLEWIDTH - 1 Do
        For jj := 0 To JPEGSAMPLEWIDTH - 1 Do
          sums [ii, jj] := 0 ;

      rowbuffer := @component_buffer [mcuoffset] ;
      for mcurow := 0 To JPEGSAMPLEWIDTH - 1 Do
        Begin
        for ii := 1 To vperiod Do
          Begin
          offset := 0 ;
          for mcucol := 0 To JPEGSAMPLEWIDTH - 1 Do
            Begin
            for jj := 1 To hperiod Do
              Begin
              Inc (sums [mcurow, mcucol], rowbuffer^ [offset]) ;
              Inc (offset) ;
              End ;
            End ;
          Inc (rowbuffer, component_buffer_columns) ;
          End ;
        End ;
      for ii := 0 To JPEGSAMPLEWIDTH -1 Do
        Begin
        for jj := 0 To JPEGSAMPLEWIDTH -1 Do
          Begin
          data [ii, jj] := sums [ii, jj] Div (vperiod * hperiod) ;
          End ;
        End ;

      ForwardDct (data, quantization_table, dct_coefficients [index]) ;
      Inc (mcuoffset, JPEGSAMPLEWIDTH * hperiod) ;
      Inc (index) ;
      End ;
    Inc (progress, progressincrement) ;
    encoder.callProgressFunction (OPERATION, progress Shr progressscale) ;
    End ;
  encoder.callProgressFunction (OPERATION, 100) ;
  End ;


Procedure TJpegEncoderComponent.sampleComponent (encoder : TJpegEncoder ;
                                                 maxhf, maxvf : JPEGSAMPLINGFREQUENCY) ;
  Begin

  du_rows := v_frequency * component_buffer_rows Div JPEGSAMPLEWIDTH Div maxvf ;
  du_cols := h_frequency * component_buffer_columns Div JPEGSAMPLEWIDTH Div maxvf ;

  SetLength (dct_coefficients, du_cols * du_rows) ;

  if (h_frequency = maxhf) And (v_frequency = maxvf) Then
    sample1to1Component (encoder)
  else
    sampleNtoNComponent (encoder, maxhf, maxvf) ;

  End ;

Procedure TJpegEncoderComponent.allocateComponentBuffer (
                                imagewidth, imageheight : Cardinal ;
                                maxhf, maxvf : JPEGSAMPLINGFREQUENCY) ;
  Var
    columns, rows : Cardinal ;
  Begin
  columns := NoninterleavedColumns (imagewidth, maxhf) ;
  rows := NoninterleavedRows (imageheight, maxvf) ;

  SetLength (component_buffer, rows * columns) ;

  component_buffer_columns := columns ;
  component_buffer_rows := rows ;

  noninterleaved_du_rows := (imageheight * maxvf
        + v_frequency * JPEGSAMPLEWIDTH - 1) // Rounding
     Div (v_frequency * JPEGSAMPLEWIDTH);
  noninterleaved_du_colummns := (imagewidth * maxhf
        + h_frequency * JPEGSAMPLEWIDTH - 1) // Rounding
     Div (h_frequency * JPEGSAMPLEWIDTH);
  End ;

Procedure TJpegEncoderComponent.freeDynamicStorage ;
  Begin
  SetLength (dct_coefficients, 0) ;
  SetLength (component_buffer, 0) ;
  End ;

End.
