unit inflatedecoder;

//
//  Title:  Inflate Decoder class implementation
//
//  Copyright 1999, 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    March 15, 1999
//
//  Version: 1
//
//  Description:
//
//    This class is a decoder for Deflate/Inflate compression.
//
//  Revision History:
//
//

interface

Uses
  sysutils, systemspecific, inflateinputstream, inflatehuffmandecoder ;

Type
  TInflateDecoder = Class
    private
    // Deflate State Variables
    stream_adler : Cardinal ;       // Adler32 Checksum value
    lz_window : Array [0..32767] of Integer ;         // LZ77 Sliding Window
    window_position : Cardinal ;  // Output position in the LZ77 window
    copy_position : Cardinal ;    // Input posiiton in the LZ77 window
    copy_count : Cardinal ;   // Number bytes remaining from a copy operation
    literal_table, distance_table : TInflateHuffmanDecoder ;
    final_data_set : Boolean ;  // Set when Final data set is read
    no_more_data : Boolean ;    // Set when no more data is available.
    literal_count : Cardinal ; // Number remaining literal bytes
    verbose_flag : boolean ;
    // Deflate Functions
    Procedure startNewDataSet (strm : TInflateInputStream) ;
    Procedure checkAdler (strm : TInflateInputStream) ;
    Procedure readLengths (strm : TInflateInputStream ;
            ht : TInflateHuffmanDecoder ;
            var lengths : Array of Cardinal ;
            lengthcount : Cardinal) ;
  Public
    Constructor Create ;
    Destructor Destroy ; Override ;
    Property Verbose : Boolean read verbose_flag write verbose_flag ;
    Procedure openStream (strm : TInflateInputStream) ;
    Function decode (strm : TInflateInputStream ; count : Cardinal ;  output : PChar)  : Integer ;
    Property NoMoreData : Boolean read no_more_data ;
  End ;

  EStreamError = Class (Exception) ;

implementation

uses deflatepvt, adler32 ;

const
// Deflate Compression Types
  Uncompressed = 0 ;
  FixedHuffmanCodes = 1 ;
  DynamicHuffmanCodes = 2 ;


Function SystemToBigEndian (value : Cardinal) : Cardinal ;
  Begin
  Result := ((value And $FF000000) Shr 24) Or((value And $FF0000) Shr 8)
         Or ((value And $FF00) Shl 8) Or ((value And $FF) Shl 24) ;
  End ;

//
// Class Constructor
//
// Nothing gets done here except initializing variables to a known state.
//
Constructor TInflateDecoder.Create ;
  Begin
  Inherited Create ;
  literal_table := TInflateHuffmanDecoder.Create ;
  distance_table := TInflateHuffmanDecoder.Create ;
  verbose_flag := false ;
  no_more_data := true ;
  End ;

//
// Class Destructor
//
Destructor TInflateDecoder.Destroy ;
  Begin
  distance_table.Destroy ;
  literal_table.Destroy ;
  End ;

//
//  Description:
//
//    This function reads Huffman-encoded code lengths for another
//    Huffman table. The Huffman encoded values range from 0..18.
//    The values have the following meanings:
//
//      0..15=>A literal length value
//      16=>Repeat the last code N times. N is found by reading
//          the next 2 bits and adding 3.
//      17=>Set the N length codes to zero. N is found by reading the
//          next 3 bits and adding 3.
//      18=>Set the N length codes to zero. N is found by reading the
//          next 7 bits and adding 11.
//
//  Parameters:
//    ht:  The Huffman table used to decode the input stream.
//    lengths: The output length values
//    lengthcount:  The number of length values to read.
//
//
Procedure TInflateDecoder.readLengths (
            strm : TInflateInputStream ;
            ht : TInflateHuffmanDecoder ;
            var lengths : Array of Cardinal ;
            lengthcount : Cardinal) ;
  var
    Index : Cardinal ;
    Command : Integer ;
    Count : Cardinal ;
    ii : Cardinal ;
  Begin
  Index := 0 ;
  While (index < lengthcount) Do
    Begin
    command := ht.decode (strm) ;
    if (command < 16) Then
      Begin
{$IFDEF VERYVERBOSE}
      if (verbose_flag) Then
        WriteLn (index, ' Literal: ', command) ;
{$ENDIF}
      // Raw Length
      lengths [index] := command ;
      Inc (index) ;
      End
    else if (command = 16) Then
      Begin
      // Repeat previous
      count := strm.getBits (2) + 3 ;
{$IFDEF VERYVERBOSE}
      if (verbose_flag) Then
        WriteLn (index, ' Repeat: ', count) ;
{$ENDIF}
      for II := 1 To Count Do
        Begin
        if (index = lengthcount) Then
          raise EStreamError.Create ('Length Command Out of Range') ;
        lengths [index] := lengths [index - 1] ;
        Inc (index) ;
        End ;
      End
    else if (command = 17) Then
      Begin
      // Run of zeros
      count := strm.getBits (3) + 3 ;
{$IFDEF VERYVERBOSE}
      if (verbose_flag) Then
        WriteLn (index, ' Zero Run: ', count) ;
{$ENDIF}
      for ii := 1 To Count Do
        Begin
        if (index = lengthcount) Then
          raise  EStreamError.Create ('Length Command Out of Range') ;
        lengths [index] := 0 ;
        Inc (index) ;
        End ;
      End
    else if (command = 18) Then
      Begin
      // Longer run of zeros
      count := strm.getBits (7) + 11 ;
{$IFDEF VERYVERBOSE}
      if (verbose_flag) Then
        WriteLn (index, ' Zero Run: ', count) ;
{$ENDIF}
      for ii := 1 To Count Do
        Begin
        if (index = lengthcount) Then
          raise EStreamError.Create ('Length Command Out of Range') ;
        lengths [index] := 0 ;
        Inc (index) ;
        End ;
      End
    else
      Begin
      raise EStreamError.Create ('Bad Length Code') ;
      End ;
    End ;
  End ;

Function TInflateDecoder.decode (
                         strm : TInflateInputStream ;
                         count : Cardinal ;
                         output : PChar) : Integer ;
  Const
    // The number of extra bits for code-257
    length_extra : Array [0..DEFLATELENGTHCODECOUNT-1] Of Integer =
      (
        0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2,
        2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0
      ) ;
      // The smallest length value for code-257. The actual length value is
      // the sum of this value and the extra bits.
      length_base : Array [0..DEFLATELENGTHCODECOUNT-1] Of Integer =
      (
         3,   4,   5,   6,   7,   8,   9,  10,  11,  13,
        15,  17,  19,  23,  27,  31,  35,  43,  51,  59,
        67,  83,  99, 115, 131, 163, 195, 227, 258
      ) ;
      // The number of extra bits for a distance code.
      distance_extra : Array [0..DEFLATEMAXDISTANCECODES-1] Of Integer =
        (
        0,  0,  0,  0,  1,  1,  2,  2,  3,  3, 4,  4,  5,  5,  6,
        6,  7,  7,  8,  8,  9,  9, 10, 10, 11, 11, 12, 12, 13, 13
        ) ;
      // The smallest value for a distance code.
      distance_base : Array [0..DEFLATEMAXDISTANCECODES-1] Of Integer =
        (
           1,    2,     3,     4,     5,    7,    9,    13,    17,    25,
          33,   49,    65,    97,   129,  193,  257,   385,   513,   769,
        1025, 1537,  2049,  3073,  4097, 6145, 8193, 12289, 16385, 24577
        ) ;


  Var
    ii : Cardinal ;
    next : byte ;
    value : Cardinal ;
    length : Cardinal ;
    distance : Cardinal ;
    Extra : Integer ;
  Begin
  ii := 0 ;
  while (ii < count) Do
    Begin
    if (literal_count > 0) Then
      Begin
      while (literal_count > 0) And (ii < count) Do
        Begin
        Dec (literal_count) ;
        next := strm.getByte () ;
        lz_window [window_position] := next ;
        window_position := (window_position + 1) And DEFLATEWINDOWMASK ;
        stream_adler := Adler (stream_adler, next) ;
        output [ii] := chr (next) ;
        Inc (ii) ;
        End ;
      if (literal_count = 0) Then
        If Not final_data_set Then
          startNewDataSet (strm)
        Else
          Begin
          no_more_data := true ;
          checkAdler (strm) ;
          End ;
      End
    else if (copy_count > 0) Then
      Begin

      // We have two idential loops except for the test. For performance,
      // instead of having two loop test, we identify which test will fail
      // first and test it alone.
      if (count - ii > copy_count) Then
        Begin
        // See if we are still processing a copy operation.
        while (copy_count <> 0) Do
          Begin
          // Copy the value in the LZ window.
          next := lz_window [copy_position] ;
          lz_window [window_position] := next ;
          Dec (copy_count) ;

          // Advance the copy and window positions.
          copy_position := (copy_position + 1) And DEFLATEWINDOWMASK ;
          window_position := (window_position + 1) And DEFLATEWINDOWMASK  ;
          // Update the Adler checksum.
          stream_adler := Adler (stream_adler, next) ;
          output [ii] := chr (next) ;
          Inc (ii) ;
          End ;
        End
      else
        Begin
        // See if we are still processing a copy operation.
        while (ii < count) Do
          Begin
          // Copy the value in the LZ window.
          next := lz_window [copy_position] ;
          lz_window [window_position] := next ;
          Dec (copy_count) ;

          // Advance the copy and window positions.
          copy_position := (copy_position + 1) And DEFLATEWINDOWMASK  ;
          window_position := (window_position + 1) And DEFLATEWINDOWMASK  ;

          // Update the Adler checksum.
          stream_adler := Adler (stream_adler, next) ;
          output [ii] := Chr (next) ; Inc (ii) ;
          End ;
        End ;
      End
    else
      Begin

      value := literal_table.decode (strm) ;

      if (value < DEFLATELITERALCOUNT) Then
        Begin
        // This is a data value. Add the value to the LZ window and update the
        // Adler checksum.
        lz_window [window_position] := value ;
        window_position := (window_position + 1) And DEFLATEWINDOWMASK ;
        stream_adler := Adler (stream_adler, value) ;
        output [ii] := Chr (value) ;
        Inc (ii) ;
        End 
      else if (value = DEFLATEENDCODE) Then
        Begin
        // We just read the end marker. There should be another data set in the
        // input stream that contains the data value.
        if (final_data_set) Then
          Begin
          // The current data set end the final bit set. That means there should
          // be no more data sets in the stream.
          no_more_data := true ;
          checkAdler (strm) ;
          Result := ii ;
          Exit ;
          End
        else
          Begin
          // The data value is in the next data set.
          startNewDataSet (strm) ;
          End ;
        End
      else if (value < DEFLATEMAXLENGTHCODES) Then
        Begin
        // The code specifies a length value. Read the extra bits
        // to find the actual length value.
        extra := length_extra [value - DEFLATEFIRSTLENGTHCODE] ;
        length := length_base [value - DEFLATEFIRSTLENGTHCODE] ;
        if (extra <> 0) Then
          Inc (length, strm.getBits (length_extra [value - DEFLATEFIRSTLENGTHCODE])) ;

        // The length value is followed by the distance value. Decode the
        // value then add the extra bits to get the distance value.
        value := distance_table.decode (strm) ;
        if (value > 29) Then
          raise EStreamError.Create ('Invalid Huffman Distance Value') ;
        extra := distance_extra [value] ;
        distance := distance_base [value] ;
        if (extra <> 0) Then
          Inc (distance, strm.getBits (extra)) ;

        // Set of the state variables that are used to find the following copied
        // bytes.
        copy_position := (DEFLATEWINDOWSIZE + window_position - distance) And DEFLATEWINDOWMASK ;
        copy_count := length ;
        // Return the first copy byte.
        value := lz_window [copy_position] ;
        lz_window [window_position] := value ;
        copy_position := (copy_position + 1) And DEFLATEWINDOWMASK ;
        window_position := (window_position + 1) And DEFLATEWINDOWMASK ;
        dec (copy_count) ;
        stream_adler := Adler (stream_adler, value) ;
        output [ii] := Chr (value) ;
        Inc (ii) ;
        End
      else
        Begin
        raise EStreamError.Create ('Invalid Huffman Literal Value') ;
        End ;
      End ;
    End ;
  Result := ii ;
  End ;

//
//  Description:
//
//    This function processes the start of a new data set within a compressed
//    stream. The start of a data set has the following format:
//
//      final: 1-bit (1 => this is the last data set)
//      compression type:  2-bits
//      The remainder depends upon the compression type.
//
//      Compression Type:  Uncompressed
//
//        Advance to the next byte boundary. The next two bytes is the
//        length of the uncompressed data. The following two bytes
//        are the ones complement of the length.  [length] uncompressed
//        bytes follow.
//
//      Compression Type:  Fixed Huffman Codes
//
//        The huffman encoded data bits immediately follow the type field. The
//        data is encoded using the huffman lengh codes defined in the deflate
//        specification.
//
//      Compression Type: Dynamic Huffman Codes
//
//        The trick here is that the literal and distance Huffman
//        tables are Huffman-encoded. The next values in the input
//        stream are:
//
//          number of literal codes: 5-bits + 257
//          number of distance codes: 5-bits + 1
//          number of code lengths: 4-bits + 4
//
//          Code Lengths: 3-bits * (code-lengths + 4)
//
//        The code lengths are used to create a huffman table that encodes
//        the literal table followed by the length table.
//
Procedure TInflateDecoder.startNewDataSet (strm : TInflateInputStream) ;
  Const
    // These are the length values that define the
    // literal huffman table.
    fixed_literals : Array [0..287] of Cardinal =
      (
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
        7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
        7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8
      ) ;
    // These length values define the distance huffman table.
    fixed_distances : Array [0..31] of Cardinal =
      (
        5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5
      ) ;

  Var
    compressiontype : Integer ;
    testcount : Integer ;
    HLIT : Cardinal ;
    HDIST : Cardinal ;
    HCLEN : Cardinal ;
    lengths : Array [0..DEFLATEMAXLENGTHLENGTHCODES-1] of Cardinal ;
    II : Cardinal ;
    ht : TInflateHuffmanDecoder ;
    literals : Array [0..287] Of Cardinal ;
    distances  : Array [0..31] Of Cardinal  ;
  Begin
  strm.enterBitMode (0) ;
  if (strm.getBits (1) = 0) Then
    final_data_set := false
  else
    final_data_set := true ;

  compressiontype := strm.getBits (2) ;
  if (verbose_flag) Then
    Begin
    WriteLn (' Final: ', final_data_set) ;
    Write (' Type:  ', compressiontype, ' ') ;
    Case compressiontype Of
      0: WriteLn ('none') ;
      1: WriteLn ('Fixed Huffman Codes') ;
      2: WriteLn ('Dynamic Huffman Codes') ;
      Else Write ('unknown') ;
      End ;
    WriteLn ('') ;
    End ;
  if (compressiontype = Uncompressed) Then
    Begin
    strm.exitBitMode ;
    literal_count := strm.getLittleEndianWord () ;
    testcount := strm.getLittleEndianWord () ;
    if ((literal_count And $FFFF) <> ((Not testcount) And $FFFF)) Or (literal_count = 0) Then
      raise EStreamError.Create ('Invalid Literal Count') ;
    End
  else if (compressiontype = FixedHuffmanCodes) Then
    Begin
    literal_table.makeTable (DEFLATEMAXLENGTHCODESIZE, 288, fixed_literals) ;
    distance_table.makeTable (DEFLATEMAXDISTANCECODESIZE, 32, fixed_distances) ;
    literal_count := 0 ;
    End
  else if (compressiontype = DynamicHuffmanCodes) Then
    Begin

    HLIT := strm.getBits (5) ;
    HDIST := strm.getBits (5) ;
    HCLEN := strm.getBits (4) ;

    if (verbose_flag) Then
      Begin
      WriteLn (' Literal Code Count: ', (HLIT + 257),
               ' (', HLIT, ')') ;
      WriteLn (' Distance Codes: ',  (HDIST + 1),
               ' (',  HDIST, ') ') ;
      WriteLn (' Code Length Codes: ', (HCLEN + 4),
               ' (', HCLEN, ')') ; ;
      End ;

    // Read the length codes used to huffman encode the literal and
    // distance tables. The unusual thing here is the Huffman values
    // are not in the order 0..18 but rather the order defined by
    // the lengthindices array.
    if (HCLEN + 4) > DEFLATEMAXLENGTHLENGTHCODES Then
      raise EStreamError.Create ('Invalid Huffman Code Length') ;
    For II := Low (lengths) To High (Lengths) Do
      lengths [ii] := 0 ;
    for ii := 0 To HCLEN + 3 Do
      lengths [DEFLATELENGTHORDER [ii]] := strm.getBits (3) ;
    ht := TInflateHuffmanDecoder.Create;
    Try
      ht.makeTable (DEFLATEMAXLENGTHLENGTHCODESIZE,
                    DEFLATEMAXLENGTHLENGTHCODES,
                    lengths) ;

{$IFDEF VERYVERBOSE}
      if (verbose_flag) Then
        Begin
        WriteLn ('  Lengths ') ;
        for ii := 0 To HCLEN + 3 Do
          WriteLn (DEFLATELENGTHORDER [ii], ') ', lengths [DEFLATELENGTHORDER [ii]]) ;
        End ;
{$ENDIF}
      // Using the Huffman table we just created read the length/literals
      // and distances Huffman tables.
      readLengths (strm, ht, literals, HLIT + 257) ;
      readLengths (strm, ht, distances, HDIST + 1) ;
    Finally
      ht.Destroy ;
      End ;
    literal_table.makeTable (DEFLATEMAXLENGTHCODESIZE, HLIT + 257, literals) ;
    distance_table.makeTable (DEFLATEMAXDISTANCECODESIZE, HDIST + 1, distances) ;
    literal_count := 0 ;
    End
  else
    Begin
    Raise EStreamError.Create ('Invalid Compression Type') ;
    End ;
  End ;

//
//  Description:
//
//    This function is called after all of the data is read to check the
//    the Adler checksum.
//
Procedure TInflateDecoder.checkAdler (strm : TInflateInputStream) ;
  Var
    streamvalue : Cardinal ;
  Begin
  strm.exitBitMode ;
  // After the end marker the next 4 bytes should be the adler checksum
  streamvalue := strm.getBigEndianLong ;
  if (verbose_flag) Then
    Begin
    WriteLn ('  Stream Adler Checksum: ', SystemToBigEndian (streamvalue)) ;
    WriteLn ('  Calculated Checksum: ', SystemToBigEndian (stream_adler)) ;
    End ;
  if (streamvalue <> stream_adler) Then
    Raise EStreamError.Create ('Stream Adler 32 Checksum error') ;
  End ;

Procedure TInflateDecoder.openStream (strm : TInflateInputStream) ;
  var
    ii : Cardinal ;
    data1, data2, CM, CINFO, FLEVEL, windowsize : Integer ;
    FDICT : Boolean ;

  Begin
  no_more_data := false ;
  For ii := Low (lz_window) To High (lz_window) Do
    lz_window [ii] := 0 ;

  stream_adler := 1 ;  // The Adler 32 is always initialized to one.

  // Initialize the LZ compression state variables.
  window_position := 0 ;
  copy_position := 0 ;
  copy_count := 0 ;

  // Read the copressed stream header.
  data1 := strm.getByte ;
  CM := (data1 And $0F) ; // Compression Method
  CINFO := (data1 And $F0) Shr 4 ;
  windowsize := (1 Shl (CINFO + 8)) ;

  data2 := strm.getByte () ;
  FDICT := (data2 And (1 Shl 5)) <> 0 ;
  FLEVEL := (data2 And $C0) Shr 6 ;
  // The header values are checked below after they have been printed out.
  if (verbose_flag) Then
    Begin
    WriteLn (' Compression Method: ', CM) ;
    WriteLn (' WindowSize: ', windowsize) ;
    WriteLn (' Preset Dictionary: ', FDICT) ;
    WriteLn (' Compression Level: ', FLEVEL) ;
    End ;

  // Make sure the header values are valid for PNG.
  if (CM <> 8) Then
    raise EStreamError.Create ('Invalid Compression Method - Not (8) Deflate') ;

  if ((data2 Or (data1 Shl 8)) Mod 31 <> 0) Then
    Raise EStreamError.Create ('Corrupt Compression Header flags') ;

  if (windowsize > (1 Shl 15)) Then
    Raise EStreamError.Create ('Invalid Compression Window') ;

  if (FDICT) Then
    Raise EStreamError.Create ('Preset dictionary flag set') ;

  // Read the start of the new Deflate data set.
  startNewDataSet (strm) ;
  End ;
End.
