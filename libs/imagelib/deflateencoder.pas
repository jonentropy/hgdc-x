unit deflateencoder;

//
//  Title:  Deflate Encoder Class
//
//  Copyright 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    May 10, 2001
//
//  Version: 1
//
//  Description:
//
//    A class for implementing Deflate compression.
//

interface

Uses sysutils, deflatehuffmanencoder, DeflateOutputStream, deflatepvt ;

Type
  PTHashEntry = ^THashEntry ;
  THashEntry = Record
    index  : Cardinal ;      // Index into the LZ Window
    next : PTHashEntry ;   // Next collision entry
    previous : PTHashEntry ;
    End ;

  TCompressionLevel = (FASTESTCOMPRESSION,
                       FASTCOMPRESSION,
                       DEFAULTCOMPRESSION,
                       MAXIMUMCOMPRESSION) ;

  LENGTHFUNCTION = Procedure (outputstream : TDeflateOutputStream ; index, code, extra, value : Cardinal) Of Object ;

  TDeflateEncoder = Class
    Private
      // Array of have table entries, one entry for each position in the LZ Window.
      hash_values : Array of THashEntry ;
      // Hash Table
      hash_table : Array of THashEntry ;

      distance_table, length_table, length_length_table : TDeflateHuffmanEncoder ;

      // Compression vs. Time selected by the user.
      compression_level : TCompressionLevel ;
      // Number of hash entries to search for a match.
      // This is the internal translation of compression_level.
      search_limit : Cardinal ;

      adler_value : Cardinal ;
      lz_window : Array of Byte ;
      lookahead_buffer : Array of Byte ;


      // Index of next position to add to the lookahead_buffer.
      lookahead_input_position : Cardinal ;
      // Index of next position to remove from the lookahead_buffer.
      lookahead_output_position : Cardinal ;
      // Current position in the lz_window buffer.
      lz_position : Cardinal ;

      bytes_written_to_window : Cardinal ;

      // LZ Compressed Block
      block_buffer : Array of Word ;
      // Size of Block Buffer
      block_buffer_size : Cardinal ;
      // Number of entries added to the block buffer so far.
      block_buffer_count : Cardinal ;

      // false => Compression not underway
      // true => Compression underway
      compression_in_progress : Boolean ;

      Procedure prepareNewBlock ;
      Procedure longestMatch (count : Cardinal ;
                              var bestlength : TDeflateLength ;
                              var bestoffset : Cardinal) ;
      Function hashValue (index : Cardinal) : Cardinal ;
      Procedure moveHashEntry (entry : Cardinal ; hashvalue : Cardinal) ;
      Procedure gatherLengthCounts (outputstream : TDeflateOutputStream ;
                                    index, code, extra, value : Cardinal) ;
      Procedure outputLengthCounts (outputstream : TDeflateOutputStream ;
                                    index, code, extra, value : Cardinal) ;
      Procedure findLengthCodes (outputstream : TDeflateOutputStream ;
                        actionfunction : LENGTHFUNCTION ;
                        encoder : TDeflateHuffmanEncoder ;
                        count : Cardinal) ;
      Procedure compressLookaheadData (outputstream : TDeflateOutputStream ;
                                       count : Cardinal ;
                                       endofstream : Boolean) ;

      Procedure initializeHashTable ;
      Procedure outputDeflateHeader (outputstream : TDeflateOutputStream ; lastblock : Boolean) ;
      Procedure outputZLibHeader (outputstream : TDeflateOutputStream) ;
      Procedure outputBlockData (outputstream : TDeflateOutputStream) ;

    Public
      Constructor Create ;
      Destructor Destroy ; Override ;
      Procedure startCompressedStream (outputstream : TDeflateOutputStream) ;
      Procedure compressData (outputstream : TDeflateOutputStream ;
                              outputbuffer : PChar ;
                              outputcount : Cardinal) ;
      Procedure endCompressedStream (outputstream : TDeflateOutputStream) ;


      Property CompressionLevel : TCompressionLevel Read compression_level Write compression_level ;

      Property BlockSize : Cardinal read block_buffer_size write block_buffer_size ;
    End ;

  EDeflateError = class (Exception) ;

implementation

Uses
  Adler32 ;

// Size definitions for the lookahead buffer.
Const
  LOOKAHEADSIZE = (1 Shl 9) ;
  LOOKAHEADMASK = LOOKAHEADSIZE - 1 ;

// Hash Table size definitions.
  HASHBITS = 5 ;
  HASHTABLESIZE = 1 Shl (3 * HASHBITS) ;

// End of stream marker for the lookahead buffer.
  ENDSTREAM = $FFFF ;

//
//  Description:
//
//    This function advances a lookahead buffer index.
//
//  Parameter:
//
//    position : The index to advance (in/out)
//
Procedure AdvanceLookaheadPosition (Var position : Cardinal) ;
  Begin
  Inc (position) ;
  position := Position AND LOOKAHEADMASK ;
  End ;

//
//  Description:
//
//    This function advances an LZ buffer index.
//
//  Parameter:
//
//    position : The index to advance (in/out)
//
Procedure AdvanceWindowPosition (Var position : Cardinal) ;
  Begin
  Inc (position) ;
  position := Position AND DEFLATEWINDOWMASK ;
  End ;

//
//  Description:
//
//    This function converts an LZ window position and offset to
//    a position in the LZ window.
//
//  Parameters:
//
//    windowposition : The current position in the LZ Window
//    offset : The LZ window offset
//
//  Return Value:
//
//    The window position
//
Function WindowOffsetToPosition (windowposition, offset : Cardinal) : Cardinal ;
  Begin
  result := (DEFLATEWINDOWSIZE + windowposition - offset)
                        And DEFLATEWINDOWMASK ;
  End ;

//
//  Description
//
//    This is the hash function used by compressor. It converts
//    a 3-byte sequence into an integer.
//
//  Parameters:
//
//    v1, v2, v3 : The 3-byte sequence to hash.
//
//  Return Value:
//
//    The hash value in the range (0 .. HASHTABLESIZE - 1)
//
Function Hash (v1, v2, v3 : Byte) : Cardinal ;
  Const
    mask = (1 Shl HASHBITS) - 1 ;

  Begin
  Result := (v1 And mask )
                     Or ((v2 And mask) SHl HASHBITS)
                     Or ((v3 And mask) Shl (2 * HASHBITS)) ;
  End ;

//
//  Description:
//
//    This function converts a distance value into a code,
//    count of extra bits, and extra bits.
//
//  Parameters:
//
//    distance  : The distance value to convert
//    code      : The corresponding distance code
//    extra     : The number of extra bits
//    value     : The extra bit value
//
Procedure DistanceToCode (distance : Cardinal ;  Var code,
                            extra, value : Cardinal) ;
  Const
    // maxvalue [n] is the maximum distance value for the code n.
    maxvalue : Array [0..DEFLATEMAXDISTANCECODES-1] Of Cardinal =
      (
         1,     2,     3,     4,     6,     8,    12,    16,    24,    32,
        48,    64,    96,   128,   192,   256,   384,   512,   768,  1024,
      1536,  2048,  3072,  4096,  6144,  8192, 12288, 16384, 24576, 32768
      ) ;
    // extras [n] is the number of extra bits for the code n.
    extras : Array [0..DEFLATEMAXDISTANCECODES-1] of Cardinal =
      (
        0, 0,  0,  0,  1,  1,  2,  2,  3, 3,
        4, 4,  5,  5,  6,  6,  7,  7,  8, 8,
        9, 9, 10, 10, 11, 11, 12, 12, 13, 13
      ) ;

    // bases [n] is the smallest distance value for code n.
    bases : Array [0..DEFLATEMAXDISTANCECODES-1] Of Cardinal =
      (
           1,    2,    3,    4,    5,    7,    9,    13,    17,    25,
          33,   49,   65,   97,  129,  193,  257,   385,   513,   769,
        1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577
      ) ;
  Begin
  Code := 0 ;
  While code < DEFLATEMAXDISTANCECODES Do
    Begin
    if distance <=  maxvalue [code] Then
      Begin
      extra := extras [code] ;
      value := distance - bases [code] ;
      Exit ;
      End ;
    Inc (Code) ;
    End ;
  End ;

//
//  Description:
//
//    This function converts a length value to a length code,
//    count of extra bits, and extra bit value.
//
//  Parameters:
//
//    length : The value to convert
//    code      : The corresponding length code
//    extra     : The number of extra bits
//    value     : The extra bit value
//
Procedure LengthToCode (length : TDeflateLength ; Var code,
                                 extra, value : Cardinal) ;
  Const
    // codes [n] is the length for for length n - 3.
    codes : Array [0..DEFLATELONGESTLENGTH-3] of Word =
      (
        257, 258, 259, 260, 261, 262, 263, 264,
        265, 265, 266, 266, 267, 267, 268, 268,
        269, 269, 269, 269, 270, 270, 270, 270,
        271, 271, 271, 271, 272, 272, 272, 272,
        273, 273, 273, 273, 273, 273, 273, 273,
        274, 274, 274, 274, 274, 274, 274, 274,
        275, 275, 275, 275, 275, 275, 275, 275,
        276, 276, 276, 276, 276, 276, 276, 276,
        277, 277, 277, 277, 277, 277, 277, 277,
        277, 277, 277, 277, 277, 277, 277, 277,
        278, 278, 278, 278, 278, 278, 278, 278,
        278, 278, 278, 278, 278, 278, 278, 278,
        279, 279, 279, 279, 279, 279, 279, 279,
        279, 279, 279, 279, 279, 279, 279, 279,
        280, 280, 280, 280, 280, 280, 280, 280,
        280, 280, 280, 280, 280, 280, 280, 280,
        281, 281, 281, 281, 281, 281, 281, 281,
        281, 281, 281, 281, 281, 281, 281, 281,
        281, 281, 281, 281, 281, 281, 281, 281,
        281, 281, 281, 281, 281, 281, 281, 281,
        282, 282, 282, 282, 282, 282, 282, 282,
        282, 282, 282, 282, 282, 282, 282, 282,
        282, 282, 282, 282, 282, 282, 282, 282,
        282, 282, 282, 282, 282, 282, 282, 282,
        283, 283, 283, 283, 283, 283, 283, 283,
        283, 283, 283, 283, 283, 283, 283, 283,
        283, 283, 283, 283, 283, 283, 283, 283,
        283, 283, 283, 283, 283, 283, 283, 283,
        284, 284, 284, 284, 284, 284, 284, 284,
        284, 284, 284, 284, 284, 284, 284, 284,
        284, 284, 284, 284, 284, 284, 284, 284,
        284, 284, 284, 284, 284, 284, 284, 285
      ) ;

    // extras [n] is the number of extra bits for code n.
    extras : Array [0..DEFLATEMAXLENGTHCODES - DEFLATEFIRSTLENGTHCODE -1] Of Byte =
      (
        0, 0, 0, 0, 0, 0, 0, 0,
        1, 1, 1, 1, 2, 2, 2, 2,
        3, 3, 3, 3, 4, 4, 4, 4,
        5, 5, 5, 5, 0
      ) ;
    basevalues : Array [0..DEFLATEMAXLENGTHCODES - DEFLATEFIRSTLENGTHCODE-1] Of Word =
      (
        3,   4,   5,   6,   7,   8,   9,  10,
       11,  13,  15,  17,  19,  23,  27,  31,
       35,  43,  51,  59,  67,  83,  99, 115,
      131, 163, 195, 227, 258
      ) ;
  Begin
  code := codes [length - 3] ;
  extra := extras [code - DEFLATEFIRSTLENGTHCODE] ;
  value := length - basevalues [code - DEFLATEFIRSTLENGTHCODE] ;
  End ;

//
//  Description:
//
//    Class Default Constructor
//
Constructor TDeflateEncoder.Create ;
  Begin
  Inherited Create ;
  compression_level := DEFAULTCOMPRESSION ;
  lz_window := Nil ;
  lookahead_buffer := Nil ;
  block_buffer := Nil ;
  block_buffer_size := $4000 ;
  distance_table := TDeflateHuffmanEncoder.Create ;
  length_table := TDeflateHuffmanEncoder.Create ;
  length_length_table := TDeflateHuffmanEncoder.Create ;
  compression_in_progress := false ;
  End ;

//
//  Description:
//
//    Class Destructor
//
Destructor TDeflateEncoder.Destroy ;
  Begin
  distance_table.Destroy ;
  length_table.Destroy ;
  length_length_table.Destroy ;
  Inherited Destroy ;
  End ;



//
//  Description:
//
//    This function calculates the hash value from a location
//    in the input stream. We create the hash value by
//    extracting a fixed number of the low order bits from each
//    of the next three data values.
//
//  Parameters:
//    index: The index into the lookahead buffer
//
//  Return Value:
//    The hash value
//
Function TDeflateEncoder.hashValue (index : Cardinal) : Cardinal ;
  Var
    i1, i2, i3 : Cardinal ;
  Begin
  i1 := index And LOOKAHEADMASK ;
  i2 := (index + 1) And LOOKAHEADMASK ;
  i3 := (index + 2) And LOOKAHEADMASK ;

  result := Hash (Ord (lookahead_buffer [i1]),
                        Ord (lookahead_buffer [i2]),
                        Ord (lookahead_buffer [i3])) ;
  End ;


//
//  Description:
//
//    This function moves a hash entry corresponding to a position
//    in the LZ window to a specified hash chain.
//
//  Parameter:
//    entry: The Hash Entry (index into the LZ window)
//    hashvalue:  The new hashvalue for the entry.
//
Procedure TDeflateEncoder.moveHashEntry (entry, hashvalue : Cardinal) ;
  Var
    he : PTHashEntry ;
  Begin
  he := @hash_values [entry] ;
  if he^.previous <> Nil Then
    he^.previous^.next := he^.next ;
  if he^.next <> Nil Then
    he^.next^.previous := he^.previous ;

  he^.next := hash_table [hashvalue].next ;
  he^.previous := @hash_table [hashvalue] ;
  hash_table [hashvalue].next := he ;
  if he^.next <> Nil Then
    he^.next^.previous := he ;
  End ;

//
//  Description:
//
//    This function finds the length encoding for
//    a length or distance table.
//
//  Parameters:
//    function: The function to process the code
//    lengths: lengths [n] = the length of the huffman code for n.
//    count: The number of code lengths
//
Procedure TDeflateEncoder.findLengthCodes (outputstream : TDeflateOutputStream ;
                                           actionfunction : LENGTHFUNCTION ;
                                           encoder : TDeflateHuffmanEncoder ;
                                           count : Cardinal) ;
  Var
    II, JJ, KK : Cardinal ;
    lengths : Array [0..DEFLATEMAXLENGTHCODES-1] Of Cardinal ;
    Code : Cardinal ;
  Begin
  ASSERT (count <= DEFLATEMAXLENGTHCODES) ;

  for II := 0 To count - 1 Do
    encoder.encode (ii, code, lengths [ii]) ;

  ii := 0 ;
  While ii < Count Do
    Begin
    if lengths [ii] <> 0 Then
      Begin
      actionfunction (outputstream, ii, lengths [ii], 0, 0) ;
      jj := ii + 1 ;
      End
    else
      Begin

      // Attempt to compact runs of zero length codes.
      // First find the number of consecutive zeros.
      jj := ii + 1 ;
      While (lengths [jj] = lengths [jj - 1]) And (jj < count) Do
        Inc (jj) ;
      // We need at least 3 consecutive zeros to compact them.
      Case jj - ii Of
        1: actionfunction (outputstream, ii, lengths [ii], 0, 0) ;
        2:
          Begin
          actionfunction (outputstream, ii, lengths [ii], 0, 0) ;
          actionfunction (outputstream, ii + 1, lengths [ii], 0, 0) ;
          End ;
        Else
          // We have at least three zeros.
          Begin
          kk := jj - ii ;
          if kk > 138 Then
            Begin
            kk := 138 ;
            jj := ii + kk ;
            End ;
          if kk > 10 Then
            actionfunction (outputstream, ii, 18, 7, kk - 11)
          else
            actionfunction (outputstream, ii, 17, 3, kk - 3) ;
          End ;
        End ;
      End ;
    ii := jj ;
    End ;
  End ;

//
//  Description:
//
//    This function is passed as a parameter to FindLengthCodes.
//    It is use to find the frequency for each code.
//
//  Parameters:
//
//    code:  The code generated by FindLengthCodes
//
Procedure TDeflateEncoder.gatherLengthCounts (outputstream : TDeflateOutputStream ;
                                    index, code, extra, value : Cardinal) ;
  Begin
  length_length_table.incrementFrequency (code) ;
  End ;

//
//  Description:
//
//    This function is passed as a parameter to FindLengthCodes.
//    It is use to encode and output the code to the output stream.
//
//  Parameters:
//
//    outputstream : The output stream for writing data to
//    code:  The code generated by FindLengthCodes
//    extra:  The number of extra bits of data for the code
//    value:  The extra value
//
Procedure TDeflateEncoder.outputLengthCounts (outputstream : TDeflateOutputStream ;
                                    index, code, extra, value : Cardinal) ;
  Var
    huffmancode, huffmansize : Cardinal ;
  Begin
  length_length_table.encode (code, huffmancode, huffmansize) ;

  outputstream.writeBits (huffmancode, huffmansize) ;
  if extra <> 0 Then
    outputstream.writeBits (value, extra) ;
  End ;

//
//  Description:
//
//    This function initializes the Hash Table.
//
//    This function needs to be called once at the start 
//    of the compression process.
//
Procedure TDeflateEncoder.initializeHashTable ;
  Var
    ii : Integer ;
  Begin
  for ii := 0 To DEFLATEWINDOWSIZE - 1 Do
    hash_values [ii].index := ii ;

  // Initialize the hash table to allow initial zero runs. Here we are
  // setting up the hash table so that it appears that we have read
  // 258 zero values before the actual compression begins. This way
  // if the first 258 data bytes contains a run of zeros then we already
  // have a code to compress them with.
  hash_table [Hash (0, 0, 0)].next := @hash_values [DEFLATEWINDOWSIZE - 1] ;
  hash_values [DEFLATEWINDOWSIZE - 1].next := @hash_table [0] ;

  For II := DEFLATEWINDOWSIZE - 2 DownTo DEFLATEWINDOWSIZE - DEFLATELONGESTLENGTH Do
    Begin
    hash_values [ii + 1].next := @hash_values [ii]  ;
    hash_values [ii].previous := @hash_values [ii + 1] ;
    ENd ;
  End ;

//
//  Description:
//
//    This function writes a Deflate block header and Huffman table
//    descriptions to the output stream.
//
//  Parameters:
//
//    outputstream : The stream to write the header to
//    lastblock: true => This is the last block in the image
//               false => There are more blocks to come
//
Procedure TDeflateEncoder.outputDeflateHeader (outputstream : TDeflateOutputStream ;
                                               lastblock : Boolean) ;
  Var
    lengthcount, distancecount : Cardinal ;
    code, size : Cardinal ;
    hclen : Cardinal ;
    ii : Integer ;
  Begin
  length_table.incrementFrequency (DEFLATEENDCODE) ;
  length_table.buildTable (DEFLATEMAXLENGTHCODESIZE) ;
  distance_table.buildTable (DEFLATEMAXDISTANCECODESIZE) ;

  // Determine the count of length/literal and distances that
  // are used.
  lengthcount := DEFLATEMAXLENGTHCODES ;
  size := 0 ;
  While (lengthcount > 0) And (size = 0) Do
    Begin
    length_table.encode (lengthcount - 1, code, size) ;
    if size = 0 Then
      Dec (lengthcount) ;
    End ;

  distancecount := DEFLATEMAXDISTANCECODES ;
  size := 0 ;
  While (distancecount > 0) And (size = 0) Do
    Begin
    distance_table.encode (distancecount - 1, code, size) ;
    if size = 0 Then
      Dec (distancecount) ;
    End ;

  // Gather the Huffman statistics for encoding the
  // lengths then create the Huffman table for doing so.
  length_length_table.reset ;
  findLengthCodes (outputstream,
                   gatherLengthCounts,
                   length_table,
                   lengthcount) ;
  findLengthCodes (outputstream,
                   gatherLengthCounts,
                   distance_table,
                   distancecount) ;
  length_length_table.buildTable (DEFLATEMAXLENGTHLENGTHCODESIZE) ;

  // Count the number of lengths we have to output.
  hclen := DEFLATEMAXLENGTHLENGTHCODES ;
  size := 0 ;
  while (hclen > 0) AND (size = 0) Do
    Begin
    length_length_table.encode (DEFLATELENGTHORDER [hclen-1], code, size) ;
    if size = 0 Then
      Dec (hclen) ;
    End ;

  // Write the Deflate header to the IDAT bock.
  if lastblock Then
    outputstream.writeBits (1, 1)
  else
    outputstream.writeBits (0, 1) ;

  outputstream.writeBits (2, 2) ; // Dynamic Huffman Codes
  outputstream.writeBits (lengthcount - 257, 5) ;
  outputstream.writeBits (distancecount - 1, 5) ;
  outputstream.writeBits (hclen - 4, 4) ;
  // Output the data for the Huffman table that encodes the Huffman tables
  for ii := 0  To hclen - 1 Do
    Begin
    length_length_table.encode (DEFLATELENGTHORDER [ii], code, size) ;
    outputstream.writeBits (size, 3) ;
    End ;
  // Huffman encode the lengths for the Length/Literal and Distance
  // Huffman tables.
  findLengthCodes (outputstream,
                   outputLengthCounts,
                   length_table,
                   lengthcount) ;
  findLengthCodes (outputstream,
                   outputLengthCounts,
                   distance_table,
                   distancecount) ;
  End ;

//
//  Description:
//
//    This function writes a ZLIB header to the output stream.
//
//  Parameters:
//
//    outputstream : The stream for writing the header to
//
Procedure TDeflateEncoder.outputZLibHeader (outputstream : TDeflateOutputStream) ;
  Const
    cmf = $78 ;  // 7=>32K Sliding Window, 8=> Deflate Compression
  Var
    flg : Byte ;
    check : Word ;
  Begin
  flg := Ord (compression_level) Shl 6 ;
  check := (cmf Shl 8) Or flg ;
  flg := flg Or (31 - (check Mod 31)) ;
  outputstream.writeBits (cmf, 8) ;
  outputstream.writeBits (flg, 8) ;
  End ;

//
//  Description:
//
//    This function Huffman encodes and outputs the block buffer data.
//
//    The buffer is encoded so that
//
//      0..255  is a literal byte
//      256-514 is a length code of N-256
//
//    Each length code is followed by a distance code.
//
//  Parameters:
//
//    outputstream : The stream to write the compressed data to.
//
Procedure TDeflateEncoder.outputBlockData (outputstream : TDeflateOutputStream) ;
  Var
    huffmancode, huffmansize : Cardinal ;
    code, extra, value : Cardinal ;
    limit : Cardinal ;
    ii : Integer ;
    length : TDeflateLength ;
    distance : Cardinal ;
  Begin
  limit := block_buffer_count ;
  ii := 0 ;
  While ii < limit Do
    Begin
    if block_buffer [ii] < 256 Then
      Begin
      length_table.encode (block_buffer [ii], huffmancode, huffmansize) ;
      outputstream.writeBits (huffmancode, huffmansize) ;
      End
    else
      Begin
      length := block_buffer [ii] - 256 ;
      Inc (ii) ;
      distance := block_buffer [ii] ;
      LengthToCode (length, code, extra, value) ;
      length_table.encode (code, huffmancode, huffmansize) ;

      outputstream.writeBits (huffmancode, huffmansize) ;
      if extra <> 0 Then
        outputstream.writeBits (value, extra) ;

      DistanceToCode (distance, code, extra, value) ;
      distance_table.encode (code, huffmancode, huffmansize) ;

      outputstream.writeBits (huffmancode, huffmansize) ;
      if extra <> 0 Then
        outputstream.writeBits (value, extra) ;
      End ;
    Inc (ii) ;
    End ;
  length_table.encode (DEFLATEENDCODE, huffmancode, huffmansize) ;
  outputstream.writeBits (huffmancode, huffmansize) ;
  block_buffer_count := 0 ;
  prepareNewBlock ;
  End ;

//
//  Description:
//
//    This function finds the longest match of the lookahead buffer 
//    in the LZ Window.
//
//  Parameters:
//
//    count : The number of characters in the lookahead buffer.
//    bestlength : The longest match
//    bestoffset : The offset of the longest match
//
Procedure TDeflateEncoder.longestMatch (count : Cardinal ;
                                   Var bestlength : TDeflateLength ;
                                   var bestoffset : Cardinal) ;
  Var
    hash : Cardinal ;
    len : Cardinal ;
    src, dest : Cardinal ;
    chain : Cardinal ;
    matchlimit : Cardinal ;
    current : PTHashEntry;
    limit : Cardinal ;
    newoffset : Cardinal ;

  Begin
  bestlength := 0 ;

  hash := hashValue (lookahead_output_position) ;
  if hash_table [hash].next = Nil Then
    Exit ;

  // count and DEFLATELONGESTLENGTH are the limits for the longest match.
  if count < DEFLATELONGESTLENGTH Then
    matchlimit := count
  Else
    matchlimit := DEFLATELONGESTLENGTH ;

  chain := 0 ;
  current := hash_table [hash].next ;
  While (current <> Nil) And (chain < search_limit) Do
    Begin
    src := lookahead_output_position ;
    dest := current.index ;

    limit := DEFLATEWINDOWSIZE
                - ((DEFLATEWINDOWSIZE + current^.index - lz_position)
                    And DEFLATEWINDOWMASK) ;
    if limit > matchlimit Then
      limit := matchlimit ;

    Len := 0 ;
    While (lookahead_buffer [src] = lz_window [dest]) And (len < DEFLATELONGESTLENGTH) And (Len < limit) Do
      Begin
      AdvanceWindowPosition (dest) ;
      AdvanceLookaheadPosition (src) ;
      Inc (Len) ;
      End ;


    // We only care about matches longer than 3
    if (len >= 3) And (len > bestlength) Then
      Begin
      newoffset := DEFLATEWINDOWSIZE
                - ((DEFLATEWINDOWSIZE + current^.index - lz_position)
                    And DEFLATEWINDOWMASK) ;
      if (bytes_written_to_window < newoffset) then
        break ;

      bestlength := len ;
      if (bestlength = DEFLATELONGESTLENGTH) Or (bestlength = count) Then
        break ;
      End ;

    current := current.next ;
    Inc (chain) ;
    End ;
  End ;

//
//  Description:
//
//    This function performs the initializations required
//    before creating a new compressed data block.
//
Procedure TDeflateEncoder.prepareNewBlock ;
  Begin
  length_table.reset ;
  distance_table.reset ;
  End ;

//
//  Description:
//
//    This function compresses data within the lookahead buffer.
//
//  Parameters:
//
//    outputstream : The stream to write the compressed data to
//    count : The number of bytes in the lookahead buffer
//    endofstream : true => no more data,
//                  false => More data to be compressed
//
Procedure TDeflateEncoder.compressLookaheadData (outputstream : TDeflateOutputStream ;
                                                 count : Cardinal ;
                                                 endofstream : Boolean) ;
  Var
    Limit : Cardinal ;
    Length : TDeflateLength ;
    Offset : Cardinal ;
    code, extra, value : Cardinal ;
    literal : Byte ;
    hash : Cardinal ;
    source : Cardinal ;
    II : Integer ;
  Begin
  // Here were are determining the number of characters to leave in the
  // lookahead buffer. For all but the last pass we stop seaching when
  // the number of characters left is less than the largest possible match
  // for a copy operation. This keeps use from using less than ideal matches.
  // The remaining characters will get processed on a subsequent call after
  // the buffer gets refilled.
  if endofstream Then
    limit := 3
  else
    limit := DEFLATELONGESTLENGTH ;

  while count >= limit Do
    Begin
    // See if we can find a match for the input in the
    // LZ window.
    longestMatch (count, length, offset) ;
    if length = 0 Then
      Begin
      // There is no match of at least three. Encode this value a
      // literal value.
      literal := Ord (lookahead_buffer [lookahead_output_position]) ;
      hash := hashValue (lookahead_output_position) ;
      moveHashEntry (lz_position, hash) ;

      lz_window [lz_position] := literal ;

      block_buffer [block_buffer_count] := literal ;
      Inc (block_buffer_count) ;

      length_table.incrementFrequency (literal) ;

      AdvanceLookaheadPosition (lookahead_output_position) ;
      AdvanceWindowPosition (lz_position) ;
      Inc (bytes_written_to_window) ;

      Dec (count) ;
      End
    else
      Begin
      // We have found a match.  First update the hash table and
      // copy the data in the LZ window.
      source := WindowOffsetToPosition (lz_position, offset) ;
      For II := 1 To Length Do
        Begin
        hash := hashValue (lookahead_output_position) ;
        moveHashEntry (lz_position, hash) ;

        lz_window [lz_position] := lz_window [source] ;
        AdvanceWindowPosition (lz_position) ;
        AdvanceLookaheadPosition (lookahead_output_position) ;
        AdvanceWindowPosition (source) ;
        End ;
      Inc (bytes_written_to_window, length) ;

      block_buffer [block_buffer_count] := 256 + length ;
      Inc (block_buffer_count) ;
      block_buffer [block_buffer_count] := offset ;
      Inc (block_buffer_count) ;

      // Gather Huffman Statistics
      LengthToCode (length, code, extra, value) ;
           length_table.incrementFrequency (code) ;
      DistanceToCode (offset, code, extra, value) ;
           distance_table.incrementFrequency (code) ;

      Dec (count, length) ;
      End ;
    // Since a code can either require 1 or 2 values
    // we need to allow at least one extra space.
    if block_buffer_count >= (block_buffer_size - 1) Then
      Begin
      outputDeflateHeader (outputstream, false) ;
      outputBlockData (outputstream) ;
      End ;
    End ;

  // During the final call to this function we need to consume all the characters.
  // Since there is a minimum of 3 characters in a copy operation, we could have
  // up to 2 character remaining that have to be transfered as literals.
  if endofstream Then
    Begin
    While Count > 0 Do
      Begin
      literal := Ord (lookahead_buffer [lookahead_output_position]) ;
      block_buffer [block_buffer_count] := literal ;
      length_table.incrementFrequency (literal) ;
      Inc (block_buffer_count) ;
      AdvanceLookaheadPosition (lookahead_output_position) ;
      Dec (count) ;
      End ;
    outputDeflateHeader (outputstream, true) ;
    outputBlockData (outputstream) ;
    End ;
  End ;


//
//  Description:
//
//    An application calls this function when it wants to start a Deflate
//    compressed stream.
//
//  Parameters:
//
//    outputstream  : The output stream the encoder writes to.
//
//  Restrictions:
//
//    endCompressedStream must be called before calling this function
//    again.
//

Procedure TDeflateEncoder.startCompressedStream (outputstream : TDeflateOutputStream) ;
  Var
    II : Cardinal ;
  Begin
  if compression_in_progress Then
    Raise EDeflateError.Create ('Compression Already Underway') ;

  // Buffer Allocation
  SetLength (lz_window, DEFLATEWINDOWSIZE) ;
  SetLength (lookahead_buffer,LOOKAHEADSIZE) ;
  SetLength (hash_values, DEFLATEWINDOWSIZE) ;
  SetLength (hash_table, HASHTABLESIZE) ;

  // Convert the compression level to the maximum depth hash
  // chains are searched.
  Case compression_level Of
    FASTESTCOMPRESSION: search_limit := 1 ;
    FASTCOMPRESSION:    search_limit := 64 ;
    DEFAULTCOMPRESSION: search_limit := 128 ;
    MAXIMUMCOMPRESSION: search_limit := $FFFFFFFF ;
    Else Raise EDeflateError.Create ('Invalid Compression Level') ;
    End ;

  // Checksum Initialization
  adler_value := 1 ;

  // Block Buffer Initialization
  SetLength (block_buffer, block_buffer_size) ;
  block_buffer_count := 0 ;

  // LZ Window and Lookahead buffer initialization
  For II := Low (lz_window) To High (lz_window) Do
    lz_window [0] := 0 ;

  lz_position := 0 ;
  lookahead_input_position := 0 ;
  lookahead_output_position := 0 ;
  bytes_written_to_window := 0 ;
  initializeHashTable ;

  // Hash Table Initialization
  prepareNewBlock ;

  // Output the Deflate Header.
  outputstream.enterBitMode (0) ;
  outputZLibHeader (outputstream) ;

  compression_in_progress := true ;
  End ;

//
//  Description:
//
//    Applications call this function to suuply raw data to the 
//    encoder. An application may make any number of calls
//    to this function.
//
//  Parameters:
//
//    outputstream  : The output stream the encoder writes to.
//    outputbuffer  : A pointer to the data to compress
//    outputcount   : The number of bytes in the output buffer.
//
//  Restrictions:
//
//    This function must be called after startCompressedStream and 
//    before calling endCompressedStream.
//
//    An application should use the same outputstream supplied to
//    startCompressedStream in each call to this function.
//
Procedure TDeflateEncoder.compressData (outputstream : TDeflateOutputStream ;
                                        outputbuffer : PChar ;
                                        outputcount : Cardinal) ;
  Var
    II : Cardinal ;
  Begin
  // Ensure that the compressor is in a valid state.
  if Not compression_in_progress Then
    Raise EDeflateError.Create ('Compression Not Underway') ;

  // Feed the raw data unto the lookahead buffer.
  for ii := 0 To outputcount - 1 Do
    Begin
    adler_value := Adler (adler_value, Ord (outputbuffer [ii])) ;
    lookahead_buffer [lookahead_input_position] := Ord (outputbuffer [ii]) ;
    AdvanceLookaheadPosition (lookahead_input_position) ;

    // When the lookahead buffer is full, compress the data.
    if lookahead_input_position = lookahead_output_position Then
        compressLookaheadData (outputstream, LOOKAHEADSIZE, false) ;
    End ;
  End ;

//
//  Description:
//
//    An application calls this function to inform the encoder that
//    all the data has been transmitted to the encoder.
//
//    Here we flush the lookahead buffer and write the checksum.
//
//  Parameters:
//
//    outputstream : The outputstream the encoder writes to.
//
//  Restrictions:
//
//    This function must follow a call to startCompressedStream.
//
//    An application should use the same outputstream supplied to
//    startCompressedStream.
//
Procedure TDeflateEncoder.endCompressedStream (outputstream : TDeflateOutputStream) ;
  Var
    Count : Cardinal ;
  Begin
  if Not compression_in_progress Then
    Raise EDeflateError.Create ('Compression Not Underway') ;

  // Compress anything that remains in the lookahead buffer.
  count := LOOKAHEADMASK And (LOOKAHEADSIZE - lookahead_output_position + lookahead_input_position) ;
  compressLookaheadData (outputstream, count, true) ;

  // Output the final Adler32 value. We need to byte align this value.
  outputstream.exitBitMode ;
  outputstream.writeBigEndianLong (adler_value) ;

  // Final cleanup.
  compression_in_progress := false ;
  End ;







end.
