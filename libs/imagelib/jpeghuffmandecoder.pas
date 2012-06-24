unit jpeghuffmandecoder;

interface

Uses jpegpvt, jpeginputstream, classes ;

type
  TJpegHuffmanDecoder = Class
    Private
      // Maximum Huffman code value of length N
      maxcode : Array [0..JPEGMAXHUFFMANCODELENGTH-1] of Integer ;
      // Minimum Huffman code value of length N
      mincode : Array [0..JPEGMAXHUFFMANCODELENGTH-1] of Integer ;
      // Index into "values" for minimum code of length N
      valptr : Array [0..JPEGMAXHUFFMANCODELENGTH-1] of Cardinal ;
      // Huffman values
      huff_values : Array [0..JPEGMAXNUMBEROFHUFFMANCODES-1] Of Byte ;

      huffcodes : Array [0..JPEGMAXNUMBEROFHUFFMANCODES-1] Of Cardinal ;
      huffsizes : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] Of Cardinal ;

      table_defined : Boolean ;
      minimum_code_length : Cardinal ;
      code_count : Cardinal ;

      // This function builds the structures needed for Huffman
      // decoding after the table data has been read.
      Procedure makeTable (huffbits : Array of Cardinal) ;

    public
      Constructor Create ;

      // DECODING FUNCTIONS

      // Returns true if the table has been defined...in other words,
      // if ReadTable () has completed successfully. This function is
      // called before the table is used to decode a scan to ensure
      // the the image does not reference a Huffman Table that has
      // not been defined.
      Function tableDefined : Boolean ;

      // This function reads a Huffman table from the input stream.
      Function readTable (inputstream : TJpegInputStream) : Cardinal ;

      // Function to decode the next value in the input stream.
      Function decode (inputstream : TJpegInputStream) : Integer ;

      // This is a debugging function that writes the Huffman table
      // to a stream.
      Procedure print ;

    End ;


implementation

uses jpegdecoder, sysutils ;
//
//  Description:
//
//    Class defualt constructor
//

Constructor TJpegHuffmanDecoder.Create ;
  Begin
  table_defined := false ;
  End ;

//
//  Description:
//
//    This function reads a Huffman table from the input stream. A DHT
//    marker can define more than one Huffman table. This function reads
//    just one of those tables.
//
//  Parameters:
//    decoder:  The JPEG decoder that owns the Huffman table
//
//  Return Value:
//    The size of the Huffman table in the input stream
//    (the number of bytes read).
//
Function TJpegHuffmanDecoder.readTable (inputstream : TJpegInputStream) : Cardinal ;
  Var
    // B.2.4.2
    huffbits : Array [0..JPEGMAXHUFFMANCODELENGTH-1] of Cardinal ;
    jj : Integer ;
  Begin

  code_count := 0 ;

  // Read the 16 1-byte length counts and count the number of
  // codes in the table.
  for jj := 0  To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    // These values are called Li in the standard.
    huffbits [jj] := inputstream.getByte ;
    Inc (code_count, huffbits [jj]) ;
    End ;
  if (code_count > JPEGMAXNUMBEROFHUFFMANCODES) Then
    Raise EJpegBadStream.Create ('Huffman count > 256') ;

  // Read the Huffman values.
  for jj := 0 To code_count - 1 Do
    huff_values [jj] := inputstream.getByte ; // These values are called Vi in the standard.

  // Generate the Structures for Huffman Decoding.
  makeTable (huffbits) ;

  table_defined := true ; // This table can now be used.

  Result := JPEGMAXHUFFMANCODELENGTH + code_count ;
  End ;

//
//  Description:
//
//    This function generates the data used for Huffman decoding.
//
//    The implicit outputs are the member variables mincode [n],
//     maxcode [n] and valptr [n]. These are the minimum Huffman Code of
//    length n+1, the maximum Huffman Code of length n+1, and the index
//    into huff_values [] for the first value with a Huffman code of length
//    n+1.
//
//  Parameters:
//    huffbits: The count of Huffman codes of length n+1)
//
Procedure TJpegHuffmanDecoder.makeTable (huffbits : Array of Cardinal) ;
  Var
    // We have to declare the loop indices here because MSVC++ does not
    // handle scoping in for statements correctly.
    ii, jj, kk : Cardinal ;
    code : Word ;
    si : Cardinal ;

  // These values in these arrays correspond to the elements of the
  // "values" array. The Huffman code for values [N] is huffcodes [N]
  // and the length of the code is huffsizes [N].

  Begin
  // Section C.2 Figure C.1
  // Convert the array "huff_bits" containing the count of codes
  // for each length 1..16 into an array containing the length for each
  // code.
  kk := 0 ;
  For ii := 0 To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    if huffbits [ii] <> 0 Then
      Begin
      for jj := 0 To huffbits [ii] - 1 Do
        Begin
        huffsizes [kk] := ii + 1 ;
        Inc (kk) ;
        End ;
      End ;
    huffsizes [kk] := 0 ;
    End ;

  // Section C.2 Figure C.2
  // Calculate the Huffman code for each Huffman value.
  code := 0 ;
  si := huffsizes [0] ;
  kk := 0 ;
  while huffsizes [kk] <> 0  Do
    Begin
    While huffsizes [kk] = si Do
      Begin
      huffcodes [kk] := code ;
      Inc (code) ;
      Inc (kk) ;
      End ;
    Inc (si) ;
    If huffsizes [kk] <> 0  Then
      code := code Shl 1 ;
    End ;

  // Section F.2.2. Figure F.15
  // Create three arrays.
  // mincode [n] : The smallest Huffman code of length n + 1.
  // maxcode [n] : The largest Huffman code of length n + 1.
  // valptr [n] : Index into the values array. First value with a code
  //                    of length n + 1.
  jj := 0 ;
  for ii :=0 To JPEGMAXHUFFMANCODELENGTH -1 Do
    Begin
    // ii is the index into Huffman code lengths
    // jj is the index into Huffman code values
    if (huffbits [ii] <> 0) Then
      Begin
      // The jj'th Huffman value is the first with a Huffman code
      // of length ii.
      valptr [ii] := Byte (jj) ;
      mincode [ii] := huffcodes [jj] ;
      Inc (jj, huffbits [ii]) ;
      maxcode [ii] := huffcodes [jj - 1] ;
      End
    else
      Begin
      // There are no Huffman codes of length (ii + 1).
      maxcode [ii] := -1 ;
      // An illegal value > maxcode[]
      mincode [ii] := JPEGMAXNUMBEROFHUFFMANCODES + 1 ;
      valptr [ii] := 0 ;
      End ;
    End ;


  for ii := 0 To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    if (huffbits [ii] <> 0) Then
      Begin
      minimum_code_length := ii + 1 ;
      Exit ;
      End ;
    End
  End ;

//
//  Description:
//
//    This function decodes the next Huffman-encoded value in the input
//    stream.
//
//  Parameters:
//    decoder:  The JPEG decoder that owns the Huffman table.
//
Function TJpegHuffmanDecoder.decode (inputstream : TJpegInputStream) : Integer ;
  Var
    code : LongInt ;
    codelength : Integer ; // Called I in the standard.
    offset : Integer ;
    index : Integer ;
  Begin
  // This function decodes the next byte in the input stream using this
  // Huffman table.

  // Section A F.2.2.3 Figure F.16
  code := inputstream.getBits (minimum_code_length) ;

  // Here we are taking advantage of the fact that 1 bits are used as
  // a prefix to the longer codes.
  codelength := minimum_code_length - 1 ;
  While (code > maxcode [codelength]) And (codelength < JPEGMAXHUFFMANCODELENGTH) Do
    Begin
    code := ((code Shl 1) Or inputstream.nextBit ()) ;
    Inc (codelength) ;
    if (codelength >= JPEGMAXHUFFMANCODELENGTH) Then
      Raise EJpegBadStream.Create ('Bad Huffman Code Length') ;
    End ;


  // Now we have a Huffman code of length (codelength + 1) that
  // is somewhere in the range
  // mincode [codelength]..maxcode [codelength].
  // This code is the (offset + 1)'th code of (codelength + 1) ;
  offset := code - mincode [codelength] ;
  // valptr [codelength] is the first code of length (codelength + 1)
  // so now we can look up the value for the Huffman code in the table.
  index := valptr [codelength] + offset ;
  Result := huff_values [index] ;

  End ;

//
//  Description:
//
//    This is a debugging function for writing the contents of the Huffman
//    table to a stream.
//
//  Parameters:
//    strm:  The output stream
//
Procedure TJpegHuffmanDecoder.print ;
  Var
    ii : Integer ;
  Begin
  If Not table_defined Then
    Exit ;

  WriteLn ('   Code Values: ') ;
  for ii := 0 To code_count - 1 Do
    WriteLn (ii, ' ', huffsizes [ii], ' ', huffcodes [ii], ' ', huff_values [ii], ' ') ;
  WriteLn ('') ;
  WriteLn ('Length', chr (9), chr (9), 'Mincode', chr (9), chr (9), 'Maxcode', chr (9), chr (9), 'Valptr') ;
  WriteLn ('-------------------------------------------------------') ;
  for ii := minimum_code_length - 1 To JPEGMAXHUFFMANCODELENGTH-1 Do
    Begin
    WriteLn (ii + 1, chr (9), chr (9), mincode [ii], chr (9), chr (9), maxcode [ii], chr (9), chr (9), valptr [ii]) ;
    End ;
  End ;

//
//  Description:
//
//   This function tells if the Huffman table has been defined
//   by the JPEG input stream.  It is used to detect corrupt
//   streams that have scans that use a Huffman table before
//   it has been defined.
//
//  Return Value:
//
//    true => The table has been defind
//    false => The table has not been defined
//

Function TJpegHuffmanDecoder.tableDefined : Boolean ;
  Begin
  Result := table_defined ;
  End ;

end.
