unit inflatehuffmandecoder;
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
//  Title:  InflateHuffmanDecoder definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class handles Huffman decoding for inflate.
//

interface

Uses
  InflateInputStream ;

Type
  TInflateHuffmanDecoder = Class
    Private
      // Maximum Huffman code value of length N
      maxcode : Array of Integer ;
      // Minimum Huffman code value of length N
      mincode : Array of Cardinal ;
      // Index into "values" for minimum code of length N
      valptr : Array of Cardinal ;
      // Huffman values
      huff_values : Array of Cardinal ;

      value_count : Cardinal ;
      max_code_size : Cardinal ;
    public
      // DECODING FUNCTIONS
      Procedure makeTable (maxcodesize : Cardinal ;
                           valuecount : Cardinal ;
                           codelengths : Array of Cardinal) ;


      // This is a debugging function that writes the Huffman table
      // to the console.
      Procedure Dump ;
      Function Decode (inputstream : TInflateInputStream) : Integer ;
    End ;

implementation

const
  BADCODE = $FFFFFFF ;

//
//  Descrition:
//
//    Converts a bit string to character string.
//
//  Parameters:
//      vv : The bit string to convert
//      ll : The length of the bit string in bits.
//
//  Return Value:
//
//     The bit string converted to a sequence of '0' and '1' characters.
//
Function Binary (vv, ll : Cardinal) : String ;
  Var
    ii : Cardinal ;
  Begin
  SetLength (Result, ll) ;

  For II := 1 To ll Do
    Begin
    If (vv And (1 Shl (ll - ii))) <> 0 Then
      Result [ii] := '1'
    Else
      Result [ii] := '0' ;
    End ;
  End ;

//
//  Description:
//
//    This function creates a Huffman table from an array of code lengths.
//
//  Parameters:
//    codelengths: Array of code lengths [0..valuecount-1]
//
//    codelengths [n] is the length of the Huffman code for the value n.
//
Procedure TInflateHuffmanDecoder.makeTable (
                           maxcodesize : Cardinal ;
                           valuecount : Cardinal ;
                           codelengths : Array of Cardinal) ;
  Var
    II, JJ : Cardinal ;
    huffsizes, huffcodes : Array Of Cardinal ;
    tmp : Cardinal ;
    lastlen : Cardinal ;
    code : Cardinal ;
    last : Cardinal ;
  Begin
  SetLength (maxcode, maxcodesize) ;
  SetLength (mincode, maxcodesize) ;
  SetLength (valptr, maxcodesize) ;
  SetLength (huff_values, valuecount) ;
  max_code_size := maxcodesize ;

  value_count := valuecount ;
  SetLength (huffsizes, valuecount + 1) ;
  // Set up arrays to associate codes with code lengths. We have to do a copy
  // because this function can be called using fixed Huffman codes.
  for ii := 0 To valuecount - 1 Do
    Begin
    huff_values [ii] := ii ;
    huffsizes [ii] := codelengths [ii] ;
    End ;

  // Sort the values. Primary key is code size. Secondary key is value.
  For ii := 0 To valuecount - 2 Do
    Begin
    for jj := ii + 1 To valuecount - 1 Do
      Begin
      if (huffsizes [jj] < huffsizes [ii])
          Or ((huffsizes [jj] = huffsizes [ii])
              And (huff_values [jj] < huff_values [ii])) Then
        Begin
        tmp := huffsizes [jj] ;
        huffsizes [jj] := huffsizes [ii] ;
        huffsizes [ii] := tmp ;
        tmp := huff_values [jj] ;
        huff_values [jj] := huff_values [ii] ;
        huff_values [ii] := tmp ;
        End ;
      End ;
    End ;

  // These values in these arrays correspond to the elements of the
  // "values" array. The Huffman code for values [N] is huffcodes [N]
  // and the length of the code is huffsizes [N].

  SetLength (huffcodes, valuecount) ;
  lastlen := 0 ;
  code := 0 ;
  for ii := 0 To valuecount - 1 Do
    Begin
    while (lastlen <> huffsizes [ii]) Do
      Begin
      Inc (lastlen) ;
      code := code Shl 1 ;
      End ;

    if (lastlen <> 0) Then
      Begin
      huffcodes [ii] := code ;
      Inc (code) ;
      End ;
    End ;

  // mincode [n] : The smallest Huffman code of length n + 1.
  // maxcode [n] : The largest Huffman code of length n + 1.
  // valptr [n] : Index into the values array. First value with a code
  //                    of length n + 1.
  for ii := 0 To max_code_size - 1 Do
    Begin
    valptr [ii] := 0 ;
    mincode [ii] := BADCODE ;
    maxcode [ii] := -1 ;
    End ;

  last := 0 ;
  for ii := 0 To valuecount - 1 Do
    Begin
    if (last <> huffsizes [ii]) Then
      Begin
      last := huffsizes [ii] ;
      valptr [last-1] := ii ;
      mincode [last-1] := huffcodes [ii] ;
      End ;
    if (last <> 0) Then
      maxcode [last-1] := huffcodes [ii] ;
    End ;
  End ;

//
//  Description:
//
//    This function decodes the next Huffman-encoded value in the input
//    stream.
//
//  Parameters:
//    decoder:  The PNG Decoder object.
//
Function TInflateHuffmanDecoder.decode (inputstream : TInflateInputStream) : Integer ;
  var
    code : Cardinal ;
    codelength : Cardinal ; // Code length minus 1
    offset, index : Cardinal ;
  Begin
  // We can't cheat by reading multiple bits here because we are reversing the
  // bits as they appear in the input stream.

  code := inputstream.nextBit () ;

  // Here we are taking advantage of the fact that 1 bits are used as
  // a prefix to the longer codes.
  codelength := 0 ;
  While (Integer (code) > maxcode [codelength]) Do
    Begin
    code := (code Shl 1) Or inputstream.nextBit ;
    Inc (codelength) ;
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
//    table to cout.
//
Procedure TInflateHuffmanDecoder.dump ;
  Var
    ii : Integer ;
    firstcode, lastcode : Cardinal ;
  Begin
  lastcode := 0 ; // Get rid of warning.
  WriteLn ('   Code Values (', value_count, '): ') ;
  for ii := 0 To value_count - 1 Do
    Begin
    Write (' (', ii, ') ', huff_values [ii]) ;
    If ((ii + 1) Mod 8 = 0) Then
      WriteLn ('') ;
    End ;
  WriteLn ('') ;
  WriteLn ('Length' + chr (9) + chr (9) + 'Mincode' + chr (9) + chr (9)
       + 'Maxcode' + chr (9) + chr (9) + 'Valptr') ;
  WriteLn ('-------------------------------------------------------') ;

  firstcode := 0 ;
  While mincode [firstcode] = BADCODE Do
    Inc (firstcode) ;

  for ii := firstcode + 1 To max_code_size - 1 Do
    Begin
    if (mincode [ii] <> BADCODE) Then
      lastcode := ii + 1 ;
    End ;

  for ii := firstcode To lastcode - 1 Do
    Begin
    if (mincode [ii] <> BADCODE) Then
      Begin
      WriteLn ((ii + 1), chr (9) + chr (9) + Binary (mincode [ii], ii + 1)
               + chr (9) + chr (9) + Binary (maxcode [ii], ii + 1) + chr (9) + chr (9),
               valptr [ii]) ;
      End ;
    End ;
  End ;
end.
