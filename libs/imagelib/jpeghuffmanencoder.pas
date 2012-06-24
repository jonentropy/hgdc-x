unit jpeghuffmanencoder;
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
//  Title:  TJpegEncoderHuffmanTable class definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class represents a Huffman Table used for compression
//    by the TJpegEncoder class.
//
interface

Uses
  jpegpvt, jpegoutputstream ;

Type
  TJpegHuffmanEncoder = Class
    private
      // frequencies [n] is the number of times the value "n" needs to
      // be encoded.
      frequencies : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] of Cardinal ;

      // Values used to represent Huffman tables in a JPEG stream
      //  huff_bits [n] is the number of codes of length "n+1"
      //  huff_values is the list of Huffman values sorted in order
      //   of code length.
      huff_bits : Array [0..2 * JPEGMAXHUFFMANCODELENGTH-1] Of Byte ; // 2x needed for encoding only.
      huff_values : Array [0..JPEGMAXNUMBEROFHUFFMANCODES-1] Of Byte ;

      // Values used to encode values.
      //   ehufsi [n] is the number of bits required to code "n"
      //   ehufco [n] is the Huffman code for "n"
      ehufsi : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] Of Byte ;
      ehufco : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] of Word ;

      // The flag is set to true when the Huffman code sizes has been determined.
      // It is cleared when the object is Reset().
      sizes_found : Boolean ;

    Public
      Constructor Create ;
      // ENCODING FUNCTIONS

      // This function resets the table so that the object can be used
      // over again.
      Procedure reset ;

      // This function increases the frequency for a huffman value.
      Procedure incrementFrequency (value : JPEGHUFFMANFALUE) ;

      // This function creates the Huffman codes from the code frequencies.
      Procedure buildTable ;

      // This function returns the Huffman code and code length to encode the
      // specified value.
      Procedure encode (value : JPEGHUFFMANFALUE ; var code : Word ; var size : Byte) ;

      // Returns the number of bytes required to write the table to the output
      // file.
      Function outputSize : Cardinal ;

      // Function to print the table definition to the output stream.
      Procedure printTable (outputstream : TJpegOutputStream) ; 
    End ;



implementation

//
//  Description:
//
//    Class default constructor
//
Constructor TJpegHuffmanEncoder.Create ;
  Begin
  Inherited Create ;
  reset ;
  End ;

//
//  Description:
//
//    After Huffman codes have been generated the object is in a state
//    where it cannot be used to create a new set of code. This function
//    places the object in a state where it can be reused to generate a
//    new set of Huffman Codes.
//

Procedure TJpegHuffmanEncoder.reset ;
  var
    ii : Integer ;
  Begin
  For ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES - 1 Do
    frequencies [ii] := 0 ;

  // We add a dummy Huffman value at the end of the table with a minimumal
  // frequency. Since we create the Huffman codes using the in the frequency
  // table this dummy value will be assigned the longest all one huffman code.
  // This ensures that no Huffman code consists entirely of 1s.
  frequencies [JPEGMAXNUMBEROFHUFFMANCODES] := 1 ;
  sizes_found := false ;
  End ;

//
//  Description:
//
//    This function generates the Huffman Codes using the frequency data. The code
//    generation process is taken directly from the JPEG standard.
//
//    The outputs from this function are the following member variables:
//
//     ehufsi [n] : The length of the Huffman Code for value "n"
//     ehufco [n] : The Huffman Code for value "n"
//     huff_bits [n] : The number of Huffman codes of length "n+1"
//     huff_values [n] : The Huffman Values sorted by code length.
//
//    The first two arrays are used to encode Huffman values. The last two
//    are for writing the table to the output file.
//
//    The code generation process is:
//
//    1. Arrange the Huffman Values into a binary tree so that the most
//       frequently used codes are closest to the root of the tree. At the end
//       of this process the temporary array codesize [n] contains the length
//       of the pure Huffman code for the value "n"
//
//    2. Determine the number of Huffman Codes of for each code length. This
//       step places the number of codes of length "n+1" in huff_bits[].
//
//    3. The JPEG standard only allows Huffman codes of up to 16-bits. If any
//       codes longer than 16-bits were generated in the previous steps then
//       we need to reduce the maximum depth of the tree created in step 1.
//       The input and output to this step is the array huff_bits[] created in
//       the previous step.
//
//    4. Remove the dummy all 1-bit code (See the Reset() function).
//
//    5. Sort the Huffman values in code length order. codesize [n] is the
//       input to this step and huff_values [n] is the output. At this point
//       all the information needed to write the Huffman Table to the output
//       stream has been found.
//
//    6. Determine the code size for each value. At the end of this step
//       the temporary array huffsizes [n] is the Huffman code length for
//       huff_values [n].
//
//    7. Determine the Huffman code for each value. The temporary array
//       huffcodes [n] is the Huffman Code of length huffsizes [n] for
//       the value huff_value [n].
//
//    8. Using huffsizes [] and huffcodes created in steps 6 and 7 create
//       the arrays ehufco [n] and ehufsi [n] giving the Huffman Code and
//       Code Size for n.
//
Procedure TJpegHuffmanEncoder.buildTable ;
  Var
    ii, jj, kk : Integer ;
    tmp : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] Of Cardinal ;
    others : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] Of Integer ;
    v1, v2 : Integer ;
    codesize : Array [0..JPEGMAXNUMBEROFHUFFMANCODES] Of Cardinal ;
    count : Cardinal ;
    huffsizes : Array [0..JPEGMAXNUMBEROFHUFFMANCODES - 1] Of Cardinal ;
    code : Cardinal ;
    si : Cardinal ;
    huffcodes : Array [0..JPEGMAXNUMBEROFHUFFMANCODES-1] of Cardinal ;
  Begin


  // See if we have already calculated the Huffman codes.
  if sizes_found Then
    Exit ;

  // The tmp array is used for validating the integrity of the Huffman code
  // table. We need a temporary array since frequencies [] gets trashed
  // during the code generation process.
  for ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES Do
    Begin
   	tmp [ii] := frequencies [ii] ;
    codesize [ii] := 0 ;
    End ;

  For ii := 0 To 2 * JPEGMAXHUFFMANCODELENGTH-1 Do
    huff_bits [ii] := 0 ;

  // Figure K.1
  // Build the Huffman Code Length Lists
  for ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES Do
    others [ii] := -1 ;

  For ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES-1 Do
    Begin
    huff_values [ii] := 0 ;
    huffsizes [ii] := 0 ;
    huffcodes [ii] := 0 ;
    ehufco [ii] := 0 ;
    ehufsi [ii] := 0 ;
    End ;

  v2 := 1 ;
  while v2 >= 0 Do
    Begin
    // Find the two smallest non-zero values
    v1 := -1 ;
    v2 := -1 ;
    for ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES Do
      Begin
      if frequencies [ii] <> 0 Then
        Begin
        // K.2 says to take the highest value value for v1 and v2
        // in case of a tie. This ensures the dummy value gets
        // the last Huffman code.
        if (v1 < 0) Or (frequencies [ii] <= frequencies [v1]) Then
          Begin
          v2 := v1 ;
          v1 := ii ;
          End
        else if (v2 < 0) Or (frequencies [ii] <= frequencies [v2]) Then
          Begin
          v2 := ii ;
          End ;
        End ;
      End ;

    if v2 >= 0 Then
      Begin
      // Join the two tree nodes.
      frequencies [v1] := frequencies [v1] + frequencies [v2] ;
      frequencies [v2] := 0 ;

      Inc (codesize [v1]) ;
      While others [v1] >= 0 Do
        Begin
        v1 := others [v1] ;
        Inc (codesize [v1]) ;
        End ;

      others [v1] := v2 ;

      Inc (codesize [v2]) ;
      While others [v2] >= 0 Do
        Begin
        v2 := others [v2] ;
        Inc (codesize [v2]) ;
        End ;
      End ;
    End ;

  // Figure K.2
  // Determine the number of codes of length [n]
  for ii := 0 To JPEGMAXNUMBEROFHUFFMANCODES Do
    Begin
    if codesize [ii] <> 0 Then
      Begin
      Inc (huff_bits [codesize [ii] - 1]) ;
      End ;
    End ;

  // Figure K.3
  // Ensure that no code is longer than 16-bits.
  for ii := 2 * JPEGMAXHUFFMANCODELENGTH -  1 Downto JPEGMAXHUFFMANCODELENGTH Do
    Begin
    while huff_bits [ii] <> 0 Do
      Begin
      jj := ii - 1 ;
      Repeat
        Dec (jj) ;
      Until huff_bits [jj] <> 0 ;

      Dec (huff_bits [ii], 2) ;
      Inc (huff_bits [ii - 1]) ;
      Inc (huff_bits [jj + 1], 2) ;
      Dec (huff_bits [jj]) ;
      End ;
    End ;

  // Remove the reserved code from the end of the list.
  for ii := JPEGMAXHUFFMANCODELENGTH - 1 DownTo 0 Do
    Begin
    if huff_bits [ii] <> 0 Then
      Begin
      Dec (huff_bits [ii]) ;
      break ;
      End ;
    End ;

  // Figure K.4
  // Sort the values in order of code length
  kk := 0 ;
  for ii := 1 To 2 * JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    for jj := 0 To JPEGMAXNUMBEROFHUFFMANCODES - 1 Do
      Begin
      if codesize [jj] = ii Then
        Begin
        huff_values [kk] := jj ;
        Inc (kk) ;
        End ;
      End ;
    End ;

  // Section C.2 Figure C.1
  // Convert the array "huff_bits" containing the count of codes for each
  // length 1..16 into an array containing the length for each code.
  kk := 0 ;
  for ii := 0 To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
     for jj := 0 To huff_bits [ii] - 1 Do
       Begin
       huffsizes [kk] := ii + 1 ;
       Inc (kk) ;
       End ;
     huffsizes [kk] := 0 ;
    End ;

  // Section C.2 Figure C.2
  // Calculate the Huffman code for each Huffman value.
  Code := 0 ;
  kk := 0 ;
  si := huffsizes [0] ;
  While huffsizes [kk] <> 0  Do
    Begin
    While huffsizes [kk] = si Do
     Begin
     huffcodes [kk] := code ;
     Inc (code) ;
     Inc (kk) ;
     End ;
    Inc (si) ;
    code := Code Shl 1 ;
    End ;

  // Section C.2 Figure C.3
  for kk := 0 To JPEGMAXNUMBEROFHUFFMANCODES - 1 Do
    Begin
    if huffsizes [kk] <> 0 Then
      Begin
      ii := huff_values [kk] ;
      ehufco [ii] := huffcodes [kk] ;
      ehufsi [ii] := huffsizes [kk] ;
      End ;
    End ;

  sizes_found := true ;
  End ;


//
//  Description:
//
//    This function writes the Huffman table data to the output stream in the
//    format specified by the JPEG standard.
//
//  Parameters:
//    encoder:    The JpegEncoder that defines the output stream.
//
Procedure TJpegHuffmanEncoder.printTable (outputstream : TJpegOutputStream) ;
  var
    ii : Cardinal ;
    count : Cardinal ;
    data : Byte ;
  Begin
  // We need this declaration here because MSVC++ does not support
  // standard scoping in for statements.

  // B.2.4.2
  count := 0 ; // Number of codes in the table.

  // Write 16 1-byte values containing the count of codes for
  // each possible Huffman code length and count the number of
  // codes used.
  for ii := 0 To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    Inc (count, huff_bits [ii]) ;
    data := huff_bits [ii] ;
    outputstream.writeByte (data) ;
    End ;

  // Write the variable length part of the table, the Huffman values
  // sorted by Huffman Code.
  for ii := 0 To count - 1 Do
    Begin
    data := huff_values [ii] ;
    outputstream.writeByte (data) ;
    End ;
  End ;
//
//  Description:
//
//    This function determines the size of the Huffman table when it is
//    written to a DHT marker. This function is used to calculate the
//    2-byte marker length that comes right after the FF-DHT sequence in
//    the output stream. Therefore we need to find the length before we
//    actually write the table.
//
Function TJpegHuffmanEncoder.outputSize : Cardinal ;
  Var
    count : Cardinal ;
    ii : Cardinal ;
  Begin
  count := 0 ;
  for ii := 0 To JPEGMAXHUFFMANCODELENGTH - 1 Do
    Begin
    Inc (count, huff_bits [ii]) ;
    End ;

  // 1 byte for each value + one byte for each of 16 code lengths +
  // 1 byte for the table class and ID.
  Result := count + JPEGMAXHUFFMANCODELENGTH + 1 ;
  End ;


//
//  Description:
//
//    Function to increment the frequency for a value.
//
//  Parameters:
//    value:  The value whose frequency is to be incremented
//

Procedure TJpegHuffmanEncoder.incrementFrequency (value : JPEGHUFFMANFALUE) ;
  Begin
  // Once the Huffman codes have been generated for this object, the reset()
  // function must be called before we can gather data again.
  Inc (frequencies [value]) ;
  End ;

//
//  Description:
//
//    This function returns the Huffman Code and Code Size for a given value.
//
//  Parameters:
//    value:  The value to encode
//    code:   The Huffman Code
//    size:   The Huffman Code Length
//
Procedure TJpegHuffmanEncoder.encode (value : JPEGHUFFMANFALUE ;
                                      var code : Word ;
                                      var size : Byte) ;
  Begin
  code := ehufco [value] ;
  size := ehufsi [value] ;
  End ;



end.
