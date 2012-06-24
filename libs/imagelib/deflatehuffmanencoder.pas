unit deflatehuffmanencoder;
//
//  Title:  Deflate Huffman Encoding Class
//
//  Copyright 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    May 25, 2001
//
//  Version: 1
//
//  Description:
//
//    This is a huffman encoding class designed for Deflate compression.
//
interface
  Type
    TValueRange = 0..285 ;

    TDeflateHuffmanEncoder = Class
    Private
      // frequencies [n] is the number of times the value "n" needs to
      // be encoded.
      frequencies : Array [TValueRange] of Cardinal ;

      // Values used to encode values.
      //   ehufsi [n] is the number of bits required to code "n"
      //   ehufco [n] is the Huffman code for "n"
      ehufsi : Array [TValueRange] of Integer ;
      ehufco : Array [TValueRange] of Cardinal ;

    public
      Constructor Create ;
      // This function resets the table so that the object can be used
      // over again.
      Procedure reset ;

      // This function increases the frequency for a huffman value.
      Procedure incrementFrequency (value : Cardinal) ;

      // This function creates the Huffman codes from the code frequencies.
      Procedure buildTable (maxcodelength : Cardinal) ;

      // This function returns the Huffman code and code length to encode the
      // specified value.
      Procedure encode (value : Cardinal ; var code : cardinal ; Var size : Cardinal) ;
      End ;



implementation

//
//  Description:
//
//    Default Class Constructor
//
Constructor TDeflateHuffmanEncoder.Create ;
  Begin
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
Procedure TDeflateHuffmanEncoder.reset ;
  Var
    ii : Cardinal ;
  Begin
  For ii := Low (ehufco) To High (ehufco) Do
    ehufco [ii] := 0 ;
  For ii := Low (ehufsi) To High (ehufsi) Do
    ehufsi [ii] := 0 ;
  For ii := Low (frequencies) To High (frequencies) Do
    frequencies [ii] := 0 ;
  End ;

//
//  Description:
//
//    Function to increment the frequency for a value.
//
//  Parameters:
//    value:  The value to increase the usage frequency of.
//
Procedure TDeflateHuffmanEncoder.incrementFrequency (value : Cardinal) ;
  Begin
  Inc (frequencies [value]) ;
  End ;

//
//  Description:
//
//    This function generates the Huffman Codes using the frequency data.
//
//    The outputs from this function are the following member variables:
//
//     ehufsi [n] : The length of the Huffman Code for value "n"
//     ehufco [n] : The Huffman Code for value "n"
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
//       step places the number of codes of length "n+1" in huffbits[].
//
//    3. The Default places limites on the size of Huffman Codes. If
//       codes longer that the specified limits were generated in the
//       previous steps then we need to reduce the maximum depth of the
//       tree created in step 1. The input and output to this step is the
//       array huffbits[] created in the previous step.
//
//    4. Sort the Huffman values in code length order. codesize [n] is the
//       input to this step and huffvalues [n] is the output. At this point
//       all the information needed to write the Huffman Table to the output
//       stream has been found.
//
//    5. Determine the code size for each value. At the end of this step
//       the temporary array huffsizes [n] is the Huffman code length for
//       huffvalues [n].
//
//    6. Determine the Huffman code for each value. The temporary array
//       huffcodes [n] is the Huffman Code of length huffsizes [n] for
//       the value huffvalue [n].
//
//    7. Using huffsizes [] and huffcodes created in steps 6 and 7 create
//       the arrays ehufco [n] and ehufsi [n] giving the Huffman Code and
//       Code Size for n.
//
//  Parameters:
//    maxcodelength:  The maximum code length to generate
//
Procedure TDeflateHuffmanEncoder.buildTable (maxcodelength : Cardinal) ;
  Var
    codestoolong : Boolean ;
    tmpfrequencies : Array [TValueRange] of Cardinal ;
    ii, jj, kk : Integer ;
    others : Array [TValueRange] of Integer ;
    codesize : Array [TValueRange] of Cardinal ;
    done : Boolean ;
    v1, v2 : Integer ;
    huffbits : Array [0..29] of Integer ;
    count : Cardinal ;
    huffvalues : Array [TValueRange] of Cardinal ;
    huffsizes : Array [TValueRange] Of Cardinal ;
    value, code, size, bit : Cardinal ;
    huffcodes : Array [TValueRange] Of Cardinal ;
    si : Cardinal ;
  Begin
  codestoolong := false ;

  // The tmp array is used for validating the integrity of the Huffman code
  // table. We need a temporary array since frequencies [] gets trashed
  // during the code generation process.
  For ii := Low (tmpfrequencies) To High (tmpfrequencies) Do
    tmpfrequencies [ii] := frequencies [ii] ;

  // Build the Huffman Code Length Lists
  For ii := Low (others) To High (others) Do
    others [ii] := -1 ;

  For ii := Low (codesize) To High (codesize) Do
    codesize [ii] := 0 ;

  done := false ;
  while Not done Do
    Begin
    // Find the two smallest non-zero values
    v1 := -1 ;
    v2 := -1 ;
    for ii := Low (frequencies) To High (frequencies) Do
      Begin
      if frequencies [ii] <> 0 Then
        Begin
        if (v1 < 0) Or (frequencies [ii] <= frequencies [v1]) Then
          Begin
          v2 := v1 ;
          v1 := ii ;
          End
        else if (v2 < 0) Or (frequencies [ii] <= frequencies [v2]) Then
          v2 := ii ;
        End ;
      End ;
    if v2 < 0 Then
      Begin
      if v1 < 0 Then
        Exit ; // No codes defined

      if codesize [v1] = 0 Then
        codesize [v1] := 1 ;  // Only one code defined
      done := true ;
      End 
    Else
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
  // Determine the number of codes of length [n]
  For ii := 0 to 2 * maxcodelength - 1 Do
    huffbits [ii] := 0 ;

  For ii := Low (codesize) To High (codesize) Do
    Begin
    if codesize [ii] <> 0 Then
      Begin
      Inc (huffbits [codesize [ii] - 1]) ;
      End ;
    End ;

  // Ensure that no code is longer than maxlength.
  For ii := 2 * maxcodelength -  1 Downto maxcodelength Do
    Begin
    while huffbits [ii] <> 0 Do
      Begin
      codestoolong := true ; // Remember that we had to reorder the tree

      jj := ii - 1 ;
      Repeat
        Dec (jj) ;
      Until huffbits [jj] <> 0 ;

      Dec (huffbits [ii], 2) ;
      Inc (huffbits [ii - 1]) ;
      Inc (huffbits [jj + 1], 2) ;
      Dec (huffbits [jj]) ;
      End ;
    End ;

  // Sort the values in order of code length.
  // What might not be clear is that codesize [n] is the length
  // of the Huffman code for n before it was shortened to maxcodelength.
  // That the values in codesize may be too large does not matter. The
  // ordering of the values by code size remains correct. As soon as this
  // step is complete, the codesize[] array is no longer used anyway.
  for II := Low (huffvalues) To High (huffvalues) Do
    huffvalues [ii] := 0 ;

  kk := 0 ;
  for ii := 1 To 2 * maxcodelength - 1 Do
    Begin
    For jj := Low (codesize) To High (codesize) Do
      Begin
      if codesize [jj] = ii Then
        Begin
        huffvalues [kk] := jj ;
        Inc (kk) ;
        End ;
      End ;
    End ;

  // Convert the array "huffbits" containing the count of codes for each
  // length 1..maxcodelength into an array containing the length for each code.
  for ii := Low (huffsizes) To High (huffsizes) Do
    huffsizes [ii] := 0 ;

  ii := 0 ;
  kk := 0 ;
  While (ii < maxcodelength) And (kk <= High (huffsizes)) Do
    Begin
    for jj := 0 To huffbits [ii] - 1 Do
      Begin
      huffsizes [kk] := ii + 1 ;
      Inc (kk) ;
      End ;
    Inc (ii) ;
    End ;

  // Calculate the Huffman code for each Huffman value.
  code := 0 ;
  kk := 0 ;
  si := huffsizes [0] ;
  While (kk <= High (huffcodes)) And (huffsizes [kk] <> 0) Do
    Begin
    While (kk <= High (huffcodes)) And (huffsizes [kk] = si) Do
      Begin
      huffcodes [kk] := code ;
      Inc (code) ;
      Inc (kk) ;
      End ;
    Inc (si) ;
    code := code Shl 1 ;
    End ;

  for kk := Low (huffsizes) To High (huffsizes) Do
    Begin
    if huffsizes [kk] <> 0 Then
      Begin
      ii := huffvalues [kk] ;
      ehufco [ii] := huffcodes [kk] ;
      ehufsi [ii] := huffsizes [kk] ;
      End ;
    End ;


  // If the pure Huffman code generation created codes longer than the
  // maximum the it is possible that the order got screwed up. Such a
  // situation could occur if the maximum code length is 15 and during the
  // pure process we the value 150 got assigned a length of 13, 100 a length
  // of 15 and 200 a length of 17. During the process of reducing the code
  // length for 200 it is possible that 150 would have its code length
  // increased to 14 and 100 would have its code length reduced to 14.
  // Unfortunately the Huffman codes would be assigned using the old
  // order so that 150 would get assigned a smaller Huffman code than
  // 100.  Here we fix that and ensure that if ehufsi [ii] == ehufsi [jj]
  //  and ii < jj then ehufco [ii] < ehufco [jj].
  if codestoolong Then
    Begin
    for ii := 0 To High (ehufco) - 1 Do
      Begin
      For jj := ii + 1 To High (ehufco) Do
        Begin
        if (ehufsi [ii] = ehufsi [jj]) And (ehufco [ii] > ehufco [jj]) Then
          Begin
          // The codes got out of order so switch them.
          kk := ehufco [jj] ;
          ehufco [jj] := ehufco [ii] ;
          ehufco [ii] := kk ;
          End ;
        End ;
      End ;
    End ;

  // If the decoder reads from the least significant bit to the most
  // significant bit, the codes need to be reversed.

  for ii := Low (ehufco) To High (ehufco) Do
    Begin
    value := 0 ;
    code := ehufco [ii] ;
    size := ehufsi [ii] ;
    for jj := 0 To ehufsi [ii] - 1 Do
      Begin
      bit := (code And (1 Shl jj)) Shr jj ;
      value := value Or (bit Shl (size - jj - 1)) ;
      End ;
    ehufco [ii] := value ;
    End ;
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
Procedure TDeflateHuffmanEncoder.encode (value : Cardinal ;
                                        var code : Cardinal ;
                                        var size : Cardinal) ;
  Begin
  code := ehufco [value] ;
  size := ehufsi [value] ;
  End ;

end.
