unit jpegoutputstream;
//
//  Title:  TJpegOutputFileStream Class Definition
//
//  Copyright 2001, Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    July 4, A.D. 2001
//
//  Version: 1
//
//  Description:
//
//    The TJpegOutputStream class defines the attributes required for JPEG
//    output.
//

interface

Uses
  outputbytestream ;

Type
  TJpegOutputStream = Class (TOutputByteStream)
    public
      Procedure outputBits (bits : Integer ; count : Cardinal) ;
      Procedure exitBitMode ;
    End ;

implementation

Const
  CHAR_BIT = 8 ;

//
//  Description:
//
//    This function outputs a single bit to the output stream.
//
//    Individual its are buffered using the bit_buffer and bit_count member
//    variables.
//
//    This function throws an exception if it is called with a zero bit count.
//    We use this for error trapping since no place in the rest of the
//    code should attempt to write zero bits.
//
//  Parameters:
//    bits: The bit string to be output
//    count: The number of bits to output
//
Procedure TJpegOutputStream.outputBits (bits : Integer ; count : Cardinal) ;
  Const
    masks : Array [0..16] Of Integer = (
                    0, 1, 1 Shl 1, 1 Shl 2, 1 Shl 3,
                    1 Shl 4, 1 Shl 5, 1 Shl 6, 1 Shl 7,
                    1 Shl 8, 1 Shl 9, 1 Shl 10, 1 Shl 11,
                    1 Shl 12, 1 Shl 13, 1 Shl 14, 1 Shl 15) ;
  Var
    ii : cardinal ;
  Begin
  for ii := count Downto 1 Do
   Begin
    if (bits And masks [ii]) <> 0 Then
      current_byte^  := Chr (Ord (current_byte^) Or masks [bit_position]) ;
    Dec (bit_position) ;
    if bit_position = 0 Then
      Begin
      if Ord (current_byte^) = $FF Then
        Begin
        // B.1.1.5 In order to ensure that a marker does not occur within
        // compressed data, the 2-byte sequence FF00 is used to represent
        // the 1-byte FF.
        nextByte ;
        current_byte^ := Chr ($00) ;
        End ;
      nextByte ;
      bit_position := CHAR_BIT ;
      End ;
    End ;
  End ;


Procedure TJpegOutputStream.exitBitMode ;
  Begin
  if bit_position <> CHAR_BIT Then
    nextByte ;
  bit_position := -1 ;
  End ;

end.
