unit inflateinputstream;
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
//  Title:  InflateInputStream definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class defines input required by the Inflate decoders.
//

interface

Uses inputbytestream ;

Type
  TInflateInputStream = class (TInputByteStream)
    Public
      Function NextBit : Cardinal ;
      Function GetBits (count : Cardinal) : Cardinal ;
      Procedure ExitBitMode ; Override ;
    End ;

implementation

Const
  CHAR_BIT = 8 ;

//
//  Description:
//
//    Retrieves the next bit in the input stream. The bits are extracted low to high.
//
//  Return Value:
//
//    The next bit in the input stream.
//
Function TInflateInputStream.nextBit : Cardinal ;
  Const
    masks : Array [0..CHAR_BIT - 1] Of Integer = ($1, $2, $4, $8, $10, $20, $40, $80) ;
  Begin
  if (bit_position >= CHAR_BIT) Then
    Begin
    Inc (current_byte) ;
    bit_position := 0 ;
    if (current_byte >= buffer_limit) Then
      Begin
      fillBuffer ;
      if (Not moreData) Then
        Raise EStreamError.Create ('Premature end of stream') ;
      End ;
    End ;
  // Extract the bit value
  if ((masks [bit_position] And Ord (current_byte^)) <> 0) Then
    result := 1
  else
    result := 0 ;
  Inc (bit_position) ;
  End ;

//
//  Description:
//
//    This function extracts a number of bits from the input stream. The bits
//    are extracted from low to high.
//
//  Parameter:
//
//    count: The number of bits to read.
//
//  Return Value:
//
//    'count' bits from the input stream.
//
Function TInflateInputStream.getBits (count : Cardinal) : Cardinal ;
  Const
    Masks : Array [0..CHAR_BIT] of Cardinal = ($0, $1, $3, $7, $F, $1F, $3F, $7F, $FF) ;
  Var
    bitswritten : Cardinal ;
  Begin
  if count <= Cardinal (CHAR_BIT - bit_position) Then
    Begin
    // Easy case. We have already enough bits to process the request.
    result := (Ord (current_byte^) Shr bit_position) And masks [count] ;
    Inc (bit_position, count) ;
    End
  else
    Begin
    result := 0 ;
    bitswritten := 0 ;
    // Save the remaining bits from the current byte.
    if (bit_position < CHAR_BIT) Then
      Begin
      bitswritten := CHAR_BIT - bit_position ;
      result := (Ord (current_byte^) Shr bit_position) And masks [bitswritten] ;
      Dec (count, bitswritten) ;
      End ;

    while (count > 0) Do
      Begin
      // Move to the next byte.
      Inc (current_byte) ;
      bit_position := 0 ;
      if (current_byte >= buffer_limit) Then
        Begin
        fillBuffer ;
        if (Not moreData) Then
          raise EStreamError.Create ('Premature end of file') ;
        End ;
      if (count > CHAR_BIT) Then
        Begin
        // We need the all of the new byte for the request.
        Dec (count, CHAR_BIT) ;
        result := Result Or (Ord (current_byte^) Shl bitswritten) ;
        bit_position := 0 ;
        Inc (bitswritten, CHAR_BIT) ;
        End
      Else
        Begin
        result := Result Or (Ord (current_byte^) And masks [count]) Shl bitswritten ;
        Inc (bit_position, count) ;
        count := 0 ; // All Done
        End ;
      End ;
    End ;
  End ;

Procedure TInflateInputStream.exitBitMode ;
  Begin
  if (bit_position >= 0) Then
    Inc (current_byte) ;
  in_bit_mode := false ;
  End ;


end.
