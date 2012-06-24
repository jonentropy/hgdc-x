unit jpeginputstream ;
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
interface

Uses inputbytestream ;

type
  TJpegInputStream = Class (TInputByteStream)
    public
      function nextBit : Integer ;
      Function getBits (count : Cardinal) : Integer ;
      Procedure exitBitMode ;
      function tellg : LongInt ; Virtual ; Abstract ;
      procedure seekg (position : LongInt) ; Virtual ; Abstract ;
    End ;

Implementation

const
  BITSPERBYTE = 8 ;

Function TJpegInputStream.nextBit : Integer ;
  const
    bits : Array [0..7] of Cardinal = ($1, $2, $4, $8, $10, $20, $40, $80) ;
  Begin
  if (bit_position = 0) Then
    Begin
    // The 2-byte sequence FF00 represents the 1-byte compressed value FF. In
    // bit mode, we initially assume FF is followed by a 00.
    If (current_byte^ = Chr ($FF)) Then
      Begin
      Inc (current_byte) ;
      If (current_byte >= buffer_limit) Then
        Begin
        fillBuffer ;
        If (Not moreData) Then
          Raise EStreamError.Create ('Premature end of file') ;
        End ;
      If (current_byte^ <> Chr (0)) Then
        Raise EStreamError.Create ('Invalid Marker in Compressed Data')
      End ;
    Inc (current_byte) ;
    bit_position := BITSPERBYTE ;
    If (current_byte >= buffer_limit) Then
      Begin
      fillBuffer ;
      If (Not moreData) Then
        Raise EStreamError.Create ('Premature end of stream') ;
      End ;
    End ;

  // Extract the bit value
  Dec (bit_position) ;
  If ((bits [bit_position] And Ord (current_byte^)) <> 0) Then
    result := 1
  Else
    result := 0 ;
  End ;

Function TJpegInputStream.getBits (count : Cardinal) : Integer ;
  Const
    masks : Array [0..8] of Cardinal = (0, 1, 3, 7, 15, 31, 63, 127, 255) ;
    highmask : Array [0..8] of Cardinal = (0, 128, 192, 224, 240, 248, 252, 254, 255) ;
  Begin
  If (count <= bit_position) Then
    Begin
    // Easy case. We have already enough bits to process the request.
    Dec (bit_position, count) ;
    result := (Ord (current_byte^) Shr bit_position) And masks [count] ;
    End
  Else
    Begin
    result := 0 ;
    // Save the remaining bits from the current byte.
    If (bit_position > 0) Then
      Begin
      result := (Ord (current_byte^) And masks [bit_position]) Shl (count - bit_position) ;
      Dec (count, bit_position) ;
      End ;

    While (count > 0) Do
      Begin
      // Move to the next byte.
      If (Ord (current_byte^) = $FF) Then
        Begin
        Inc (current_byte) ;
        If (current_byte >= buffer_limit) Then
          Begin
          fillBuffer ;
          If (Not moreData) Then
            Raise EStreamError.Create ('Premature end of file') ;
          End ;
        If (Ord (current_byte^) <> 0) Then
          Raise EStreamError.Create ('Invalid Marker in Compressed Stream') ;
        End ;
      Inc (current_byte) ;

      bit_position := BITSPERBYTE ;
      If (current_byte >= buffer_limit) THen
        Begin
        fillBuffer ;
        If (Not moreData) Then
          Raise EStreamError.Create ('Premature end of file') ;
        End ;
      // Process markers.

      If (count >= BITSPERBYTE) Then
        Begin
        // We need the all of the new byte for the request.
        Dec (count, BITSPERBYTE) ;
        result :=  Result OR (Ord (current_byte^) Shl count) ;
        bit_position := 0 ;
        End
      Else
        Begin
        // We only need part of the new byte.
        result := Result Or ((Ord (current_byte^) And highmask [count]) Shr
                  (BITSPERBYTE - count)) ;
        Dec (bit_position, count) ;
        Exit ; // All Done
        End ;
      End ;
    End ;
  End ;

Procedure TJpegInputStream.exitBitMode ;
  Begin
  If in_bit_mode Then
    Begin
    // The FF00 sequence represents the bit value FF. If we are in the
    // middle of this sequence we need to skip two bytes.
    if (Ord (current_byte^) = $FF) Then
      Begin
      Inc (current_byte) ;
      if (current_byte >= buffer_limit) Then
        Begin
        fillBuffer ;
        if (Not moreData) Then
          Raise EStreamError.Create ('Premature end of file') ;
        End ;
      if (Ord (current_byte^) <> 0) Then
        Raise EStreamError.Create ('Invalid Marker in Compressed Stream') ;
      Inc (current_byte) ;
      End
    else
      Begin
      Inc (current_byte) ;
      End ;
    in_bit_mode := false ;
    End ;
  End ;
End.
