unit inputbytestream ;
//
// Copyright (c) 1999, 2001, 2005 Colosseum Builders, Inc.
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
//  Title:  InputByteStream definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This abstract class defines byte level input used by image decoders.
//

interface

Uses sysutils ;

type
  TInputByteStream = class
    private
    protected
      current_byte : PCHAR ;
      buffer_limit : PCHAR ;
      bit_position : Integer ;

      in_bit_mode : Boolean ;

      Procedure fillBuffer ; Virtual ; Abstract ;
      Function endReached : Boolean ; Virtual ; Abstract ;
    public
      Constructor Create ;
      Function getByte : Byte ;
      Function getBigEndianWord : Word ;
      Function getLittleEndianWord : Word ;
      Function getBigEndianLong : LongWord ;
      Function getLittleEndianLong : Longword ;
      Function read (var buffer ; count : Cardinal) : Integer ;
      Function moreData : Boolean ;
      Procedure enterBitMode (initialposition : Integer) ;
      Procedure ExitBitMode ; Virtual ; Abstract ;
      Procedure AdvanceCurrentByte ;
      End ;

  EStreamError = class (Exception) ;

implementation

Uses systemspecific ;

Constructor TInputByteStream.Create ;
  Begin
  Inherited Create ;
  bit_position := -1 ;
  current_byte := Nil ;
  buffer_limit := Nil ;
  in_bit_mode := False ;
  End ;

Function TInputByteStream.moreData : Boolean ;
  Begin
  Result := (current_byte < buffer_limit) Or Not endReached ;
  End ;

Procedure TInputByteStream.enterBitMode (initialposition : Integer) ;
  Begin
  if Not in_bit_mode  Then
    Begin
    in_bit_mode := true ;
    bit_position := initialposition ;
    end ;
  If current_byte >= buffer_limit Then
    FillBuffer ;
  End ;

//
//  Description:
//
//    The function returns the next byte in the input stream.
//
//  Return Value:
//
//   The next byte.
//
Function TInputByteStream.getByte : Byte ;
  Begin
  if in_bit_mode  Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;

  if current_byte = buffer_limit Then
    fillBuffer ;

  result := Ord (current_byte^) ;
  Inc (current_byte) ;
  End ;
//
//  Description:
//
//    The function returns the next 2-byte integer in the input stream interpreted
//    as a big endian value.
//
//    This function may only be called when in byte input mode.
//
//  Return Value:
//
//   The next 2-byte integer converted from big-endian to system format.
//
Function TInputByteStream.getBigEndianWord : Word ;
 Begin
  if in_bit_mode Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;

  read (Result, sizeof (Result)) ;
  result := BigEndianWordToSystem (result) ;
  End ;
//
//  Description:
//
//    The function returns the next 2-byte integer in the input stream interpreted
//    as a little endian value.
//
//    This function may only be called when in byte input mode.
//
//  Return Value:
//
//   The next 2-byte integer converted from little-endian to system format.
//
Function TInputByteStream.getLittleEndianWord : Word ;
  Begin
  if in_bit_mode Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;
  read (result, sizeof (result)) ;
  End ;
//
//  Description:
//
//    The function returns the next 4-byte integer in the input stream interpreted
//    as a big endian value.
//
//    This function may only be called when in byte input mode.
//
//  Return Value:
//
//   The next 4-byte integer converted from big-endian to system format..
//
Function TInputByteStream.getBigEndianLong : Cardinal ;
  Begin
  if in_bit_mode Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;
  read (Result, sizeof (result)) ;
  result := BigEndianLongToSystem (result) ;
  End ;
//
//  Description:
//
//    The function returns the next 4-byte integer in the input stream interpreted
//    as a little endian value.
//
//    This function may only be called when in byte input mode.
//
//  Return Value:
//
//   The next 4-byte integer converted from little-endian to system format..
//
Function TInputByteStream.getLittleEndianLong : Cardinal ;
  Begin
  if in_bit_mode Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;
  read (Result, sizeof (result)) ;
  result := BigEndianLongToSystem (result) ;
  End ;
//
//  Description:
//
//    The function reads a block from the input stream.
//
//  Parameters:
//
//    buffer: The input buffer
//    count:  The number of bytes to read
//
//  Return Value:
//
//   The number of bytes read.
//
Function TInputByteStream.read (var buffer ; count : Cardinal) : Integer ;
  Var
    remaining : LongInt ;
    dest : PCHAR ;
    ii : LongInt ;
  begin
  if in_bit_mode Then
    raise EStreamError.Create ('Attempt to read bytes while stream is in bit mode') ;

  remaining := buffer_limit - current_byte ;
  if (count <= remaining) Then
    Begin
    // We can respond to this request using the data already in the buffer.
    Dest := @buffer ;
    For ii := 0 To count - 1 Do
      Begin
      Dest [ii] := current_byte^ ;
      Inc (current_byte) ;
      End ;
    result := count ;
    End
  else
    begin
    // Consume all of the characters already in the buffer.
    dest := @buffer ;
    result := remaining ;
    For ii := 0 To remaining - 1 Do
      dest [ii] := current_byte [ii] ;
    Inc (dest, remaining) ;
    Dec (count, remaining) ;
    // Refill the buffer and continue
    while (count > 0) And moreData Do
      Begin
      fillBuffer ;
      remaining := buffer_limit - current_byte ;
      if (count <= remaining) Then
        Begin
        // We have read enough data to handle this request.
        For ii := 0 To Count - 1 Do
          dest [ii] := current_byte [ii] ;
        Inc (current_byte, count) ;
        Inc (result, count) ;
        Exit ;
        End
      else
        Begin
        // The new buffer does not have enough data to
        // handle this request.
        Inc (result, remaining) ;
        For ii := 0 To remaining - 1 Do
          dest [ii] := current_byte [ii] ;
        Inc (dest, remaining) ;
        Dec (count, remaining) ;
        End ;
      End ;
    End
  End ;

Procedure TInputByteStream.AdvanceCurrentByte ;
  Begin
  Inc (current_byte) ;
  if current_byte = buffer_limit Then
    fillBuffer ;
  End ;

End.


