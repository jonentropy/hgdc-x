unit outputbytestream;
//
//  Title:  Output Byte Stream Class
//
//  Copyright 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    May 15, 2991
//
//  Version: 1
//
//  Description:
//
//

interface

Type
  TOutputByteStream = Class
    Protected
      current_byte : PChar ; // Current Byte Position
      buffer_limit : PChar ; // Limit of the output byffer
      bit_position : Integer ;
      Procedure flushBuffer ; Virtual ; Abstract ;

    Public
      Constructor Create ;

      Procedure write (buffer : PCHAR ; count : Cardinal) ;
      Procedure writeByte (value : Byte) ;
      Procedure writeBigEndianWord (Value : Word) ;
      Procedure writeBigEndianLong (value : Cardinal) ;

      Procedure enterBitMode (initialposition : Integer) ;
      Procedure nextByte ;

      Function remainingBufferSpace : Cardinal ;
    End ;

implementation

Constructor TOutputByteStream.Create ;
  Begin
  Inherited Create ;
  bit_position := -1 ;
  End ;

//
//  Description:
//
//    Function to write a block of bytes.
//
//  Parameters:
//
//    buffer : The output buffer
//    count : The number of bytes to write
//
Procedure TOutputByteStream.write (buffer : PCHAR ; count : Cardinal) ;
  Var
    ii : Cardinal ;
  Begin

  if buffer_limit < (current_byte + count) Then
    Begin
    // There is enough room for all the data in the buffer.
    For II := 1 To Count Do
      Begin
      current_byte^ := buffer^ ;
      Inc (buffer) ;
      Inc (current_byte) ;
      End ;
    End
  Else
    Begin
    While count > 0 Do
      Begin
      While (current_byte < buffer_limit) And (count > 0) Do
        Begin
        current_byte^ := buffer^ ;
        Inc (current_byte) ;
        Inc (buffer) ;
        Dec (count) ;
        End ;
      If current_byte = buffer_limit Then
        flushBuffer ;
      End ;
    End ;
  End ;

//
//  Description:
//
//    This function writes a single byte to the output stream.
//
//  Parameters:
//
//    value : The byte to write
//
Procedure TOutputByteStream.writeByte (value : Byte) ;
  Begin
  if current_byte >= buffer_limit Then
    flushBuffer ;
  current_byte^ := Chr (value) ;
  Inc (current_byte) ;
  End ;
//
//  Description:
//
//    This function writes a 2-byte integer to the output stream
//    in big endian format.
//
//  Parameters:
//
//    value : The value to output
//
Procedure TOutputByteStream.writeBigEndianWord (value : Word) ;
  Begin
  if (current_byte + sizeof (value)) >= buffer_limit Then
    flushBuffer ;

  current_byte^ := Chr ((value Shr 8) And $FF) ;
  Inc (current_byte) ;
  current_byte^ := Chr (value And $FF) ;
  Inc (current_byte) ;
  if current_byte >= buffer_limit Then
    flushBuffer ;
  End ;

//
//  Description:
//
//    This function writes a 4-byte integer to the output stream
//    in big endian format.
//
//  Parameters:
//
//    value : The value to output
//
Procedure TOutputByteStream.writeBigEndianLong (value : Cardinal) ;
  Begin
  if (current_byte + sizeof (value)) >= buffer_limit Then
    flushBuffer ;

  current_byte^ := Chr ((value Shr 24) And $FF) ;
  Inc (current_byte) ;
  current_byte^ := Chr ((value Shr 16) And $FF) ;
  Inc (current_byte) ;
  current_byte^ := Chr ((value Shr 8) And $FF) ;
  Inc (current_byte) ;
  current_byte^ := Chr (value And $FF) ;
  Inc (current_byte) ;

  if current_byte >= buffer_limit Then
    flushBuffer ;
  End ;

//
//  Description:
//
//    This function puts the output stream in bit mode.
//
//  Parameters:
//
//    initialposition : The first bit position (usually 8 or 0)
//
Procedure TOutputByteStream.enterBitMode (initialposition : Integer) ;
  Begin
  if current_byte >= buffer_limit Then
    flushBuffer ;
  bit_position := initialposition ;
  current_byte^ := Chr (0) ;
  End ;

//
//  Description:
//
//    This function returns the number of bytes remaining in the
//    output buffer
//
//  Return Value:
//
//    The number of bytes remaining in the output buffer.
//
Function TOutputByteStream.remainingBufferSpace : Cardinal ;
  Begin
  result := buffer_limit - current_byte ;
  End ;

Procedure TOutputByteStream.nextByte ;
  Begin
  Inc (current_byte) ;
  if current_byte >= buffer_limit Then
    flushBuffer ;
  current_byte^ := Chr (0) ;
  End ;

End.
