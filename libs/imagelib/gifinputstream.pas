unit gifinputstream;
//
// Copyright (c) 2005 Colosseum Builders, Inc.
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

uses
  inputbytestream, systemspecific, bitmapimage ;

type
  TGifInputStream = class (TInputByteStream)
    Private
      block_count : Cardinal ;
    Protected
    Public
      Procedure EnterBitMode ;
      Function NextCode (codesize : Cardinal) : Cardinal ;
      Function NextBit : Cardinal ;
      Procedure ExitBitMode ; Override ;
    Public
    End ;

    EGifStreamError = Class (EGraphicsException) ;


implementation

Const
  CHAR_BIT = 8 ; // Bits per byte.

Procedure TGifInputStream.EnterBitMode ;
  Begin
  Inherited EnterBitMode (0) ;
  block_count := Cardinal (current_byte^) ;
  AdvanceCurrentByte ;
  End ;

//
//  This function reads the next code from teh input stream.
//
//  Parameters:
//    codesize : The code size or number of bits to read.
//

Function TGifInputStream.NextCode (CodeSize : Cardinal) : Cardinal ;
  Var
    ii : Integer ;
  Begin
  Result := 0 ;
  for ii := 0 to CodeSize - 1 Do
    Result := Result OR (NextBit SHL ii) ;

  End ;

Function TGifInputStream.NextBit : Cardinal ;
  Const
    Masks : Array [0..CHAR_BIT-1] of Cardinal = ($1, $2, $4, $8, $10, $20, $40, $80) ;
  Begin
  if bit_position = CHAR_BIT then
    Begin
    Dec (block_count) ;
    If (block_count = 0) Then
      Begin
      AdvanceCurrentByte ;
      block_count := Cardinal (current_byte^) ;
      if block_count = 0 then
        raise EGifStreamError.Create ('GIF: Attempt to read bits beyond compressed data') ;
      End ;
    AdvanceCurrentByte ;
    bit_position := 0 ;
    End ;
  if (Cardinal (current_byte^) AND Masks [bit_position]) <> 0 Then
    Result := 1
  Else
    Result := 0 ;
  Inc (bit_position) ;
  End ;

Procedure TGifInputStream.ExitBitMode ;
  Var
    value : Integer ;
  Begin
  While block_count > 0 Do
    Begin
    Dec (block_count) ;
    AdvanceCurrentByte ;
    End ;
  in_bit_mode := false ;

  value := getByte ;
  if (value <> 0) then
    raise EGifStreamError.Create ('GIF: Missing terminator after compressed data') ;
  End ;


end.
