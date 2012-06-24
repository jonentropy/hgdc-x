unit deflateoutputstream;

interface

Uses
  outputbytestream ;

Type
  TDeflateOutputStream = class (TOutputByteStream)
    Public
      Procedure writeBits (bits : Integer ; count : Cardinal) ;
      Procedure exitBitMode ;
    End ;

implementation

Const
  CHAR_BIT  = 8 ;

Procedure TDeflateOutputStream.writeBits (bits : Integer ; count : Cardinal) ;
  Const
    masks : Array [0..31] Of Cardinal = (
      1,        1 Shl 1,  1 Shl 2,  1 Shl 3,  1 Shl 4,  1 Shl 5,  1 Shl 6,  1 Shl 7,
      1 Shl 8,  1 Shl 9,  1 Shl 10, 1 Shl 11, 1 Shl 12, 1 Shl 13, 1 Shl 14, 1 Shl 15,
      1 Shl 16, 1 Shl 17, 1 Shl 18, 1 Shl 19, 1 Shl 20, 1 Shl 21, 1 Shl 22, 1 Shl 23,
      1 Shl 24, 1 Shl 25, 1 Shl 26, 1 Shl 27, 1 Shl 28, 1 Shl 29, 1 Shl 30, $80000000
      ) ;
    complements : Array [0..7] of Cardinal =
     (
      $FFFFFFFE, $FFFFFFFD, $FFFFFFFB, $FFFFFFF7,
      $FFFFFFEF, $FFFFFFDF, $FFFFFFBF, $FFFFFF7F
     ) ;
  Var
    ii : Cardinal ;
  Begin
  For ii := 0 To Count - 1 Do
    Begin
    if bit_position >= CHAR_BIT Then
      Begin
      nextByte ;
      bit_position := 0 ;
      End ;
    if (bits And masks [ii]) <> 0 Then
      current_byte^ := Chr (Ord (current_byte^) Or masks [bit_position])
    else
      current_byte^ := Chr (Ord (current_byte^) And complements [bit_position]) ;

    Inc (bit_position) ;
    End ;
  End ;

Procedure TDeflateOutputStream.exitBitMode ;
  Begin
  if bit_position <> 0 Then
    nextByte ;
  bit_position := -1 ;
  End ;

end.
