unit systemspecific;

interface

type
  UBYTE4 = 0 .. $FFFFFFFF ;
  BYTE4 = -2147483648 .. 2147483647 ;
  UBYTE2 = 0 .. $FFFF ;
  BYTE2 = -32768 .. 32767 ;
  UBYTE1 = 0 .. 255 ;
  BYTE1 = -128 .. 127 ;

Function BigEndianLongToSystem (value : UBYTE4) : UBYTE4 ;
Function BigEndianWordToSystem (value : UBYTE4) : UBYTE4 ;
Function SystemToBigEndianLong (value : UBYTE4) : UBYTE4 ;
Function SystemToBigEndianWord (value : UBYTE4) : UBYTE4 ;



implementation

Function BigEndianLongToSystem (value : UBYTE4) : UBYTE4 ;
  Begin
  Result := ((value Shr 24) And $FF)
         Or ((value Shr 8 ) And $FF00)
         Or ((value Shl 8 ) And $FF0000)
         Or ((value Shl 24 ) And $FF000000)
  End ;

Function BigEndianWordToSystem (value : UBYTE4) : UBYTE4 ;
  Begin
  Result := ((value Shr 8) And $FF)
         Or ((value Shl 8 ) And $FF00) ;
  End ;

Function SystemToBigEndianLong (value : UBYTE4) : UBYTE4 ;
  Begin
  Result := ((value Shr 24) And $FF)
         Or ((value Shr 8 ) And $FF00)
         Or ((value Shl 8 ) And $FF0000)
         Or ((value Shl 24 ) And $FF000000)
  End ;

Function SystemToBigEndianWord (value : UBYTE4) : UBYTE4 ;
  Begin
  Result := ((value Shr 8) And $FF)
         Or ((value Shl 8 ) And $FF00) ;
  End ;

End.
