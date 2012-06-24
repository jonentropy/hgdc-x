unit crc32;

interface

Type
  TCrc32 = class
    Private
      crc_register : Cardinal ;

    Public
      Constructor Create ;
      Procedure Reset ;
      Function Value : Cardinal ;
      Procedure Update (buffer : PChar ; length : Cardinal) ;
    End ;

implementation

const
  INITIALVALUE = $FFFFFFFF ;
  POLYNOMIAL = $EDB88320 ; // Bit reverse of 04C11DB7 ;

Var
  crc_table : Array [0..255] of Cardinal ;
  II, JJ : Cardinal ;


Constructor TCrc32.Create ;
  Begin
  Inherited Create ;
  Reset ;
  End ;

Procedure TCrc32.Reset ;
  Begin
  crc_register := INITIALVALUE ;
  End ;

Procedure TCrc32.Update  (buffer : PChar ; length : Cardinal) ;
  Var
    II : Integer ;
  Begin
  For II := 0 To Integer (Length) - 1 Do
    crc_register := crc_table [(crc_register Xor Ord (buffer [ii])) And $FF]
                     Xor (crc_register Shr 8) ;
  End ;

Function TCrc32.Value : Cardinal ;
  Begin
  Result := crc_register Xor $FFFFFFFF ;
  End ;


Initialization
  Begin
  For II := Low (crc_table) To High (crc_table) Do
    Begin
    crc_table [ii] := ii ;
    For JJ := 1 To 8 Do
      Begin
      If (crc_table [ii] And 1) = 0 Then
        crc_table [ii] := crc_table [ii] Shr 1 
      Else
        crc_table [ii] := (crc_table [ii] Shr 1) Xor POLYNOMIAL ;
      End ;
    End ;
  End ;
End.
