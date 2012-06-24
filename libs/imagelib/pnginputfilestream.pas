unit pnginputfilestream;
//
//  Title:  PngInputFileStream class Definition
//
//  Copyright 1999, 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    March 15, 1999
//
//  Version: 1
//
//  Description:
//
//    The PngInputFileStream file stream implements PNG output
//    using a file.
//

interface

Uses classes, sysutils, pnginputstream ;

Type
  TPngInputFileStream = Class (TPngInputStream)
    Private
      input_buffer : Array of Char ;
      input_stream : TFileStream ;
      end_reached : Boolean ;
    Public
      Destructor Destroy ; Override ;
      Function readChunkLength : Cardinal ; Override ;
      Function readChunkData (chunklength : Cardinal) : Pointer ; Override ;
      Function readCrc : Cardinal ; Override ;
      Function endReached : Boolean ; Override ;
      Procedure readRaw (count :Cardinal ; var buffer : String) ; Override ;
      Procedure Open (filename : String) ;
      Procedure Close ;
    End ;

implementation

Uses
  systemspecific ;

Destructor TPngInputFileStream.Destroy ;
  Begin
  Close ;
  End ;


//
//  Description:
//
//    This function reads and returns the chunk length
//    from the input stream.
//
//  Return Value:
//
//    The length of the next chunk in system format.
//
Function TPngInputFileStream.readChunkLength : Cardinal ;
  Var
    count : LongInt ;
    Buffer : Cardinal ;
  Begin
  count := input_stream.read (Buffer, sizeof (Buffer)) ;
  if (count <> sizeof (Buffer)) Then
    raise EStreamError.Create ('Premature End of File') ;
  result := BigEndianLongToSystem (Buffer) ;
  End ;

//
//  Description:
//
//    This function reads the chunk data from the input file.
//
//  Parameters:
//
//    length : Number of bytes to read
//    buffer (out) : Returns a pointer to the buffer containing the data.
//
Function TPngInputFileStream.readChunkData (chunklength : Cardinal) : Pointer ;
  var
    Count : LongInt ;
  Begin
  if chunklength > length (input_buffer) Then
    SetLength (input_buffer, chunklength) ;

  count := input_stream.read (input_buffer [0], chunklength) ;
  if (count <> chunklength) Then
    Raise EStreamError.Create ('Premature End of File') ;
  Result := @input_buffer [0] ;
  End ;

//
//  Description:
//
//    This function reads the chunk CRC from the input file.
//
//  Return Value:
//
//    The chunk CRC
//
Function TPngInputFileStream.readCrc : Cardinal ;
  var
    Count : LongInt ;
  Begin
  count := input_stream.read (result, sizeof (result)) ;
  if (count <> sizeof (result)) Then
    raise EStreamError.Create ('Premature End of File') ;
  result := BigEndianLongToSystem (result) ;
  End ;

//
//  Description:
//
//    This function opens the input file.
//
//  Parameters:
//
//    filename : The name of the file to open
//
Procedure TPngInputFileStream.open (filename : String) ;
  Begin
  Close ;
  input_stream := TFileStream.Create (filename, fmOpenRead Or fmShareDenyWrite) ;
  End ;
//
//  Description:
//
//    This function closes the output file.
//
Procedure TPngInputFileStream.close ;
  Begin
  If Assigned (input_stream) Then
    Begin
    input_stream.Destroy ;
    input_stream := Nil ;
    End ;
  End ;
//
//  Description:
//
//    This function reads raw bytes from the input file.
//
//
//  Parameters:
//
//    count : The number of bytes to read
//    buffer (out) : A pointer to the data read.
//
Procedure TPngInputFileStream.readRaw (count : Cardinal ; var buffer : String) ;
  var
    ReadCount : LongInt ;
  Begin
  SetLength (buffer, count) ;

  Readcount := input_stream.read (buffer [1], count) ;
  if (Readcount <>  count) Then
    raise EStreamError.Create ('Premature End of File') ;
  End ;

Function TPngInputFileStream.endReached : Boolean ;
  Begin
  Result := end_reached ;
  End ;


end.
