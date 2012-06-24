unit pnginputstream;
//
//  Title:  PNG Input File Stream
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
//    This class is a PNG input stream attached to a file.
//

interface

uses sysutils, inflateinputstream ;

Type

  TPngInputStream = Class (TInflateInputStream)
    Protected
      chunk_type : String ;
      chunk_data : Pointer ;
      chunk_length : Cardinal ;
      Procedure fillBuffer ; Override ;
      Function readChunkLength : Cardinal ; Virtual ; Abstract ;
      Function readChunkData (length : Cardinal) : Pointer ; Virtual ; Abstract ;
      Function readCrc : Cardinal ; Virtual ; Abstract ;
    Public
      Procedure getNextChunk ;
      Property ChunkType : String Read chunk_type ;
      Property ChunkDataLength : Cardinal Read chunk_length ;
      Property ChunkData : Pointer read chunk_data ;
      Procedure readRaw (count : Cardinal ; var buffer : String) ; Virtual ; Abstract ;
    End ;

  EStreamError = Class (Exception) ;


implementation

Uses
  crc32 ;

//
// Reads the next PNG chunk from the input stream.
//
Procedure TPngInputStream.getNextChunk ;
  Var
    filecrc : Cardinal ;
    crc : TCrc32 ;
    crcvalue : Cardinal ;
    ctype : PCHAR ; // Chunk Type
  Begin

  chunk_length := readChunkLength ;

  chunk_data := readChunkData (chunk_length + sizeof (chunk_length)) ;

  // Check the CRC of the chunk data.
  filecrc := readCrc ;
  crc := Tcrc32.Create ;
  Try
    crc.update (chunk_data, chunk_length + sizeof (chunk_length)) ;
    crcvalue := crc.value ;
    if (filecrc <> crcvalue) Then
      Raise EStreamError.Create ('Invalid Chunk CRC') ;
  Finally
    crc.Destroy ;
    End ;

  // Get the chunk type.
  ctype := chunk_data ;
  SetLength (chunk_type, 4) ;
  chunk_type [1] := ctype^ ; Inc (ctype) ;
  chunk_type [2] := ctype^ ; Inc (ctype) ;
  chunk_type [3] := ctype^ ; Inc (ctype) ;
  chunk_type [4] := ctype^ ; Inc (ctype) ;

  // Seet up the pointers to the chunk data.
  chunk_data := ctype ;
  current_byte := chunk_data ;
  buffer_limit := @ctype [chunk_length] ;
  End ;

Procedure TPngInputStream.fillBuffer ;
  Begin
  GetNextChunk ;
  if (chunk_type <> 'IDAT') then
    Raise EStreamError.Create ('Missing IDAT Chunk') ;
  current_byte := chunk_data ;
  buffer_limit := @current_byte [chunk_length] ;
  End ;
End.
