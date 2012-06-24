unit pngoutputstream;
//
//  Title:  PNG Output Stream
//
//  Copyright 2001 Colosseum Builders, Inc.
//  All rights reserved.
//
//  Colosseum Builders, Inc. makes no warranty, expressed or implied
//  with regards to this software. It is provided as is.
//
//  Author:  John M. Miano (miano@colosseumbuilders.com)
//
//  Date:    June 10, 2001
//
//  Version: 1
//
//  Description:
//
//    This an abstract class the represents an PNG output stream.
//


interface

Uses
  deflateoutputstream ;

Type
  TChunkType = Array [1..4] of Char ;

  TPngOutputStream = Class (TDeflateOutputStream)
    protected
      chunk_type : TChunkType ;
    public
      Constructor Create ;
      Procedure startChunk (chunktype : TChunkType) ; 
      Procedure endChunk ;
      Procedure writeRaw (buffer : PChar ; size : Cardinal) ; Virtual ; Abstract ;
    End ;

implementation

Constructor TPngOutputStream.Create ;
  Var
    ii : integer ;
  Begin
  Inherited Create ;
  for ii := Low (chunk_type) To High (chunk_type) Do
    chunk_type [ii] := Chr (0) ;
  End ;

//
//  Description:
//
//    This function starts a new chunk.
//
//  Parameters:
//
//    chunktype : The 4-byte chunk type
//
Procedure TPngOutputStream.startChunk (chunktype : TChunkType) ;
  Var
    ii : Cardinal ;
  Begin
  for ii := Low (chunk_type) To High (chunk_type) DO
    chunk_type [ii] := chunktype [ii] ;
  End ;
//
//  Description:
//
//    This function ends the current chunk.
//
Procedure TPngOutputStream.endChunk ;
  Var
    ii : integer ;
  Begin
  flushBuffer ;
  for ii := Low (chunk_type) To High (chunk_type) Do
    chunk_type [ii] := Chr (0) ;
  End ;

end.
