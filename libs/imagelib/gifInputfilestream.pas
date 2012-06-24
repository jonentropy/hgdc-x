unit gifInputfilestream;
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

Uses
  GifInputStream, classes ;

Type
  TGifInputFileStream = class (TGifInputStream)
    Private
      input_stream : TFileStream ;
      end_reached : Boolean ;
      input_buffer : Array of Char ;
    Protected
    Public
      Constructor Create (buffersize : Cardinal) ;
      Destructor Destroy ; Override ;
      Procedure open (filename : String) ;
      Procedure close ;
      Function tellg : LongInt ;
      Procedure seekg (position : LongInt) ;
      Procedure FillBuffer ; Override ;
      Function EndReached : Boolean ; Override ;
    End ;

implementation

Uses Sysutils ;

Const
  DefaultBufferSize = 1 SHL 16 ;

Constructor TGifInputFileStream.Create (buffersize : Cardinal) ;
  Begin
  Inherited Create ;
  if (buffersize = 0) then
    SetLength (input_buffer, DefaultBufferSize)
  Else
    SetLength (input_buffer, buffersize) ;
  end_reached := true ;
  input_stream := Nil ;
  End ;

Destructor TGifInputFileStream.Destroy ;
  Begin
  Close ;
  Inherited Destroy ;
  End ;

Procedure TGifInputFileStream.open (filename : String) ;
  Begin
  if Assigned (input_stream) Then
    input_stream.Destroy ;

  input_stream := TFileStream.Create (filename, fmOpenRead Or fmShareDenyWrite	) ;
  end_reached := false ;
  End ;

Function TGifInputFileStream.tellg : LongInt ;
  Begin
  Result := LongInt (input_stream.position) ;
  End ;


Procedure TGifInputFileStream.seekg (position : LongInt) ;
  Begin
  exitBitMode ;
  input_stream.seek (position, soFromBeginning) ;
  End ;

Procedure TGifInputFileStream.close ;
  Begin
  if Assigned (input_stream) Then
    input_stream.Destroy ;
  input_stream := Nil ;
  End ;

Procedure TGifInputFileStream.FillBuffer ;
  Var
    count : LongInt ;
  Begin
  count := input_stream.read (input_buffer [0], Length (input_buffer)) ;
  if (count <= 0) Then
    Begin
    current_byte := Nil ;
    buffer_limit := Nil ;
    End
  else
    Begin
    current_byte := @input_buffer [0] ;
{$RANGECHECKS OFF}
    buffer_limit := @input_buffer [count] ;
{$RANGECHECKS ON}
    End ;
  if (count <> Length (input_buffer)) Then
    end_reached := true ;
  End ;

Function TGifInputFileStream.EndReached : Boolean ;
  Begin
  Result := end_reached ;
  End ;
  
end.
