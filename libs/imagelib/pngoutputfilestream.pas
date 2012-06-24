unit pngoutputfilestream;
//
//  Title:  PNG Output File Stream
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
//    This class is a PNG output stream directed to a file.
//


interface

Uses
  pngoutputstream, classes ;

Type
  TPngOutputFileStream = Class (TPngOutputStream)
    private
      output_stream : TStream ;
      output_buffer : Array of Byte ;
    protected
      Procedure flushBuffer ; Override ;
    public
      Destructor Destroy ; Override ;
      Procedure Open (filename : String) ;
      Procedure Close ;
      Procedure writeRaw (buffer : PChar ; size : Cardinal) ; Override ;
    End ;

implementation

Uses
  sysutils, crc32, systemspecific, bitmapimage ;

Const
  BUFFERSIZE = 32768 ;

//
//  Description:
//
//    Class Destructor
//
Destructor TPngOutputFileStream.Destroy ;
  Begin
  close ;
  Inherited Destroy ;
  End ;

//
//  Description:
//
//    This function writes the contents of the buffer to 
//    the output stream
//
Procedure TPngOutputFileStream.flushBuffer ;
  Var
    crc : TCrc32 ;
    length : cardinal ;
    chunklength : Cardinal ;
    crcvalue : cardinal ;
  Begin
  crc := TCrc32.Create ;
  Try
    crc.update (@chunk_type [1], High (chunk_type)) ;
    length := current_byte - output_buffer ;
    crc.update (@output_buffer [0], length) ;
    chunklength := SystemToBigEndianLong (length) ;
    output_stream.write (chunklength, sizeof (chunklength)) ;
    output_stream.write (chunk_type, sizeof (chunk_type)) ;
    output_stream.write (output_buffer [0], length) ;
    crcvalue := crc.value ;
    crcvalue := SystemToBigEndianLong (crcvalue) ;
    output_stream.write (crcvalue, sizeof (crcvalue)) ;
    current_byte := @output_buffer [0];
  Finally
    crc.Destroy ;
    End ;
  End ;
//
//  Description:
//
//    This function opens an output file.
//
//  Parameters:
//
//    filename : The output file name
//
Procedure TPngOutputFileStream.open (filename : String) ;
  Begin
  if output_stream <> Nil Then
    Raise EGraphicsException.Create ('Output stream already open') ;

  output_stream := TFileStream.Create (filename, fmCreate Or fmShareDenyWrite) ;
  setlength (output_buffer, BUFFERSIZE + 1) ;

  current_byte := @output_buffer [0] ;
  buffer_limit := @output_buffer [length (output_buffer) - 1]  ;
  End ;

//
//  Description:
//
//    This function closes the output file.
//
Procedure TPngOutputFileStream.close ;
  Begin
  SetLength (output_buffer, 0) ;
  current_byte := Nil ;
  buffer_limit := Nil ;
  output_stream.destroy ;
  output_stream := Nil ;
  End ;

//
//  Description:
//
//    This function writes a string of raw bytes to the output stream,
//    bypassing chunks.
//
//  Parameters:
//
//    buffer : The output buffer
//    size  : The number of bytes to write
//
Procedure TPngOutputFileStream.writeRaw (buffer : PChar ; size : Cardinal) ;
  Begin
  output_stream.write (buffer [0], size) ;
  End ;

end.
