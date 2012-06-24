Unit jpeginputfile ;

Interface

uses jpeginputstream, classes ;

Type
  TJpegInputFileStream = Class (TJpegInputStream)
    private
      input_buffer : Array of Char ;
      input_stream : TFileStream ;
      end_reached : Boolean ;

    Protected
      Procedure fillBuffer ; Override ;
      Function endReached : Boolean ; Override ;

    public
      Constructor Create (buffersize : Cardinal) ;
      Destructor Destroy ; Override ;
      Procedure open (filename : String) ;
      Procedure close ;
      Function tellg : LongInt ; Override ;
      Procedure seekg (position : LongInt) ; Override ;
    End ;

implementation

Uses Sysutils ;

//
//  Description:
//
//    Default Constructor
//
Constructor TJpegInputFileStream.Create (buffersize : Cardinal) ;
  Begin
  Inherited Create ;
  SetLength (input_buffer, buffersize) ;
  end_reached := false ;
  input_stream := nil ;
  End ;

Destructor TJpegInputFileStream.Destroy ;
  Begin
  Close ;
  Inherited Destroy ;
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
Procedure TJpegInputFileStream.open (filename : String) ;
  Begin
  if Assigned (input_stream) Then
    input_stream.Destroy ;

  input_stream := TFileStream.Create (filename, fmOpenRead Or fmShareDenyWrite	) ;
  End ;

//
//  Description:
//
//    This function closes the input stream.
//
Procedure TJpegInputFileStream.close ;
  Begin
  if Assigned (input_stream) Then
    input_stream.Destroy ;
  input_stream := Nil ;
  End ;
//
//  Description:
//
//    This function reads the input stream and fills the input buffer.
//
Procedure TJpegInputFileStream.fillBuffer ;
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

//
//  Description:
//
//    This function returns the current position in the input stream.
//
//  Return Value:
//
//    The current input position.
//
Function TJpegInputFileStream.tellg : LongInt ;
  Begin
  Result := LongInt (input_stream.position) ;
  Dec (result, buffer_limit - current_byte) ;
  End ;
//
//  Description:
//
//    This function moves the input stream to a specified position.
//
//  Parameters:
//
//    position: The absolute position in the input stream to move to.
//
Procedure TJpegInputFileStream.seekg (position : LongInt) ;
  Begin
  exitBitMode ;
  input_stream.seek (position, soFromBeginning) ;
  fillBuffer ;
  End ;


Function TJpegInputFileStream.endReached : Boolean ;
  Begin
  Result := end_reached ;
  End ;

End.
