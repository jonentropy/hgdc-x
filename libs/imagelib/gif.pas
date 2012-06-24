unit gif ;
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

//
// Description:
//
//   The Module contains definitions for the GIF image format.
//
//   These come directly from
//   "GRAPHICS INTERCHANGE FORMAT", CompuServe, July 31, 1990.
//   Markers of the form #N reference section N of this document.
//
//  Author: John M. Miano - miano@colosseumbuilders.com
//  Date:  June 10, 2005
//
interface

uses
  systemspecific ;

Const
  GifBitsUsed = 12 ; // Up to 12 bits can be used in compresson
  // Largest number of data bytes in a data block.
  GifInputBufferSize = 255 ;
  GifMaxColors = 256 ;

Type
  // #17
  GifHeader = Packed Record
    file_signature : Array [0..2] of char ;
    file_version : Array [0..2] of char ;
    End ;

  // #18
  GifScreenDescriptor = Packed Record
    logical_screen_width : UBYTE2 ;
    logical_screen_height : UBYTE2 ;
    flags : UBYTE1 ;
    // 0-2: Global Color Table Size
    // 3: Sort Flag
    // 4-6: Color Resolution
    // 7: Global Color Table
    background_color_index : UBYTE1 ;
    pixel_aspect_ratio : UBYTE1 ;
    End ;

  // #20
  GifImageDescriptor = Packed Record
    left_position : UBYTE2 ;
    top_position : UBYTE2 ;
    image_width : UBYTE2 ;
    image_height : UBYTE2 ;
    flags : UBYTE1 ;
    // 0-2: Local Color Table Size
    // 3-4: Reserved
    // 5: Sort Flag
    // 6: Interlace Flag
    // 7: Local Color Table Flag
    End ;

  // #19
  GifColorTableEntry = Packed Record
    Red : UBYTE1 ; //UBYTE1 ;
    Green : UBYTE1 ;
    Blue : UBYTE1 ;
    End ;

  // #23
  GifGraphicControlBlock = Packed Record
    block_size : UBYTE1 ;
    flags : UBYTE1 ;
    // 0: Transparent Color Flag
    // 1: User Input Flag
    // 2-4: Disposal Method
    // 5-7: Reserved
    delay_time : UBYTE2 ;
    transparent_color : UBYTE1 ;
    End ;

  // #26
  GifApplicationBlock = Packed Record
    block_size : UBYTE1 ;
    application_name : Array [0..7] of Char ;
    authentication_code : Array [0..2] of UBYTE1 ;
    End ;

  // #25
  GifPlainTextBlock = Packed Record
    Block_size : UBYTE1 ;
    Text_left : UBYTE2 ;
    Text_top : UBYTE2 ;
    Text_width : UBYTE2 ;
    Text_height : UBYTE2 ;
    char_width : UBYTE1 ;
    char_height : UBYTE1 ;
    foreground_color : UBYTE1 ;
    background_color : UBYTE1 ;
    End ;

Const  // From Appendix A.
  GifTerminator = $0 ;
  GifImageSeparator = $2C ;
  GifExtension = $21 ;
  GifCommentExtension = $FE ;
  GifPlainTextExtension = $01 ;
  GifApplicationExtension = $FF ;
  GifGraphicControlExtension=$F9 ;
  GifTrailer = $3B ;

  GifMaxBitsPerCode = 12 ; // Appendix F.

// These functions extract bit fields from the various blocks defined above.
Function GlobalColorTableSize (input : GifScreenDescriptor) : Cardinal ;
Function SortFlag (input : GifScreenDescriptor) : Boolean ; Overload ;
Function ColorResolution (input : GifScreenDescriptor) : Cardinal ;
Function GlobalColorTableFlag (input : GifScreenDescriptor) : Boolean ;
Function TransparentColorFlag (block : GifGraphicControlBlock) : Boolean ;
Function UserInputFlag (block : GifGraphicControlBlock) : Boolean ;
Function DisposalMethod (block : GifGraphicControlBlock) : Cardinal ;
Function LocalColorTableFlag (block : GifImageDescriptor) : Boolean ;
Function ColorTableSize (block : GifImageDescriptor) : Cardinal ;
Function SortFlag (input : GifImageDescriptor) : Boolean ; Overload ;
Function InterlaceFlag (input : GifImageDescriptor) : Boolean ;

// These functions print the contents of the various blocks defined above.
Procedure Print (block : GifHeader) ; Overload ;
Procedure Print (block : GifScreenDescriptor) ; Overload ;
Procedure Print (block : GifImageDescriptor) ; Overload ;
Procedure Print (block : GifPlainTextBlock) ; Overload ;
Procedure Print (block : GifGraphicControlBlock) ; Overload ;

implementation


Function GlobalColorTableSize (input : GifScreenDescriptor) : Cardinal ;
  Begin
  Result := 1 SHL ((input.Flags And $7) + 1) ;
  End ;

Function SortFlag (input : GifScreenDescriptor) : Boolean ; Overload ;
  Begin
  Result := (input.Flags And $8) <> 0 ;
  End ;

Function ColorResolution (input : GifScreenDescriptor) : Cardinal ;
  Begin
  Result := (input.Flags SHR 4) AND $7
  End ;

Function GlobalColorTableFlag (input : GifScreenDescriptor) : Boolean ;
  Begin
  Result := (input.Flags And $80) <> 0 ;
  End ;

Function TransparentColorFlag (block : GifGraphicControlBlock) : Boolean ;
  Begin
  Result := (block.flags AND $1) <> 0 ;
  End ;

Function UserInputFlag (block : GifGraphicControlBlock) : Boolean ;
  Begin
  Result := (block.flags AND $2) <> 0 ;
  End ;

Function DisposalMethod (block : GifGraphicControlBlock) : Cardinal ;
  Begin
  Result := (block.flags Shr 2) And $7 ;
  End ;

Function LocalColorTableFlag (block : GifImageDescriptor) : Boolean ;
  Begin
  Result := (block.flags AND $8) <> 0 ;
  End ;

Function ColorTableSize (block : GifImageDescriptor) : Cardinal ;
  Begin
  Result := (block.flags And $7) ;
  End ;

Function SortFlag (input : GifImageDescriptor) : Boolean ; Overload ;
  Begin
  Result := (input.Flags And $10) <> 0 ;
  End ;

Function InterlaceFlag (input : GifImageDescriptor) : Boolean ;
  Begin
  Result := (input.Flags And $40) <> 0 ;
  End ;


Procedure Print (block : GifHeader) ;
  Begin
  WriteLn ('{ GIF Header') ;
  WriteLn ('  Signature:  ', block.file_signature [0],
                  block.file_signature [1], block.file_signature [2]) ;
  WriteLn ('    Version:  ', block.file_version [0],
                block.file_version [1],  block.file_version [2]) ;
  WriteLn ('}') ;
  End ;

Procedure Print (block : GifScreenDescriptor) ;
  Begin
  Writeln ('{ Screen Descriptor') ;
  WriteLn ('       Logical Screen Width:  ', block.logical_screen_width) ;
  WriteLn ('      Logical Screen Height:  ', block.logical_screen_height) ;
  WriteLn (' Size of Global Color Table:  ', GlobalColorTableSize (block)) ;
  WriteLn ('                  Sort Flag:  ', SortFlag (block)) ;
  WriteLn ('           Color Resolution:  ', ColorResolution (block)) ;
  WriteLn ('    Global Color Table Flag:  ', GlobalColorTableFlag (block)) ;
  WriteLn ('}') ;
 End ;

Procedure Print (block : GifImageDescriptor) ; Overload ;
  Begin
  WriteLn ('{ GIF Image Descriptor') ;
  WriteLn ('          Left Position:  ', block.left_position) ;
  WriteLn ('           Top Position:  ', block.top_position) ;
  WriteLn ('                  Width:  ', block.image_width) ;
  WriteLn ('                 Height:  ', block.image_height) ;
  WriteLn (' Local Color Table Flag:  ', LocalColorTableFlag (block)) ;
  WriteLn ('         Interlace Flag:  ', InterlaceFlag (block)) ;
  WriteLn ('              Sort Flag:  ', SortFlag (block)) ;
  WriteLn ('       Color Table Size:  ', ColorTableSize (block)) ;
  WriteLn ('}') ;
  End ;


Procedure Print (block : GifPlainTextBlock) ; Overload ;
  Begin
  WriteLn ('{ Plain Text Extension Block') ;
  WriteLn ('  Block Size: ', block.Block_size) ;
  WriteLn ('  Text Grid Left Position: ', block.Text_left) ;
  WriteLn ('  Text Grid Top Position: ', block.Text_top) ;
  WriteLn ('  Text Grid Width: ', block.Text_width) ;
  WriteLn ('  Text Grid Height: ', block.Text_height) ;
  WriteLn ('  Character Cell Width: ', block.char_width) ;
  WriteLn ('  Character Cell Height: ', block.char_height) ;
  WriteLn ('  Foreground Color Index: ', block.foreground_color) ;
  WriteLn ('  Background Color Index: ', block.background_color) ;
  WriteLn ('}') ;
  End ;

Procedure Print (block : GifGraphicControlBlock) ;
  Begin
  WriteLn ('{ Graphic Control Block ') ;
  WriteLn ('    Delay Time: ',  BigEndianWordToSystem (block.delay_time)) ;
  if TransparentColorFlag (block) Then
    WriteLn ('    Transparent Color: ', block.transparent_color) ;
  WriteLn ('    User Input:  ', UserInputFlag (block)) ;
  WriteLn ('    Disposal Method:  ',  DisposalMethod (block)) ;
  WriteLn ('}') ;
  End ;

end.
