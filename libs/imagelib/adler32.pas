unit adler32;

interface

//
// Copyright (c) 1997,1998, 2001 Colosseum Builders, Inc.
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
//  Title:  PNG Checksum Functions
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    Adler32 Checksum Definition and Implementation
//


Function Adler (input : Cardinal ; value : Byte) : Cardinal ;


implementation

Function Adler (input : Cardinal ; value : Byte) : Cardinal ;
  Const
    prime = 65521 ;
  Var
    lo, hi : Cardinal ;
  Begin
  lo := input And $FFFF ;
  hi := (input Shr 16) And $FFFF ;

  lo := (lo + value) Mod prime ;
  hi := (lo + hi) Mod prime ;
  Result := (hi Shl 16) Or lo ;
  End ;


end.
