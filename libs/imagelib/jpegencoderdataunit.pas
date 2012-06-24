unit jpegencoderdataunit;
//
// Copyright (c) 2001 Colosseum Builders, Inc.
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
//  Title:  JpegEncoderDataUnit class definition
//
//  Author:  John M. Miano  miano@colosseumbuilders.com
//
//  Description:
//
//    This class represents a data unit in the JPEG encoder.
//
//  Date:
//
//    June 17, 2001

Interface


Uses jpegpvt, systemspecific ;

Type
  JpegCoefficient = BYTE2 ; // Must be signed
  COEFFICIENTINDEX = 0..JPEGSAMPLESIZE - 1 ;

  TJpegEncoderDataUnit = Array [0..JPEGSAMPLEWIDTH-1, 0..JPEGSAMPLEWIDTH-1] Of JPEGSAMPLE ;
  TJpegCoefficientBlock = Array [COEFFICIENTINDEX] Of JpegCoefficient ;

  TJpegEncoderQuantizationTable = Class
    Private

      // The integer quantization values stored in normal (not zigzag) order.
      data_values : Array [COEFFICIENTINDEX] of  UBYTE2 ;
      // The scaled quantization values stored in normal (not zigzag) order.
      float_scaling : Array [COEFFICIENTINDEX] of Double ;

      is_used : Boolean ; // Set to true if any component uses this table.

      Function getValue (index : COEFFICIENTINDEX) : UBYTE2 ;
      Procedure setValue (index : COEFFICIENTINDEX ; value : UBYTE2) ;

    Public
      Constructor Create ;
      Property isUsed : Boolean read is_used write is_used ;
      Procedure buildScaledTables ;
      Property Values [index : COEFFICIENTINDEX] : UBYTE2 read getValue write setValue ; Default ;

    End ;

Procedure ForwardDct (input : TJpegEncoderDataUnit ;
                      qt : TJpegEncoderQuantizationTable ;
                      var output : TJpegCoefficientBlock) ;

Procedure Print (input : TJpegEncoderDataUnit) ; Overload ;
Procedure Print (input : TJpegCoefficientBlock) ; Overload ;

implementation

Uses
  jfif, math ;

const
  PI = 3.1415926535897932384626433832795 ;
  FC4 = 0.707106781186547524400844362104849 ;   // cos (PI * 4.0 / 16.0) ;
  FSEC2 = 0.541196100146196984399723205366389 ; // 0.5 / cos (PI * 2.0 / 16.0) ;
  FSEC6 = 1.306562964876376527856643173427192 ;  // 0.5 / cos (PI * 6.0 / 16.0) ;


  floatscaling : Array [COEFFICIENTINDEX] of Double =
(
 0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,  0.125,                  0.159094822571604233,  0.2309698831278216846, 0.4530637231764438333,
 0.09011997775086849627, 0.0649728831185362593,  0.0689748448207357645,  0.07664074121909414394, 0.09011997775086849627, 0.1147009749634507608, 0.1665200058287998886, 0.3266407412190940884,
 0.09567085809127244544, 0.0689748448207357645,  0.0732233047033631207,  0.08136137691302557096, 0.09567085809127244544, 0.1217659055464329343, 0.1767766952966368932, 0.3467599613305368256,
 0.1063037618459070632,  0.07664074121909414394, 0.08136137691302557096, 0.09040391826073060355, 0.1063037618459070632,  0.135299025036549253,  0.1964237395967755595, 0.3852990250365491698,
 0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,  0.125,                  0.159094822571604233,  0.2309698831278216846, 0.4530637231764438333,
 0.159094822571604233,   0.1147009749634507608,  0.1217659055464329343,  0.135299025036549253,   0.159094822571604233,   0.2024893005527218515, 0.2939689006048396558, 0.5766407412190940329,
 0.2309698831278216846,  0.1665200058287998886,  0.1767766952966368932,  0.1964237395967755595,  0.2309698831278216846,  0.2939689006048396558, 0.4267766952966368654, 0.8371526015321518744,
 0.4530637231764438333,  0.3266407412190940884,  0.3467599613305368256,  0.3852990250365491698,  0.4530637231764438333,  0.5766407412190940329, 0.8371526015321518744, 1.642133898068010689
) ;


//
//  Description:
//
//    This is an implementation of the Forward Discrete Transform based
//    on matrix factorization of the DCT matrix. My first factorization
//    always left a constant factor of 1/8 at the end which could be merged
//    with quantization or as part of an integer descaling.
//
//    Using the cosine product formula it was possible to eliminate some
//    multiplication operations which resulted in a more complex scaling
//    matrix.
//
//    I have documented the derivation process for this DCT process as well
//    as another I have tried. I have some more factorization ideas to try
//    out when I have to to get around to it. Unfortunately matrix factorization
//    is a very tedious process. When you see the documents it looks easy, but
//    believe me and all the legal pads I went through that it is not.
//
//    This implementation is a litteral mapping of the matrix implementation.
//    Each set of temporaries represents one matrix multiplication. Hopefully
//    your compile will optimize the temporaries out. It is possible to
//    reorder the operations to reduce the number of temporaries variables
//    required (The Intel C++ compile does this on its own) which seems to be
//    the best optimization to try next.
//
//
//    I have not implemented a scaled integer version of the FDCT because I
//    believe that most people will want quality over speed in encoding.
//
//  Parameters:
//
//    input : The input values
//    qt:  The quantization table
//    output:  The output DCT coefficients
//

Procedure ForwardDct (input : TJpegEncoderDataUnit ;
                      qt : TJpegEncoderQuantizationTable ;
                      var output : TJpegCoefficientBlock) ;
  Var
    tmp : Array [0..JPEGSAMPLEWIDTH-1, 0..JPEGSAMPLEWIDTH-1] Of Double ;
    a0, a1, a2, a3, a4, a5, a6, a7,
    b0, b1, b2, b3, b4, b5, b6, b7,
    c0, c1, c2, c3, c4, c5, c6, c7,
    d0, d1, d2, d3, d4, d5, d6, d7,
    e0, e1, e2, e3, e4, e5, e6, e7,
    f0, f1, f2, f3, f4, f5, f6, f7,
    g0, g1, g2, g3, g4, g5, g6, g7,
    h0, h1, h2, h3, h4, h5, h6, h7,
    i0, i1, i2, i3, i4, i5, i6, i7 : Double ;
    row, col : Cardinal ;

  Begin

  for col := 0 To JPEGSAMPLEWIDTH - 1 Do
    Begin
    a0 := input [0][col] + input [7][col] - 2 * JPEGMIDPOINTSAMPLEVALUE ;
    a1 := input [1][col] + input [6][col] - 2 * JPEGMIDPOINTSAMPLEVALUE ;
    a2 := input [2][col] + input [5][col] - 2 * JPEGMIDPOINTSAMPLEVALUE ;
    a3 := input [3][col] + input [4][col] - 2 * JPEGMIDPOINTSAMPLEVALUE ;
    a4 := input [3][col] - input [4][col] ;
    a5 := input [2][col] - input [5][col] ;
    a6 := input [1][col] - input [6][col] ;
    a7 := input [0][col] - input [7][col] ;

    b0 := a0 + a3 ;
    b1 := a1 + a2 ;
    b2 := a1 - a2 ;
    b3 := a0 - a3 ;
    b4 := a4 ;
    b5 := a5 ;
    b6 := a6 ;
    b7 := a7 ;

    c0 := b0 ;
    c1 := b1 ;
    c2 := b2 + b3 ;
    c3 := b3 ;
    c4 := b4 + b5 ;
    c5 := b5 + b6 ;
    c6 := b6 + b7 ;
    c7 := b7 ;

    d0 := c0 ;
    d1 := c1 ;
    d2 := c2 ;
    d3 := c3 ;
    d4 := c4 + c6 ;
    d5 := c5 ;
    d6 := c6 ;
    d7 := c7 ;

    e0 := d0 + d1 ;
    e1 := d0 - d1 ;
    e2 := FC4 * d2 ;
    e3 := d3 ;
    e4 := FC4 * d4 ;
    e5 := FC4 * d5 ;
    e6 := d6 ;
    e7 := d7 ;

    f0 := e0 ;
    f1 := e1 ;
    f2 := e2 ;
    f3 := e3 ;
    f4 := e4 + e6 ;
    f5 := e5 ;
    f6 := e4 - e6 ;
    f7 := e7 ;

    g0 := f0 ;
    g1 := f1 ;
    g2 := f2 ;
    g3 := f3 ;
    g4 := FSEC2 * f4 ;
    g5 := f7 - f5 ;
    g6 := FSEC6 * f6 ;
    g7 := f5 + f7 ;

    h0 := g0 ;
    h1 := g1 ;
    h2 := g2 + g3 ;
    h3 := g3 - g2 ;
    h4 := g4 + g7 ;
    h5 := g5 + g6 ;
    h6 := g5 - g6 ;
    h7 := g7 - g4 ;

    tmp [0][col] := h0 ;
    tmp [1][col] := h4 ;
    tmp [2][col] := h2 ;
    tmp [3][col] := h6 ;
    tmp [4][col] := h1 ;
    tmp [5][col] := h5 ;
    tmp [6][col] := h3 ;
    tmp [7][col] := h7 ;
    End ;

  for row := 0 To JPEGSAMPLEWIDTH - 1 Do
    Begin
    a0 := tmp [row][0] + tmp [row][7] ;
    a1 := tmp [row][1] + tmp [row][6] ;
    a2 := tmp [row][2] + tmp [row][5] ;
    a3 := tmp [row][3] + tmp [row][4] ;
    a4 := tmp [row][3] - tmp [row][4] ;
    a5 := tmp [row][2] - tmp [row][5] ;
    a6 := tmp [row][1] - tmp [row][6] ;
    a7 := tmp [row][0] - tmp [row][7] ;

    b0 := a0 + a3 ;
    b1 := a1 + a2 ;
    b2 := a1 - a2 ;
    b3 := a0 - a3 ;
    b4 := a4 ;
    b5 := a5 ;
    b6 := a6 ;
    b7 := a7 ;

    c0 := b0 ;
    c1 := b1 ;
    c2 := b2 + b3 ;
    c3 := b3 ;
    c4 := b4 + b5 ;
    c5 := b5 + b6 ;
    c6 := b6 + b7 ;
    c7 := b7 ;

    d0 := c0 ;
    d1 := c1 ;
    d2 := c2 ;
    d3 := c3 ;
    d4 := c4 + c6 ;
    d5 := c5 ;
    d6 := c6 ;
    d7 := c7 ;

    e0 := d0 + d1 ;
    e1 := d0 - d1 ;
    e2 := FC4 * d2 ;
    e3 := d3 ;
    e4 := FC4 * d4 ;
    e5 := FC4 * d5 ;
    e6 := d6 ;
    e7 := d7 ;

    f0 := e0 ;
    f1 := e1 ;
    f2 := e2 ;
    f3 := e3 ;
    f4 := e4 + e6 ;
    f5 := e5 ;
    f6 := e4 - e6 ;
    f7 := e7 ;

    g0 := f0 ;
    g1 := f1 ;
    g2 := f2 ;
    g3 := f3 ;
    g4 := FSEC2 * f4 ;
    g5 := f7 - f5 ;
    g6 := FSEC6 * f6 ;
    g7 := f5 + f7 ;

    h0 := g0 ;
    h1 := g1 ;
    h2 := g2 + g3 ;
    h3 := g3 - g2 ;
    h4 := g4 + g7 ;
    h5 := g5 + g6 ;
    h6 := g5 - g6 ;
    h7 := g7 - g4 ;
    i0 := h0 * qt.float_scaling [row * JPEGSAMPLEWIDTH] ;
    i1 := h4 * qt.float_scaling [row * JPEGSAMPLEWIDTH+1] ;
    i2 := h2 * qt.float_scaling [row * JPEGSAMPLEWIDTH+2] ;
    i3 := h6 * qt.float_scaling [row * JPEGSAMPLEWIDTH+3] ;
    i4 := h1 * qt.float_scaling [row * JPEGSAMPLEWIDTH+4] ;
    i5 := h5 * qt.float_scaling [row * JPEGSAMPLEWIDTH+5] ;
    i6 := h3 * qt.float_scaling [row * JPEGSAMPLEWIDTH+6] ;
    i7 := h7 * qt.float_scaling [row * JPEGSAMPLEWIDTH+7] ;

    output [row * JPEGSAMPLEWIDTH] := JpegCoefficient (Floor (i0 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+1] := JpegCoefficient (Floor (i1 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+2] := JpegCoefficient (Floor (i2 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+3] := JpegCoefficient (Floor (i3 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+4] := JpegCoefficient (Floor (i4 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+5] := JpegCoefficient (Floor (i5 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+6] := JpegCoefficient (Floor (i6 + 0.5)) ;
    output [row * JPEGSAMPLEWIDTH+7] := JpegCoefficient (Floor (i7 + 0.5)) ;
    End ;
  End ;

//******************************************************************************
Constructor TJpegEncoderQuantizationTable.Create ;
  Begin
  Inherited Create ;
  is_used := True ;
  End ;


//
//  Description:
//
//    This function creates the scaled quantization tables used by the fast
//    FDCT algorithm.
//
Procedure TJpegEncoderQuantizationTable.buildScaledTables ;
  Var
    ii : Cardinal ;
  Begin
  for ii := 0 To JPEGSAMPLESIZE - 1 Do
    float_scaling [ii] := floatscaling [ii] / data_values [ii] ;
  End ;

Function TJpegEncoderQuantizationTable.getValue (index : COEFFICIENTINDEX) : UBYTE2 ;
  Begin
  Result := data_values [index] ;
  End ;

Procedure TJpegEncoderQuantizationTable.setValue (index : COEFFICIENTINDEX ; value : UBYTE2) ;
  Begin
  data_values [index] := value ;
  End ;

//******************************************************************************
Procedure Print (input : TJpegEncoderDataUnit) ;
  Var
    ii, jj : Cardinal ;
  Begin
  WriteLn ('{ Data Unit') ;
  For ii := 0 To JPEGSAMPLEWIDTH - 1 Do
    Begin
    For jj := 0 To JPEGSAMPLEWIDTH - 1 Do
      Begin
      Write (input [ii, jj], ' ') ;
      End ;
    WriteLn
    End ;
  WriteLn ('}') ;
  End ;

Procedure Print (input : TJpegCoefficientBlock) ;
  Var
    II : Cardinal ;
  Begin
  WriteLn ('{ Coefficient Block') ;
  For II := Low (COEFFICIENTINDEX) To High (COEFFICIENTINDEX) Do
    Write (input [ii], ' ') ;
  WriteLn ('}') ;
  End ;

End.
