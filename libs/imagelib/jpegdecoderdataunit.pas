unit jpegdecoderdataunit;
{$DEFINE USEFLOATINGIDCT}

interface

Uses jpegpvt, jpeginputstream ;

Const
  QuantizationIntegerScale = 12 ;

Type
  JpegCoefficient = SmallInt ;
  JpegCoefficientBlock = Array [COEFFICIENTINDEX] of JpegCoefficient ;
  DecoderDataUnit = Array [DATAUNITINDEX, DATAUNITINDEX] of JPEGSAMPLE ;

  TJpegDecoderQuantizationTable = Class
    Private
      // Quantization Values in Zig-Zag Order.
      data_values : Array [COEFFICIENTINDEX] Of SmallInt ;

      // Scaled quantization values used for the fast IDCT implementations.
      float_scaling : Array [COEFFICIENTINDEX] of Double ;
      integer_scaling : Array [COEFFICIENTINDEX] of LongInt ;

      // This flag gets set to true when the quantization is defined in the
      // JPEG input stream. It is used to ensure that an compressed scan does
      // not attempt to use an undefined quantization table.
      table_defined : Boolean ;
    Public
      Constructor Create ;
      property tableDefined : boolean read table_defined ;
      // Function to read the quantization table from the input stream.
      Procedure readTable (inputstream : TJpegInputStream ;
                           precision : QUANTIZATIONPRECISION) ;
      Procedure print ;
    End ;

  Procedure inverseDCT (cb : JpegCoefficientBlock ;
                        qt : TJpegDecoderQuantizationTable ;
                        var output : DecoderDataUnit) ;

implementation

Uses
  math, jpegdecoder ;

Const
//
// This table consists of the values
//
//   F (i, j) = X (i) X (j) / 8
//
// where
//
//  X (n) = 1, n = 0, 4
//  X (n) = 1 / sqrt(2) / cos (n*PI/16)
//

  floatscaling : Array [COEFFICIENTINDEX] of Double = (
    0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,  0.125,                  0.159094822571604233,  0.2309698831278216846, 0.4530637231764438333,
    0.09011997775086849627, 0.0649728831185362593,  0.0689748448207357645,  0.07664074121909414394, 0.09011997775086849627, 0.1147009749634507608, 0.1665200058287998886, 0.3266407412190940884,
    0.09567085809127244544, 0.0689748448207357645,  0.0732233047033631207,  0.08136137691302557096, 0.09567085809127244544, 0.1217659055464329343, 0.1767766952966368932, 0.3467599613305368256,
    0.1063037618459070632,  0.07664074121909414394, 0.08136137691302557096, 0.09040391826073060355, 0.1063037618459070632,  0.135299025036549253,  0.1964237395967755595, 0.3852990250365491698,
    0.125,                  0.09011997775086849627, 0.09567085809127244544, 0.1063037618459070632,  0.125,                  0.159094822571604233,  0.2309698831278216846, 0.4530637231764438333,
    0.159094822571604233,   0.1147009749634507608,  0.1217659055464329343,  0.135299025036549253,   0.159094822571604233,   0.2024893005527218515, 0.2939689006048396558, 0.5766407412190940329,
    0.2309698831278216846,  0.1665200058287998886,  0.1767766952966368932,  0.1964237395967755595,  0.2309698831278216846,  0.2939689006048396558, 0.4267766952966368654, 0.8371526015321518744,
    0.4530637231764438333,  0.3266407412190940884,  0.3467599613305368256,  0.3852990250365491698,  0.4530637231764438333,  0.5766407412190940329, 0.8371526015321518744, 1.642133898068010689
    ) ;

 IntegerScale = 6 ;
 FC4 = 0.707106781186547524400844362104849 ; // cos (PI * 4.0 / 16.0) ;
 FSEC2 = 0.541196100146196984399723205366389 ; // 0.5 / cos (PI * 2.0 / 16.0) ;
 FSEC6 = 1.30656296487637652785664317342719 ; // 0.5 / cos (PI * 6.0 / 16.0) ;

Var
  IC4, ISEC2, ISEC6 : LongInt ;
//
//  Description:
//
//    This function descales a scaled integer value.
//
//    This implementation is simplay a shift operation. We
//    use an inline function to give one place to change in case
//    we want to switch to a rounded scale.
//
//  Parameters:
//    value: The value to descale
//    amount:  The amount to descale
//
//  Return Value:
//    The descaled value.
//
Function Descale (value : LongInt ; amount : Integer) : LongInt ;
  Var
    scale : LongInt ;
  Begin
  // A more precise value would be
  // result = (value + (1 << (amount - 1))) >> amount ;
  scale := 1 Shl amount ;
  Result := (value + (scale Div 2)) Div scale ;
  End ;

Function SampleRange (value : LongInt) : JPEGSAMPLE ;
  Begin
  if value < Low (JPEGSAMPLE) Then
    Result := Low (JPEGSAMPLE)
  else if value > High (JPEGSAMPLE) Then
    Result := High (JPEGSAMPLE)
  else
    Result := value ;
  End ;

{$IFDEF USEFLOATINGIDCT}
//
//  Description:
//
//    This is a floating point implementation of the Inverse
//    Discrete Cosine Transform (IDCT).
//
//    This implementation uses a factorization of the DCT matrix.
//    The first steps in this factorization is a matrix multiplication
//    is the multiplication of each row/column by a scale. This
//    scalor multipliation has been combined with quantization
//    to eliminate 128 multiplication steps.
//
//    We use a lot of temporaries here in order to clearly
//    show the matrix multiplication steps.  Hopefully
//    your compiler will optimize out the unnecessary
//    intermediate variables.
//
//    If your compiler does not aggressively optimize. It is possible
//    to reorder the operations to reduce the number of temporaries
//    required.
//
//  Parameters:
//    data: The 8x8 matrix to perform the IDCT on.
//    qt: The prescaled quantization table.
//
Procedure inverseDCT (cb : JpegCoefficientBlock ;
                      qt : TJpegDecoderQuantizationTable ;
                      var output : DecoderDataUnit) ;
  Const
    rounding = JPEGMIDPOINTSAMPLEVALUE + 0.5 ;
  Var
    tmp : Array [COEFFICIENTINDEX] Of Double ;
    ii : Cardinal ;
    a0, a1, a2, a3, a4, a5, a6, a7 : Extended ;
    b0, b1, b2, b3, b4, b5, b6, b7 : Extended ;
    c0, c1, c2, c3, c4, c5, c6, c7 : Extended ;
    d0, d1, d2, d3, d4, d5, d6, d7 : Extended ;
    e0, e1, e2, e3, e4, e5, e6, e7 : Extended ;
    f0, f1, f2, f3, f4, f5, f6, f7 : Extended ;
    g0, g1, g2, g3, g4, g5, g6, g7 : Extended ;
    h0, h1, h2, h3, h4, h5, h6, h7 : Extended ;
  Begin
  ii := Low (COEFFICIENTINDEX) ;
  While ii <= High (COEFFICIENTINDEX) Do
    Begin
    // This optimization does not seem to be worth the trouble in the
    // second loop.
    if ((cb [ii+1] Or cb [ii+2] Or cb [ii+3] Or cb [ii+4] Or
         cb [ii+5] Or cb [ii+6] Or cb [ii+7]) = 0) AND FALSE Then
      Begin
      tmp [ii] := cb [ii] * qt.float_scaling [ii] ;
      tmp [ii+1] := tmp [ii] ;
      tmp [ii+2] := tmp [ii] ;
      tmp [ii+3] := tmp [ii] ;
      tmp [ii+4] := tmp [ii] ;
      tmp [ii+5] := tmp [ii] ;
      tmp [ii+6] := tmp [ii] ;
      tmp [ii+7] := tmp [ii] ;
      End
    else
      Begin
      a0 := cb [ii+0] * qt.float_scaling [ii+0] ;
      a1 := cb [ii+4] * qt.float_scaling [ii+4] ;
      a2 := cb [ii+2] * qt.float_scaling [ii+2] ;
      a3 := cb [ii+6] * qt.float_scaling [ii+6] ;
      a4 := cb [ii+1] * qt.float_scaling [ii+1] ;
      a5 := cb [ii+5] * qt.float_scaling [ii+5] ;
      a6 := cb [ii+3] * qt.float_scaling [ii+3] ;
      a7 := cb [ii+7] * qt.float_scaling [ii+7] ;

      b0 := a0 ;
      b1 := a1 ;
      b2 := a2 - a3 ;
      b3 := a2 + a3 ;
      b4 := a4 - a7 ;
      b5 := a5 + a6;
      b6 := a5 - a6 ;
      b7 := a4 + a7 ;

      c0 := b0 ;
      c1 := b1 ;
      c2 := b2 ;
      c3 := b3 ;
      c4 := FSEC2 * b4 ;
      c5 := b7 - b5 ;
      c6 := FSEC6 * b6 ;
      c7 := b5 + b7 ;

      d0 := c0 ;
      d1 := c1 ;
      d2 := c2 ;
      d3 := c3 ;
      d4 := c4 + c6 ;
      d5 := c5 ;
      d6 := c4 - c6 ;
      d7 := c7 ;

      e0 := d0 + d1 ;
      e1 := d0 - d1 ;
      e2 := d2 * FC4 ;
      e3 := d3 ;
      e4 := d4 * FC4 ;
      e5 := d5 * FC4 ;
      e6 := d6 ;
      e7 := d7 ;

      f0 := e0 ;
      f1 := e1 ;
      f2 := e2 ;
      f3 := e3 ;
      f4 := e4 ;
      f5 := e5 ;
      f6 := e4 + e6 ;
      f7 := e7 ;

      g0 := f0 ;
      g1 := f1 ;
      g2 := f2 ;
      g3 := f2 + f3 ;
      g4 := f4 ;
      g5 := f4 + f5 ;
      g6 := f5 + f6 ;
      g7 := f6 + f7 ;

      h0 := g0 + g3 ;
      h1 := g1 + g2 ;
      h2 := g1 - g2 ;
      h3 := g0 - g3 ;
      h4 := g4 ;
      h5 := g5 ;
      h6 := g6 ;
      h7 := g7 ;

      tmp [ii+0] := h0 + h7 ;
      tmp [ii+1] := h1 + h6 ;
      tmp [ii+2] := h2 + h5 ;
      tmp [ii+3] := h3 + h4 ;
      tmp [ii+4] := h3 - h4 ;
      tmp [ii+5] := h2 - h5 ;
      tmp [ii+6] := h1 - h6 ;
      tmp [ii+7] := h0 - h7 ;
      End ;
    Inc (ii, JPEGSAMPLEWIDTH) ;
    End ;

  for ii := 0 To JPEGSAMPLEWIDTH - 1 Do
    Begin
    a0 := tmp [ii] ;
    a1 := tmp [ii+32] ;
    a2 := tmp [ii+16] ;
    a3 := tmp [ii+48] ;
    a4 := tmp [ii+8] ;
    a5 := tmp [ii+40] ;
    a6 := tmp [ii+24] ;
    a7 := tmp [ii+56] ;

    b0 := a0 ;
    b1 := a1 ;
    b2 := a2 - a3 ;
    b3 := a2 + a3 ;
    b4 := a4 - a7 ;
    b5 := a5 + a6;
    b6 := a5 - a6 ;
    b7 := a4 + a7 ;

    c0 := b0 ;
    c1 := b1 ;
    c2 := b2 ;
    c3 := b3 ;
    c4 := FSEC2 * b4 ;
    c5 := b7 - b5 ;
    c6 := FSEC6 * b6 ;
    c7 := b5 + b7 ;

    d0 := c0 ;
    d1 := c1 ;
    d2 := c2 ;
    d3 := c3 ;
    d4 := c4 + c6 ;
    d5 := c5 ;
    d6 := c4 - c6 ;
    d7 := c7 ;

    e0 := d0 + d1 ;
    e1 := d0 - d1 ;
    e2 := d2 * FC4 ;
    e3 := d3 ;
    e4 := d4 * FC4 ;
    e5 := d5 * FC4 ;
    e6 := d6 ;
    e7 := d7 ;

    f0 := e0 ;
    f1 := e1 ;
    f2 := e2 ;
    f3 := e3 ;
    f4 := e4 ;
    f5 := e5 ;
    f6 := e4 + e6 ;
    f7 := e7 ;

    g0 := f0 ;
    g1 := f1 ;
    g2 := f2 ;
    g3 := f2 + f3 ;
    g4 := f4 ;
    g5 := f4 + f5 ;
    g6 := f5 + f6 ;
    g7 := f6 + f7 ;

    h0 := g0 + g3 ;
    h1 := g1 + g2 ;
    h2 := g1 - g2 ;
    h3 := g0 - g3 ;
    h4 := g4 ;
    h5 := g5 ;
    h6 := g6 ;
    h7 := g7 ;

    output [Low (DATAUNITINDEX)+0][ii] := SampleRange (Round ((h0 + h7) + rounding)) ;
    output [Low (DATAUNITINDEX)+1][ii] := SampleRange (Round ((h1 + h6) + rounding)) ;
    output [Low (DATAUNITINDEX)+2][ii] := SampleRange (Round ((h2 + h5) + rounding)) ;
    output [Low (DATAUNITINDEX)+3][ii] := SampleRange (Round ((h3 + h4) + rounding)) ;
    output [Low (DATAUNITINDEX)+4][ii] := SampleRange (Round ((h3 - h4) + rounding)) ;
    output [Low (DATAUNITINDEX)+5][ii] := SampleRange (Round ((h2 - h5) + rounding)) ;
    output [Low (DATAUNITINDEX)+6][ii] := SampleRange (Round ((h1 - h6) + rounding)) ;
    output [Low (DATAUNITINDEX)+7][ii] := SampleRange (Round ((h0 - h7) + rounding)) ;
    End ;
  End ;

{$ELSE}

//
//  Description:
//
//    This is a scaled integer implementation of the Inverse
//    Discrete Cosine Transform (IDCT).
//
//    This implementation uses a factorization of the DCT matrix.
//    The first steps in this factorization is a matrix multiplication
//    is the multiplication of each row/column by a scale. This
//    scalor multipliation has been combined with quantization
//    to eliminate 128 multiplication steps.
//
//    We use a lot of temporaries here in order to clearly
//    show the matrix multiplication steps.  Hopefully
//    your compiler will optimize out the unnecessary
//    intermediate variables.
//
//    If your compiler does not aggressively optimize. It is possible
//    to reorder the operations to reduce the number of temporaries
//    required.
//
//  Parameters:
//    data: The 8x8 matrix to perform the IDCT on.
//    qt: The prescaled quantization table.
//
Procedure inverseDCT (cb : JpegCoefficientBlock ;
                      qt : TJpegDecoderQuantizationTable ;
                      var output : DecoderDataUnit) ;
  Const
    rounding = (High (JPEGSAMPLE) + 2) Shl (QuantizationIntegerScale-1) ;
  Var
    a0, a1, a2, a3, a4, a5, a6, a7 : LongInt ;
    b0, b1, b2, b3, b4, b5, b6, b7 : LongInt ;
    c0, c1, c2, c3, c4, c5, c6, c7 : LongInt ;
    d0, d1, d2, d3, d4, d5, d6, d7 : LongInt ;
    e0, e1, e2, e3, e4, e5, e6, e7 : LongInt ;
    f0, f1, f2, f3, f4, f5, f6, f7 : LongInt ;
    g0, g1, g2, g3, g4, g5, g6, g7 : LongInt ;
    h0, h1, h2, h3, h4, h5, h6, h7 : LongInt ;
    ii : Integer ;
    tmp : Array [COEFFICIENTINDEX] Of LongInt ;
  Begin

  ii := Low (COEFFICIENTINDEX) ;
  While ii <= High (COEFFICIENTINDEX) Do
    Begin
    // This optimization does not seem to be worth the trouble in the
    // second loop.
    if ((cb [ii+1] Or cb [ii+2] Or cb [ii+3] Or cb [ii+4] Or
         cb [ii+5] Or cb [ii+6] Or cb [ii+7]) = 0) Then
      Begin
      tmp [ii] := cb [ii] * qt.integer_scaling [ii] ;
      tmp [ii+1] := tmp [ii] ;
      tmp [ii+2] := tmp [ii] ;
      tmp [ii+3] := tmp [ii] ;
      tmp [ii+4] := tmp [ii] ;
      tmp [ii+5] := tmp [ii] ;
      tmp [ii+6] := tmp [ii] ;
      tmp [ii+7] := tmp [ii] ;
      End
    else
      Begin
      a0 := cb [ii+0] * qt.integer_scaling [ii+0] ;
      a1 := cb [ii+4] * qt.integer_scaling [ii+4] ;
      a2 := cb [ii+2] * qt.integer_scaling [ii+2] ;
      a3 := cb [ii+6] * qt.integer_scaling [ii+6] ;
      a4 := cb [ii+1] * qt.integer_scaling [ii+1] ;
      a5 := cb [ii+5] * qt.integer_scaling [ii+5] ;
      a6 := cb [ii+3] * qt.integer_scaling [ii+3] ;
      a7 := cb [ii+7] * qt.integer_scaling [ii+7] ;

      b0 := a0 ;
      b1 := a1 ;
      b2 := a2 - a3 ;
      b3 := a2 + a3 ;
      b4 := a4 - a7 ;
      b5 := a5 + a6;
      b6 := a5 - a6 ;
      b7 := a4 + a7 ;

      c0 := b0 ;
      c1 := b1 ;
      c2 := b2 ;
      c3 := b3 ;
      c4 := Descale (ISEC2 * b4, IntegerScale) ;
      c5 := b7 - b5 ;
      c6 := Descale (ISEC6 * b6, IntegerScale) ;
      c7 := b5 + b7 ;

      d0 := c0 ;
      d1 := c1 ;
      d2 := c2 ;
      d3 := c3 ;
      d4 := c4 + c6 ;
      d5 := c5 ;
      d6 := c4 - c6 ;
      d7 := c7 ;

      e0 := d0 + d1 ;
      e1 := d0 - d1 ;
      e2 := Descale (d2 * IC4, IntegerScale) ;
      e3 := d3 ;
      e4 := Descale (d4 * IC4, IntegerScale) ;
      e5 := Descale (d5 * IC4, IntegerScale) ;
      e6 := d6 ;
      e7 := d7 ;

      f0 := e0 ;
      f1 := e1 ;
      f2 := e2 ;
      f3 := e3 ;
      f4 := e4 ;
      f5 := e5 ;
      f6 := e4 + e6 ;
      f7 := e7 ;

      g0 := f0 ;
      g1 := f1 ;
      g2 := f2 ;
      g3 := f2 + f3 ;
      g4 := f4 ;
      g5 := f4 + f5 ;
      g6 := f5 + f6 ;
      g7 := f6 + f7 ;

      h0 := g0 + g3 ;
      h1 := g1 + g2 ;
      h2 := g1 - g2 ;
      h3 := g0 - g3 ;
      h4 := g4 ;
      h5 := g5 ;
      h6 := g6 ;
      h7 := g7 ;

      tmp [ii] := h0 + h7 ;
      tmp [ii+1] := h1 + h6 ;
      tmp [ii+2] := h2 + h5 ;
      tmp [ii+3] := h3 + h4 ;
      tmp [ii+4] := h3 - h4 ;
      tmp [ii+5] := h2 - h5 ;
      tmp [ii+6] := h1 - h6 ;
      tmp [ii+7] := h0 - h7 ;
      End ;
    Inc (ii, JPEGSAMPLEWIDTH) ;
    End ;

  for ii := 0 To JPEGSAMPLEWIDTH - 1 Do
    Begin
    a0 := tmp [ii] ;
    a1 := tmp [ii+32] ;
    a2 := tmp [ii+16] ;
    a3 := tmp [ii+48] ;
    a4 := tmp [ii+8] ;
    a5 := tmp [ii+40] ;
    a6 := tmp [ii+24] ;
    a7 := tmp [ii+56] ;

    b0 := a0 ;
    b1 := a1 ;
    b2 := a2 - a3 ;
    b3 := a2 + a3 ;
    b4 := a4 - a7 ;
    b5 := a5 + a6;
    b6 := a5 - a6 ;
    b7 := a4 + a7 ;

    c0 := b0 ;
    c1 := b1 ;
    c2 := b2 ;
    c3 := b3 ;
    c4 := Descale (ISEC2 * b4, IntegerScale) ;
    c5 := b7 - b5 ;
    c6 := Descale (ISEC6 * b6, IntegerScale) ;
    c7 := b5 + b7 ;

    d0 := c0 ;
    d1 := c1 ;
    d2 := c2 ;
    d3 := c3 ;
    d4 := c4 + c6 ;
    d5 := c5 ;
    d6 := c4 - c6 ;
    d7 := c7 ;

    e0 := d0 + d1 ;
    e1 := d0 - d1 ;
    e2 := Descale (d2 * IC4, IntegerScale) ;
    e3 := d3 ;
    e4 := Descale (d4 * IC4, IntegerScale) ;
    e5 := Descale (d5 * IC4, IntegerScale) ;
    e6 := d6 ;
    e7 := d7 ;

    f0 := e0 ;
    f1 := e1 ;
    f2 := e2 ;
    f3 := e3 ;
    f4 := e4 ;
    f5 := e5 ;
    f6 := e4 + e6 ;
    f7 := e7 ;

    g0 := f0 + rounding ;
    g1 := f1 + rounding ;
    g2 := f2 ;
    g3 := f2 + f3 ;
    g4 := f4 ;
    g5 := f4 + f5 ;
    g6 := f5 + f6 ;
    g7 := f6 + f7 ;

    h0 := g0 + g3 ;
    h1 := g1 + g2 ;
    h2 := g1 - g2 ;
    h3 := g0 - g3 ;
    h4 := g4 ;
    h5 := g5 ;
    h6 := g6 ;
    h7 := g7 ;

    output [Low (DATAUNITINDEX)][ii] := SampleRange (Descale (h0 + h7, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+1][ii] := SampleRange (Descale (h1 + h6, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+2][ii] := SampleRange (Descale (h2 + h5, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+3][ii] := SampleRange (Descale (h3 + h4, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+4][ii] := SampleRange (Descale (h3 - h4, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+5][ii] := SampleRange (Descale (h2 - h5, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+6][ii] := SampleRange (Descale (h1 - h6, QuantizationIntegerScale)) ;
    output [Low (DATAUNITINDEX)+7][ii] := SampleRange (Descale (h0 - h7, QuantizationIntegerScale)) ;
    End ;
  End ;
{$ENDIF}


Constructor TJpegDecoderQuantizationTable.Create ;
  Begin
  Inherited Create ;
  table_defined := false ;
  End ;


//
//  Description:
//
//    This function reads a quantization table from a JPEG stream.
//
//  Parameters:
//    decoder:  The JPEG decoder that owns the table and the JPEG stream.
//    precision: The quantization table precision
//
Procedure TJpegDecoderQuantizationTable.readTable (
                           inputstream : TJpegInputStream ;
                           precision : QUANTIZATIONPRECISION) ;
  Var
    ii : Integer ;
  //    This function creates scaled quantization tables that
  //    allow quantization to be merged with the IDCT process.
  //    We factor the DCT matrix so that the first step in the
  //    IDCT is to multiply each value by a constant. Here we
  //    merge that constant with the quantization table valus.
  Procedure buildScaledTables ;
    Var
      ii : Integer ;
    Begin
    For ii := Low (COEFFICIENTINDEX) To High (COEFFICIENTINDEX) Do
      Begin
      float_scaling [ii] := data_values [JpegZigZagOutputOrder [ii]] * floatscaling [ii] ;
      integer_scaling [ii] :=  Round ((1 Shl QuantizationIntegerScale)
                                   * floatscaling [ii]
                                   * data_values  [JpegZigZagOutputOrder [ii]]) ;
      End ;
    End ;
  Begin
  // B 2.4.1
  // Determine if 16-bit or 8-bit precision is used for the quantization
  // values in the file.
  Case precision Of
    1:
// Our source code only allows 8-bit data. The standard says
// 16-bit quantization tables are not allowed with 8-bit data.
// The commented code shows how 16-bit tables would be implemented.
//
//    // Read 16-bit values.
//    for (unsigned int ii = 0 ; ii < SampleSize ; ++ ii)
//    {
//      data_values[ii] = decoder.ReadWord () ;
//      if (data_values[ii] == 0)
//        throw EJpegBadData ("Zero value in quantization table") ;
//    }
      raise EJpegBadStream.Create ('Only 8-bit data is supported') ;
    0:
      Begin
      // Read 8-bit values.
      For ii := Low (COEFFICIENTINDEX) To High (COEFFICIENTINDEX) Do
        Begin
        data_values[ii] := inputstream.getByte ;
        if (data_values[ii] = 0) Then
          Raise EJpegBadStream.Create ('Zero value in quantization table') ;
        End ;
      End ;
    End ;
  buildScaledTables ;
  table_defined := true ;
  End ;

//
//  Description:
//
//   This procedure prints the values in the quantization table.
//
Procedure TJpegDecoderQuantizationTable.print ;
  Var
    ii, jj : Integer ;
  Begin
  ii := Low (COEFFICIENTINDEX) ;
  While ii <= High (COEFFICIENTINDEX) Do
    Begin
    Write ('        ') ;
    For jj := 0 To JPEGSAMPLEWIDTH - 1 Do
      Write (data_values [ii + jj], ' ') ;
    WriteLn ('') ;
    Inc (ii, JPEGSAMPLEWIDTH) ;
    End ;
  End ;

Initialization
  Begin
  IC4 := Round ((1 Shl IntegerScale) * cos (PI * 4.0/16.0)) ;
  ISEC2 := Round ((1 Shl (IntegerScale - 1)) / cos (PI * 2.0 / 16.0)) ;
  ISEC6 := Round ((1 Shl (IntegerScale - 1)) / cos (PI * 6.0 / 16.0)) ;
  End ;

end.
