unit bmppvt;

interface

const
  BITSPERBYTE = 8 ;
  BI_RGB = 0;
  BI_RLE8 = 1;
  BI_RLE4 = 2;
  BI_BITFIELDS = 3;

type
  DWORD = Cardinal ;
  FXPT2DOT30 = Longint;

  RLEOPCODE = Packed Record
    count, command : Byte ;
    End ;
  BITMAPFILEHEADER = packed record
    bfType: Word;
    bfSize: Cardinal ;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Cardinal ;
  End;

  BITMAPCOREHEADER = packed record
    bcSize: DWORD ;
    bcWidth: Word;
    bcHeight: Word;
    bcPlanes: Word;
    bcBitCount: Word;
  end;

  RGBTRIPLE = packed record
    rgbtBlue: Byte;
    rgbtGreen: Byte;
    rgbtRed: Byte;
  end;

  BITMAPINFOHEADER = packed record
    biSize: DWORD;
    biWidth: Longint;
    biHeight: Longint;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: DWORD;
    biSizeImage: DWORD;
    biXPelsPerMeter: Longint;
    biYPelsPerMeter: Longint;
    biClrUsed: DWORD;
    biClrImportant: DWORD;
  end;

  TCIEXYZ = packed record
    ciexyzX: FXPT2DOT30;
    ciexyzY: FXPT2DOT30;
    ciexyzZ: FXPT2DOT30;
  end;


  TCIEXYZTRIPLE = packed record
    ciexyzRed: TCIEXYZ;
    ciexyzGreen: TCIEXYZ;
    ciexyzBlue: TCIEXYZ;
  end;

  BITMAPV4HEADER = packed record
    bV4Size: DWORD;
    bV4Width: Longint;
    bV4Height: Longint;
    bV4Planes: Word;
    bV4BitCount: Word;
    bV4V4Compression: DWORD;
    bV4SizeImage: DWORD;
    bV4XPelsPerMeter: Longint;
    bV4YPelsPerMeter: Longint;
    bV4ClrUsed: DWORD;
    bV4ClrImportant: DWORD;
    bV4RedMask: DWORD;
    bV4GreenMask: DWORD;
    bV4BlueMask: DWORD;
    bV4AlphaMask: DWORD;
    bV4CSType: DWORD;
    bV4Endpoints: TCIEXYZTriple;
    bV4GammaRed: DWORD;
    bV4GammaGreen: DWORD;
    bV4GammaBlue: DWORD;
  end;


implementation

end.
