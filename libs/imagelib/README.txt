Colosseum Builders C++ Image Library

Copyright 1997, 1998, 1999, 2000, 2001, 2005
All Rights Reserved

Permision is granted to use, modify this software provided these conditions
are followed:

1. You understand this software comes with no warranties of any kind 
   whatsoever.
2. You assume full responsibility for the use of this software and transmit
   no liability to the authors.
3. You assume the responsility for obtaining licenses for from any patent
   holders.
4. If you distribute this code in source form, you must include this file
   unmodified and document the changes you have made.
5. If you incorporate this code in a product distributed without source code
   you state included in your documentation that it is based in part on
   software from Colosseum Builders, Inc.

THIS SOFTWARE IS PROVIDED AS IS. WE CAN'T TEST IT ON YOUR SYSTEM SO
USE AT YOUR OWN RISK. YOU HAVE THE SOURCE SO YOU HAVE NO EXCUSES.

Intro:

This archive contains the source code for the Colosseum Builders image
file library. This library is based upon the book

"Compressed Image File Formats: JPEG, PNG, GIF, XBM and BMP"
by John Miano
Addison-Wesley-Longman 1999

This library is an enhancement of the sample library that comes with the
book. While their structures are similar, this library sacrifices clarity
in some places to increase performance.

If you want to learn how the decoding and encoding processes in the library
work, you should buy a copy of this book.

Purpose:

The intent of this library is to provide a base of image file source code
that people can customize to suit their own purposes. If you simply want
to plug an image library into your application, we recommend using the
PNG reference library and the Independent JPEG Group's library, both of
which are more extensively tested.

What is In Here
- Bitmapimage: A class representing an image in 32 bpp format (RGBa). This class serves
as the main input to encoders and the main output to decoders.
- JpegDecoder: A JPEG decoder class. It supports baseline, sequential and progressive frames in
in the JFIF format. There is no support for Exif or Adobe proprietary format.
- JpegEncoder: A JPEG encoder class. It supports sequential and progressive frames.
- PngDecoder: A PNG decoder class. It should be able to read all PNG files. However, it 
only implements a subset of markers.
- PngEncoder: A PNG encoder class. Non-interleaved only at this time.
- BmpDecoder: A BMP Decoder class. Supports all BMP formats.
- BmpEncoder: A BMP Encoder class: Supports 16, 24 and 32 BPP output.

JPEG "Quality"
It has become a standard practice for JPEG encoders to have a "quality" setting. This setting 
controls the quantization tables by the compressor. Frequently, quality settings are implemented
by scaling a standard quantization table. 

This implementation does not follow that scheme but rather uses 9 standard tables and one table
consisting of all ones (no quantization). The table is selected using a "quality" value in the range
0..9. The tables are arranged such that lower quality values produce smaller images. Typically,
a quality setting of 0 gives images about 1/5 the size of quality setting 8. Setting 9 (no
quantization), tends to create files more than twice the size of setting 8.

The term "quality" is a bit of a misnomer in this implementation. A quality of setting of 0 
generally produces acceptable results. In some JPEG implementations, extremely low "quality" 
settings produce noticeably distorted images. That is not the case here.

Quality setting 3 uses the quantization tables given in the JPEG standard.

Free Pascal 2.0

Under Free Pascal 2.0, this code does not work with the "Level 2 Optimations" selected unless 
"Use register-variables" is selected as well. 


Changes:

Version 4.3 August 2005

o Fixed bug in block filter upsampling for Delphi.
o Eliminated the check for JFIF in the JPEG decoder.
o Allow 4-component JPEGs. Treated as Adobe CMYK and converted to RGB.
o Added GIF decoder.

Version 4.2 May 2005

o JPEG: Changed the usage of "quality" and quantization tables. Now, the quality values select ]
different standard quantization tables. As such, there is no correlation between higher 
(or lower) quality values and greater preservation of the original image.

I expect this to be an area where users may wish to modify the source code.

o PNG: Fixed a bug in the compressor that allowed access to value in the LZ window that had 
not been written to.

o Made the DELPHI code compatible with Free Pascal (www.freepascal.org).

o Added the programs  JpgToBmp, JpgToPng,  PngToBmp and  PngToJpg in the DELPHI 
directory as examples user use with Free Pascal.

Version 4.1 December 2001

o Bug Fixes in gamma correction in the BCB Viewer and Gray scale JPEG.
o Bug Fixed in sequental JPEG for Delphi 

Version 4.0 July 2001

o Added support for Delphi. This source code should be considered experimental.
o Fixed PNG problems introduced in the last version.

Tested on Borland C++Builder V5 and Borland Delphi V6.

1-November-2000

Changed PNGDECODER.CPP to correct a bug handling in interlaced RGBa images.
                                             
25-April-2000

Corrected problems with PNG files containing uncompressed blocks and with BMP files of
less than 8 bits per pixel.

14-January-2000

The JPEG encoder now uses predefined quantization tables rather than tables
that have been scaled from the sample table in the JPEG standard. The tables
are still selected using a quality factor in the range 1-100. However, there
no longer 100 different tables. Quality 2 is the same as quality 3.

In the future we would like to come up with a mechanism for directly selecting
the quantization tables by component. We just have not come up with a good
user interface. If you come up with a mechanism it would be easy to implement
yourself.

The decoder implements filtering for downsampled components. Filtering
eliminates the blocking effects created when using low sampling frequencies
for the Cb and Cr components. On the other hand, if you have an image with
where blocks are part of the image, filtering can cause unnatural blurring.

Filtering is disabled by default. The sample JPEG decoder uses the -f switch to enable
filtering.

3-January-2000

The BitmapImage class now always uses 32-bits to represent a pixel. This
allows us to have an Alpha channel and, in the future, four-color component
colorspaces (eg CMYK). Color quantization was removed. Support for
alpha-merging images was added.

The BmpDecoder class can now read 16 and 32-bit images.

The BmpEncoder no longer creates image with fewer than 16-bits per pixel. The
setBitsPerPixel member function can set the number of bits used to 16, 24, or
32.  24-bit images use the widely used BITMAPINFOHEADER. The other sizes
use the newer BITMAPV4HEADER that is supported on Win95 and later.

Note that in typical Microsoft fashion, support for the newer bitmap format
is inconsistent. While the Win32 API supports the new bitmap header format
most Microsoft image applications appear not to. So you can view images
with the new format in your own applications that use the API, you may
have a hard time finding other applications that can view them.

The PngEncoder has the setUseAlphaChannel function that enables or disables
the creation of an Alpha channel.

The PngDecoder class now stores the Alpha channel in the BitmapImage object.

Building:

The directories BC and MSVC contain procedures for building the library using 
Borland C++Builder and MSVC++ respectively. 

For other compilers you will have to create your own procedure and you will 
probably need to make changes to the source code as well.

If you are using Borland C++ V5 (Not Borland C++Builder), the problem you will 
encounter is that standard I/O is not included in the namespace std. You will 
have to edit the code to remove "std::" qualifiers from these declarations.

The changes required for GNU vary among systems an implementations. The main
problems deal with include files and standard classes (notably exceptions).

If you build an application using the libimage.lib object library you need
to ensure that you compile the library using the same compiler options for
both the application and library. Borland C++Bulder V5 introduced many
problems because of differences between the command line and IDE compiler
options.

We have attempted to move the library closer to the ANSI C++ standard.
Unfortunately Microsoft apparently sees no need to conform to the standard.
Getting something to work with any other compiler and Visual C++ is difficult.
The MICROSOFT subdirectory contains replacement for files that VC++ chokes on
but shouldn't.


Furture Directions:

We would like to have a completely ANSI C++ version of the library. Right now
our major obstacle is Microsoft's...err...liberal interpretation of the
standard. If the next version of the compiler is not ANSI compliant we
may get tired ofwaiting and abandon VC++ support.

We are also planning to add formal Linux support some time this year.

We are limited in the platforms an compilers we can support by those that
we have available (Windows and Linux) and the compilers we have available
to us.

Other enhancements are likely to come as a result of writing efforts or
consulting work.

Send Questions or comments to

info@colosseumbuilders.com

Send Bug Reports to:

imagebugs@colosseumbuilders.com


