#!/bin/sh

# hgdc-x Mac build script
#   Generates universal binary to target Tiger 10.4 and above, Intel and PPC
#
#   Assumes all build tools are setup the way I set them up, which did require some
#     symlinks and copying of SDKs from Leopard into a Lion install, and concurrent
#     installations of XCode 4 and 3.
#
#   Copyright Tristan Linnell 2012
#  

echo "Building Hgdc-X for Mac OS X..."
echo "Building for Intel..."
lazbuild --build-all --build-mode="Release Mac OS X" hgdcx.lpi &&
echo "Building for PowerPC..." &&
lazbuild --compiler="/usr/local/bin/ppcppc" --cpu=powerpc --build-all --build-mode="Release Mac OS X" hgdcx.lpi &&
echo "Stripping binaries..." &&
strip ./build/release/powerpc-darwin-carbon/hgdcx &&
strip ./build/release/i386-darwin-carbon/hgdcx &&
echo "Making Universal Binary..." &&
lipo -create ./build/release/powerpc-darwin-carbon/hgdcx ./build/release/i386-darwin-carbon/hgdcx -output ./build/release/universal-darwin-carbon/Hgdc-X.app/Contents/MacOS/hgdcx &&
echo "Build Succeeded!"

