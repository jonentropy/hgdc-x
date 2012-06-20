# Run this script to generate debian and redhat packages.
# The RPM packages are created using alien.
# The control file in the current directory is used 
# in the creation of the debian packages.

#!/bin/sh
DIR="$( cd "$( dirname "$0" )" && pwd )"

if [ "$1" != "" ]; then
PackageName="Hgdc-X-$1_i386"
  echo "Building Debian package $PackageName"
  sudo rm -R /tmp/$PackageName 2>/dev/null

  mkdir /tmp/$PackageName
  mkdir /tmp/$PackageName/DEBIAN
  mkdir /tmp/$PackageName/usr
  mkdir /tmp/$PackageName/usr/bin
  cp ../../build/release/i386-linux-gtk2/hgdcx /tmp/$PackageName/usr/bin
  strip --strip-all /tmp/$PackageName/usr/bin/hgdcx


#Control
  echo "Package: Hgdc-X" > /tmp/$PackageName/DEBIAN/control
  echo "Version: $1" >> /tmp/$PackageName/DEBIAN/control

  cat control >> /tmp/$PackageName/DEBIAN/control
  
  mkdir /tmp/$PackageName/usr/share
  mkdir /tmp/$PackageName/usr/share/applications
  cp hgdcx.desktop /tmp/$PackageName/usr/share/applications
  mkdir /tmp/$PackageName/usr/share/Hgdc-X
  cp hgdcx.png /tmp/$PackageName/usr/share/Hgdc-X/hgdcx.png

  sudo chown -R root:root /tmp/$PackageName/
  sudo chmod -R 755 /tmp/$PackageName/

  dpkg-deb --build /tmp/$PackageName
  cd /tmp
  sudo alien -r /tmp/$PackageName.deb
  sudo chmod a+r /tmp/Hgdc-X-"$1"*.i386.rpm
  cd "$DIR"
  cp "/tmp/$PackageName.deb" .
  cp /tmp/Hgdc-X-"$1"*.i386.rpm .
  sudo rm -R /tmp/$PackageName
else
  echo "Usage: build_linux_packages.sh [version_number]"
fi

