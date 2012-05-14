About hgdc-x
============

hgdc-x is a cross-platform desktop client for the Hackathon Gunther Daemon music player project.
It requires you to be running an hgd server on your local network. The official github repository for the server can be found at https://github.com/vext01/hgd.

hgdc-x is a feature complete GUI client for hgd, including SSL encryption and administrative commands. It is written in cross-platform Lazarus/Freepascal and is designed to target Linux (GTK or QT), Mac OS X (Carbon) and Windows (Win32 or QT). Other targets, for example FreeBSD are possible but untested.


Building
========

hgdc-x will build with no additional libraries (except for synapse, which is statically linked and included in the source tree), with Lazarus 0.9.30 or later, which can be downloaded from the website at http://lazarus.freepascal.com


Binaries
========

Pre-compiled binaries and a Windows installer for each release are available from the "Downloads" tab above.
To use SSL you will need OpenSSL libraries installed, and if using the QT version you will need libQT4pas.
On Windows, you can download the installer package which includes the OpenSSL libraries.

The offical website is at http://hgdcx.canthack.org, where an Ubuntu PPA for hgdc-x as well as the main hgd server project can be found.


Licensing
=========

hgdc-x is licensed with a permissive BSD style licence, please refer to the COPYING file for more details.

Icons are from the Fat Cow icon set, which is Creative Commons licensed and can be found at http://www.fatcow.com/free-icons.


Support/Develop
===============

There is an IRC room at #hgdcx on irc.freenode.net. I can be found in there for any questions or conversation about hgdc-x. Commits are also posted there as they happen.


Acknowledgements
================

Thanks to the hgd developers, Edd and Mex for such a great music daemon, and hence the opportunity for this client to exist.
Thanks to Hannah for "GUI consultancy" and Ed for Mac OS X testing.

Copyright (C) Tristan Linnell 2012
http://hgdcx.canthack.org
http://canthack.org


Donate
======

If you find hgdc-x useful, please buy me a beer! :)
[![Buy me a beer, Flattr hgdc-x](http://api.flattr.com/button/flattr-badge-large.png)](http://flattr.com/thing/401840/hgdc-x)
