#!/bin/sh

# Find out if a TIFF is compressed.

if gdalinfo $1 | grep COMPRESSION >/dev/null ; then
  echo "$1 IS COMPRESSED!!!"
  exit 0
else
  echo "need to compress $1"
  exit 0
fi
