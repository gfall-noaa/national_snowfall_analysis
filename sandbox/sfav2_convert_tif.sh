#!/bin/sh

# Convert GeoTIFF images in snowfall operations/web to use LZW
# compression instead of the less effective PackBits compression we have
# used previously.

# Greg Fall, NOHRSC
# July 2019

# First copied all TIF data from the web server to a temporary directory
# with
#
# % mkdir /net/tmp/snowfall_v2_web_data
# % rsync -auv \
#   --exclude "*.png" --exclude "*.nc" --exclude "*.grb2" --exclude "*.xml" \
#   /net/http0/htdocs/snowfall_v2/data/ \
#   /net/tmp/snowfall_v2_web_data/

SFAV2_DIR=/net/tmp/snowfall_v2_web_data
if [ ! -d $SFAV2_DIR ] ; then
  echo "Need to create (and populate) $SFAV2_DIR" 1>&2
  exit 1
fi
OUTPUT_DIR=/net/tmp/snowfall_v2_lzw_web_data
if [ ! -d $OUTPUT_DIR ] ; then
  mkdir $OUTPUT_DIR
fi
cd $SFAV2_DIR
#VOLUME_KIB_BEFORE=`du -s . | awk '{print $1}'`
for MONTH_DIR in [0-9][0-9][0-9][0-9][0-9][0-9] ; do
    # Only modify data through September 20118
    if [ "$MONTH_DIR" == "201810" ] ; then
        break
    fi
    echo $MONTH_DIR
    if [ ! -d ${OUTPUT_DIR}/${MONTH_DIR} ] ; then
        mkdir ${OUTPUT_DIR}/${MONTH_DIR}
    fi
    cd $MONTH_DIR
    for IMAGE in *.tif ; do
        MODIFY_TIME=`stat -c %y ${IMAGE}`
        if [ -e ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE} ] ; then
            echo "output image ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE}" \
                 "exists; skipping"
            continue
        fi
        gdal_translate -q -co "COMPRESS=lzw" \
                       ${IMAGE} ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE}
        if [ $? -ne 0 ] ; then
            echo "gdal_translate -q -co \"COMPRESS=lzw\"" \
                 "${IMAGE} ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE} failed" 1>&2
            exit 1
        fi
        if [ ! -e ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE} ] ; then
            echo "output image ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE}" \
                 "not found" 1>&2
            exit 1
        fi
        touch -d "$MODIFY_TIME" ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE}
        if gdalinfo ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE} | grep Float32 \
           > /dev/null ; then
            echo "${IMAGE} ok"
        else
            echo "output image ${OUTPUT_DIR}/${MONTH_DIR}/${IMAGE}" \
                 "has the wrong type" 1>&2
            exit 1
        fi
    done
    cd ..
done
