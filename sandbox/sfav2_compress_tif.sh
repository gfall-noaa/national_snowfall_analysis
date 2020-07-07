#!/bin/sh

# Identify uncompressed GeoTIFF images from the National Snowfall Analysis and
# compress them. 

# Greg Fall, NOHRSC
# April 2020

# First copied all TIF data from the web server to a temporary directory
# with
#
# % mkdir /net/tmp/snowfall_v2_web_data
# % rsync -auv \
#   --exclude "*.png" --exclude "*.nc" --exclude "*.grb2" --exclude "*.xml" \
#   /net/http0/htdocs/snowfall_v2/data/ \
#   /net/tmp/snowfall_v2_web_data/

function tif_is_compressed() {
    if gdalinfo $1 | grep COMPRESSION >/dev/null ; then
        true
    else
        false
    fi
}

function tif_is_lzw_compressed() {
    if gdalinfo $1 | grep "COMPRESSION=LZW" >/dev/null ; then
        true
    else
        false
    fi
}

function tif_has_ndv() {
    if gdalinfo $1 | grep -E "[nN]o.*[dD]ata.*[vV]alue=.+" >/dev/null ; then
        true
    else
        false
    fi
}

#SFAV2_DIR=/operations/misc/snowfall_v2
#SFAV2_DIR=/net/http0/htdocs/snowfall_v2/data
SFAV2_DIR=/net/scratch/fall/operations_misc_snowfall_v2
if [ ! -d $SFAV2_DIR ] ; then
  echo "Need to create (and populate) $SFAV2_DIR" 1>&2
  exit 1
fi
#OUTPUT_DIR=/net/tmp/sfav2_tif_comp
#OUTPUT_DIR=/net/tmp/sfav2_tif_comp_web
OUTPUT_DIR=/net/scratch/fall/operations_misc_snowfall_v2_comp
if [ ! -d $OUTPUT_DIR ] ; then
  mkdir $OUTPUT_DIR
fi
cd $SFAV2_DIR
find . -name "*.tif" -print0 | while IFS= read -r -d '' FILE; do
    if [ -z "$NUM_FILES_COMPRESSED" ] ; then
        NUM_FILES_COMPRESSED=0
        TOTAL_SIZE_OLD=0
        TOTAL_SIZE_NEW=0
    fi
    if ! tif_is_lzw_compressed $FILE ; then
        # if tif_is_compressed $FILE ; then
        #     echo "need to recompress $FILE using LZW"
        # else
        #     echo "need to compress $FILE"
        # fi
        # check for a no-data value
        NDV_OPTION=
        if tif_has_ndv $FILE ; then
            NDV_STR=`gdalinfo $FILE | \
                     grep -E "[nN]o[^=]*[dD]ata[^=]*[vV]alue=.+" | \
                     sed -e 's/[^=]\+=//g'`
            #echo "(NDV is $NDV_STR)"
            NDV_OPTION=" -a_nodata $NDV_STR"
        else
            echo "WARNING: ${FILE} has no no-data value" 1>&2
        fi
    else
        #echo "do not need to compress $FILE"
        #exit 0 # should be continue
        continue
    fi
    FILE_DIR=`dirname $FILE`
    if [ ! -d "${OUTPUT_DIR}/${FILE_DIR}" ] ; then
        mkdir ${OUTPUT_DIR}/${FILE_DIR}
    fi
    if [ -f ${OUTPUT_DIR}/${FILE} ] ; then
        echo "WARNING: already have a copy of ${FILE} in ${OUTPUT_DIR}"
        continue
    fi
    cmd="gdal_translate -q -of gtiff -co COMPRESS=LZW $NDV_OPTION ${SFAV2_DIR}/${FILE} ${OUTPUT_DIR}/${FILE}"
    echo $cmd
    # continue
    $cmd
    STATUS=$?
    if [ $STATUS -ne 0 ] ; then
        echo "\"$cmd\" failed" 1>&2
        exit 1
    fi
    NUM_FILES_COMPRESSED=$((NUM_FILES_COMPRESSED+1))
    FILE_SIZE_OLD=`stat -c %s ${SFAV2_DIR}/${FILE}`
    TOTAL_SIZE_OLD=$((TOTAL_SIZE_OLD+FILE_SIZE_OLD))
    FILE_SIZE_NEW=`stat -c %s ${OUTPUT_DIR}/${FILE}`
    TOTAL_SIZE_NEW=$((TOTAL_SIZE_NEW+FILE_SIZE_NEW))
    echo "Compression of $NUM_FILES_COMPRESSED files has converted" \
         "$TOTAL_SIZE_OLD bytes" \
         "to $TOTAL_SIZE_NEW bytes."
done
