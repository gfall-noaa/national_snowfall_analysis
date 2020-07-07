#!/bin/sh

# Verify data integrity for GeoTIFF files compressed with
# sfav2_compress_tif.sh

# Uses gdal_calc.py, part of the gdal-python package, only runs on
# development.

# Greg Fall, NOAA/NWS/OWP/NOHRSC
# April 2020

function tif_is_lzw_compressed() {
    if gdalinfo $1 | grep "COMPRESSION=LZW" >/dev/null ; then
        true
    else
        false
    fi
}

function tif_is_float32() {
    if gdalinfo $1 | grep "Type=Float32" >/dev/null ; then
        true
    else
        false
    fi
}

function tif_is_float64() {
    if gdalinfo $1 | grep "Type=Float64" >/dev/null ; then
        true
    else
        false
    fi
}

UNIQ_STR=`date -u +%Y%m%d%H%M%S`.$$
ORIG_DIR=/net/scratch/fall/operations_misc_snowfall_v2
if [ ! -d $ORIG_DIR ] ; then
  echo "Need to create (and populate) $ORIG_DIR" 1>&2
  exit 1
fi
COMP_DIR=/net/scratch/fall/operations_misc_snowfall_v2_comp
if [ ! -d $COMP_DIR ] ; then
  echo "Need to create (and populate) $COMP_DIR" 1>&2
  exit 1
fi
cd $ORIG_DIR
find . -name "*.tif" -print0 | while IFS= read -r -d '' FILE; do
    if [ -z "$NUM_FILES_COMPRESSED" ] ; then
        NUM_FILES_COMPRESSED=0
        TOTAL_SIZE_OLD=0
        TOTAL_SIZE_NEW=0
    fi
    # Date check.
    # DIR=`dirname $FILE`
    # DATE=${DIR#./sfav2_}
    # if [ "${DATE:0:7}" != "2017122" ] && [ "${DATE:0:7}" != "2017123" ] ; then
    #     continue
    # fi
    # Confirm original file band 1 data is "Float32"
    ORIG_TIF_BITS=4
    if ! tif_is_float32 $FILE ; then
        if tif_is_float64 $FILE ; then
            ORIG_TIF_BITS=8
            echo "WARNING: ${ORIG_DIR}/${FILE} is Float64;" \
                 "Float32 expected." 1>&2
        else
            echo "ERROR: ${ORIG_DIR}/${FILE} is not Float32" 1>&2
            exit 1
        fi
    fi
    # If the file in ORIG_DIR is LZW compressed confirm there is no
    # counterpart in COMP_DIR.
    if tif_is_lzw_compressed $FILE ; then
        if [ -f ${COMP_DIR}/${FILE} ] ; then
            echo "ERROR: Copy of compressed ${FILE} found in ${COMP_DIR}" \
                1>&2
            exit 1
        fi
        continue
    fi
    # File in ORIG_DIR is not LZW compressed. Confirm counterpart exists.
    if [ ! -f ${COMP_DIR}/${FILE} ] ; then
        echo "ERROR: No copy of ${FILE} found in ${COMP_DIR}" 1>&2
        exit 1
    fi
    # Confirm counterpart is LZW compressed.
    if ! tif_is_lzw_compressed ${COMP_DIR}/${FILE} ; then
        echo "ERROR: Copy of ${FILE} in ${COMP_DIR} is not LZW compressed" \
            1>&2
        exit 1
    fi
    if [ $ORIG_TIF_BITS -eq 4 ] ; then
        # Confirm counterpart band 1 data is "Float32".
        if ! tif_is_float32 ${COMP_DIR}/${FILE} ; then
            echo "ERROR: Copy of ${FILE} in ${COMP_DIR} is not Float32" 1>&2
            exit 1
        fi
    fi
    if [ $ORIG_TIF_BITS -eq 8 ] ; then
        # Confirm counterpart band 1 data is "Float64".
        if ! tif_is_float64 ${COMP_DIR}/${FILE} ; then
            echo "ERROR: Copy of ${FILE} in ${COMP_DIR} is not Float64" 1>&2
            exit 1
        fi
    fi
    # Verify geospatial information matches.
    gdalinfo ${FILE} \
        | grep -v "Files:" \
        | grep -v "COMPRESSION" > /net/tmp/${UNIQ_STR}.orig
    gdalinfo ${COMP_DIR}/${FILE} \
        | grep -v "Files:" \
        | grep -v "COMPRESSION=LZW" > /net/tmp/${UNIQ_STR}.comp
    if ! diff --brief /net/tmp/${UNIQ_STR}.orig /net/tmp/${UNIQ_STR}.comp ; \
        then
        echo "ERROR: ${ORIG_DIR}/${FILE} and ${COMP_DIR}/${FILE}" \
            "have conflicting geospatial information" 1>&2
        diff /net/tmp/${UNIQ_STR}.orig /net/tmp/${UNIQ_STR}.comp
        exit 1
    fi
    rm -f /net/tmp/${UNIQ_STR}.orig /net/tmp/${UNIQ_STR}.comp
    # Compare grids.
    gdal_calc.py -A ${ORIG_DIR}/${FILE} \
                 -B ${COMP_DIR}/${FILE} \
                 --outfile=/net/tmp/${UNIQ_STR}.tif \
                 --calc="A-B" \
                 --NoDataValue=-99999.0 \
                 --type=Float32 >/dev/null
    if [ $? -ne 0 ] ; then
        echo "ERROR: gdal_calc.py failed on ${ORIG_DIR}/${FILE}" 1>&2
        exit 1
    fi
    if [ ! -f /net/tmp/${UNIQ_STR}.tif ] ; then
        echo "ERROR: Missing output TIFF /net/tmp/${UNIQ_STR}.tif" 1>&2
        exit 1
    fi
    DIFF_MIN=`gdalinfo -mm -nomd /net/tmp/${UNIQ_STR}.tif | \
              grep "Computed Min/Max=" | \
              sed 's/.*Computed\ Min\/Max=//g' | \
              sed 's/,.*//g'`
    DIFF_MAX=`gdalinfo -mm -nomd /net/tmp/${UNIQ_STR}.tif | \
              grep "Computed Min/Max=" | \
              sed 's/.*Computed\ Min\/Max=//g' | \
              sed 's/.*,//g'`
    # Compare the values.
    EQUAL=`echo "1 st ${DIFF_MIN} ${DIFF_MAX} =t \
                 0 sf ${DIFF_MIN} ${DIFF_MAX} !=f p" | dc`
    if [ $EQUAL -eq 0 ] ; then
        echo "ERROR: Non-compressed ${ORIG_DIR}/${FILE} and" \
             "${COMP_DIR}/${FILE} do not match" 1>&2
        exit 1
    fi
    rm -f /net/tmp/${UNIQ_STR}.tif
    echo "$FILE compression ok"
done
